#![ allow (non_camel_case_types) ]

use std::collections::HashSet;
use std::ops::Deref;
use std::sync::{Arc, RwLock};

use strum_macros::EnumIter;

use crate::{*, ModeState_T::*};





# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter) ]
/// All the supported mode-states, (whether they have triggering keys registered or not)
pub enum ModeState_T {
    no_ms,  // no_ms can be useful to fill in fns set to take somethhing .. its ignored at its not in bitmaps
    sel, del, word, fast,
    qks, qks1, qks2, qks3,
    mngd_ctrl_dn, rght_ms_scrl,
}





# [ derive (Debug) ]
/// ModeState representation for mode-flags (and any associated trigger keys they have)
pub struct _ModeState {
    pub ms_t     : ModeState_T,
        key      : Arc <RwLock <Option<KbdKey>>>,
    pub down     : Flag,
    pub consumed : Flag,
}

# [ derive (Debug, Clone) ]
/// Implements the (Arc wrapped) ModeState functionality
pub struct ModeState ( Arc <_ModeState> );

impl Deref for ModeState {
    type Target = _ModeState;
    fn deref(&self) -> &_ModeState { &self.0 }
}





# [ derive (Debug) ]
/// Holds all the ModeStates together, common functionality is impld here
pub struct _ModeStates {
    _private : (),
    // l2 mode states
    pub sel  : ModeState,
    pub del  : ModeState,
    pub word : ModeState,
    pub fast : ModeState,
    // quick-keys mode states
    pub qks  : ModeState,
    pub qks1 : ModeState,
    pub qks2 : ModeState,
    pub qks3 : ModeState,
    // then the computed flags
    pub some_l2_mode_active   : Flag,
    pub some_qks_mode_active  : Flag,
    pub some_combo_mode_active: Flag,

    // we'll also maintain a set of our registered mode-trigger-keys for quick lookup
    mode_keys : Arc <RwLock <HashSet <KbdKey>>>,

}

# [ derive (Debug, Clone) ]
/// Implements the (Arc wrapped) ModeStates-holder functionality
pub struct ModeStates ( Arc <_ModeStates> );

impl Deref for ModeStates {
    type Target = _ModeStates;
    fn deref(&self) -> &_ModeStates { &self.0 }
}








/// Implements the (Arc wrapped) ModeState functionality
impl ModeState {

    pub fn new (ms_t: ModeState_T) -> ModeState {
        ModeState ( Arc::new ( _ModeState {
            ms_t,
            key      : Arc::new ( RwLock::new(None) ),
            down     : Flag::default(),
            consumed : Flag::default(),
        } ) )
    }

    /// mark the mode-key consumed by mode-action (so further inputs will be ignored until its released.. helps avoid straggling key events)
    pub fn mode_key_consuming_action (&self, af:AF) -> AF {
        let ms = self.clone();
        Arc::new ( move || { ms.consumed.set(); af(); } )
    }

    /// get a copy of the registered key as option if set
    pub fn key (&self) -> Option<KbdKey> {
        self.key.read().unwrap().clone()
    }

    /// registration fn is private so we dont do it from outside MSS (where we can add the key to registered keys set)
    fn register_key (&self, key:KbdKey) {
        *self.key.write().unwrap() = Some(key);
    }


    /// Binds mode-key-down event on registered mod-key to flag update action (and disables key-repeats if the mode-key-dn is 'consumed')
    fn bind_mode_key_down (&self, k:&Krusty) {
        use crate::{EventPropagationDirective::*, KbdEventCbMapKeyType::*, KbdEvCbComboProcDirective::*, KbdEventCallbackFnType::*};
        let (ms, mss) = (self.clone(), k.ks.mode_states.clone());
        let mss_cba : AF = {
            if ModeStates::static_l2_modes() .contains(&self.ms_t) {
                Arc::new ( move || { mss.some_l2_mode_active.set();  mss.some_combo_mode_active.set(); } )
            } else if ModeStates::static_qks_modes() .contains(&self.ms_t) {
                Arc::new ( move || { mss.some_qks_mode_active.set(); mss.some_combo_mode_active.set(); } )
            } else if ModeStates::static_combo_modes() .contains(&self.ms_t) {
                Arc::new ( move || { mss.some_combo_mode_active.set(); } )
            } else { Arc::new (move || { }) }
        };
        // note that these should be inline so the flags are certain to be set by the time combo-processing for this key happens
        let cb = KbdEvCbFn_InlineCallback ( Arc::new ( move |_| {
            ms.down.set(); mss_cba();
            if ms.consumed.check() { KbdEvProcDirectives::new (EventProp_Stop, ComboProc_Disable) }
            else { KbdEvProcDirectives::new (EventProp_Continue, ComboProc_Enable) }
        } ) );
        let event_proc_d = KbdEvProcDirectives::new (EventProp_Undetermined, ComboProc_Undetermined);
        if let Some(key) = self.key() {
            k.iproc.kbd_bindings .bind_kbd_event ( key, KeyEventCb_KeyDown, KbdEventCallbackEntry { event_proc_d, cb } );
        }
    }

    /// Binds mode-key-up event on registered mod-key to flag update action
    fn bind_mode_key_up (&self, k:&Krusty) {
        use crate::{EventPropagationDirective::*, KbdEventCbMapKeyType::*, KbdEvCbComboProcDirective::*, KbdEventCallbackFnType::*};
        let (ms, mss) = (self.clone(), k.ks.mode_states.clone());
        let mss_cba : AF = {
            if      ModeStates::static_l2_modes()    .contains(&self.ms_t) { Arc::new ( move || mss.refresh_l2_mode_active_flag() ) }
            else if ModeStates::static_qks_modes()   .contains(&self.ms_t) { Arc::new ( move || mss.refresh_qks_mode_active_flag() ) }
            else if ModeStates::static_combo_modes() .contains(&self.ms_t) { Arc::new ( move || mss.refresh_caps_mode_active_flag() ) }
            else { Arc::new ( || { } ) }
        };
        let event_proc_d = KbdEvProcDirectives::new (EventProp_Continue, ComboProc_Enable);
        let cb = KbdEvCbFn_InlineCallback ( Arc::new ( move |_| {
            ms.down.clear(); ms.consumed.clear(); mss_cba();
            event_proc_d
        } ) );
        if let Some(key) = self.key() {
            k.iproc.kbd_bindings .bind_kbd_event ( key, KeyEventCb_KeyUp, KbdEventCallbackEntry { event_proc_d, cb } );
        }
    }


    /// For mode-key btns (in addition to any combo maps action) we'll want individual binding callbacks that update flags.
    /// Note that after these binding callbacks process, they will still go through bulk processing for their default/combo actions.
    /// (This is as opposed to default-keys/combos that are handled in bulk w/o individual callback bindings)
    pub fn bind_mode_key_action (&self, k:&Krusty) {
        self.bind_mode_key_down(k);
        self.bind_mode_key_up(k);
    }

}





/// Implements the (Arc wrapped) ModeStates-holder functionality
impl ModeStates {

    pub fn new() -> ModeStates {
        let (_sel, _del,  _word, _fast) = (ModeState::new(sel), ModeState::new(del), ModeState::new(word), ModeState::new(fast));
        let (_qks, _qks1, _qks2, _qks3) = (ModeState::new(qks), ModeState::new(qks1), ModeState::new(qks2), ModeState::new(qks3));
        let (some_l2, some_qks, some_cmk) = (Flag::default(), Flag::default(), Flag::default() );

        ModeStates ( Arc::new ( _ModeStates {
            _private : (),
            sel: _sel, del:  _del,  word: _word, fast: _fast,
            qks: _qks, qks1: _qks1, qks2: _qks2, qks3: _qks3,
            some_l2_mode_active: some_l2, some_qks_mode_active: some_qks, some_combo_mode_active: some_cmk,
            mode_keys: Arc::new (RwLock::new (HashSet::new())),
        } ) )
    }

    // we'll just define all enum subsets we need rather than trying to partly/fully iterating through ModeState_T
    pub fn static_l2_modes () -> [ModeState_T;4] {
        static L2_MODES : [ModeState_T;4]  = [sel, del, word, fast];
        L2_MODES
    }
    pub fn static_qks_modes () -> [ModeState_T;4] {
        static QKS_MODES : [ModeState_T;4] = [qks, qks1, qks2, qks3];
        QKS_MODES
    }
    pub fn static_l2_qks_modes () -> [ModeState_T;8] {
        // NOTE that this will be our source of ordering for the mode-states bits in combo bitmap!
        static L2_QKS_MODES : [ModeState_T;8]  = [sel, del, word, fast, qks, qks1, qks2, qks3];
        L2_QKS_MODES
    }
    pub fn static_combo_modes() -> [ModeState_T;9] {
        // note that this also includes mngd_ctrl_dn which is a flag in KrS (i.e. not managed here)
        static COMBO_MODES: [ModeState_T;9] = [sel, del, word, fast, qks, qks1, qks2, qks3, mngd_ctrl_dn];
        COMBO_MODES
    }


    pub fn mode_flag_pairs (&self) -> [(ModeState_T, &ModeState);8] { [
        // NOTE that the ordering here MUST match that given by the static_l2_qks_modes above
        // .. as this is what we will use to populate the combo bitmap and compare to current combo-mode-states!
        (sel, &self.sel), (del,  &self.del),  (word, &self.word), (fast, &self.fast),
        (qks, &self.qks), (qks1, &self.qks1), (qks2, &self.qks2), (qks3, &self.qks3),
    ] }

    pub fn get_cur_mode_states_bitmap (&self) -> ComboStatesBits_Modes {
        self.mode_flag_pairs() .map (|(_,ms)| ms.down.check())
    }
    pub fn make_combo_mode_states_bitmap (modes:&[ModeState_T]) -> ComboStatesBits_Modes {
        ModeStates::static_l2_qks_modes() .map (|ms| modes.contains(&ms))
    }

    pub fn register_mode_key (&self, key:Key, ms_t:ModeState_T) {
        if let Some(ms) = self.get_mode_flag(ms_t) {
            ms.register_key(key);
            self.mode_keys.write().unwrap().insert(key);
        }
    }
    pub fn check_if_mode_key (&self, key:Key) -> bool {
        // this check needs to happen at runtime, so maintaining a small hashmap to do it fast rather than iterating through flags
        self.mode_keys.read().unwrap().contains(&key)
    }
    pub fn get_mode_flag (&self, mst:ModeState_T) -> Option<&ModeState> {
        self.mode_flag_pairs() .iter() .find (|(ms_t,_)| *ms_t == mst) .map(|(_,ms)| *ms)
    }
    pub fn get_mode_t (&self, key:Key) -> Option<ModeState_T> {
        self.mode_flag_pairs() .iter() .find (|(_, ms)| ms.key.read().unwrap().filter(|&k| k==key).is_some()) .map (|(ms_t,_)| *ms_t)
    }
    pub fn mode_key_consuming_action (&self, ms_t:ModeState_T, af:AF) -> AF {
        if let Some(ms) = self.get_mode_flag(ms_t) { ms.mode_key_consuming_action(af) }
        else { af }
    }

    pub fn refresh_qks_mode_active_flag (&self) {
        if !self.qks.down.check() && !self.qks1.down.check() && !self.qks2.down.check() && !self.qks3.down.check() {
            self.some_qks_mode_active.clear()
        }
        self.refresh_caps_mode_active_flag();
    }
    pub fn refresh_l2_mode_active_flag (&self) {
        if !self.sel.down.check() && !self.del.down.check() && !self.word.down.check() && !self.fast.down.check() {
            self.some_l2_mode_active.clear()
        }
        self.refresh_caps_mode_active_flag();
    }
    pub fn refresh_caps_mode_active_flag (&self) {
        if !self.some_qks_mode_active.check() && !self.some_l2_mode_active.check() {
            self.some_combo_mode_active.clear()
        }
    }
    pub fn clear_flags (&self) {
        self.mode_flag_pairs() .iter() .for_each (|(_,ms)| ms.down.clear());
        self.some_l2_mode_active.clear(); self.some_qks_mode_active.clear(); self.some_combo_mode_active.clear();
    }

    pub fn bind_mode_keys_actions (&self, k:&Krusty) {
        self.mode_flag_pairs() .iter() .for_each (|(_,ms)| ms.bind_mode_key_action(k))
    }

}







