#![ allow (non_camel_case_types) ]

use std::sync::Arc;
use std::mem::size_of;
use atomic_refcell::AtomicRefCell;

use derive_deref::Deref;
use rustc_hash::FxHashSet;
use strum_macros::EnumIter;

use crate::{*, ModeState_T::*};





# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter) ]
/// All the supported mode-states, (whether they have triggering keys registered or not)
pub enum ModeState_T {
    no_ms,  // no_ms can be useful to fill in fns set to take somethhing .. its ignored at its not in bitmaps
    sel, del, word, fast,
    qks, qks1, qks2, qks3,
    latch_1, latch_2, latch_3, latch_4,
    //mngd_ctrl_dn, //ctrl_tab_scrl, //right_ms_scrl,
    // note: ^^ want minimal flags use here, as we dont want a set flag to change the combo state so other combos w/o flags get invalidated
}





# [ derive (Debug) ]
/// ModeState representation for mode-flags (and any associated trigger keys they have)
/// Note that key triggered mode-states are active ONLY while the assigned key is held down
pub struct _ModeState {
    // Note that we'll use AtomicRefCell instead of Arc-RwLock for the key, as runtime should have no writes to it (after initial setup)
    pub ms_t     : ModeState_T,
        key      : AtomicRefCell <Option<KbdKey>>,
    pub down     : Flag,
    pub consumed : Flag,
}

# [ derive (Debug, Clone, Deref) ]
/// Implements the (Arc wrapped) ModeState functionality
pub struct ModeState ( Arc <_ModeState> );




# [ derive (Debug) ]
/// LatchState representation for mode-flags (and any associated trigger keys they have)
/// Note that key triggered latch-states are TOGGLED upon assigned key presses
pub struct _LatchState {
    pub ms_t     : ModeState_T,     // we'll just add to the ModeState enum to keep usage simpler
        key      : AtomicRefCell <Option<KbdKey>>,
    pub active   : Flag,
}

# [ derive (Debug, Clone, Deref) ]
/// Implements the (Arc wrapped) ModeState functionality
pub struct LatchState ( Arc <_LatchState> );





# [ derive (Debug) ]
/// Holds all the ModeStates together, common functionality is impld here
pub struct ModeStates {
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
    // latching layer states
    pub latch_1 : LatchState,
    pub latch_2 : LatchState,
    pub latch_3 : LatchState,
    pub latch_4 : LatchState,
    // then the computed flags
    pub some_l2_mode_active     : Flag,
    pub some_qks_mode_active    : Flag,
    pub some_combo_mode_active  : Flag,
    //pub some_latch_state_active : Flag,

    // we'll also maintain a set of our registered mode-trigger-keys for quick lookup
    // NOTE: we're using AtomicRefCell instead of Arc-RwLock as there should be no writes are runtime (after initial setup)
    mode_keys : AtomicRefCell <FxHashSet <KbdKey>>,
    //latch_keys : AtomicRefCell <FxHashSet <KbdKey>>,     // no runtime need yet to check against these

}









/// Implements the (Arc wrapped) ModeState functionality
impl ModeState {

    pub fn new (ms_t: ModeState_T) -> ModeState {
        ModeState ( Arc::new ( _ModeState {
            ms_t,
            key      : AtomicRefCell::new(None),
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
        self.key.borrow().clone()
    }

    /// registration fn is private so we dont do it from outside MSS (where we can add the key to registered keys set)
    fn register_key (&self, key:KbdKey) {
        *self.key.borrow_mut() = Some(key);
    }


    /// Binds mode-key-down event on registered mod-key to flag update action (and disables key-repeats if the mode-key-dn is 'consumed')
    fn bind_mode_key_down (&self, k:&Krusty) {
        use crate::{EvProp_D::*, KbdEvCbMapKey_T::*, ComboProc_D::*, EvCbFn_T::*, EvCbMapKey_Action::*};
        let (ms, ks) = (self.clone(), k.ks.clone());
        let mss_cba : AF = {
            if ModeStates::static_l2_modes() .contains(&self.ms_t) {
                Arc::new ( move || { ks.mode_states.some_l2_mode_active.set();  ks.mode_states.some_combo_mode_active.set(); } )
            } else if ModeStates::static_qks_modes() .contains(&self.ms_t) {
                Arc::new ( move || { ks.mode_states.some_qks_mode_active.set(); ks.mode_states.some_combo_mode_active.set(); } )
            } else if ModeStates::static_combo_modes() .contains(&self.ms_t) {
                Arc::new ( move || { ks.mode_states.some_combo_mode_active.set(); } )
            } else { Arc::new (move || { }) }
        };
        // note that these should be inline so the flags are certain to be set by the time combo-processing for this key happens
        let cb = EvCbFn_Inline( Arc::new ( move |_| {
            ms.down.set(); mss_cba();
            if ms.consumed.is_set() { EvProc_Ds::new (EvProp_Stop, ComboProc_Disable) }
            else { EvProc_Ds::new (EvProp_Continue, ComboProc_Enable) }
        } ) );
        let ev_proc_ds = EvProc_Ds::new (EvProp_Undet, ComboProc_Undet);
        if let Some(key) = self.key() {
            k.iproc.input_bindings .bind_kbd_event (key, KeyEventCb(KeyEventCb_KeyDown), EvCbEntry { ev_proc_ds, cb } );
        }
    }

    /// Binds mode-key-up event on registered mod-key to flag update action
    fn bind_mode_key_up (&self, k:&Krusty) {
        use crate::{EvProp_D::*, KbdEvCbMapKey_T::*, ComboProc_D::*, EvCbFn_T::*, EvCbMapKey_Action::*};
        let (ms, ks) = (self.clone(), k.ks.clone());
        let mss_cba : AF = {
            if      ModeStates::static_l2_modes()    .contains(&self.ms_t) { Arc::new ( move || ks.mode_states.refresh_l2_mode_active_flag() ) }
            else if ModeStates::static_qks_modes()   .contains(&self.ms_t) { Arc::new ( move || ks.mode_states.refresh_qks_mode_active_flag() ) }
            else if ModeStates::static_combo_modes() .contains(&self.ms_t) { Arc::new ( move || ks.mode_states.refresh_caps_mode_active_flag() ) }
            else { Arc::new ( || { } ) }
        };
        let ev_proc_ds = EvProc_Ds::new (EvProp_Continue, ComboProc_Enable);
        let cb = EvCbFn_Inline( Arc::new ( move |_| {
            ms.down.clear(); ms.consumed.clear(); mss_cba();
            ev_proc_ds
        } ) );
        if let Some(key) = self.key() {
            k.iproc.input_bindings .bind_kbd_event (key, KeyEventCb(KeyEventCb_KeyUp), EvCbEntry { ev_proc_ds, cb } );
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



/// Implements the (Arc wrapped) LatchState functionality
impl LatchState {

    pub fn new (ms_t: ModeState_T) -> LatchState {
        LatchState ( Arc::new ( _LatchState {
            ms_t,
            key    : AtomicRefCell::new(None),
            active : Flag::default(),
        } ) )
    }

    /// get a copy of the registered key as option if set
    pub fn key (&self) -> Option<KbdKey> {
        self.key.borrow().clone()
    }
    /// registration fn is private so we dont do it from outside MSS (where we can add the key to registered keys set)
    fn register_key (&self, key:KbdKey) {
        *self.key.borrow_mut() = Some(key);
    }

    /// Binds mode-key-down event on registered mod-key to flag update action (and disables key-repeats if the mode-key-dn is 'consumed')
    fn bind_latch_key_down(&self, k:&Krusty) {
        use crate::{EvProp_D::*, KbdEvCbMapKey_T::*, ComboProc_D::*, EvCbFn_T::*, EvCbMapKey_Action::*};
        let (ls, ks) = (self.clone(), k.ks.clone());
        let ev_proc_ds = EvProc_Ds::new (EvProp_Continue, ComboProc_Enable);
        let cb = EvCbFn_Inline( Arc::new ( move |_| {
            if ks.mod_keys.caps.dbl_tap.is_set() {
                // note that latch keys only update latch state when the keypress is on dbl_caps
                //ls.active.toggle();
                // ^^ we'll instead set it so only one latch state is active at a time (for usability reasons)
                let prior_state = ls.active.is_set();
                ks.mode_states.clear_latch_state_flags();
                ls.active.store(!prior_state);
                //mss.some_latch_state_active.store(ls.active.is_set())         // no need for this yet
            }
            ev_proc_ds
        } ) );
        if let Some(key) = self.key() {
            k.iproc.input_bindings .bind_kbd_event (key, KeyEventCb(KeyEventCb_KeyDown), EvCbEntry { ev_proc_ds, cb } );
        }
    }

    /// For mode-key btns (in addition to any combo maps action) we'll want individual binding callbacks that update flags.
    /// Note that after these binding callbacks process, they will still go through bulk processing for their default/combo actions.
    /// (This is as opposed to default-keys/combos that are handled in bulk w/o individual callback bindings)
    pub fn bind_latch_key_action(&self, k:&Krusty) {
        self.bind_latch_key_down(k);
        // note that there's nothing to do on key-up for latch keys
    }

}




/// Implements the (Arc wrapped) ModeStates-holder functionality
impl ModeStates {

    pub fn new() -> ModeStates {
        let (_sel, _del,  _word, _fast) = (ModeState::new(sel), ModeState::new(del), ModeState::new(word), ModeState::new(fast));
        let (_qks, _qks1, _qks2, _qks3) = (ModeState::new(qks), ModeState::new(qks1), ModeState::new(qks2), ModeState::new(qks3));
        let (_l1, _l2, _l3, _l4) = (LatchState::new(latch_1), LatchState::new(latch_2), LatchState::new(latch_3), LatchState::new(latch_4));

        ModeStates {
            _private : (),

            sel: _sel, del:  _del,  word: _word, fast: _fast,
            qks: _qks, qks1: _qks1, qks2: _qks2, qks3: _qks3,

            latch_1: _l1, latch_2: _l2, latch_3: _l3, latch_4: _l4,

            some_l2_mode_active    : Flag::default(),
            some_qks_mode_active   : Flag::default(),
            some_combo_mode_active : Flag::default(),
            //some_latch_state_active: Flag::default(),     // no need for this yet

            mode_keys  : AtomicRefCell::new (FxHashSet::default()),
            //latch_keys : AtomicRefCell::new (FxHashSet::default()),   // dont really need to track these
        }
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
    pub fn static_combo_modes() -> [ModeState_T; size_of::<ComboStatesBits_Modes>()] {
        //static COMBO_MODES: [ModeState_T; size_of::<ComboStatesBits_Modes>()] = [sel, del, word, fast, qks, qks1, qks2, qks3, mngd_ctrl_dn];
        static COMBO_MODES: [ModeState_T; size_of::<ComboStatesBits_Modes>()] = [sel, del, word, fast, qks, qks1, qks2, qks3];
        COMBO_MODES
    }
    pub fn static_latch_states() -> [ModeState_T; size_of::<ComboStatesBits_Latches>()] {
        static LATCH_STATES: [ModeState_T; size_of::<ComboStatesBits_Latches>()] = [latch_1, latch_2, latch_3, latch_4];
        LATCH_STATES
    }


    pub fn mode_flag_pairs (&self) -> [(ModeState_T, &ModeState); size_of::<ComboStatesBits_Modes>() ] { [
        // NOTE that the ordering here MUST match that given by the static_l2_qks_modes above
        // .. as this is what we will use to populate the combo bitmap and compare to current combo-mode-states!
        (sel, &self.sel), (del,  &self.del),  (word, &self.word), (fast, &self.fast),
        (qks, &self.qks), (qks1, &self.qks1), (qks2, &self.qks2), (qks3, &self.qks3),
    ] }
    pub fn latch_flag_pairs (&self) -> [(ModeState_T, &LatchState); size_of::<ComboStatesBits_Latches>()] { [
        // NOTE that the ordering here MUST match that given by the static_latch_states above
        // .. as this is what we will use to populate the combo bitmap and compare to current combo-mode-states!
        (latch_1, &self.latch_1), (latch_2,  &self.latch_2),  (latch_3, &self.latch_3), (latch_4, &self.latch_4),
    ] }

    pub fn get_cur_mode_states_bitmap (&self) -> ComboStatesBits_Modes {
        self.mode_flag_pairs() .map (|(_,ms)| ms.down.is_set())
    }
    pub fn get_cur_latch_states_bitmap (&self) -> ComboStatesBits_Latches {
        self.latch_flag_pairs() .map (|(_,ms)| ms.active.is_set())
    }

    pub fn make_combo_mode_states_bitmap (modes:&[ModeState_T]) -> ComboStatesBits_Modes {
        ModeStates::static_combo_modes() .map (|ms| modes.contains(&ms))
    }
    pub fn make_combo_latch_states_bitmap (modes:&[ModeState_T]) -> ComboStatesBits_Latches {
        ModeStates::static_latch_states() .map (|ms| modes.contains(&ms))
    }



    pub fn register_mode_key (&self, key:Key, ms_t:ModeState_T) {
        if let Some(ms) = self.get_mode_flag(ms_t) {
            ms.register_key(key);
            self.mode_keys.borrow_mut().insert(key);
        }
    }
    pub fn check_if_mode_key (&self, key:Key) -> bool {
        // this check needs to happen at runtime, so maintaining a small hashmap to do it fast rather than iterating through flags
        self.mode_keys.borrow().contains(&key)
        // ^^ note that we're ignoring latch keys here, they have no special runtime implication
    }
    pub fn get_mode_flag (&self, mst:ModeState_T) -> Option<&ModeState> {
        self.mode_flag_pairs() .iter() .find (|(ms_t,_)| *ms_t == mst) .map(|(_,ms)| *ms)
    }
    pub fn get_mode_t (&self, key:Key) -> Option<ModeState_T> {
        self.mode_flag_pairs() .iter() .find (|(_, ms)| ms.key.borrow().filter(|&k| k==key).is_some()) .map (|(ms_t,_)| *ms_t)
    }

    pub fn register_latch_key (&self, key:Key, ms_t:ModeState_T) {
        if let Some(ms) = self.get_latch_flag(ms_t) {
            ms.register_key(key);
            //self.latch_keys.write().unwrap().insert(key);     // dont really need to track these .. no runtime need yet
        }
    }
    pub fn get_latch_flag (&self, mst:ModeState_T) -> Option<&LatchState> {
        self.latch_flag_pairs() .iter() .find (|(ms_t,_)| *ms_t == mst) .map(|(_,ms)| *ms)
    }

    pub fn mode_key_consuming_action (&self, ms_t:ModeState_T, af:AF) -> AF {
        if let Some(ms) = self.get_mode_flag(ms_t) { ms.mode_key_consuming_action(af) }
        else { af }
    }

    pub fn refresh_qks_mode_active_flag (&self) {
        self.some_qks_mode_active.store (
            self.qks.down.is_set() || self.qks1.down.is_set() || self.qks2.down.is_set() || self.qks3.down.is_set()
        );
        self.refresh_caps_mode_active_flag();
    }
    pub fn refresh_l2_mode_active_flag (&self) {
        self.some_l2_mode_active.store (
            self.sel.down.is_set() || self.del.down.is_set() || self.word.down.is_set() || self.fast.down.is_set()
        );
        self.refresh_caps_mode_active_flag();
    }
    pub fn refresh_caps_mode_active_flag (&self) {
        self.some_combo_mode_active.store (
            self.some_qks_mode_active.is_set() || self.some_l2_mode_active.is_set()
        );
    }

    // latch states are exclusive, so instead of refreshing is-any-latch-active, we instead clear them before toggling any
    pub fn clear_latch_state_flags (&self) {
        self.latch_flag_pairs() .iter() .for_each (|(_,ms)| ms.active.clear());
    }

    pub fn clear_flags (&self) {
        self.mode_flag_pairs()  .iter() .for_each (|(_,ms)| ms.down.clear());
        self.latch_flag_pairs() .iter() .for_each (|(_,ms)| ms.active.clear());
        self.some_l2_mode_active.clear();
        self.some_qks_mode_active.clear();
        self.some_combo_mode_active.clear();
        //self.some_latch_state_active.clear();
    }

    pub fn bind_mode_keys_actions (&self, k:&Krusty) {
        self.mode_flag_pairs()  .iter() .for_each (|(_,ms)| ms.bind_mode_key_action(k));
        self.latch_flag_pairs() .iter() .for_each (|(_,ms)| ms.bind_latch_key_action(k));
    }

}







