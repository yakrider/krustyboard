#![ allow (non_camel_case_types, non_snake_case) ]

use std::sync::Arc;
use atomic_refcell::AtomicRefCell;
use derive_deref::Deref;
use strum_macros::EnumIter;

use crate::{*, ModeState_T::*};





# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter) ]
/// All the supported mode-states, (whether they have triggering keys registered or not)
pub enum ModeState_T {
    no_ms,                      // no_ms can be useful to fill in fns set to take somethhing .. its ignored at its not in bitmaps
    msE, msD, msF, msR,         // typically for l2 [sel, del, word, fast] actions respectively
    msE_dbl, msD_dbl, msF_dbl, msR_dbl,
    qks, qks1, qks2, qks3, qks4,
    qks_dbl, qks1_dbl, qks2_dbl, qks3_dbl, qks4_dbl,
    //mngd_ctrl_dn, //ctrl_tab_scrl, //right_ms_scrl,
    // note: ^^ want minimal flags use here, as we dont want a set flag to change the combo state so other combos w/o flags get invalidated
}





# [ derive (Debug) ]
/// ModeState representation for mode-flags (and any associated trigger keys they have)
/// Note that key triggered mode-states are active ONLY while the assigned key is held down
pub struct _ModeState {
    // Note that we'll use AtomicRefCell instead of Arc-RwLock for the key, as runtime should have no writes to it (after initial setup)
    pub ms_t     : ModeState_T,
    pub ms_dbl_t : ModeState_T,
        key      : AtomicRefCell <Option<KbdKey>>,
    pub down     : Flag,
    pub consumed : Flag,
    pub stamp    : EventStamp,
    pub dbl_tap  : Flag,
}

# [ derive (Debug, Clone, Deref) ]
/// Implements the (Arc wrapped) ModeState functionality
pub struct ModeState ( Arc <_ModeState> );




# [ derive (Debug) ]
/// Holds all the ModeStates together, common functionality is impld here
pub struct ModeStates {
    _private : (),

    // l2 mode states
    pub msE : ModeState,
    pub msD : ModeState,
    pub msF : ModeState,
    pub msR : ModeState,

    // quick-keys mode states
    pub qks  : ModeState,
    pub qks1 : ModeState,
    pub qks2 : ModeState,
    pub qks3 : ModeState,
    pub qks4 : ModeState,

    // then the computed flags .. (helps avoid multiple checks at mouse-drag etc)
    pub some_l2_mode_active     : Flag,
    pub some_qks_mode_active    : Flag,
    pub some_mode_state_active  : Flag,
    pub some_mode_dbl_active    : Flag,

}





/// Implements the (Arc wrapped) ModeState functionality
impl ModeState {

    pub fn new (ms_t: ModeState_T, ms_dbl_t: ModeState_T) -> ModeState {
        ModeState ( Arc::new ( _ModeState {
            ms_t, ms_dbl_t,
            key      : AtomicRefCell::new(None),
            down     : Flag::default(),
            consumed : Flag::default(),
            stamp    : EventStamp::default(),
            dbl_tap  : Flag::default(),
        } ) )
    }

    /// mark the mode-key consumed by mode-action (so further inputs will be ignored until its released.. helps avoid straggling key events)
    pub fn mode_key_consuming_action (&self, af:AF) -> AF {
        let ms = self.clone();
        Arc::new ( move || { ms.consumed.set(); af(); } )
    }

    /// get a copy of the registered key as option if set
    pub fn key (&self) -> Option<KbdKey> {
        *self.key.borrow()
    }

    /// registration fn is private so we dont do it from outside MSS (where we can add the key to registered keys set)
    fn register_key (&self, key:KbdKey) {
        *self.key.borrow_mut() = Some(key);
    }


    /// Binds mode-key-down event on registered mod-key to flag update action (and disables key-repeats if the mode-key-dn is 'consumed')
    fn bind_mode_key_down (&self, k:&Krusty) {
        use crate::{EvProp_D::*, KbdEvCbMapKey_T::*, ComboProc_D::*, EvCbFn_T::*};
        // first we'll prep any supplemental actions specific to different types of mode-state keys
        let ks = k.ks.clone();
        let mss_cba : AF = {
            if ModeStates::static_l2_modes() .contains(&self.ms_t) {
                Arc::new ( move || ks.mode_states.some_l2_mode_active.set() )
            } else if ModeStates::static_qks_modes() .contains(&self.ms_t) {
                Arc::new ( move || ks.mode_states.some_qks_mode_active.set() )
            } else { Arc::new (move || { }) }
        };
        // now we can build the actual binding actions
        // (note that these should be inline so the flags are certain to be set by the time combo-processing for this key happens)
        let (ms, ks) = (self.clone(), k.ks.clone());
        let cb = EvCbFn_Inline ( Arc::new ( move |ev:Event| {
            if ms.down.is_clear() {
                // i.e. not a repeat
                if update_stamp_key_dbl_tap (ev.stamp, &ms.stamp, &ms.dbl_tap) {
                    ks.mode_states.some_mode_dbl_active.set()
                }
                ms.down.set(); ks.mode_states.some_mode_state_active.set(); mss_cba();
                ks.mouse.vwheel.spin_invalidated.set();

                // we'll set modkey behavior to disable repeat by default (if caps is held) ..
                // .. and for other cases, can set that selectively at combo declaration time
                if ks.mod_keys.caps.down.is_set() { ms.consumed.set() }

                EvProc_Ds::new (EvProp_Continue, ComboProc_Enable)
            }
            else if ms.consumed.is_clear() {
                // so this is a repeat, but its not marked consumed, so we'll let it go through
                EvProc_Ds::new (EvProp_Continue, ComboProc_Enable)
            } else {
                // now finally are repeats that we can block
                EvProc_Ds::new (EvProp_Stop, ComboProc_Disable)
            }
        } ) );
        // and finally we can actually bind the action
        let ev_proc_ds = EvProc_Ds::new (EvProp_Undet, ComboProc_Undet);
        if let Some(key) = self.key() {
            k.iproc.input_bindings .bind_kbd_event (key, KeyEventCb_KeyDown, EvCbEntry { ev_proc_ds, cb } );
        }
    }

    /// Binds mode-key-up event on registered mod-key to flag update action
    fn bind_mode_key_up (&self, k:&Krusty) {
        use crate::{EvProp_D::*, KbdEvCbMapKey_T::*, ComboProc_D::*, EvCbFn_T::*};
        // again, first we'll prep any supplemental actions specific to different types of mode-state keys
        let ks = k.ks.clone();
        let mss_cba : AF = {
            if      ModeStates::static_l2_modes()  .contains(&self.ms_t) { Arc::new ( move || ks.mode_states.refresh_l2_mode_active_flag() ) }
            else if ModeStates::static_qks_modes() .contains(&self.ms_t) { Arc::new ( move || ks.mode_states.refresh_qks_mode_active_flag() ) }
            else { Arc::new ( || { } ) }
        };
        // then build the actual binding actions
        let (ms, ks) = (self.clone(), k.ks.clone());
        let ev_proc_ds = EvProc_Ds::new (EvProp_Continue, ComboProc_Enable);
        let cb = EvCbFn_Inline ( Arc::new ( move |_| {
            ms.down.clear(); ms.consumed.clear(); mss_cba();
            if ms.dbl_tap.is_set() {
                // we wanna call _dbl refresh-check only if it was set .. but must first clear it before we attempt the refresh
                ms.dbl_tap.clear(); ks.mode_states.refresh_mode_dbl_active_flag()
            }
            ks.mouse.vwheel.spin_invalidated.set();
            ev_proc_ds
        } ) );
        // and finally we can actually bind the action
        if let Some(key) = self.key() {
            k.iproc.input_bindings .bind_kbd_event (key, KeyEventCb_KeyUp, EvCbEntry { ev_proc_ds, cb } );
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
        ModeStates {
            _private : (),
            msE  : ModeState::new (msE,  msE_dbl),        // key :  E
            msD  : ModeState::new (msD,  msD_dbl),        // key :  D
            msF  : ModeState::new (msF,  msF_dbl),        // key :  F
            msR  : ModeState::new (msR,  msR_dbl),        // key :  R
            qks  : ModeState::new (qks,  qks_dbl),        // key :  Q
            qks1 : ModeState::new (qks1, qks1_dbl),       // key :  1
            qks2 : ModeState::new (qks2, qks2_dbl),       // key :  2
            qks3 : ModeState::new (qks3, qks3_dbl),       // key :  3
            qks4 : ModeState::new (qks4, qks4_dbl),       // key :  4

            some_l2_mode_active    : Flag::default(),
            some_qks_mode_active   : Flag::default(),
            some_mode_state_active : Flag::default(),
            some_mode_dbl_active   : Flag::default(),
        }
    }

    // we'll just define all enum subsets we need rather than trying to partly/fully iterating through ModeState_T
    pub fn static_ordered_modes() -> [ModeState_T; N__COMBO_STATES_BITS__MODES] {
        static COMBO_MODES: [ModeState_T; N__COMBO_STATES_BITS__MODES] = [
            msE, msD, msF, msR, qks, qks1, qks2, qks3, qks4
        ];
        COMBO_MODES
    }
    pub fn static_ordered_modes_dbl() -> [ModeState_T; N__COMBO_STATES_BITS__MODES] {
        static COMBO_MODES_DBL: [ModeState_T; N__COMBO_STATES_BITS__MODES] = [
            msE_dbl, msD_dbl, msF_dbl, msR_dbl, qks_dbl, qks1_dbl, qks2_dbl, qks3_dbl, qks4_dbl
        ];
        COMBO_MODES_DBL
    }
    pub fn static_l2_modes () -> [ModeState_T;4] {
        static L2_MODES : [ModeState_T;4]  = [msE, msD, msF, msR];
        L2_MODES
    }
    pub fn static_qks_modes () -> [ModeState_T;5] {
        static QKS_MODES : [ModeState_T;5] = [qks, qks1, qks2, qks3, qks4];
        QKS_MODES
    }


    pub fn mode_flag_pairs (&self) -> [(ModeState_T, &ModeState); N__COMBO_STATES_BITS__MODES] { [
        // NOTE that the ordering here MUST match that given by the static_l2_qks_modes above
        // .. as this is what we will use to populate the combo bitmap and compare to current combo-mode-states!
        (msE, &self.msE), (msD,  &self.msD),  (msF, &self.msF), (msR, &self.msR),
        (qks, &self.qks), (qks1, &self.qks1), (qks2, &self.qks2), (qks3, &self.qks3), (qks4, &self.qks4)
    ] }



    pub fn register_mode_key (&self, key:Key, ms_t:ModeState_T) {
        if let Some(ms) = self.get_mode_flag(ms_t) {
            ms.register_key(key);
        }
    }
    pub fn get_mode_flag (&self, mst:ModeState_T) -> Option<&ModeState> {
        self.mode_flag_pairs() .iter() .find (|(ms_t,_)| *ms_t == mst) .map(|(_,ms)| *ms)
    }
    pub fn get_mode_t (&self, key:Key) -> Option<ModeState_T> {
        self.mode_flag_pairs() .iter() .find (|(_, ms)| ms.key.borrow().filter(|&k| k==key).is_some()) .map (|(ms_t,_)| *ms_t)
    }

    
    pub fn mode_key_consuming_action (&self, ms_t:ModeState_T, af:AF) -> AF {
        if let Some(ms) = self.get_mode_flag(ms_t) { ms.mode_key_consuming_action(af) }
        else { af }
    }
    

    pub fn refresh_qks_mode_active_flag (&self) {
        self.some_qks_mode_active.store (
            self.qks.down.is_set() || self.qks1.down.is_set() || self.qks2.down.is_set() || self.qks3.down.is_set() || self.qks4.down.is_set()
        );
        self.refresh_mode_state_active_flag();
    }
    pub fn refresh_l2_mode_active_flag (&self) {
        self.some_l2_mode_active.store (
            self.msE.down.is_set() || self.msD.down.is_set() || self.msF.down.is_set() || self.msR.down.is_set()
        );
        self.refresh_mode_state_active_flag();
    }
    pub fn refresh_mode_state_active_flag(&self) {
        self.some_mode_state_active.store (
            self.some_qks_mode_active.is_set() || self.some_l2_mode_active.is_set()
        );
    }
    pub fn refresh_mode_dbl_active_flag (&self) {
        self.some_mode_dbl_active.store (
            self.mode_flag_pairs() .iter() .any (|(_,s)| s.dbl_tap.is_set())
        );
    }
    

    pub fn clear_flags (&self) {
        self.mode_flag_pairs()  .iter() .for_each (|(_,ms)| { ms.down.clear(); ms.dbl_tap.clear() });
        self.some_l2_mode_active.clear();
        self.some_qks_mode_active.clear();
        self.some_mode_state_active.clear();
        self.some_mode_dbl_active.clear();
    }

    pub fn bind_mode_keys_actions (&self, k:&Krusty) {
        self.mode_flag_pairs()  .iter() .for_each (|(_,ms)| ms.bind_mode_key_action(k));
    }

}







