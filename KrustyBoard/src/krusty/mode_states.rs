#![ allow (non_camel_case_types, non_snake_case) ]

use std::sync::Arc;
use atomic_refcell::AtomicRefCell;
use once_cell::sync::OnceCell;
use strum_macros::EnumIter;

use crate::{*, ModeState_T::*};





# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter) ]
/// All the supported mode-states, (whether they have triggering keys registered or not)
pub enum ModeState_T {
    // note below that no_ms can be useful to fill in fns set to take somethhing .. (it is ignored at its not in bitmaps)
    // and in general, [msE, msD, msF, msR] are to be used for l2 [sel, del, word, fast] actions respectively
    no_ms,
    msE,      msD,       msF,       msR,
    msE_dbl,  msD_dbl,   msF_dbl,   msR_dbl,
    qks,      qks1,      qks2,      qks3,      qks4,
    qks_dbl,  qks1_dbl,  qks2_dbl,  qks3_dbl,  qks4_dbl,
}

impl ModeState_T {
    pub fn is_l2 (self) -> bool {
        matches! (self,  msE | msD | msF | msR)
    }
    pub fn is_qks (self) -> bool {
        matches! (self,  qks | qks1 | qks2 | qks3 | qks4)
    }
    pub fn is_dbl (self) -> bool {
        matches! (self,  msE_dbl | msD_dbl | msF_dbl | msR_dbl | qks_dbl | qks1_dbl | qks2_dbl | qks3_dbl | qks4_dbl)
    }
}





# [ derive (Debug) ]
/// ModeState representation for mode-flags (and any associated trigger keys they have)
/// Note that key triggered mode-states are active ONLY while the assigned key is held down
pub struct ModeState {
    // Note that we'll use AtomicRefCell instead of Arc-RwLock for the key, as runtime should have no writes to it (after initial setup)
    pub ms_t     : ModeState_T,
    pub ms_dbl_t : ModeState_T,
        key      : AtomicRefCell <Option<KbdKey>>,
    pub down     : Flag,
    pub consumed : Flag,
    pub dbl_tap  : Flag,
}




# [ derive (Debug) ]
/// Holds all the ModeStates together, common functionality is impld here
pub struct ModeStates {
    _private : (),

    // l2 mode states
    pub msE : &'static ModeState,
    pub msD : &'static ModeState,
    pub msF : &'static ModeState,
    pub msR : &'static ModeState,

    // quick-keys mode states
    pub qks  : &'static ModeState,
    pub qks1 : &'static ModeState,
    pub qks2 : &'static ModeState,
    pub qks3 : &'static ModeState,
    pub qks4 : &'static ModeState,

    // then the computed flags .. (helps avoid multiple checks at mouse-drag etc)
    pub some_l2_mode_active     : Flag,
    pub some_qks_mode_active    : Flag,
    pub some_mode_state_active  : Flag,
    pub some_mode_dbl_active    : Flag,

}





/// Implements the (Arc wrapped) ModeState functionality
impl ModeState {

    pub fn new (ms_t: ModeState_T, ms_dbl_t: ModeState_T) -> ModeState {
        ModeState {
            ms_t, ms_dbl_t,
            key      : AtomicRefCell::new(None),
            down     : Flag::default(),
            consumed : Flag::default(),
            dbl_tap  : Flag::default(),
        }
    }

    /// mark the mode-key consumed by mode-action (so further inputs will be ignored until its released.. helps avoid straggling key events)
    pub fn mode_key_consuming_action (&'static self, af:AF) -> AF {
        Arc::new ( move || { self.consumed.set(); af(); } )
    }

    /// get a copy of the registered key as option if set
    pub fn key (&'static self) -> Option<KbdKey> {
        //self.key.borrow()
        unsafe { *self.key.as_ptr() }
        // ^^we access this without guards as this never gets written to during runtime
    }

    /// registration fn is private so we dont do it from outside MSS (where we can add the key to registered keys set)
    fn register_key (&'static self, key:KbdKey) {
        *self.key.borrow_mut() = Some(key);
    }


    /// Binds mode-key-down event on registered mod-key to flag update action (and disables key-repeats if the mode-key-dn is 'consumed')
    fn bind_mode_key_down (&'static self, k:KR) {
        use crate::{EvProp_D::*, KbdEv_MapKey_T::*, ComboProc_D::*, EvCbFn_T::*};
        // first we'll prep any supplemental actions specific to different types of mode-state keys
        let mss_cba : AF = {
            if      self.ms_t.is_l2()  { Arc::new ( move || k.ks.mode_states.some_l2_mode_active.set() ) }
            else if self.ms_t.is_qks() { Arc::new ( move || k.ks.mode_states.some_qks_mode_active.set() ) }
            else { Arc::new ( || { } ) }
        };
        // now we can build the actual binding actions
        // (note that these should be inline so the flags are certain to be set by the time combo-processing for this key happens)
        let cb = EvCbFn_Inline ( Arc::new ( move |ev:Event| {
            if self.down.is_clear() {
                // i.e. not a repeat
                if update_dbl_tap (&ev, &self.dbl_tap) {
                    k.ks.mode_states.some_mode_dbl_active.set();
                    blip_cursor(1);
                }
                self.down.set(); k.ks.mode_states.some_mode_state_active.set(); mss_cba();
                k.ks.mouse.vwheel.spin_invalidated.set();

                // we'll set modkey behavior to disable repeat by default (if caps is held) ..
                // .. and for other cases, can set that selectively at combo declaration time
                //if ks.mod_keys.caps.down.is_set() { self.consumed.set() }
                // ^^ naah, not even this, there are combos we want to allow this

                EvProc_Ds::new (EvProp_Continue, ComboProc_Enable)
            }
            else if self.consumed.is_clear() {
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
    fn bind_mode_key_up (&'static self, k:KR) {
        use crate::{EvProp_D::*, KbdEv_MapKey_T::*, ComboProc_D::*, EvCbFn_T::*};
        // again, first we'll prep any supplemental actions specific to different types of mode-state keys
        let mss_cba : AF = {
            if      self.ms_t.is_l2()  { Arc::new ( move || k.ks.mode_states.refresh_l2_mode_active_flag() ) }
            else if self.ms_t.is_qks() { Arc::new ( move || k.ks.mode_states.refresh_qks_mode_active_flag() ) }
            else { Arc::new ( || { } ) }
        };
        // then build the actual binding actions
        let ev_proc_ds = EvProc_Ds::new (EvProp_Continue, ComboProc_Enable);
        let cb = EvCbFn_Inline ( Arc::new ( move |_| {
            self.down.clear(); self.consumed.clear(); mss_cba();
            if self.dbl_tap.is_set() {
                // we wanna call _dbl refresh-check only if it was set .. but must first clear it before we attempt the refresh
                self.dbl_tap.clear(); k.ks.mode_states.refresh_mode_dbl_active_flag()
            }
            k.ks.mouse.vwheel.spin_invalidated.set();
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
    pub fn bind_mode_key_action (&'static self, k:KR) {
        self.bind_mode_key_down(k);
        self.bind_mode_key_up(k);
    }

}




/// Implements the (Arc wrapped) ModeStates-holder functionality
impl ModeStates {

    pub fn instance() -> &'static ModeStates {

        // we'll make each of our modestates be 'static references to OnceCell instances ..
        // .. this allows us to pass each modestate into threads or AFs w/o having to make them clone

        static MS_E     : OnceCell<ModeState> = OnceCell::new();   // key :  E
        static MS_D     : OnceCell<ModeState> = OnceCell::new();   // key :  D
        static MS_F     : OnceCell<ModeState> = OnceCell::new();   // key :  F
        static MS_R     : OnceCell<ModeState> = OnceCell::new();   // key :  R
        static MS_QKS   : OnceCell<ModeState> = OnceCell::new();   // key :  Q
        static MS_QKS_1 : OnceCell<ModeState> = OnceCell::new();   // key :  1
        static MS_QKS_2 : OnceCell<ModeState> = OnceCell::new();   // key :  2
        static MS_QKS_3 : OnceCell<ModeState> = OnceCell::new();   // key :  3
        static MS_QKS_4 : OnceCell<ModeState> = OnceCell::new();   // key :  4

        // further, since ModeStates contains not just these static instances but also flags we populate here,
        // we want to hold an instance of this struct itself to avoid calling instance() here creating separate sets of flags

        static INSTANCE : OnceCell<ModeStates> = OnceCell::new();

        INSTANCE .get_or_init ( || {
            ModeStates {
                _private : (),

                msE  : MS_E     .get_or_init (|| ModeState::new (msE,  msE_dbl )),
                msD  : MS_D     .get_or_init (|| ModeState::new (msD,  msD_dbl )),
                msF  : MS_F     .get_or_init (|| ModeState::new (msF,  msF_dbl )),
                msR  : MS_R     .get_or_init (|| ModeState::new (msR,  msR_dbl )),
                qks  : MS_QKS   .get_or_init (|| ModeState::new (qks,  qks_dbl )),
                qks1 : MS_QKS_1 .get_or_init (|| ModeState::new (qks1, qks1_dbl)),
                qks2 : MS_QKS_2 .get_or_init (|| ModeState::new (qks2, qks2_dbl)),
                qks3 : MS_QKS_3 .get_or_init (|| ModeState::new (qks3, qks3_dbl)),
                qks4 : MS_QKS_4 .get_or_init (|| ModeState::new (qks4, qks4_dbl)),

                some_l2_mode_active    : Flag::default(),
                some_qks_mode_active   : Flag::default(),
                some_mode_state_active : Flag::default(),
                some_mode_dbl_active   : Flag::default(),
            }
        } )
    }


    pub fn ordered_mode_states (&'static self) -> [&ModeState; 9] { [
        // NOTE that the ordering here will be uses to populate the combo bitmap and compare to current combo-mode-states
        self.msE, self.msD,  self.msF,  self.msR,
        self.qks, self.qks1, self.qks2, self.qks3, self.qks4
    ] }


    pub fn register_mode_key (&'static self, key:Key, ms_t:ModeState_T) {
        for ms in self.ordered_mode_states() {
            if ms.ms_t == ms_t {
                ms.register_key(key); break
        }  }
    }


    pub fn refresh_qks_mode_active_flag (&'static self) {
        self.some_qks_mode_active.store (
            self.qks.down.is_set() || self.qks1.down.is_set() || self.qks2.down.is_set() || self.qks3.down.is_set() || self.qks4.down.is_set()
        );
        self.refresh_mode_state_active_flag();
    }
    pub fn refresh_l2_mode_active_flag (&'static self) {
        self.some_l2_mode_active.store (
            self.msE.down.is_set() || self.msD.down.is_set() || self.msF.down.is_set() || self.msR.down.is_set()
        );
        self.refresh_mode_state_active_flag();
    }
    pub fn refresh_mode_state_active_flag(&'static self) {
        self.some_mode_state_active.store (
            self.some_qks_mode_active.is_set() || self.some_l2_mode_active.is_set()
        );
    }
    pub fn refresh_mode_dbl_active_flag (&'static self) {
        self.some_mode_dbl_active.store (
            self.ordered_mode_states() .iter() .any (|ms| ms.dbl_tap.is_set())
        );
    }
    

    pub fn clear_flags (&'static self) {
        for ms in self.ordered_mode_states() {
            ms.down.clear(); ms.dbl_tap.clear(); ms.consumed.clear();
        }
        self.some_l2_mode_active.clear();
        self.some_qks_mode_active.clear();
        self.some_mode_state_active.clear();
        self.some_mode_dbl_active.clear();
    }

    pub fn bind_mode_keys_actions (&'static self, k:KR) {
        for ms in self.ordered_mode_states() {
            ms.bind_mode_key_action(k)
        }
    }

}







