#![ allow (non_camel_case_types) ]

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
pub struct ModeState {
    _private    : (),
    pub ms_t    : ModeState_T,
    key         : Arc <RwLock <Option<KbdKey>>>,
    pub mk_down : Flag,
}

# [ derive (Debug, Clone) ]
/// Implements the (Arc wrapped) ModeState functionality
pub struct MS (Arc<ModeState>);

impl Deref for MS {
    type Target = ModeState;
    fn deref(&self) -> &ModeState { &self.0 }
}





# [ derive (Debug) ]
/// Holds all the ModeStates together, common functionality is impld here
pub struct ModeStates {
    _private : (),
    // l2 mode states
    pub sel  : MS,
    pub del  : MS,
    pub word : MS,
    pub fast : MS,
    // quick-keys mode states
    pub qks  : MS,
    pub qks1 : MS,
    pub qks2 : MS,
    pub qks3 : MS,
    // then the computed flags
    pub some_l2_mode_active   : Flag,
    pub some_qks_mode_active  : Flag,
    pub some_caps_mode_active : Flag,

}

# [ derive (Debug, Clone) ]
/// Implements the (Arc wrapped) ModeStates-holder functionality
pub struct MSS (Arc<ModeStates>);

impl Deref for MSS {
    type Target = ModeStates;
    fn deref(&self) -> &ModeStates { &self.0 }
}








/// Implements the (Arc wrapped) ModeState functionality
impl MS {

    pub fn new (ms_t: ModeState_T) -> MS {
        MS ( Arc::new ( ModeState {
            _private : (),
            ms_t,
            key     : Arc::new(RwLock::new(None)),
            mk_down : Flag::default()
        }
    ) ) }


    pub fn key (&self) -> Option<KbdKey> {
        self.key.read().unwrap().clone()
    }
    pub fn register_key (&self, key:KbdKey) {
        *self.key.write().unwrap() = Some(key);
    }


    /// Generates mode-key flag update action for key-down on registered mode-key
    fn gen_mode_key_down_action (&self, mss:MSS) -> AF {
        let flag = self.mk_down.clone();
        let mss_cba : AF = {
            if MSS::static_l2_modes() .contains(&self.ms_t) {
                Arc::new ( move || { mss.some_l2_mode_active.set();  mss.some_caps_mode_active.set(); } )
            } else if MSS::static_qks_modes() .contains(&self.ms_t) {
                Arc::new ( move || { mss.some_qks_mode_active.set(); mss.some_caps_mode_active.set(); } )
            } else if MSS::static_caps_modes() .contains(&self.ms_t) {
                Arc::new ( move || { mss.some_caps_mode_active.set(); } )
            } else { Arc::new (move || { }) }
        };
        Arc::new (move || { flag.set(); mss_cba(); })
    }

    /// Generates mode-key flag update action for key-up on registered mode-key
    fn gen_mode_key_up_action (&self, mss:MSS) -> AF {
        let flag = self.mk_down.clone();
        let mss_cba : AF = {
            if      MSS::static_l2_modes()   .contains(&self.ms_t) { Arc::new ( move || mss.refresh_l2_mode_active_flag() ) }
            else if MSS::static_qks_modes()  .contains(&self.ms_t) { Arc::new ( move || mss.refresh_qks_mode_active_flag() ) }
            else if MSS::static_caps_modes() .contains(&self.ms_t) { Arc::new ( move || mss.refresh_caps_mode_active_flag() ) }
            else { Arc::new ( || { } ) }
        };
        Arc::new ( move || { flag.clear(); mss_cba() } )
    }


    /// For mode-key btns (in addition to any combo maps action) we'll want individual binding callbacks that update flags.
    /// Note that after these binding callbacks process, they will still go through bulk processing for their default/combo actions.
    /// (This is as opposed to default-keys/combos that are handled in bulk w/o individual callback bindings)
    pub fn bind_mode_key_action (&self, k:&Krusty) {

        use crate::{EventPropagationDirective::*, KbdEventCbMapKeyType::*, KbdEvCbComboProcDirective::*, KbdEventCallbackFnType::*};

        let cma_dn = self.gen_mode_key_down_action (k.ks.mode_states.clone());
        let cma_up = self.gen_mode_key_up_action (k.ks.mode_states.clone());

        if let Some(key) = self.key() {

            k.kbb .bind_kbd_event ( key, KeyEventCb_KeyDown, KbdEventCallbackEntry {
                event_prop_directive: EventProp_Continue,
                combo_proc_directive: ComboProc_Enable,
                cb : KbdEvCbFn_InlineCallback ( Arc::new ( move |_| { cma_dn(); EventProp_Continue } ) ),
            } );

            k.kbb .bind_kbd_event ( key, KeyEventCb_KeyUp, KbdEventCallbackEntry {
                event_prop_directive: EventProp_Continue,
                combo_proc_directive: ComboProc_Enable,
                cb : KbdEvCbFn_InlineCallback ( Arc::new ( move |_| { cma_up(); EventProp_Continue } ) ),
            } );
    }  }

}





/// Implements the (Arc wrapped) ModeStates-holder functionality
impl MSS {

    pub fn new() -> MSS {
        let (_sel, _del,  _word, _fast) = (MS::new(sel), MS::new(del), MS::new(word), MS::new(fast));
        let (_qks, _qks1, _qks2, _qks3) = (MS::new(qks), MS::new(qks1), MS::new(qks2), MS::new(qks3));
        let (some_l2, some_qks, some_caps) = (Flag::default(), Flag::default(), Flag::default() );

        MSS ( Arc::new ( ModeStates {
            _private : (),
            sel: _sel, del:  _del,  word: _word, fast: _fast,
            qks: _qks, qks1: _qks1, qks2: _qks2, qks3: _qks3,
            some_l2_mode_active: some_l2, some_qks_mode_active: some_qks, some_caps_mode_active: some_caps,
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
    pub fn static_caps_modes () -> [ModeState_T;9] {
        // note that this also includes mngd_ctrl_dn which is a flag in KrS (i.e. not managed here)
        static CAPS_MODES : [ModeState_T;9] = [sel, del, word, fast, qks, qks1, qks2, qks3, mngd_ctrl_dn];
        CAPS_MODES
    }


    pub fn mode_flag_pairs (&self) -> [(ModeState_T, &MS);8] { [
        // NOTE that the ordering here MUST match that given by the static_l2_qks_modes above
        // .. as this is what we will use to populate the combo bitmap and compare to current combo-mode-states!
        (sel, &self.sel), (del,  &self.del),  (word, &self.word), (fast, &self.fast),
        (qks, &self.qks), (qks1, &self.qks1), (qks2, &self.qks2), (qks3, &self.qks3),
    ] }

    pub fn get_cur_mode_states_bitmap (&self) -> ComboStatesBits_Modes {
        self.mode_flag_pairs() .map (|(_,ms)| ms.mk_down.check())
    }
    pub fn make_combo_mode_states_bitmap (modes:&[ModeState_T]) -> ComboStatesBits_Modes {
        MSS::static_l2_qks_modes() .map (|ms| modes.contains(&ms))
    }

    pub fn refresh_qks_mode_active_flag (&self) {
        if !self.qks.mk_down.check() && !self.qks1.mk_down.check() && !self.qks2.mk_down.check() && !self.qks3.mk_down.check() {
            self.some_qks_mode_active.clear()
        }
        self.refresh_caps_mode_active_flag();
    }
    pub fn refresh_l2_mode_active_flag (&self) {
        if !self.sel.mk_down.check() && !self.del.mk_down.check() && !self.word.mk_down.check() && !self.fast.mk_down.check() {
            self.some_l2_mode_active.clear()
        }
        self.refresh_caps_mode_active_flag();
    }
    pub fn refresh_caps_mode_active_flag (&self) {
        if !self.some_qks_mode_active.check() && !self.some_l2_mode_active.check() {
            self.some_caps_mode_active.clear()
        }
    }
    pub fn clear_flags (&self) {
        self.mode_flag_pairs() .iter() .for_each (|(_,ms)| ms.mk_down.clear());
        self.some_l2_mode_active.clear(); self.some_qks_mode_active.clear(); self.some_caps_mode_active.clear();
    }

    pub fn bind_mode_keys_actions (&self, k:&Krusty) {
        self.mode_flag_pairs() .iter() .for_each (|(_,ms)| ms.bind_mode_key_action(k))
    }

}







