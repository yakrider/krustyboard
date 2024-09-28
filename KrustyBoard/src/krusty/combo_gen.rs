

use std::sync::Arc;
use crate::*;

# [ derive (Clone) ]
/// Combo-Generator for a specified Key Combo (as opposed to a combo that triggers non-key action etc)
/// (Note that the 'a lifetime cascades throughout from KrustyState since we're only holding a ref to that)
pub struct ComboGen<'a> {
    pub(crate) ks       : &'a KrustyState,
    pub(crate) cmk      : EvCbMapKey,         // combo-trigger key .. the same structure as we would have as key in input bindings map
    pub(crate) mks      : Vec<ModKey>,        // modifier keys that should be down to trigger this combo
    pub(crate) modes    : Vec<ModeState_T>,   // mode-states that should match for this combo to trigger
    pub(crate) wc_mks   : Option<Vec<ModKey>>,        // modifier keys that can be ignored (marked as wildcard) .. (defined but empty-list means global wc)
    pub(crate) wc_modes : Option<Vec<ModeState_T>>,   // mode-states that can be ignored (marked as wildcard)   .. (defined but empty-list means global wc)
    pub(crate) cond     : Option<ComboCond>,  // optional condition to check before triggering this combo
    // the modifier-key consume flag marks that the release of mod-keys in this combo should be masked
    pub(crate) mod_key_no_consume  : bool,
    // the mode-ken consume flag marks that key-repeats on mode-keys in this combo should be suppressed until they are released
    pub(crate) mode_kdn_no_consume : bool,

}


/// Combo-Generator for a specified Key Combo (as opposed to a combo that triggers non-key action etc)
impl<'a> ComboGen<'a> {
    /// Create ComboGen around a input-event-callback-map-key
    pub fn new (ks: &'a KrustyState, cmk:EvCbMapKey) -> ComboGen {
        ComboGen { ks, cmk, mks:Vec::new(), modes:Vec::new(), wc_mks:None, wc_modes:None,
                   cond:None, mod_key_no_consume:false, mode_kdn_no_consume:false }
    }
    /// Create ComboGen around a kbd-key .. (will create a kbdkey-down combo)
    pub fn new_w_key (ks: &'a KrustyState, k:Key) -> ComboGen {
        ComboGen::new (ks, EvCbMapKey::key_ev_t (k, KbdEvCbMapKey_T::KeyEventCb_KeyDown))
    }
    /// Create ComboGen around a mouse button .. (will create a mouse-btn-down combo)
    pub fn new_w_mbtn (ks: &'a KrustyState, mbtn:MouseButton) -> ComboGen {
        ComboGen::new (ks, EvCbMapKey::btn_ev_t (mbtn, MouseBtnEv_T::BtnDown))
    }
    /// Create ComboGen around mouse vertical/horizontal wheel
    pub fn new_w_whl (ks: &'a KrustyState, whl:MouseWheel) -> ComboGen {
        ComboGen::new (ks, EvCbMapKey::wheel_ev_t (whl, MouseWheelEv_T::WheelBackwards))
    }

    /// Set the combo to be for release instead of press (for a kbd-key or mouse-btn)
    pub fn rel (mut self) -> ComboGen<'a> {
        use {KbdEvCbMapKey_T::*, MouseBtnEv_T::*, EvCbMapKey::*};
        match self.cmk {
            key_ev_t (key, ..) => { self.cmk = key_ev_t (key, KeyEventCb_KeyUp) }
            btn_ev_t (btn, ..) => { self.cmk = btn_ev_t (btn, BtnUp) }
            _ => { }
        }
        self
    }
    /// Set the direction of the combo wheel (if applicable) to forwards/upwards instead of backwards/downwards
    pub fn frwd (mut self) -> ComboGen<'a> {
        use {EvCbMapKey::*, MouseWheelEv_T::*};
        if let wheel_ev_t (whl, ..) = self.cmk {
            self.cmk = wheel_ev_t (whl, WheelForwards)
        }
        self
    }
    /// Set the direction of the combo wheel (if applicable) to backwards (or downwards) <br>
    /// (this is mostly for usage symmetry, as its already the default)
    pub fn bkwd (mut self) -> ComboGen<'a> {
        use {EvCbMapKey::*, MouseWheelEv_T::*};
        if let wheel_ev_t (whl, ..) = self.cmk {
            self.cmk = wheel_ev_t (whl, WheelBackwards)
        }
        self
    }

    /// Add a modifier key to the combo
    pub fn m (mut self, mk:ModKey) -> ComboGen<'a> {
        if !self.mks.contains(&mk) { self.mks.push(mk) }; self
    }
    /// Add a mode-state to the combo
    pub fn s (mut self, md: ModeState_T) -> ComboGen<'a> {
        if !self.modes.contains(&md) { self.modes.push(md) }; self
    }

    /// Add a wildcard mod-key to the combo
    pub fn wcm (mut self, mk:ModKey) -> ComboGen<'a> {
        if let Some(wc_mks) = self.wc_mks.as_mut() {
            if !wc_mks.is_empty() && !wc_mks.contains(&mk) { wc_mks.push(mk) }
            // ^^ note that we treat a defined but empty list as global wildcard
        } else { self.wc_mks = Some (vec![mk]) }
        self
    }
    /// Add a wildcard mode-state to the combo
    pub fn wcs (mut self, md:ModeState_T) -> ComboGen<'a> {
        if let Some(wc_modes) = self.wc_modes.as_mut() {
            if !wc_modes.is_empty() && !wc_modes.contains(&md) { wc_modes.push(md) }
        } else { self.wc_modes = Some (vec![md]) }
        self
    }
    /// Add all non-specified mod-keys as wildcards to the combo
    pub fn wcma (mut self) -> ComboGen<'a> {
        self.wc_mks = Some (vec![]); self
    }
    /// Add all non-specified mode-states as wildcards to the combo
    pub fn wcsa (mut self) -> ComboGen<'a> {
        self.wc_modes = Some (vec![]); self
    }

    /// Add a condition to the combo
    pub fn c (mut self, cond:ComboCond) -> ComboGen<'a> {
        if self.cond.is_none() {
            self.cond = Some(cond);
        } else {
            let cond_old = self.cond.take().unwrap();
            self.cond = Some ( Arc::new ( move |ks,e| cond_old(ks,e) && cond(ks,e) ) );
        }
        self
    }

    /// Disable consuming mod-key key-downs for this combo. <br>
    /// (The default is to consume (i.e. do masking when releasing modkey) any modkey kdn on registered combos)
    pub fn mk_nc (mut self) -> ComboGen<'a> {
        self.mod_key_no_consume = true; self
    }
    /// Disable consuming mode-trigger-key key-downs for this combo. <br>
    /// (The default is to consume (i.e. disable further key-events until released) any mode-trigger-key kdn on registered combos)
    pub fn msk_nc (mut self) -> ComboGen<'a> {
        self.mode_kdn_no_consume = true; self
    }

}



