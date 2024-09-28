#![ allow (non_camel_case_types) ]

use std::sync::Arc;
use crate::*;


// Note : Since we get by using very few states here, and they have no shared behavior, we'll just create two separate structs 
// .. (instead of implementing actual parameterized generic Type-States with sealed Traits for states etc)

// Also note below, that we merged the type-states of combo-gen-key and combo-gen-mbtn into combo-gen-init ..
// .. this lets us have the default behavior be press without having to specify it in the next type-state (as we do for wheel)

pub struct ComboGen_Init { }
//pub struct ComboGen_Key { key : Key }
//pub struct ComboGen_MouseButton { mbtn : MouseButton }
pub struct ComboGen_Wheel { wheel : MouseWheel }


/// Combo-Generator for a specified key/mouse/wheel combo
pub struct ComboGen {
    _private : (),   // prevent direct init of this struct
    pub cmk      : EvCbMapKey,         // combo-trigger key .. the same structure as we would have as key in input bindings map
    pub mks      : Vec<ModKey>,        // modifier keys that should be down to trigger this combo
    pub modes    : Vec<ModeState_T>,   // mode-states that should match for this combo to trigger
    pub wc_mks   : Option<Vec<ModKey>>,        // modifier keys that can be ignored (marked as wildcard) .. (defined but empty-list means global wc)
    pub wc_modes : Option<Vec<ModeState_T>>,   // mode-states that can be ignored (marked as wildcard)   .. (defined but empty-list means global wc)
    pub cond     : Option<ComboCond>,  // optional condition to check before triggering this combo
    // the modifier-key consume flag marks that the release of mod-keys in this combo should be masked
    pub mod_key_no_consume  : bool,
    // the mode-ken consume flag marks that key-repeats on mode-keys in this combo should be suppressed until they are released
    pub mode_kdn_no_consume : bool,
}



impl ComboGen_Init {
    /// Create ComboGen around a keyboard key press
    pub fn k (self, key:Key) -> ComboGen {
        ComboGen::new ( EvCbMapKey::key_ev_t (key, KbdEvCbMapKey_T::KeyEventCb_KeyDown) )
    }
    /// Create ComboGen around a keyboard key release
    pub fn k_up(self, key:Key) -> ComboGen {
        ComboGen::new ( EvCbMapKey::key_ev_t (key, KbdEvCbMapKey_T::KeyEventCb_KeyUp) )
    }

    /// Create ComboGen around a mouse button press
    pub fn mbtn (self, mbtn: MouseButton) -> ComboGen {
        ComboGen::new ( EvCbMapKey::btn_ev_t (mbtn, MouseBtnEv_T::BtnDown) )
    }
    /// Create ComboGen around a mouse button release
    pub fn mbtn_up (self, mbtn: MouseButton) -> ComboGen {
        ComboGen::new ( EvCbMapKey::btn_ev_t (mbtn, MouseBtnEv_T::BtnUp) )
    }

    /// Create ComboGen around mouse vertical wheel
    pub fn whl (self) -> ComboGen_Wheel {
        ComboGen_Wheel { wheel: MouseWheel::DefaultWheel }
    }
    /// Create ComboGen around mouse horizontal wheel
    pub fn hwhl (self) -> ComboGen_Wheel {
        ComboGen_Wheel { wheel : MouseWheel::HorizontalWheel }
    }
}


impl ComboGen_Wheel {
    /// Set the direction of the combo wheel to forwards/upwards
    pub fn frwd (self) -> ComboGen {
        ComboGen::new ( EvCbMapKey::wheel_ev_t (self.wheel, MouseWheelEv_T::WheelForwards) )
    }
    /// Set the direction of the combo wheel to backwards/downwards
    pub fn bkwd (self) -> ComboGen {
        ComboGen::new ( EvCbMapKey::wheel_ev_t (self.wheel, MouseWheelEv_T::WheelBackwards) )
    }
}


impl ComboGen {

    /// private new fn, to be used via the init/builder structs
    fn new (cmk: EvCbMapKey) -> ComboGen {
        ComboGen { _private:(), cmk, mks:Vec::new(), modes:Vec::new(),
            wc_mks:None, wc_modes:None, cond:None, mod_key_no_consume:false, mode_kdn_no_consume:false,
        }
    }
    
    /// Add a modifier key to the combo
    pub fn m (mut self, mk:ModKey) -> Self {
        if !self.mks.contains(&mk) { self.mks.push(mk) }; self
    }
    /// Add a mode-state to the combo
    pub fn s (mut self, md: ModeState_T) -> Self {
        if !self.modes.contains(&md) { self.modes.push(md) }; self
    }

    /// Add a wildcard mod-key to the combo
    pub fn wcm (mut self, mk:ModKey) -> Self {
        if let Some(wc_mks) = self.wc_mks.as_mut() {
            if !wc_mks.is_empty() && !wc_mks.contains(&mk) { wc_mks.push(mk) }
            // ^^ note that we treat a defined but empty list as global wildcard
        } else { self.wc_mks = Some (vec![mk]) }
        self
    }
    /// Add a wildcard mode-state to the combo
    pub fn wcs (mut self, md:ModeState_T) -> Self {
        if let Some(wc_modes) = self.wc_modes.as_mut() {
            if !wc_modes.is_empty() && !wc_modes.contains(&md) { wc_modes.push(md) }
        } else { self.wc_modes = Some (vec![md]) }
        self
    }

    /// Add all non-specified mod-keys as wildcards to the combo
    pub fn wcma (mut self) -> Self {
        self.wc_mks = Some (vec![]); self
    }
    /// Add all non-specified mode-states as wildcards to the combo
    pub fn wcsa (mut self) -> Self {
        self.wc_modes = Some (vec![]); self
    }

    /// Add a condition to the combo
    pub fn c (mut self, cond:ComboCond) -> Self {
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
    pub fn mk_nc (mut self) -> Self {
        self.mod_key_no_consume = true; self
    }
    /// Disable consuming mode-trigger-key key-downs for this combo. <br>
    /// (The default is to consume (i.e. disable further key-events until released) any mode-trigger-key kdn on registered combos)
    pub fn msk_nc (mut self) -> Self {
        self.mode_kdn_no_consume = true; self
    }

}



