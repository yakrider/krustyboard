#![ allow (non_camel_case_types) ]

use std::sync::Arc;
use crate::*;

/*
ComboGen API Type-State machinery notes:
    - ComboGen has states for combo-gen-[w-key, w-mbtn, w-wheel] ...
    - we'd like the defaults on combo-gen to be press etc and directly buildable, but also have it be modifiable to rel etc before we build
    - this overlapping distribution of methods is best served by having groups of methods in traits that we impl for the states
    - and hence, we'll impl them as parameterized generic type-states
*/


pub struct ComboGenSt_Init     { }
pub struct ComboGenSt_Key      { key  : Key,         action : KbdEvCbMapKey_T }
pub struct ComboGenSt_MouseBtn { mbtn : MouseButton, action : MouseBtnEv_T }
pub struct ComboGenSt_Wheel    { whl  : MouseWheel,  action : MouseWheelEv_T }

pub struct ComboGenSt_Inited   { pub cmk: EvCbMapKey }
// ^^ The inited state holds the combo-maps-key .. the same structure as we would have as key in input bindings map
// ^^ note above that the action field when default will make combo-gen that triggers on press, else a release trigger can be specified


// we'll define a common Combo-Gen-State trait for all the above states
pub trait ComboGenSt {}
impl ComboGenSt for ComboGenSt_Init {}
impl ComboGenSt for ComboGenSt_Key {}
impl ComboGenSt for ComboGenSt_MouseBtn {}
impl ComboGenSt for ComboGenSt_Wheel {}
impl ComboGenSt for ComboGenSt_Inited {}


// and separately, for those states from which a Combo can directly be generated, we'll defined ComboGenable trait
pub trait ComboGenable {}
impl ComboGenable for ComboGenSt_Key {}
impl ComboGenable for ComboGenSt_MouseBtn {}
impl ComboGenable for ComboGenSt_Wheel {}
impl ComboGenable for ComboGenSt_Inited {}


/// Combo-Generator progressive state struct
pub struct ComboGen <S: ComboGenSt + ComboGenable> {
    _private : (),   // prevent direct init of this struct

    pub mks      : Vec<ModKey>,        // modifier keys that should be down to trigger this combo
    pub modes    : Vec<ModeState_T>,   // mode-states that should match for this combo to trigger
    pub wc_mks   : Option<Vec<ModKey>>,        // modifier keys that can be ignored (marked as wildcard) .. (defined but empty-list means global wc)
    pub wc_modes : Option<Vec<ModeState_T>>,   // mode-states that can be ignored (marked as wildcard)   .. (defined but empty-list means global wc)
    pub cond     : Option<ComboCond>,  // optional condition to check before triggering this combo

    pub mod_key_no_consume  : bool,
    // ^^ the modifier-key consume flag marks that the release of mod-keys in this combo should be masked
    pub mode_kdn_no_consume : bool,
    // ^^ the mode-ken consume flag marks that key-repeats on mode-keys in this combo should be suppressed until they are released

    pub _state : S,
    // ^^ internal state specific data .. either the combo-map-key, or the requisites to create one
}

/// alias for the finalied ComboGen state, since we'll be passing that around to downstream processing fns
pub type CG = ComboGen <ComboGenSt_Inited>;



impl ComboGenSt_Init {
    /// helper fn to init non-state fields of ComboGen<_>
    fn new <S: ComboGenSt + ComboGenable> (st:S) -> ComboGen<S> {
        ComboGen { _private:(),
            mks:Vec::new(), modes:Vec::new(),
            wc_mks:None, wc_modes:None, cond:None,
            mod_key_no_consume:false, mode_kdn_no_consume:false,
            _state: st
        }
    }
    /// Create ComboGen around a keyboard key action (default action is press)
    pub fn k (&self, key:Key) -> ComboGen<ComboGenSt_Key> {
        Self::new ( ComboGenSt_Key { key, action: KbdEvCbMapKey_T::KeyEventCb_KeyDown } )
    }
    /// Create ComboGen around a mouse button action (default action is press)
    pub fn mbtn (&self, mbtn:MouseButton) -> ComboGen<ComboGenSt_MouseBtn> {
        Self::new ( ComboGenSt_MouseBtn { mbtn, action: MouseBtnEv_T::BtnDown } )
    }
    /// Create ComboGen around mouse vertical wheel action (default action is wheel-backwards/downwards)
    pub fn whl (&self) -> ComboGen<ComboGenSt_Wheel> {
        Self::new ( ComboGenSt_Wheel { whl: MouseWheel::DefaultWheel, action: MouseWheelEv_T::WheelBackwards } )
    }
    /// Create ComboGen around mouse horizontal wheel action (default action is wheel-backwards/leftwards)
    pub fn hwhl (&self) -> ComboGen<ComboGenSt_Wheel> {
        Self::new ( ComboGenSt_Wheel { whl: MouseWheel::HorizontalWheel, action: MouseWheelEv_T::WheelBackwards } )
    }
}


/// Common methods for ComboGenable states
impl <S: ComboGenSt + ComboGenable> ComboGen<S> {

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


/// methods specific to generating ComboGen for key actions
impl ComboGen <ComboGenSt_Key> {
    /// Specify the key trigger action to be release (instead of the default press)
    pub fn rel (mut self) -> Self {
        self._state.action = KbdEvCbMapKey_T::KeyEventCb_KeyUp;
        self
    }
}

/// methods specific to generating ComboGen for mouse-btn actions
impl ComboGen <ComboGenSt_MouseBtn> {
    /// Specify the mouse btn trigger action to be release (instead of the default press)
    pub fn rel (mut self) -> Self {
        self._state.action = MouseBtnEv_T::BtnUp;
        self
    }
}

/// methods specific to generating ComboGen for mouse-wheel actions
impl ComboGen <ComboGenSt_Wheel> {
    /// Specify the direction of the combo wheel trigger action to forwards/upwards
    pub fn frwd (mut self) -> Self {
        self._state.action = MouseWheelEv_T::WheelForwards;
        self
    }
    /// Specify the direction of the combo wheel trigger action to backwards/downwards
    pub fn bkwd (mut self) -> Self {
        self._state.action = MouseWheelEv_T::WheelBackwards;
        self
    }
}



// So we're allowing the ComboGennable states to directly gen the final _Inited state at any part of process ..
// .. and we'll do that by making them transformable 'into' the final _Inited state

impl ComboGen <ComboGenSt_Inited> {
    /// helper fn to move over fields from various ComboGennable states to the final _Inited state
    pub fn new <S: ComboGenSt + ComboGenable> (cmk: EvCbMapKey, cg:ComboGen<S>) -> Self {
        Self { _private:(),
            mks: cg.mks, modes: cg.modes, wc_mks: cg.wc_mks, wc_modes: cg.wc_modes, cond: cg.cond,
            mod_key_no_consume: cg.mod_key_no_consume, mode_kdn_no_consume: cg.mode_kdn_no_consume,
            _state: ComboGenSt_Inited{cmk}
        }
    }
}

impl From<ComboGen<ComboGenSt_Key>> for ComboGen<ComboGenSt_Inited> {
    fn from (cg : ComboGen<ComboGenSt_Key>) -> Self {
        Self::new ( EvCbMapKey::key_ev_t (cg._state.key, cg._state.action), cg )
    }
}
impl From<ComboGen<ComboGenSt_MouseBtn>> for ComboGen<ComboGenSt_Inited> {
    fn from (cg : ComboGen<ComboGenSt_MouseBtn>) -> Self {
        Self::new ( EvCbMapKey::btn_ev_t (cg._state.mbtn, cg._state.action), cg )
    }
}
impl From<ComboGen<ComboGenSt_Wheel>> for ComboGen<ComboGenSt_Inited> {
    fn from (cg : ComboGen<ComboGenSt_Wheel>) -> Self {
        Self::new ( EvCbMapKey::wheel_ev_t (cg._state.whl, cg._state.action), cg )
    }
}


