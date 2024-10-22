#![ allow (non_camel_case_types) ]

use std::sync::Arc;
use derivative::Derivative;
use crate::*;

/*
ComboGen API Type-State machinery notes:
    - ComboGen has states for combo-gen-[w-key, w-mbtn, w-wheel] ...
    - we'd like the defaults on combo-gen to be press etc and directly buildable, but also have it be modifiable to rel etc before we build
    - this overlapping distribution of methods is best served by having groups of methods in traits that we impl for the states
    - and hence, we'll impl them as parameterized generic type-states
*/


#[derive (Debug, Clone)] pub struct ComboGenSt_Init     { }
#[derive (Debug, Clone)] pub struct ComboGenSt_Key      { key  : Key,         action : KbdEvCbMapKey_T }
#[derive (Debug, Clone)] pub struct ComboGenSt_MouseBtn { mbtn : MouseButton, action : MouseBtnEv_T }
#[derive (Debug, Clone)] pub struct ComboGenSt_Wheel    { whl  : MouseWheel,  action : MouseWheelEv_T }

#[derive (Debug, Clone)] pub struct ComboGenSt_Inited   { cmk: EvCbMapKey }
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



/// Combo-Generator progressive state struct inner data
//# [ derive (Clone) ]
# [ derive (Clone, Derivative) ]
# [ derivative (Debug) ]
pub struct _ComboGen {

    /// modifier keys that should be down to trigger this combo
    pub mks      : Vec<ModKey>,

    /// mode-states that should match for this combo to trigger
    pub modes    : Vec<ModeState_T>,

    /// modifier keys that can be ignored (marked as wildcard) .. (defined but empty-list means global wc)
    pub wc_mks   : Option<Vec<ModKey>>,

    /// mode-states that can be ignored (marked as wildcard)   .. (defined but empty-list means global wc)
    pub wc_modes : Option<Vec<ModeState_T>>,

    /// optional condition to check before triggering this combo
    # [ derivative (Debug="ignore") ]
    pub cond : Option<ComboCond>,

    /// optional first-stroke combo that must immediately precede this for this to trigger
    pub first_stroke : Option<CG>,

    /// the modifier-key consume flag marks that the release of mod-keys in this combo should be masked
    pub mod_key_no_consume  : bool,

    /// the mode-ken consume flag marks that key-repeats on mode-keys in this combo should be suppressed until they are released
    pub mode_kdn_no_consume : bool,
}

impl _ComboGen {
    fn new () -> _ComboGen {
        _ComboGen {
            mks:Vec::new(), modes:Vec::new(),
            wc_mks:None, wc_modes:None, cond:None, first_stroke:None,
            mod_key_no_consume:false, mode_kdn_no_consume:false,
        }
    }
}



/// Combo-Generator progressive state struct
# [ derive (Debug, Clone) ]
pub struct ComboGen <S: ComboGenSt = ComboGenSt_Init> {

    /// all the data that ComboGen actually holds through the construction states
    pub dat : Box<_ComboGen>,

    /// internal state specific data .. either the combo-map-key, or the requisites to create one
    st  : S,
}


/// alias for the finalied ComboGen state, since we'll be passing that around to downstream processing fns
pub type CG = ComboGen <ComboGenSt_Inited>;



impl ComboGen <ComboGenSt_Init> {
    /// Create a new ComboGen at the _Init state (which is default)
    pub fn new () -> Self {
        ComboGen { dat: Box::new(_ComboGen::new()), st: ComboGenSt_Init{} }
    }
    /// Create ComboGen around a keyboard key action (default action is press)
    pub fn k (self, key:Key) -> ComboGen <ComboGenSt_Key> {
        let st = ComboGenSt_Key { key, action: KbdEvCbMapKey_T::KeyEventCb_KeyDown };
        ComboGen { dat: self.dat, st }
    }
    /// Create ComboGen around a mouse button action (default action is press)
    pub fn mbtn (self, mbtn:MouseButton) -> ComboGen <ComboGenSt_MouseBtn> {
        let st = ComboGenSt_MouseBtn { mbtn, action: MouseBtnEv_T::BtnDown };
        ComboGen { dat: self.dat, st }
    }
    /// Create ComboGen around mouse vertical wheel action (default action is wheel-backwards/downwards)
    pub fn whl (self) -> ComboGen <ComboGenSt_Wheel> {
        let st = ComboGenSt_Wheel { whl: MouseWheel::DefaultWheel, action: MouseWheelEv_T::WheelBackwards };
        ComboGen { dat: self.dat, st }
    }
    /// Create ComboGen around mouse horizontal wheel action (default action is wheel-backwards/leftwards)
    pub fn hwhl (self) -> ComboGen <ComboGenSt_Wheel> {
        let st = ComboGenSt_Wheel { whl: MouseWheel::HorizontalWheel, action: MouseWheelEv_T::WheelBackwards };
        ComboGen { dat: self.dat, st }
    }
}


/// Common methods for ComboGenable states
impl <S> ComboGen<S>
    where S : ComboGenSt + ComboGenable
{
    /// Add a modifier key to the combo
    pub fn m (mut self, mk:ModKey) -> Self {
        if !self.dat.mks.contains(&mk) { self.dat.mks.push(mk) }; self
    }
    /// Add a mode-state to the combo
    pub fn s (mut self, md: ModeState_T) -> Self {
        if !self.dat.modes.contains(&md) { self.dat.modes.push(md) }; self
    }

    /// Add a wildcard mod-key to the combo
    pub fn wcm (mut self, mk:ModKey) -> Self {
        if let Some(wc_mks) = self.dat.wc_mks.as_mut() {
            if !wc_mks.is_empty() && !wc_mks.contains(&mk) { wc_mks.push(mk) }
            // ^^ note that we treat a defined but empty list as global wildcard
        } else { self.dat.wc_mks = Some (vec![mk]) }
        self
    }
    /// Add a wildcard mode-state to the combo. <br>
    /// Note that all exactly matching combos are executed first, before any wildcard matching combos are searched and executed
    pub fn wcs (mut self, md:ModeState_T) -> Self {
        if let Some(wc_modes) = self.dat.wc_modes.as_mut() {
            if !wc_modes.is_empty() && !wc_modes.contains(&md) { wc_modes.push(md) }
        } else { self.dat.wc_modes = Some (vec![md]) }
        self
    }

    /// Add all non-specified mod-keys as wildcards to the combo
    pub fn wcma (mut self) -> Self {
        self.dat.wc_mks = Some (vec![]); self
    }
    /// Add all non-specified mode-states as wildcards to the combo
    pub fn wcsa (mut self) -> Self {
        self.dat.wc_modes = Some (vec![]); self
    }

    /// Add a condition to the combo. <br>
    /// Note that all conditional combos that satisfy the condition are ran when a combo triggers. <br>
    /// However, if any conditional combo triggers, then any remaining non-conditional combos will be ignored. <br>
    /// (Tip: To ensure a combo always runs even if other conditional combos trigger, can give it an always-true condition). <br>
    /// (Note that wildcard combos are still only checked after exact match combos for that combo-map-key, regardless of conditionals etc)
    pub fn c (mut self, cond:ComboCond) -> Self {
        if self.dat.cond.is_none() {
            self.dat.cond = Some(cond);
        } else {
            let cond_old = self.dat.cond.take().unwrap();
            self.dat.cond = Some ( Arc::new ( move |ks,e| cond_old(ks,e) && cond(ks,e) ) );
        }
        self
    }

    /// Require a First-Stroke-Combo (fsc) that must immediately precede (excl modifier keys) this combo for this to trigger <br>
    /// Note: if the fsc matches, other registered actions for this combo without fsc-match will be ignored
    /// Note: second-stroke combos with mode-states aren't advisable as a mode-state-key press would itself be the next stroke!
    pub fn fsc <ICG> (mut self, cg:&ICG) -> Self
        where ICG : Into<CG> + Clone
    {
        self.dat.first_stroke = Some(cg.clone().into());
        self
    }

    /// Disable consuming mod-key key-downs for this combo. <br>
    /// (The default is to consume (i.e. do masking when releasing modkey) any modkey kdn on registered combos)
    pub fn mk_nc (mut self) -> Self {
        self.dat.mod_key_no_consume = true; self
    }
    /// Disable consuming mode-trigger-key key-downs for this combo. <br>
    /// (The default is to consume (i.e. disable further key-events until released) any mode-trigger-key kdn on registered combos)
    pub fn msk_nc (mut self) -> Self {
        self.dat.mode_kdn_no_consume = true; self
    }

}


/// methods specific to generating ComboGen for key actions
impl ComboGen <ComboGenSt_Key> {
    /// Specify the key trigger action to be release (instead of the default press)
    pub fn rel (mut self) -> Self {
        self.st.action = KbdEvCbMapKey_T::KeyEventCb_KeyUp;
        self
    }
}

/// methods specific to generating ComboGen for mouse-btn actions
impl ComboGen <ComboGenSt_MouseBtn> {
    /// Specify the mouse btn trigger action to be release (instead of the default press)
    pub fn rel (mut self) -> Self {
        self.st.action = MouseBtnEv_T::BtnUp;
        self
    }
}

/// methods specific to generating ComboGen for mouse-wheel actions
impl ComboGen <ComboGenSt_Wheel> {
    /// Specify the direction of the combo wheel trigger action to forwards/upwards
    pub fn frwd (mut self) -> Self {
        self.st.action = MouseWheelEv_T::WheelForwards;
        self
    }
    /// Specify the direction of the combo wheel trigger action to backwards/downwards
    pub fn bkwd (mut self) -> Self {
        self.st.action = MouseWheelEv_T::WheelBackwards;
        self
    }
}

/// methods specific to the fully Inited ComboGen
impl ComboGen <ComboGenSt_Inited> {
    /// get the Combo-Maps-Key (cmk)
    /// (for internal use restrict cmk access)
    pub fn get_cmk (&self) -> EvCbMapKey {
        self.st.cmk
    }
}



// So we're allowing the ComboGennable states to directly gen the final _Inited state at any part of process ..
// .. and we'll do that by making them transformable 'into' the final _Inited state (type aliased as CG)

impl From <ComboGen <ComboGenSt_Key>> for CG {
    fn from (cg : ComboGen <ComboGenSt_Key>) -> Self {
        let cmk = EvCbMapKey::key_ev_t (cg.st.key, cg.st.action);
        ComboGen { dat: cg.dat, st: ComboGenSt_Inited {cmk} }
    }
}
impl From <ComboGen <ComboGenSt_MouseBtn>> for CG {
    fn from (cg : ComboGen <ComboGenSt_MouseBtn>) -> Self {
        let cmk = EvCbMapKey::btn_ev_t (cg.st.mbtn, cg.st.action);
        ComboGen { dat: cg.dat, st: ComboGenSt_Inited {cmk} }
    }
}
impl From <ComboGen <ComboGenSt_Wheel>> for CG {
    fn from (cg : ComboGen <ComboGenSt_Wheel>) -> Self {
        let cmk = EvCbMapKey::wheel_ev_t (cg.st.whl, cg.st.action);
        ComboGen { dat: cg.dat, st: ComboGenSt_Inited {cmk} }
    }
}


