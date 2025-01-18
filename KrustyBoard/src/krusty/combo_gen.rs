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
#[derive (Debug, Clone)] pub struct ComboGenSt_Key      { key  : Key,         action : KbdEv_MapKey_T }
#[derive (Debug, Clone)] pub struct ComboGenSt_MouseBtn { mbtn : MouseButton, action : MouseBtnEv_T }
#[derive (Debug, Clone)] pub struct ComboGenSt_Wheel    { whl  : MouseWheel,  action : MouseWheelEv_T }

#[derive (Debug, Clone)] pub struct ComboGenSt_Inited   { pub bmk : BindingsMapKey }
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

    /// Modifier keys that should be down to trigger this combo
    pub mks : Vec<ModKey>,

    /// Mode-states that should match for this combo to trigger
    pub modes : Vec<ModeState_T>,

    /// Modifier keys that can be ignored (marked as wildcard) .. (defined but empty-list means global wc)
    pub wc_mks : Option<Vec<ModKey>>,

    /// Mode-states that can be ignored (marked as wildcard)   .. (defined but empty-list means global wc)
    pub wc_modes : Option<Vec<ModeState_T>>,

    /// The dbl_tap flag marks that this combo should only be activated upon double-tap of the trigger key/btn
    pub dbl_tap : bool,

    /// Optional condition to check before triggering this combo
    # [ derivative (Debug="ignore") ]
    pub cond : Option<ComboCond>,

    /// Optional hash of a (sticky or latching) first-stroke-combo (fsc) that must be active for this combo to trigger. <br>
    /// Note that only one fsc will can recorded for a combo (whether it is sticky or latching). <br>
    /// Further, at execution time, a sfsc match is attempted first, and only if no sfsc matches, will a lfsc match be attempted.
    pub first_stroke : ComboHash,

    /// The modifier-key consume flag marks that the release of mod-keys in this combo should be masked
    pub mod_key_no_consume : bool,

    /// The mode-ken consume flag marks that key-repeats on mode-keys in this combo should be suppressed until they are released. <br>
    /// (This is useful to  avoid stragglers .. e.g say Alt-qks1-wheel for brightness, if alt is released first, we dont want '1's spamming out). <br>
    /// (This is less important for caps-modekey-<?>combos, as modekdy bindings now auto set consumption flag when it is seen w caps).
    pub mode_kdn_no_consume : bool,

    /// The repeat-suppresed flag marks that this combo should not trigger on key-repeats, only on fresh key presses. <br>
    /// (This applies to the combo trigger key even if the key is not a tracked key like mode-keys).
    pub repeat_suppressed : bool,
}

impl _ComboGen {
    fn new () -> _ComboGen {
        _ComboGen {
            mks:Vec::new(), modes:Vec::new(),
            wc_mks:None, wc_modes:None, cond:None, first_stroke:ComboHash::default(),
            dbl_tap:false, mod_key_no_consume:false, mode_kdn_no_consume:false, repeat_suppressed:false,
        }
    }
}



/// Combo-Generator progressive state struct
# [ derive (Debug, Clone) ]
pub struct ComboGen <S: ComboGenSt = ComboGenSt_Init> {

    /// all the data that ComboGen actually holds through the construction states
    pub dat : Box<_ComboGen>,

    /// internal state specific data .. either the combo-map-key, or the requisites to create one
    pub st  : S,
}


/// alias for the finalied ComboGen state, since we'll be passing that around to downstream processing fns
pub type CG = ComboGen <ComboGenSt_Inited>;

/// Utlity function to create a new Combo-Generator. <br>
/// (Note that ComboGen might access the singleton KrustyState for internal referencing.
pub fn cg() -> ComboGen { ComboGen::new() }


impl ComboGen <ComboGenSt_Init> {
    /// Create a new ComboGen at the _Init state (which is default)
    pub fn new () -> Self {
        ComboGen { dat: Box::new(_ComboGen::new()), st: ComboGenSt_Init{} }
    }
    /// Create ComboGen around a keyboard key action (default action is press)
    pub fn k (self, key:Key) -> ComboGen <ComboGenSt_Key> {
        let st = ComboGenSt_Key { key, action: KbdEv_MapKey_T::KeyEventCb_KeyDown };
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

    /// Require a (sticky or latching) first-stroke-combo (fsc) that must be active for this combo to trigger. <br>
    /// A sticky fsc (sfsc) remains active after the first-stroke is pressed until all modkeys are released. <br>
    /// A latching fsc (lfsc) remains active after the first-stroke is pressed until clear-latching-first-stroke is triggered. <br>
    /// Note: if the fsc matches, other registered actions for this combo without fsc-match will be ignored.
    pub fn fsc (mut self, fsc:ComboHash) -> Self {
        self.dat.first_stroke = fsc;
        self
    }


    /// Disable consuming mod-key key-downs for this combo. <br>
    /// (The default is to consume (i.e. do masking when releasing modkey) any modkey kdn on registered combos)
    pub fn mk_nc (mut self) -> Self {
        self.dat.mod_key_no_consume = true; self
    }
    /// Disable consuming mode-trigger-key key-downs for this combo. <br>
    /// (The default is to consume (i.e. disable further key-events until released) any mode-trigger-key kdn on registered combos). <br>
    /// (However, if the combo key is a mode-state-key itself, they will be set to no-consume by default). <br>
    /// (There should seldom be need to use this, as the default behavior should be ideal for almost all usecases!)
    pub fn msk_nc (mut self) -> Self {
        self.dat.mode_kdn_no_consume = true; self
    }

}


/// methods specific to generating ComboGen for key actions
impl ComboGen <ComboGenSt_Key> {
    /// Specify the key trigger action to be release (instead of the default press)
    pub fn rel (mut self) -> Self {
        self.st.action = KbdEv_MapKey_T::KeyEventCb_KeyUp;
        self
    }
    /// Specify that this combo should only activate when the trigger-key is double-tapped. <br>
    /// Note that upon the second tap of the dbl-tap, if combos for both single and dbl-tap are specified
    /// then first the single tap AF, then the dbl-tap AF will execute. <br>
    /// As such care must be taken if defining both, that such behavior (which is also typical of OS btns etc) is acceptable.
    pub fn dbl (mut self) -> Self {
        self.dat.dbl_tap = true;
        self
    }
    /// Disable triggering this combo on key-repeats (without having to press the key again). <br>
    /// (The default is to allow repeated combo activation on key-repeats)
    pub fn no_rpt (mut self) -> Self {
        self.dat.repeat_suppressed = true; self
    }
}

/// methods specific to generating ComboGen for mouse-btn actions
impl ComboGen <ComboGenSt_MouseBtn> {
    /// Specify the mouse btn trigger action to be release (instead of the default press)
    pub fn rel (mut self) -> Self {
        self.st.action = MouseBtnEv_T::BtnUp;
        self
    }
    /// Specify that this combo should only activate when the trigger-mbtn is double-tapped
    pub fn dbl (mut self) -> Self {
        self.dat.dbl_tap = true;
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
    /// get the Bindings-Maps-Key (bmk)
    /// (for internal use to restrict bmk access)
    pub fn get_bmk (&self) -> BindingsMapKey {
        self.st.bmk
    }
}



// So we're allowing the ComboGennable states to directly gen the final _Inited state at any part of process ..
// .. and we'll do that by making them transformable 'into' the final _Inited state (type aliased as CG)

impl From <ComboGen <ComboGenSt_Key>> for CG {
    fn from (cg : ComboGen <ComboGenSt_Key>) -> Self {
        let bmk = BindingsMapKey::key_ev_t (cg.st.key, cg.st.action);
        ComboGen { dat: cg.dat, st: ComboGenSt_Inited {bmk} }
    }
}
impl From <ComboGen <ComboGenSt_MouseBtn>> for CG {
    fn from (cg : ComboGen <ComboGenSt_MouseBtn>) -> Self {
        let bmk = BindingsMapKey::btn_ev_t (cg.st.mbtn, cg.st.action);
        ComboGen { dat: cg.dat, st: ComboGenSt_Inited {bmk} }
    }
}
impl From <ComboGen <ComboGenSt_Wheel>> for CG {
    fn from (cg : ComboGen <ComboGenSt_Wheel>) -> Self {
        let bmk = BindingsMapKey::wheel_ev_t (cg.st.whl, cg.st.action);
        ComboGen { dat: cg.dat, st: ComboGenSt_Inited {bmk} }
    }
}


