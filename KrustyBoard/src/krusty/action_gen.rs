#![ allow (non_camel_case_types) ]

use std::sync::Arc;
use std::sync::atomic::Ordering;

use crate::{*, key_utils::*};


/*
ActionGen API Type-State machinery notes:
    - ActionGen has states for action-gen-[w-key, w-mbtn, w-AF] ...
    - we'd like the defaults on action-gen to be press etc and directly buildable, but also have it be modifiable to rel etc before we build
    - this overlapping distribution of methods is best served by having groups of methods in traits that we impl for the states
    - and hence, we'll impl them as parameterized generic type-states
*/


pub struct ActionGenSt_Init     { }
pub struct ActionGenSt_Key      { key  : Key,         action : Option<KbdEvCbMapKey_T> }
pub struct ActionGenSt_MouseBtn { mbtn : MouseButton, action : Option<MouseBtnEv_T> }
pub struct ActionGenSt_Wheel    { wheel: MouseWheel,  action : MouseWheelEv_T }
pub struct ActionGenSt_AF       { af : AF }

pub struct ActionGenSt_Inited   { pub af : AF }
// ^^ The inited state holds the actual provided AF or an AF generated with the key/mouse action specifications
// ^^ note above that the action option, if unspecified will produce press-release output, else a press or rel action can be specified


// we'll define a common Action-Gen-State trait for all the above states
pub trait ActionGenSt {}
impl ActionGenSt for ActionGenSt_Init {}
impl ActionGenSt for ActionGenSt_Key {}
impl ActionGenSt for ActionGenSt_MouseBtn {}
impl ActionGenSt for ActionGenSt_Wheel {}
impl ActionGenSt for ActionGenSt_AF {}
impl ActionGenSt for ActionGenSt_Inited {}


// and separately, for those states from which an Action can directly be generated, we'll defined ActionGenable trait
pub trait ActionGenable {}
impl ActionGenable for ActionGenSt_Key {}
impl ActionGenable for ActionGenSt_MouseBtn {}
impl ActionGenable for ActionGenSt_Wheel {}
impl ActionGenable for ActionGenSt_AF {}
impl ActionGenable for ActionGenSt_Inited {}




/// Action-Generator progressive state struct
pub struct ActionGen <S: ActionGenSt = ActionGenSt_Init> {

    /// modifier-keys to wrap the specified action-function for this combo-action
    pub mks : Vec<ModKey>,

    /// the wrap_mod_key_guard flag will set the generated action for this combo to be wrapped in activation/inactivation guards
    pub wrap_mkg : bool,

    /// internal state specific data .. either the AF supplied, or the key/mbtn and event type to generate an action AF
    pub st : S,

}


/// alias for the finalied ActionGen state, since we'll be passing that around to downstream processing fns
pub type AG = ActionGen <ActionGenSt_Inited>;



impl ActionGen<ActionGenSt_Init> {
    /// Create a new ActionGen at the _Init state (which is default)
    pub fn new () -> Self {
        ActionGen { mks: Vec::new(), wrap_mkg: true, st: ActionGenSt_Init{} }
    }
    /// Create a new Combo-Action generator (key-action output type) <br>
    /// The default action generated will be press-release .. else press-only / release-only can be specified later. <br>
    /// By default, it WILL wrap the AF with modifier key guard actions, can be set to not do so w .mkg_nw()
    pub fn k (self, key:Key) -> ActionGen <ActionGenSt_Key> {
        ActionGen { mks: self.mks, wrap_mkg: true, st: ActionGenSt_Key { key, action:None } }
    }
    /// Create a new Combo-Action generator (mouse-button-action output type) <br>
    /// The default action generated will be press-release .. else press-only / release-only can be specified later. <br>
    /// By default, it WILL wrap the AF with modifier key guard actions, can be set to not do so w .mkg_nw()
    pub fn mbtn (self, mbtn:MouseButton) -> ActionGen <ActionGenSt_MouseBtn> {
        ActionGen { mks: self.mks, wrap_mkg: true, st: ActionGenSt_MouseBtn { mbtn, action:None } }
    }
    /// Create a new Combo-Action generator (mouse vertical-wheel action output type) <br>
    /// By default, it WILL wrap the AF with modifier key guard actions, can be set to not do so w .mkg_nw()
    pub fn whl (self) -> ActionGen <ActionGenSt_Wheel> {
        let st = ActionGenSt_Wheel { wheel: MouseWheel::DefaultWheel, action:MouseWheelEv_T::WheelBackwards };
        ActionGen { mks: self.mks, wrap_mkg: true, st }
    }
    /// Create a new Combo-Action generator (mouse horizontal-wheel action output type) <br>
    /// By default, it WILL wrap the AF with modifier key guard actions, can be set to not do so w .mkg_nw()
    pub fn hwhl (self) -> ActionGen <ActionGenSt_Wheel> {
        let st = ActionGenSt_Wheel { wheel: MouseWheel::HorizontalWheel, action:MouseWheelEv_T::WheelBackwards };
        ActionGen { mks: self.mks, wrap_mkg: true, st }
    }
    /// Create a new Combo-Action-generator (non-key action-function output type). <br>
    /// By default, it WILL NOT wrap the AF with modifier key guard actions, can be set to do so w .mkg_w()
    pub fn af (self, af:AF) -> ActionGen <ActionGenSt_AF> {
        ActionGen { mks: self.mks, wrap_mkg: false, st: ActionGenSt_AF { af } }
    }
}


/// Common methods for ActionGenable states
impl <S> ActionGen<S>
    where S : ActionGenSt + ActionGenable
{
    /// Add a modifier key to the combo
    pub fn m (mut self, mk:ModKey) -> Self {
        self.mks.push(mk);
        self
    }
}

/// direct AF gen method for states that can be transformed into the final Inited state
impl <S> ActionGen<S>
    where S : ActionGenSt + ActionGenable,
          ActionGen <ActionGenSt_Inited> : From<ActionGen<S>>,
          // ^^ compiler explicitly wants _Inited to have 'from' our state (maybe as 'S' is not sealed?)
{
    /// Generate the action for this ActionGen, w mod-key guard wrapping as specified during construction
    pub fn gen_af (self) -> AF {
        Combo::gen_af (&self.into(), None)
    }
}

/// methods specific to generating ActionGen for key actions
impl ActionGen <ActionGenSt_Key> {
    /// Specify the key action to be press only (not the default press-release)
    pub fn press (mut self) -> Self {
        self.st.action = Some (KbdEvCbMapKey_T::KeyEventCb_KeyDown);
        self
    }
    /// Specify the key action to be release only (not the default press-release)
    pub fn rel (mut self) -> Self {
        self.st.action = Some (KbdEvCbMapKey_T::KeyEventCb_KeyUp);
        self
    }
}

/// methods specific to generating ActionGen for mouse-btn actions
impl ActionGen <ActionGenSt_MouseBtn> {
    /// Specify the mouse btn action to be press only (not the default press-release)
    pub fn press (mut self) -> Self {
        self.st.action = Some (MouseBtnEv_T::BtnDown);
        self
    }
    /// Specify the mouse btn action to be release only (not the default press-release)
    pub fn rel (mut self) -> Self {
        self.st.action = Some (MouseBtnEv_T::BtnUp);
        self
    }
}

/// methods specific to generating ActionGen for mouse-wheel actions
impl ActionGen <ActionGenSt_Wheel> {
    /// Specify the direction of the wheel action to forwards/upwards
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

/// methods specific to generating ActionGen for directly specified Action-Function
impl ActionGen <ActionGenSt_AF> {
    /// Enable wrapping with mod-key guard actions (which disables their key repeat). <br>
    /// (The default for ActionGenSt_AF is disabled)
    pub fn mkg_w (mut self) -> Self {
        self.wrap_mkg = true;
        self
    }
}


/*
    - Now note that for both key/mbtn/wheel type action-gen, the default is to wrap mod-key-guard ..
    - .. and so we'd like to provide a common method to disable that wrapping
    - Hence we'll define a new trait for the types with default wrapping behavior (and add those two states there)
    - .. and then finally impl the warpping-disable method under that trait
*/

/// trait for action-gen states that have modkey-guard wrapping by default (i.e. for kbd/mbtn types but not Af type)
pub trait ActionGenS_mkgWrapped {}
impl ActionGenS_mkgWrapped for ActionGenSt_Key { }
impl ActionGenS_mkgWrapped for ActionGenSt_MouseBtn { }
impl ActionGenS_mkgWrapped for ActionGenSt_Wheel { }

impl <S> ActionGen<S>
    where S : ActionGenSt + ActionGenable + ActionGenS_mkgWrapped
{
    /// Disable wrapping with mod-key guard actions (which disables their key repeat)
    /// (The default for ActionGenSt_Key and ActionGenSt_MouseBtn is disabled)
    pub fn mkg_nw (mut self) -> Self {
        self.wrap_mkg = false;
        self
    }
}



// So we're allowing the ActionGennable states to directly gen the final _Inited state at any part of process ..
// .. and we'll do that by making them transformable 'into' the final _Inited state (type aliased as AG)

impl From <ActionGen <ActionGenSt_Key>> for AG {
    fn from (ag : ActionGen <ActionGenSt_Key>) -> Self {
        let af = match ag.st.action {
            Some (KbdEvCbMapKey_T::KeyEventCb_KeyDown) => action_p1 (Key::press,         ag.st.key),
            Some (KbdEvCbMapKey_T::KeyEventCb_KeyUp)   => action_p1 (Key::release,       ag.st.key),
            None                                       => action_p1 (Key::press_release, ag.st.key),
        };
        Self { mks: ag.mks, wrap_mkg: ag.wrap_mkg, st: ActionGenSt_Inited{af} }
    }
}
impl From <ActionGen <ActionGenSt_MouseBtn>> for AG {
    fn from (ag : ActionGen <ActionGenSt_MouseBtn>) -> Self {
        let af = match ag.st.action {
            Some (MouseBtnEv_T::BtnDown) => action_p1 (MouseButton::press,         ag.st.mbtn),
            Some (MouseBtnEv_T::BtnUp)   => action_p1 (MouseButton::release,       ag.st.mbtn),
            None                         => action_p1 (MouseButton::press_release, ag.st.mbtn),
        };
        Self { mks: ag.mks, wrap_mkg: ag.wrap_mkg, st: ActionGenSt_Inited{af} }
    }
}
impl From <ActionGen <ActionGenSt_Wheel>> for AG {
    fn from (ag : ActionGen <ActionGenSt_Wheel>) -> Self {
        // Note : This is a lil hacky as we've so far avoided passing in event data to AFs ..
        // .. so for wheel scroll delta, we'll just try and use last recorded .. meh
        let af = if let Some(ws) = KrustyState::instance() .mouse.get_wheel_state(ag.st.wheel) {
            let mult = if ag.st.action == MouseWheelEv_T::WheelForwards { 1 } else { -1 };
            Arc::new ( move || {
                let delta = ws.last_delta.load(Ordering::Relaxed).abs() * mult;
                ws.wheel.scroll(delta)
            } )
        } else { no_action() };
        Self { mks: ag.mks, wrap_mkg: ag.wrap_mkg, st: ActionGenSt_Inited{af} }
    }
}
impl From <ActionGen <ActionGenSt_AF>> for AG {
    fn from (ag : ActionGen<ActionGenSt_AF>) -> Self {
        Self { mks: ag.mks, wrap_mkg: ag.wrap_mkg, st: ActionGenSt_Inited {af: ag.st.af} }
    }
}

