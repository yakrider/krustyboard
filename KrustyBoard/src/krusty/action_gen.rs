#![ allow (non_camel_case_types) ]

use crate::{*, key_utils::*};


// ActionGen has states for action-gen-[w-key, w-mouse, and w-AF] .. and the methods we want for them overlap
// .. hence instead of just making them separate progressive structs, we'll impl them as parameterized generic Type-States


pub struct ActionGenSt_Init     { }
pub struct ActionGenSt_Key      { key  : Key,         action : Option<KbdEvCbMapKey_T> }
pub struct ActionGenSt_MouseBtn { mbtn : MouseButton, action : Option<MouseBtnEv_T> }
pub struct ActionGenSt_AF       { af : AF }
pub struct ActionGenSt_Inited   { pub af : AF }

// ^^ note above that the action option, if unspecified will produce press-release output, else a press or rel action can be specified

pub trait ActionGenSt {}
impl ActionGenSt for ActionGenSt_Init {}
impl ActionGenSt for ActionGenSt_Key {}
impl ActionGenSt for ActionGenSt_MouseBtn {}
impl ActionGenSt for ActionGenSt_AF {}
impl ActionGenSt for ActionGenSt_Inited {}


impl ActionGenSt_Init {
    /// Create a new Combo-Action generator (key-action output type) <br>
    /// By default, it WILL wrap the AF with modifier key guard actions, can be set to not do so w .mkg_nw()
    pub fn k (&self, key:Key) -> ActionGen<ActionGenSt_Key> {
        ActionGen { mks: Vec::new(), wrap_mod_key_guard: true, _state: ActionGenSt_Key { key, action:None } }
    }
    /// Create a new Combo-Action generator (mouse-button-action output type) <br>
    /// By default, it WILL wrap the AF with modifier key guard actions, can be set to not do so w .mkg_nw()
    pub fn mbtn (&self, mbtn:MouseButton) -> ActionGen<ActionGenSt_MouseBtn> {
        ActionGen { mks: Vec::new(), wrap_mod_key_guard: true, _state: ActionGenSt_MouseBtn { mbtn, action:None } }
    }
    /// Create a new Combo-Action-generator (non-key action-function output type). <br>
    /// By default, it WILL NOT wrap the AF with modifier key guard actions, can be set to do so w .mkg_w()
    pub fn af (&self, af:AF) -> ActionGen<ActionGenSt_AF> {
        ActionGen { mks: Vec::new(), wrap_mod_key_guard: false, _state: ActionGenSt_AF { af } }
    }
}

pub trait ActionGenable {}
impl ActionGenable for ActionGenSt_Key {}
impl ActionGenable for ActionGenSt_MouseBtn {}
impl ActionGenable for ActionGenSt_AF {}
impl ActionGenable for ActionGenSt_Inited {}

pub struct ActionGen <S: ActionGenSt + ActionGenable> {
    pub mks : Vec<ModKey>,
    // ^ modifier-keys to wrap the specified action-function for this combo-action
    pub wrap_mod_key_guard : bool,
    // ^^ the wrap_mod_key_guard flag will set the generated action for this combo to be wrapped in activation/inactivation guards
    pub _state : S,
    // ^^ internal state specific data .. either the AF supplied, or the key/mbtn and event type to generate an action AF
}



impl <S: ActionGenSt + ActionGenable> ActionGen<S> {
    /// Add a modifier key to the combo
    pub fn m (mut self, mk:ModKey) -> Self {
        self.mks.push(mk);
        self
    }
}

impl <S: ActionGenSt + ActionGenable> ActionGen<S>
    where ActionGen<ActionGenSt_Inited> : From<ActionGen<S>>
{   // ^^ the compiler wants explicitly that our state is castable to Inited (presumably as the S here is not sealed?)

    /// Generate the action for this ActionGen, w mod-key guard wrapping as specified during construction
    pub fn gen_af (self) -> AF {
        Combo::gen_af (&self.into(), None)
    }
}

impl ActionGen <ActionGenSt_Key> {
    /// Specify the key action to be press only (not the default press-release)
    pub fn press (mut self) -> Self {
        self._state.action = Some (KbdEvCbMapKey_T::KeyEventCb_KeyDown);
        self
    }
    /// Specify the key action to be release only (not the default press-release)
    pub fn rel (mut self) -> Self {
        self._state.action = Some (KbdEvCbMapKey_T::KeyEventCb_KeyUp);
        self
    }

}
impl ActionGen <ActionGenSt_MouseBtn> {
    /// Specify the mouse btn action to be press only (not the default press-release)
    pub fn press (mut self) -> Self {
        self._state.action = Some (MouseBtnEv_T::BtnDown); self
    }
    /// Specify the mouse btn action to be release only (not the default press-release)
    pub fn rel (mut self) -> Self {
        self._state.action = Some (MouseBtnEv_T::BtnUp); self
    }
}

pub trait ActionGenS_mkgWrapped {}
impl ActionGenS_mkgWrapped for ActionGenSt_Key { }
impl ActionGenS_mkgWrapped for ActionGenSt_MouseBtn { }

impl <S : ActionGenSt + ActionGenable + ActionGenS_mkgWrapped> ActionGen<S> {
    /// Disable wrapping with mod-key guard actions (which disables their key repeat)
    /// (The default for ActionGenSt_Key and ActionGenSt_MouseBtn is disabled)
    pub fn mkg_nw (mut self) -> Self {
        self.wrap_mod_key_guard = false;
        self
    }
}
impl ActionGen<ActionGenSt_AF> {
    /// Enable wrapping with mod-key guard actions (which disables their key repeat). <br>
    /// (The default for ActionGenSt_AF is disabled)
    pub fn mkg_w (mut self) -> Self {
        self.wrap_mod_key_guard = true;
        self
    }
}

impl From<ActionGen<ActionGenSt_Key>> for ActionGen<ActionGenSt_Inited> {
    fn from (ag : ActionGen<ActionGenSt_Key>) -> Self {
        let af = match ag._state.action {
            Some (KbdEvCbMapKey_T::KeyEventCb_KeyDown) => action_p1 (Key::press,         ag._state.key),
            Some (KbdEvCbMapKey_T::KeyEventCb_KeyUp)   => action_p1 (Key::release,       ag._state.key),
            _                                          => action_p1 (Key::press_release, ag._state.key),
        };
        Self { mks: ag.mks, wrap_mod_key_guard: ag.wrap_mod_key_guard, _state: ActionGenSt_Inited{af} }
    }
}
impl From<ActionGen<ActionGenSt_MouseBtn>> for ActionGen<ActionGenSt_Inited> {
    fn from (ag : ActionGen<ActionGenSt_MouseBtn>) -> Self {
        let af = match ag._state.action {
            Some (MouseBtnEv_T::BtnDown) => action_p1 (MouseButton::press,         ag._state.mbtn),
            Some (MouseBtnEv_T::BtnUp)   => action_p1 (MouseButton::release,       ag._state.mbtn),
            _                            => action_p1 (MouseButton::press_release, ag._state.mbtn),
        };
        Self { mks: ag.mks, wrap_mod_key_guard: ag.wrap_mod_key_guard, _state: ActionGenSt_Inited{af} }
    }
}
impl From<ActionGen<ActionGenSt_AF>> for ActionGen<ActionGenSt_Inited> {
    fn from (ag : ActionGen<ActionGenSt_AF>) -> Self {
        Self { mks: ag.mks, wrap_mod_key_guard: ag.wrap_mod_key_guard, _state: ActionGenSt_Inited { af: ag._state.af } }
    }
}

