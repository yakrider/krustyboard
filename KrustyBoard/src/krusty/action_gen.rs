#![ allow (non_camel_case_types) ]

use crate::{*, key_utils::*};


# [ derive (Clone) ]
/// Combo-Action-Generator for a combo targeting a key-action (e.g. mapping a combo to another key).
/// By default, the action is press-release, but just press or release can be specified via .press() or .rel()
pub struct ActionGen_wKey<'a> {
    pub(crate) ks     : &'a KrustyState,
    pub(crate) key    : Key,                     // key on which the output combo-action will be built
    pub(crate) action : Option<KbdEvCbMapKey_T>, // if unspecified, action is press-release, else a press or release can be specified
    pub(crate) mks    : Vec<ModKey>,             // modifier-keys to send out with this combo-action
    // the wrap_mod_key_guard flag will set the generated action for this combo to be wrapped in activation/inactivation guards
    pub(crate) wrap_mod_key_guard : bool,
}

/// Combo-Action-Generator for a combo targeting a mouse-button-action (e.g. mapping a combo to a click).
/// By default, the action is press-release, but just press or release can be specified via .press() or .rel()
pub struct ActionGen_wMouseBtn<'a> {
    pub(crate) ks     : &'a KrustyState,
    pub(crate) btn    : MouseButton,             // mouse-button on which the output combo-action will be built
    pub(crate) action : Option<MouseBtnEv_T>,    // if unspecified, action is press-release, else a press or release can be specified
    pub(crate) mks    : Vec<ModKey>,             // modifier-keys to send out with this combo-action
    // the wrap_mod_key_guard flag will set the generated action for this combo to be wrapped in activation/inactivation guards
    pub(crate) wrap_mod_key_guard : bool,
}

# [ derive (Clone) ]
/// Combo-Action-Generator for a a combo targeting a non-key action (e.g. moving windows etc)
pub struct ActionGen<'a> {
    pub(crate) ks  : &'a KrustyState,
    pub(crate) mks : Vec<ModKey>,      // modifier-keys to wrap the specified action-function for this combo-action
    pub(crate) af  : AF,               // the action-function to build this combo-output-action with
    // the wrap_mod_key_guard flag will set the generated action for this combo to be wrapped in activation/inactivation guards
    pub(crate) wrap_mod_key_guard : bool,
}




/// Action-Generator for a specified Key Combo (as opposed to a combo that triggers non-key action etc)<br>
impl<'a> ActionGen_wKey<'a> {
    pub fn new (key:Key, ks: &'a KrustyState) -> ActionGen_wKey {
        ActionGen_wKey { ks, key, action:None, mks:Vec::new(), wrap_mod_key_guard:true }
    }
    /// Add a modifier key to the combo
    pub fn m (mut self, mk:ModKey) -> ActionGen_wKey<'a> {
        self.mks.push(mk); self
    }
    /// Specify the key action to be press only (not the default press-release)
    pub fn press (mut self) -> ActionGen_wKey<'a> {
        self.action = Some (KbdEvCbMapKey_T::KeyEventCb_KeyDown); self
    }
    /// Specify the key action to be release only (not the default press-release)
    pub fn rel (mut self) -> ActionGen_wKey<'a> {
        self.action = Some (KbdEvCbMapKey_T::KeyEventCb_KeyUp); self
    }
    /// Disable wrapping with mod-key guard actions (which disables their key repeat). <br>
    /// (The default for ActionGen_wKey is enabled)
    pub fn mkg_nw (mut self) -> ActionGen_wKey<'a> {
        self.wrap_mod_key_guard = false; self
    }
    /// Generate the action for this ActionGen, w mod-key guard wrapping as specified during construction
    pub fn gen_af (self) -> AF {
        Combo::gen_af (&self.into(), None)
    }
}



/// Action-Generator for a specified mouse-btn Combo (as opposed to a combo that triggers non-key action etc)<br>
impl<'a> ActionGen_wMouseBtn<'a> {
    pub fn new (btn:MouseButton, ks: &'a KrustyState) -> ActionGen_wMouseBtn {
        ActionGen_wMouseBtn { ks, btn, action:None, mks:Vec::new(), wrap_mod_key_guard:true }
    }
    /// Add a modifier key to the combo
    pub fn m (mut self, mk:ModKey) -> ActionGen_wMouseBtn<'a> {
        self.mks.push(mk); self
    }
    /// Specify the key action to be press only (not the default press-release)
    pub fn press (mut self) -> ActionGen_wMouseBtn<'a> {
        self.action = Some (MouseBtnEv_T::BtnDown); self
    }
    /// Specify the key action to be release only (not the default press-release)
    pub fn rel (mut self) -> ActionGen_wMouseBtn<'a> {
        self.action = Some (MouseBtnEv_T::BtnUp); self
    }
    /// Disable wrapping with mod-key guard actions (which disables their key repeat). <br>
    /// (The default for ActionGen_wKey is enabled)
    pub fn mkg_nw (mut self) -> ActionGen_wMouseBtn<'a> {
        self.wrap_mod_key_guard = false; self
    }
    /// Generate the action for this ActionGen, w mod-key guard wrapping as specified during construction
    pub fn gen_af (self) -> AF {
        Combo::gen_af (&self.into(), None)
    }
}



/// Action-Generator for a a combo targeting any pre-supplied action (e.g. moving windows etc)
impl<'a> ActionGen<'a> {
    pub fn new  (af:AF, ks: &'a KrustyState) -> ActionGen {
        ActionGen { ks, mks:Vec::new(), af, wrap_mod_key_guard:false }
    }
    pub fn m (mut self, mk:ModKey) -> ActionGen<'a> {
        self.mks.push(mk); self
    }
    /// Enable wrapping with mod-key guard actions (which disables their key repeat). <br>
    /// (The default for ActionGen is disabled)
    pub fn mkg_w (mut self) -> ActionGen<'a> {
        self.wrap_mod_key_guard = true; self
    }
    /// Generate the action for this ActionGen, w mod-key guard wrapping as specified during construction
    pub fn gen_af (self) -> AF {
        Combo::gen_af (&self, None)
    }
}
impl<'a> From<ActionGen_wKey<'a>> for ActionGen<'a> {
    fn from (ag: ActionGen_wKey<'a>) -> Self {
        let af = match ag.action {
            Some (KbdEvCbMapKey_T::KeyEventCb_KeyDown) => action_p1 (Key::press,         ag.key),
            Some (KbdEvCbMapKey_T::KeyEventCb_KeyUp)   => action_p1 (Key::release,       ag.key),
            _                                          => action_p1 (Key::press_release, ag.key),
        };
        ActionGen { ks: ag.ks, mks: ag.mks, af, wrap_mod_key_guard: ag.wrap_mod_key_guard }
    }
}
impl<'a> From<ActionGen_wMouseBtn<'a>> for ActionGen<'a> {
    fn from (ag: ActionGen_wMouseBtn<'a>) -> Self {
        let af = match ag.action {
            Some (MouseBtnEv_T::BtnDown) => action_p1 (MouseButton::press,         ag.btn),
            Some (MouseBtnEv_T::BtnUp)   => action_p1 (MouseButton::release,       ag.btn),
            _                            => action_p1 (MouseButton::press_release, ag.btn),
        };
        ActionGen { ks: ag.ks, mks: ag.mks, af, wrap_mod_key_guard: ag.wrap_mod_key_guard }
    }
}




