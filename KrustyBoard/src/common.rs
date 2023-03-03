
// allow non camel-case names for this entire file
#![allow(non_camel_case_types)]

use crate::public::*;
use once_cell::sync::Lazy;
pub use std::{
    collections::hash_map::HashMap,
    sync::atomic::{AtomicPtr, Ordering},
    sync::{Arc, RwLock},
    thread::spawn,
};


# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone) ]
pub enum EventPropagationDirective {
    EventProp_Continue,
    EventProp_Stop,
    EventProp_Undetermined,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum KbdEventCbMapKeyType {
    KeyDownCallback,
    KeyUpCallback,
}
impl KbdEventCbMapKeyType {
    pub(crate) fn from_key_down_state (b:bool) -> KbdEventCbMapKeyType {
        if b { KbdEventCbMapKeyType::KeyDownCallback } else { KbdEventCbMapKeyType::KeyUpCallback }
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct KbdEventCbMapKey {
    key : KbdKey,
    key_state: KbdEventCbMapKeyType,
}
impl KbdEventCbMapKey {
    pub(crate) fn from_key_state(key: KbdKey, key_state: KbdEventCbMapKeyType) -> KbdEventCbMapKey {
        KbdEventCbMapKey { key, key_state }
    }
    pub(crate) fn from_key_down_state(key: KbdKey, key_is_down: bool) -> KbdEventCbMapKey {
        KbdEventCbMapKey { key, key_state:KbdEventCbMapKeyType::from_key_down_state(key_is_down) }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum KbdEventType {
    KbdEvent_KeyDown,
    KbdEvent_SysKeyDown,
    KbdEvent_KeyUp,
    KbdEvent_SysKeyUp,
    KbdEvent_Unrecognized,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct KbdEvent {
    pub event : KbdEventType,
    pub key : KbdKey,
    pub vk_code: u32,
    pub sc_code: u32,
}

# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone) ]
pub enum KbdEvCbComboProcDirective {
    ComboProc_Enable,
    ComboProc_Disable,
}

# [ derive (Clone) ]
pub enum KbdEventCallbackFnType {
    KbdEvCbFn_InlineCallback { cb: KbdEvCbFn_InlineCb_T },
    KbdEvCbFn_SpawnedCallback { cb: KbdEvCbFn_SpawnedCb_T },
}

pub type KbdEvCbFn_InlineCb_T = Arc <dyn Fn (KbdEvent) -> EventPropagationDirective + Send + Sync + 'static>;
pub type KbdEvCbFn_SpawnedCb_T = Arc <dyn Fn (KbdEvent) + Send + Sync + 'static>;

# [ derive (Clone) ]
pub struct KbdEventCallbackEntry {
    pub event_prop_directive : EventPropagationDirective,
    pub combo_proc_directive : KbdEvCbComboProcDirective,
    pub cb : KbdEventCallbackFnType,
}


pub type KbdEventCbMap = HashMap <KbdEventCbMapKey, KbdEventCallbackEntry>;

pub static KEYBD_CALLBACKS: Lazy<RwLock<KbdEventCbMap>> = Lazy::new(|| RwLock::new(KbdEventCbMap::new()));

pub static COMBO_MAPS_PROCESSOR: Lazy <RwLock <Option <KbdEventCallbackEntry>>> = Lazy::new(|| RwLock::new(None));





#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct MouseEventCbMapKeyInput {
    btn    : Option<MouseButton>,
    wheel  : Option<MouseWheel>,
    cursor : Option<MousePointer>,
}
impl MouseEventCbMapKeyInput {
    pub fn for_btn (btn:MouseButton) -> MouseEventCbMapKeyInput {
        MouseEventCbMapKeyInput { btn: Some(btn), wheel: None, cursor: None }
    }
    pub fn for_wheel (wheel:MouseWheel) -> MouseEventCbMapKeyInput {
        MouseEventCbMapKeyInput { btn: None, wheel: Some(wheel), cursor: None }
    }
    pub fn for_pointer () -> MouseEventCbMapKeyInput {
        MouseEventCbMapKeyInput { btn: None, wheel: None, cursor: Some(MousePointer) }
    }


}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum MouseEventCbMapKeyAction {
    BtnDownCb,
    BtnUpCb,
    DblClickCb,
    WheelForwardCb,
    WheelBackwardCb,
    MouseMoveCb,
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct MouseEventCbMapKey {
    ev_input  : MouseEventCbMapKeyInput,
    ev_action : MouseEventCbMapKeyAction,
}
impl MouseEventCbMapKey {
    pub fn for_btn_action (btn:MouseButton, ev_action:MouseEventCbMapKeyAction) -> MouseEventCbMapKey {
        let ev_input = MouseEventCbMapKeyInput::for_btn(btn);
        MouseEventCbMapKey { ev_input, ev_action }
    }
    pub fn for_btn_down_state (btn:MouseButton, down_not_up:bool) -> MouseEventCbMapKey {
        use MouseEventCbMapKeyAction::*;
        let ev_action = if down_not_up { BtnDownCb } else { BtnUpCb };
        MouseEventCbMapKey::for_btn_action (btn, ev_action)
    }
    pub fn for_wheel_action (wheel:MouseWheel, ev_action:MouseEventCbMapKeyAction) -> MouseEventCbMapKey {
        let ev_input = MouseEventCbMapKeyInput::for_wheel(wheel);
        MouseEventCbMapKey { ev_input, ev_action }
    }
    pub fn for_wheel_fwd_state (wheel:MouseWheel, fwd_not_bkwd:bool) -> MouseEventCbMapKey {
        use MouseEventCbMapKeyAction::*;
        let ev_action = if fwd_not_bkwd { WheelForwardCb } else { WheelBackwardCb };
        MouseEventCbMapKey::for_wheel_action(wheel, ev_action)
    }
    pub fn for_pointer() -> MouseEventCbMapKey {
        let ev_input = MouseEventCbMapKeyInput::for_pointer();
        MouseEventCbMapKey { ev_input, ev_action : MouseEventCbMapKeyAction::MouseMoveCb }
    }
}



#[derive(Debug, Clone)]
pub struct MouseBtnEventData   { pub btn:MouseButton, pub down_not_up:bool }

#[derive(Debug, Clone)]
pub struct MouseWheelEventData { pub delta:i32 }

#[derive(Debug, Clone)]
pub struct MouseMoveEventData  { pub x_pos:i32, pub y_pos:i32 }

#[derive(Debug, Clone)]
pub struct MouseEvent {
    // note that details from hook obtained event are only partially encoded here as necessary
    pub ev_src : MouseEventCbMapKeyInput,
    pub btn_ev_data     : Option <MouseBtnEventData>,
    pub wheel_ev_data   : Option <MouseWheelEventData>,
    pub mouse_move_data : Option <MouseMoveEventData>,
}
impl MouseEvent {
    pub fn from_btn_ev (btn:MouseButton, down_not_up:bool) -> MouseEvent {
        MouseEvent {
            ev_src: MouseEventCbMapKeyInput::for_btn(btn),
            btn_ev_data: Some ( MouseBtnEventData { btn, down_not_up } ),
            wheel_ev_data: None, mouse_move_data: None
        }
    }
    pub fn from_wheel_ev (wheel:MouseWheel, delta:i32) -> MouseEvent {
        MouseEvent {
            ev_src: MouseEventCbMapKeyInput::for_wheel(wheel),
            wheel_ev_data: Some ( MouseWheelEventData { delta } ),
            btn_ev_data: None, mouse_move_data: None,
        }
    }
    pub fn from_pointer_ev (x_pos:i32, y_pos:i32) -> MouseEvent {
        MouseEvent {
            ev_src: MouseEventCbMapKeyInput::for_pointer(),
            mouse_move_data: Some ( MouseMoveEventData { x_pos, y_pos } ),
            btn_ev_data: None, wheel_ev_data: None,
        }
    }
}

# [ derive (Clone) ]
pub enum MouseEventCallbackFnType {
    MouseEvCbFn_InlineCallback  { cb: MouseEvCbFn_InlineCb_T },
    MouseEvCbFn_SpawnedCallback { cb: MouseEvCbFn_SpawnedCb_T },
}

pub type MouseEvCbFn_InlineCb_T  = Arc <dyn Fn (MouseEvent) -> EventPropagationDirective + Send + Sync + 'static>;
pub type MouseEvCbFn_SpawnedCb_T = Arc <dyn Fn (MouseEvent) + Send + Sync + 'static>;

# [ derive (Clone) ]
pub struct MouseEventCallbackEntry {
    pub event_prop_directive : EventPropagationDirective,
    pub cb : MouseEventCallbackFnType,
}

pub type MouseEventCbMap = HashMap <MouseEventCbMapKey, MouseEventCallbackEntry>;

pub static MOUSE_CALLBACKS: Lazy<RwLock<MouseEventCbMap>> = Lazy::new(|| RwLock::new(MouseEventCbMap::new()));
