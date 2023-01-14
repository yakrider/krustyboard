use crate::public::*;
use once_cell::sync::Lazy;
pub use std::{
    collections::hash_map::HashMap,
    sync::atomic::{AtomicPtr, Ordering},
    sync::{Arc, Mutex},
    thread::spawn,
};
use std::fmt;


#[derive(Debug, Eq, PartialEq, Hash)]
pub enum KbdEvntCbMapKeyType {
    KeyDownCallback,
    KeyUpCallback,
}
impl KbdEvntCbMapKeyType {
    pub fn from_key_down_state (b:bool) -> KbdEvntCbMapKeyType {
        if b { KbdEvntCbMapKeyType::KeyDownCallback } else { KbdEvntCbMapKeyType::KeyUpCallback }
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct KbdEvntCbMapKey {
    key : KbdKey,
    key_state: KbdEvntCbMapKeyType,
}
impl KbdEvntCbMapKey {
    pub(crate) fn from_key_state(key: KbdKey, key_state: KbdEvntCbMapKeyType) -> KbdEvntCbMapKey {
        KbdEvntCbMapKey { key, key_state }
    }
}

#[derive(Debug)]
pub enum KbdEvntType {
    KbdEvntKeyDown,
    KbdEvntSysKeyDown,
    KbdEvntKeyUp,
    KbdEvntSysKeyUp,
    Unrecognized,
}

#[derive(Debug)]
pub struct KbdEvent {
    pub event : KbdEvntType,
    pub vk_code: u32,
    pub sc_code: u32,
}

pub enum KbdEvntCallback {
    NonBlockingCallback { cb : Arc <dyn Fn (KbdEvent) + Send + Sync + 'static> },
    BlockingCallback    { cb : Arc <dyn Fn (KbdEvent) + Send + Sync + 'static> },
    BlockableCallback   { cb : Arc <dyn Fn (KbdEvent) -> BlockInput + Send + Sync + 'static> },
}

pub type KbdEvntCbMap = HashMap <KbdEvntCbMapKey, KbdEvntCallback>;

pub static KEYBD_CALLBACKS: Lazy<Mutex<KbdEvntCbMap>> = Lazy::new(|| Mutex::new(KbdEvntCbMap::new()));


#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct MouseEventCbMapKeyInput {
    btn : Option<MouseButton>,
    wheel : Option<MouseWheel>,
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
    ev_input : MouseEventCbMapKeyInput,
    ev_action :  MouseEventCbMapKeyAction,
}
impl MouseEventCbMapKey {
    pub fn for_btn_action (btn:MouseButton, down_not_up:bool) -> MouseEventCbMapKey {
        use MouseEventCbMapKeyAction::*;
        let ev_input = MouseEventCbMapKeyInput::for_btn(btn);
        let ev_action = if down_not_up { BtnDownCb } else { BtnUpCb };
        MouseEventCbMapKey { ev_input, ev_action }
    }
    pub fn for_wheel_action (wheel:MouseWheel, fwd_not_bkwd:bool) -> MouseEventCbMapKey {
        use MouseEventCbMapKeyAction::*;
        let ev_input = MouseEventCbMapKeyInput::for_wheel(wheel);
        let ev_action = if fwd_not_bkwd { WheelForwardCb } else { WheelBackwardCb };
        MouseEventCbMapKey { ev_input, ev_action }
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
pub struct MouseMoveEventData  { pub x_pos:u32, pub y_pos:u32 }

#[derive(Debug, Clone)]
pub struct MouseEvent {
    // note that details from hook obtained event are only partially encoded here as necessary
    pub ev_src : MouseEventCbMapKeyInput,
    pub btn_ev_data    : Option <MouseBtnEventData>,
    pub wheel_ev_data  : Option <MouseWheelEventData>,
    pub mouse_move_data: Option <MouseMoveEventData>,
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
    pub fn from_pointer_ev (x_pos:u32, y_pos:u32) -> MouseEvent {
        MouseEvent {
            ev_src: MouseEventCbMapKeyInput::for_pointer(),
            mouse_move_data: Some ( MouseMoveEventData { x_pos: x_pos, y_pos: y_pos } ),
            btn_ev_data: None, wheel_ev_data: None,
        }
    }
}


pub enum MouseEventCallback {
    NonBlockingCallback { cb : Arc<dyn Fn (MouseEvent) + Send + Sync + 'static> },
    BlockingCallback    { cb : Arc<dyn Fn (MouseEvent) + Send + Sync + 'static> },
    BlockableCallback   { cb : Arc<dyn Fn (MouseEvent) -> BlockInput + Send + Sync + 'static> },
}
impl fmt::Debug for MouseEventCallback {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<some callback>")
    }
}

pub type MouseEventCbMap = HashMap <MouseEventCbMapKey, MouseEventCallback>;

pub static MOUSE_CALLBACKS: Lazy<Mutex<MouseEventCbMap>> = Lazy::new(|| Mutex::new(MouseEventCbMap::new()));