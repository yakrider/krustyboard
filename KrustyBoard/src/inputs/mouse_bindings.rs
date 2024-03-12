
// allow non camel-case names for this entire file
#![allow(non_camel_case_types)]

use std::{
    sync::{Arc, RwLock},
};

use derive_deref::Deref;
use rustc_hash::FxHashMap;
use once_cell::sync::OnceCell;

use crate::{*, MouseEventCbMapKeyAction::*};


/// Mouse-Events callback bindings map type: the map-key has event-src and-event type, the map-value is the callback entry
pub type MouseEventCbMap = FxHashMap <MouseEventCbMapKey, MouseEventCallbackEntry>;


/// The bindings-map event-source type can be specified button, specified wheel, or the pointer
#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum MouseEventCbMapKeySrc {
    btn   (MouseButton),
    wheel (MouseWheel),
    pointer,
}


/// The bindings map key contains the mouse-event-source, and the event-action upon which the callback is to trigger
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct MouseEventCbMapKey {
    pub ev_src    : MouseEventCbMapKeySrc,
    pub ev_action : MouseEventCbMapKeyAction,
}

/// The bindings map mouse-event-action can be btn-down/up/dblClick, wheel-fwd/backward, or pointer-move
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum MouseEventCbMapKeyAction {
    BtnEventCb (MouseBtnEvent_T),
    WheelEventCb,
    MouseMoveCb,
}


impl MouseEventCbMapKey {
    pub fn from_event (mouse_event: &MouseEvent) -> MouseEventCbMapKey {
        use {MouseEventDat::*, MouseEventCbMapKeySrc::*};
        match mouse_event.dat {
            move_event  {..}                 => MouseEventCbMapKey { ev_src: pointer,          ev_action: MouseMoveCb },
            btn_event   {src_btn, ev_t, ..}  => MouseEventCbMapKey { ev_src: btn(src_btn),     ev_action: BtnEventCb(ev_t) },
            wheel_event {src_wheel, ..}      => MouseEventCbMapKey { ev_src: wheel(src_wheel), ev_action: WheelEventCb },
    }  }
}




/// The bindings callback type can either be inline-execution type (must return EventPropagationDirective)
/// or spawned out in a child thread (no return val)
# [ derive (Clone) ]
pub enum MouseEventCallbackFnType {
    MouseEvCbFn_InlineCallback  (MouseEvCbFn_InThreadCb_T),
    MouseEvCbFn_SpawnedCallback (MouseEvCbFn_OffThreadCb_T),
    MouseEvCbFn_QueuedCallback  (MouseEvCbFn_OffThreadCb_T),
}
pub type MouseEvCbFn_InThreadCb_T  = Arc <dyn Fn (MouseEvent) -> EventPropagationDirective + Send + Sync + 'static>;
pub type MouseEvCbFn_OffThreadCb_T = Arc <dyn Fn (MouseEvent) + Send + Sync + 'static>;



/// The callback entry includes the callback-type and the directive on whether to signal further event-propagation (to OS) after it done
# [ derive (Clone) ]
pub struct MouseEventCallbackEntry {
    pub event_prop_directive : EventPropagationDirective,
    pub cb : MouseEventCallbackFnType,
}


/// The mouse-events-action-bindings object itself, Arc/RwLock wrapped for safe sharing/setting/invocation across threads
# [ derive (Clone, Deref) ]
pub struct MouseBindings ( Arc <RwLock <MouseEventCbMap>> );





impl MouseBindings {

    pub fn instance() -> MouseBindings {
        static INSTANCE: OnceCell <MouseBindings> = OnceCell::new();
        INSTANCE .get_or_init ( ||
            MouseBindings ( Arc::new ( RwLock::new ( FxHashMap::default() ) ) )
        ) .clone()
    }

    pub fn bind_btn_event (&self, btn:MouseButton, btn_action: MouseEventCbMapKeyAction, cb_entry: MouseEventCallbackEntry) {
        let cb_map_key = MouseEventCbMapKey { ev_src: MouseEventCbMapKeySrc::btn(btn), ev_action: btn_action };
        self .write().unwrap() .insert (cb_map_key, cb_entry);
    }
    pub fn bind_wheel_event (&self, wheel:MouseWheel, cb_entry: MouseEventCallbackEntry) {
        let cb_map_key = MouseEventCbMapKey { ev_src: MouseEventCbMapKeySrc::wheel(wheel), ev_action: WheelEventCb };
        self .write().unwrap() .insert (cb_map_key, cb_entry);
    }
    pub fn bind_pointer_event (&self, cb_entry: MouseEventCallbackEntry) {
        let cb_map_key = MouseEventCbMapKey { ev_src: MouseEventCbMapKeySrc::pointer, ev_action: MouseMoveCb };
        self .write().unwrap() .insert (cb_map_key, cb_entry);
    }


    pub fn unbind_btn_event (&self, btn:MouseButton, btn_action: MouseEventCbMapKeyAction) {
        let cb_map_key = MouseEventCbMapKey { ev_src: MouseEventCbMapKeySrc::btn(btn), ev_action: btn_action };
        self .write().unwrap() .remove (&cb_map_key);
    }
    pub fn unbind_wheel_event (&self, wheel:MouseWheel, wheel_action: MouseEventCbMapKeyAction) {
        let cb_map_key = MouseEventCbMapKey { ev_src: MouseEventCbMapKeySrc::wheel(wheel), ev_action: wheel_action };
        self .write().unwrap() .remove (&cb_map_key);
    }
    pub fn unbind_pointer_event (&self) {
        let cb_map_key = MouseEventCbMapKey { ev_src: MouseEventCbMapKeySrc::pointer, ev_action: MouseMoveCb };
        self .write().unwrap() .remove (&cb_map_key);
    }

}

