
// allow non camel-case names for this entire file
#![allow(non_camel_case_types)]

use std::sync::Arc;
use atomic_refcell::AtomicRefCell;

use derive_deref::Deref;
use rustc_hash::FxHashMap;

use crate::*;
use {EventDat::*, EvCbMapKey_Src::*, EvCbMapKey_Action::*};



/// The bindings-map event-source type can be specified button, specified wheel, or the pointer
#[derive (Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum EvCbMapKey_Src {
    key   (KbdKey),
    btn   (MouseButton),
    wheel (MouseWheel),
    pointer,
}


/// The bindings map key contains the mouse-event-source, and the event-action upon which the callback is to trigger
#[derive (Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub struct EvCbMapKey {
    pub ev_src    : EvCbMapKey_Src,
    pub ev_action : EvCbMapKey_Action,
}

/// The bindings-map key type for any key can be key-down or key-up .. (no sys-key-dn/up, press-rel, hold, dbl-click etc)
#[derive (Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum KbdEvCbMapKey_T {
    KeyEventCb_KeyDown,
    KeyEventCb_KeyUp,
}
impl From<KbdEvent_T> for KbdEvCbMapKey_T {
    fn from (ev_t: KbdEvent_T) -> Self {
        use KbdEvent_T::*;
        match ev_t {
            KbdEvent_KeyDown | KbdEvent_SysKeyDown => KbdEvCbMapKey_T::KeyEventCb_KeyDown,
            KbdEvent_KeyUp   | KbdEvent_SysKeyUp   => KbdEvCbMapKey_T::KeyEventCb_KeyUp,
    }  }
}

/// The bindings map mouse-event-action can be btn-down/up/dblClick, wheel-fwd/backward, or pointer-move
#[derive (Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum EvCbMapKey_Action {
    KeyEventCb (KbdEvCbMapKey_T),
    BtnEventCb (MouseBtnEvent_T),
    WheelEventCb,
    MouseMoveCb,
}


impl EvCbMapKey {
    pub fn new (ev_src: EvCbMapKey_Src, ev_action: EvCbMapKey_Action) -> EvCbMapKey {
        EvCbMapKey {ev_src, ev_action}
    }
    pub fn from_event (event: &Event) -> EvCbMapKey {
        match event.dat {
            move_event  {..}                 => EvCbMapKey::new ( pointer,          MouseMoveCb             ),
            btn_event   {src_btn, ev_t, ..}  => EvCbMapKey::new ( btn(src_btn),     BtnEventCb(ev_t)        ),
            wheel_event {src_wheel, ..}      => EvCbMapKey::new ( wheel(src_wheel), WheelEventCb            ),
            key_event   {src_key, ev_t, ..}  => EvCbMapKey::new ( key(src_key),     KeyEventCb(ev_t.into()) ),
    }  }
}






/// The bindings callback type can either be inline-execution type (must return EventPropagationDirective)
/// or spawned out in a child thread (no return val)
# [ derive (Clone) ]
pub enum EvCbFn_T {
    EvCbFn_Inline  (EvCbFn_InThread_T),
    EvCbFn_Spawned (EvCbFn_OffThread_T),
    EvCbFn_Queued  (EvCbFn_OffThread_T),
}

/// the inline cb type will have to return directives for further combo processing and OS event propagation
pub type EvCbFn_InThread_T = Arc <dyn Fn (Event) -> EvProc_Ds + Send + Sync + 'static>;

/// the spawned cb type cant have a return val (but the cb-entry will include the further processing directives)
pub type EvCbFn_OffThread_T = Arc <dyn Fn (Event) + Send + Sync + 'static>;

/// the combo-proc cb will return only the event-propagation-directive <br>
/// the semantic type is : Arc < dyn Fn (event-cb-map-key, was-binding-found, input-event) -> event-prop-directives >
pub type EvCbFn_ComboProc_T = Arc <dyn Fn (EvCbMapKey, bool, Event) -> EvProp_D + Send + Sync + 'static>;

/// finally, we'll ahve a type for queued packaged computation with all the args already preapplied (to exec from the queue)
pub type EvCbFn_QueuedProc_T = Box <dyn Fn() + Send + Sync + 'static>;

/// The bindings callback entry includes the appropriate callback-type and directives on whether to proceed
/// with combo processing after it done, and whether to signal to OS for further event-propagation
# [ derive (Clone) ]
pub struct EvCbEntry {
    pub ev_proc_ds : EvProc_Ds,
    pub cb : EvCbFn_T,
}




/// The input-events-action-bindings object itself, Arc/RwLock wrapped for safe sharing/setting/invocation across threads.
/// The map-key has event-src and-event type, the map-value is the callback entry .. used for both kbd and mouse events
//# [ derive (Clone, Deref) ]
//pub struct Bindings ( Arc <RwLock <EventCbMap>> );

# [ derive (Deref) ]
pub struct Bindings (
    AtomicRefCell <FxHashMap <EvCbMapKey, EvCbEntry>>
    // ^^ note that we use AtomicRefCell instead of RwLock as we dont ever write to it at runtime (and it is faster at runtime)
    // .. so if want to support dynamic binding/unbinding at runtime, we should switch back to RwLock
);





impl Bindings {

    pub fn new() -> Bindings {
        Bindings( AtomicRefCell::new ( FxHashMap::default() ) )
    }

    pub fn bind_kbd_event (&self, src_key:Key, key_action:EvCbMapKey_Action, cb_entry:EvCbEntry) {
        let cb_map_key = EvCbMapKey::new (key(src_key), key_action);
        self .borrow_mut() .insert (cb_map_key, cb_entry);
    }
    pub fn bind_btn_event (&self, src_btn:MouseButton, btn_action:EvCbMapKey_Action, cb_entry:EvCbEntry) {
        let cb_map_key = EvCbMapKey::new (btn(src_btn), btn_action);
        self .borrow_mut() .insert (cb_map_key, cb_entry);
    }
    pub fn bind_wheel_event (&self, src_wheel:MouseWheel, cb_entry:EvCbEntry) {
        let cb_map_key = EvCbMapKey::new (wheel(src_wheel), WheelEventCb);
        self .borrow_mut() .insert (cb_map_key, cb_entry);
    }
    pub fn bind_pointer_event (&self, cb_entry:EvCbEntry) {
        let cb_map_key = EvCbMapKey::new (pointer, MouseMoveCb);
        self .borrow_mut() .insert (cb_map_key, cb_entry);
    }


    // NOTE that these unbinds below are NOT intended to be called at runtime as we're using AtomicRefCell instead of RwLock
    // if we want to enable dynamic binding/unbinding at runtime, we should switch bindings back to RwLock

    pub fn unbind_kbd_event (&self, src_key:Key, key_action:EvCbMapKey_Action) {
        let cb_map_key = EvCbMapKey::new (key(src_key), key_action);
        self .borrow_mut() .remove (&cb_map_key);
    }
    pub fn unbind_btn_event (&self, src_btn:MouseButton, btn_action:EvCbMapKey_Action) {
        let cb_map_key = EvCbMapKey::new (btn(src_btn), btn_action);
        self .borrow_mut() .remove (&cb_map_key);
    }
    pub fn unbind_wheel_event (&self, src_wheel:MouseWheel, wheel_action:EvCbMapKey_Action) {
        let cb_map_key = EvCbMapKey::new (wheel(src_wheel), wheel_action);
        self .borrow_mut() .remove (&cb_map_key);
    }
    pub fn unbind_pointer_event (&self) {
        let cb_map_key = EvCbMapKey::new (pointer, MouseMoveCb);
        self .borrow_mut() .remove (&cb_map_key);
    }

}

