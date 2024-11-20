
// allow non camel-case names for this entire file
#![allow(non_camel_case_types)]

use std::sync::Arc;
use atomic_refcell::AtomicRefCell;

use derive_deref::Deref;
use rustc_hash::FxHashMap;

use crate::*;
use {EventDat::*, BindingsMapKey::*};



// we'll define some easier type aliases (CallBack, ActionFn etc) to pass around triggered actions and so on

/// Arc/Action-Function Fn() representation that can be passed around between threads
pub type AF  = Arc <dyn Fn() + Send + Sync + 'static> ;

/// the inline cb type will have to return directives for further combo processing and OS event propagation
pub type EvCbFn_InThread_T = Arc <dyn Fn (Event) -> EvProc_Ds + Send + Sync + 'static>;

/// the spawned cb type cant have a return val (but the cb-entry will include the further processing directives)
pub type EvCbFn_OffThread_T = Arc <dyn Fn (Event) + Send + Sync + 'static>;

/// finally, we'll have a type for queued computation (Boxed for 'Sized') with all the args already preapplied (to exec from the queue)
pub type EvCbFn_QueuedProc_T = Box <dyn Fn() + Send + Sync + 'static>;



/// The bindings callback type can either be inline-execution type (must return EventPropagationDirective)
/// or spawned out in a child thread (no return val)
# [ derive (Clone) ]
pub enum EvCbFn_T {
    EvCbFn_Inline  (EvCbFn_InThread_T),
    EvCbFn_Spawned (EvCbFn_OffThread_T),
    EvCbFn_Queued  (EvCbFn_OffThread_T),
}

/// The bindings callback entry includes the appropriate callback-type and directives on whether to proceed
/// with combo processing after it done, and whether to signal to OS for further event-propagation
# [ derive (Clone) ]
pub struct EvCbEntry {
    pub ev_proc_ds : EvProc_Ds,
    pub cb : EvCbFn_T,
}



/// The bindings-map key type for any key can be key-down or key-up .. (no sys-key-dn/up, press-rel, hold, dbl-click etc)
#[derive (Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum KbdEv_MapKey_T {
    KeyEventCb_KeyDown,
    KeyEventCb_KeyUp,
}
impl From<KbdEvent_T> for KbdEv_MapKey_T {
    fn from (ev_t: KbdEvent_T) -> Self {
        use KbdEvent_T::*;
        match ev_t {
            KbdEvent_KeyDown | KbdEvent_SysKeyDown => KbdEv_MapKey_T::KeyEventCb_KeyDown,
            KbdEvent_KeyUp   | KbdEvent_SysKeyUp   => KbdEv_MapKey_T::KeyEventCb_KeyUp,
    }  }
}



/// The bindings map key contains the mouse-event-source, and the event-action upon which the callback is to trigger. <br>
/// (This is defined matching the EventDat types minus the actual event-specific data)
#[derive (Eq, PartialEq, Hash, Copy, Clone)]
pub enum BindingsMapKey {
    key_ev_t   ( KbdKey,       KbdEv_MapKey_T ),
    btn_ev_t   ( MouseButton,  MouseBtnEv_T   ),
    wheel_ev_t ( MouseWheel,   MouseWheelEv_T ),
    move_ev_t
}
impl BindingsMapKey {
    pub fn from_event (event: &Event) -> BindingsMapKey {
        match event.dat {
            key_event   {key,   ev_t, ..}  => key_ev_t   (key, ev_t.into()),
            btn_event   {btn,   ev_t, ..}  => btn_ev_t   (btn, ev_t),
            wheel_event {wheel, delta   }  => wheel_ev_t (wheel, delta.into()),
            move_event  {..}               => move_ev_t,
    }  }
}
impl std::fmt::Debug for BindingsMapKey {
    fn fmt (&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use {KbdEv_MapKey_T::*, MouseBtnEv_T::*, MouseWheelEv_T::*};
        match &self {
            key_ev_t   (key,   KeyEventCb_KeyDown) => write! (f, "Down  {:?}", key),
            key_ev_t   (key,   KeyEventCb_KeyUp)   => write! (f, "  Up  {:?}", key),
            btn_ev_t   (btn,   BtnDown)            => write! (f, "Down  {:?}", btn),
            btn_ev_t   (btn,   BtnUp)              => write! (f, "  Up  {:?}", btn),
            wheel_ev_t (wheel, WheelBackwards)     => write! (f, "Bkwd  {:?}", wheel),
            wheel_ev_t (wheel, WheelForwards)      => write! (f, " Fwd  {:?}", wheel),
            move_ev_t                              => write! (f, "Mouse Moved"),
        }
    }
}




/// The input-events-action-bindings object itself, Arc/RwLock wrapped for safe sharing/setting/invocation across threads.
/// The map-key has event-src and-event type, the map-value is the callback entry .. used for both kbd and mouse events
# [ derive (Deref) ]
pub struct Bindings (
    AtomicRefCell <FxHashMap <BindingsMapKey, EvCbEntry>>
    // ^^ note that we use AtomicRefCell instead of RwLock as we dont ever write to it at runtime (and it is faster at runtime)
    // .. so if want to support dynamic binding/unbinding at runtime, we should switch back to RwLock
);




impl Bindings {

    pub fn new() -> Bindings {
        Bindings ( AtomicRefCell::new ( FxHashMap::default() ) )
    }

    pub fn bind_kbd_event (&self, key:Key, ev_t:KbdEv_MapKey_T, cbe:EvCbEntry) {
        self .borrow_mut() .insert ( key_ev_t (key, ev_t), cbe );
    }
    pub fn bind_btn_event (&self, mbtn:MouseButton, ev_t:MouseBtnEv_T, cbe:EvCbEntry) {
        self .borrow_mut() .insert ( btn_ev_t (mbtn, ev_t), cbe );
    }
    pub fn bind_wheel_event (&self, whl:MouseWheel, ev_t:MouseWheelEv_T, cbe:EvCbEntry) {
        self .borrow_mut() .insert ( wheel_ev_t (whl, ev_t), cbe );
    }
    pub fn bind_pointer_event (&self, cbe:EvCbEntry) {
        self .borrow_mut() .insert ( move_ev_t, cbe );
    }


    // NOTE that these unbinds below are NOT intended to be called at runtime as we're using AtomicRefCell instead of RwLock
    // if we want to enable dynamic binding/unbinding at runtime, we should switch bindings back to RwLock

    pub fn unbind_kbd_event (&self, key:Key, ev_t:KbdEv_MapKey_T) {
        self .borrow_mut() .remove ( & key_ev_t (key, ev_t) );
    }
    pub fn unbind_btn_event (&self, mbtn:MouseButton, ev_t:MouseBtnEv_T) {
        self .borrow_mut() .remove ( & btn_ev_t (mbtn, ev_t) );
    }
    pub fn unbind_wheel_event (&self, whl:MouseWheel, ev_t:MouseWheelEv_T) {
        self .borrow_mut() .remove ( & wheel_ev_t (whl, ev_t) );
    }
    pub fn unbind_pointer_event (&self) {
        self .borrow_mut() .remove ( & move_ev_t );
    }

}

