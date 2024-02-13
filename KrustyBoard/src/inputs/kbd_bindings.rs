
// allow non camel-case names for this entire file
#![allow(non_camel_case_types)]


use std::{
    sync::{Arc, RwLock},
    ops::Deref,
};

use once_cell::sync::OnceCell;
use rustc_hash::FxHashMap;


use crate::{*, KbdEventCbMapKeyType::*};


/// Keyboard bindings map type : the map-key has the key and event-type, the map-value is the callback entry
pub type KbdEventCbMap = FxHashMap <KbdEventCbMapKey, KbdEventCallbackEntry>;


/// The bindings-map key type for any key can be key-down or key-up .. (no press-rel, hold, dbl-click etc)
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum KbdEventCbMapKeyType {
    KeyEventCb_KeyDown,
    KeyEventCb_KeyUp,
}

/// The bindings map key contains the kbd-key itself, and its key-action-type (key-down/key-up)
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub struct KbdEventCbMapKey {
    pub(crate) key       : KbdKey,
    pub(crate) map_key_t : KbdEventCbMapKeyType,
}

impl KbdEventCbMapKey {
    pub(crate) fn from_event (event:&KbdEvent) -> KbdEventCbMapKey {
        use KbdEventType::*;
        match event.ev_t {
            KbdEvent_KeyDown | KbdEvent_SysKeyDown => KbdEventCbMapKey { key: event.key, map_key_t: KeyEventCb_KeyDown },
            KbdEvent_KeyUp   | KbdEvent_SysKeyUp   => KbdEventCbMapKey { key: event.key, map_key_t: KeyEventCb_KeyUp },
    }  }
}




/// The directive included in a bindings callback entry that indicates whether to skip or proceed
/// with combo processing after it is done
# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone) ]
pub enum KbdEvCbComboProcDirective {
    ComboProc_Enable,
    ComboProc_Disable,
    ComboProc_Undetermined,
}
/// directives on whether to continue with combo processing or OS event propagation upon a kbd-callback
# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone) ]
pub struct KbdEvProcDirectives {
    pub event_prop_d : EventPropagationDirective,
    pub combo_proc_d : KbdEvCbComboProcDirective,
}

impl KbdEvProcDirectives {
    /// pure syntatic sugar to crate kbd-event-processin-directive slightly less verbosely
    pub fn new (event_prop_d: EventPropagationDirective, combo_proc_d: KbdEvCbComboProcDirective) -> KbdEvProcDirectives {
        KbdEvProcDirectives { event_prop_d, combo_proc_d, }
    }
    pub fn default() -> KbdEvProcDirectives {
        use self::{EventPropagationDirective::*, KbdEvCbComboProcDirective::*};
        KbdEvProcDirectives { event_prop_d: EventProp_Continue, combo_proc_d: ComboProc_Enable }
    }
}

/// The bindings callback type can either be inline-execution type (must return EventPropagationDirective)
/// or spawned out in a child thread (no return val)
# [ derive (Clone) ]
pub enum KbdEventCallbackFnType {
    KbdEvCbFn_InlineCallback  (KbdEvCbFn_InThreadCb_T),
    KbdEvCbFn_SpawnedCallback (KbdEvCbFn_OffThreadCb_T),
    KbdEvCbFn_QueuedCallback  (KbdEvCbFn_OffThreadCb_T),
}

/// the inline cb type will have to return directives for further combo processing and OS event propagation
pub type KbdEvCbFn_InThreadCb_T = Arc <dyn Fn (KbdEvent) -> KbdEvProcDirectives + Send + Sync + 'static>;

/// the spawned cb type cant have a return val (but the cb-entry will include the further processing directives)
pub type KbdEvCbFn_OffThreadCb_T = Arc <dyn Fn (KbdEvent) + Send + Sync + 'static>;

/// the combo-proc cb will return only the event-propagation-directive
pub type KbdEvCbFn_ComboProc_T = Arc <dyn Fn (KbdEvent) -> EventPropagationDirective + Send + Sync + 'static>;


/// The bindings callback entry includes the appropriate callback-type and directives on whether to proceed
/// with combo processing after it done, and whether to signal to OS for further event-propagation
# [ derive (Clone) ]
pub struct KbdEventCallbackEntry {
    pub event_proc_d : KbdEvProcDirectives,
    pub cb : KbdEventCallbackFnType,
}




/// The key-action-bindings object itself, Arc/RwLock wrapped for safe sharing/setting/invocation across threads
# [ derive (Clone) ]
pub struct KbdBindings ( Arc <RwLock <KbdEventCbMap>> );

impl Deref for KbdBindings {
    type Target = RwLock <KbdEventCbMap>;
    fn deref (&self) -> &Self::Target { &self.0 }
}



impl KbdBindings {

    pub fn instance() -> KbdBindings {
        static INSTANCE: OnceCell <KbdBindings> = OnceCell::new();
        INSTANCE .get_or_init ( ||
            KbdBindings ( Arc::new ( RwLock::new ( FxHashMap::default() ) ) )
        ) .clone()
    }

    pub fn bind_kbd_event (&self, key:Key, map_key_t: KbdEventCbMapKeyType, cb_entry: KbdEventCallbackEntry ) {
        let map_key = KbdEventCbMapKey { key, map_key_t };
        self .write().unwrap() .insert (map_key, cb_entry);
    }

    pub fn unbind_kbd_event (&self, key:Key, map_key_t: KbdEventCbMapKeyType) {
        let map_key = KbdEventCbMapKey { key, map_key_t };
        self .write().unwrap() .remove (&map_key);
    }

}
