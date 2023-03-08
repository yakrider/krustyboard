
// allow non camel-case names for this entire file
#![allow(non_camel_case_types)]


use std::{
    collections::hash_map::HashMap,
    sync::{Arc, RwLock},
    ops::Deref,
};

use once_cell::sync::OnceCell;


use crate::{*, KbdEventCbMapKeyType::*};


/// Keyboard bindings map type : the map-key has the key and event-type, the map-value is the callback entry
pub type KbdEventCbMap = HashMap <KbdEventCbMapKey, KbdEventCallbackEntry>;


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
}


/// The bindings callback type can either be inline-execution type (must return EventPropagationDirective)
/// or spawned out in a child thread (no return val)
# [ derive (Clone) ]
pub enum KbdEventCallbackFnType {
    KbdEvCbFn_InlineCallback (KbdEvCbFn_InlineCb_T),
    KbdEvCbFn_SpawnedCallback (KbdEvCbFn_SpawnedCb_T),
}
pub type KbdEvCbFn_InlineCb_T = Arc <dyn Fn (KbdEvent) -> EventPropagationDirective + Send + Sync + 'static>;
pub type KbdEvCbFn_SpawnedCb_T = Arc <dyn Fn (KbdEvent) + Send + Sync + 'static>;


/// The bindings callback entry includes the appropriate callback-type and directives on whether to proceed
/// with combo processing after it done, and whether to signal to OS for further event-propagation
# [ derive (Clone) ]
pub struct KbdEventCallbackEntry {
    pub event_prop_directive : EventPropagationDirective,
    pub combo_proc_directive : KbdEvCbComboProcDirective,
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
            KbdBindings ( Arc::new ( RwLock::new ( HashMap::new() ) ) )
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
