
use std::{
    time::Instant,
    ops::Deref,
    sync::{Arc, RwLock},
    sync::atomic::{AtomicBool, Ordering},
};

use once_cell::sync::OnceCell;

use crate::*;





// todo : just a reminder that we added some hacky meddling into keycodes and sending key events to get L/R scancodes out on alt/ctrl/shift


// we'll define some easier type aliases (CallBack, ActionFn etc) to pass around triggered actions and so on
/// Arc/Action-Function Fn() representation that can be passed around between threads
pub type AF  = Arc <dyn Fn() + Send + Sync + 'static> ;

pub type Key = KbdKey ;





# [ derive (Debug, Default, Clone) ]
/// representation for all our flags for states mod-states, modifier-keys, mouse-btn-state etc
pub struct Flag (Arc<AtomicBool>);
// ^^ simple sugar that helps reduce clutter in code

impl Flag {
    pub fn new (state:bool) -> Flag { Flag ( Arc::new ( AtomicBool::new(state) ) ) }

    pub fn set   (&self) { self.0 .store (true,  Ordering::SeqCst) }
    pub fn clear (&self) { self.0 .store (false, Ordering::SeqCst) }
    pub fn store (&self, state:bool) { self.0 .store (state, Ordering::SeqCst) }

    pub fn check    (&self) -> bool { self.0 .load (Ordering::SeqCst) }
    pub fn is_set   (&self) -> bool { true  == self.0 .load (Ordering::SeqCst) }
    pub fn is_clear (&self) -> bool { false == self.0 .load (Ordering::SeqCst) }
}


# [ derive (Debug, Clone) ]
pub struct TimeStamp (Arc<RwLock<Instant>>);

impl TimeStamp {
    pub fn new() -> TimeStamp {
        TimeStamp ( Arc::new ( RwLock::new ( Instant::now() ) ) )
    }
    pub fn default() -> TimeStamp {
        TimeStamp::new()
    }
    pub fn capture (&self) -> Instant {
        let stamp = Instant::now();
        *self.0.write().unwrap() = stamp;
        stamp
    }
    pub fn get (&self) -> Instant {
        *self.0.read().unwrap()
    }
}


# [ derive (Debug, Clone) ]
pub struct EventStamp (Arc<RwLock<u32>>);

impl EventStamp {
    pub fn new() -> EventStamp {
        EventStamp ( Arc::new ( RwLock::new(0) ) )
    }
    pub fn default() -> EventStamp {
        EventStamp::new()
    }
    pub fn set (&self, stamp:u32) { *self.0.write().unwrap() = stamp }
    pub fn get (&self) -> u32 { *self.0.read().unwrap() }
}




# [ derive (Debug) ]    // note that we def dont want this clonable (we'd rather clone its Arc than all underlying!)
/// KrustyState holds all our direct state flags, or encapsulating state objects like mode-states or modifier-keys collections
pub struct _KrustyState {
    // having this disallows direct instantiation
    _private : (),

    // used for toggling key processing .. should only listen to turn-back-on combo
    pub in_disabled_state: Flag,

    // mod-keys .. manages the modifier-keys, their flags, and their action-wrapping
    pub mod_keys : ModKeys,

    // mode states .. manages the flagged caps-mode states, their trigger keys etc
    pub mode_states : ModeStates,

    // mouse .. manages the mouse btns, wheels, wheel-spin invalidations etc
    pub mouse : Mouse,

    // win-groups .. maanges the three supported window-grouping functionalty
    pub win_groups : WinGroups,

    // for caps-ctrl eqv for caps-tab, caps-wheel, ctrl-move etc, we'll send ctrl press/rel at the right times, and will need to track that
    pub in_managed_ctrl_down_state: Flag,
    // and since wheel support during ctrl-tab is missing in many applications incl IDEs, we'll impl that ourselves
    pub in_ctrl_tab_scroll_state: Flag,
    // and for right-mouse-btn-wheel switche support, we'll track that state too (and send switche specific keys)
    pub in_right_btn_scroll_state: Flag,

}


# [ derive (Debug, Clone) ]
/// Arc wrapped KrustyState for cheap cloning/sharing
pub struct KrustyState ( Arc <_KrustyState> );
// ^^ we'll use this wrapped type so cloning and passing around is cheap

impl Deref for KrustyState {
    type Target = _KrustyState;
    fn deref(&self) -> &_KrustyState { &self.0 }
}





/// Representation for all full state and data fro our Krusty-Board application
pub struct Krusty {
    // this is mostly just a utility wrapper sugar to pass things around
    _private : (),   // prevents direct instantiation of this struct
    // ks is Arc<KrustyState>, holds all state flags
    pub ks : KrustyState,
    // we'll have a combos map to register all combos (key + modifiers + modes) to their mapped actions
    pub cm : CombosMap,
    // we have the InputProcessor itself, which will hold the kbd/mouse bindings, combo-processing-af, the side-thread-queues
    pub iproc : InputProcessor,
}





/// impl for Krusty-State
impl KrustyState {

    pub fn instance () -> KrustyState {
        static INSTANCE: OnceCell<KrustyState> = OnceCell::new();
        INSTANCE .get_or_init ( ||
            KrustyState ( Arc::new ( _KrustyState {
                _private : (),
                in_disabled_state : Flag::default(),

                mod_keys    : ModKeys::new(),
                mode_states : ModeStates::new(),
                mouse       : Mouse::new(),
                win_groups  : WinGroups::new(),

                in_managed_ctrl_down_state : Flag::default(),
                in_ctrl_tab_scroll_state   : Flag::default(),
                in_right_btn_scroll_state  : Flag::default(),
            } ) )
        ) .clone()
    }

    pub fn unstick_all (&self) {
        self.mode_states.clear_flags();
        self.mod_keys.unstick_all();

        use MouseButton::*;
        LeftButton.release(); RightButton.release(); MiddleButton.release();
        X1Button.release(); X2Button.release();

        [  &self.mouse.lbtn.down, &self.mouse.rbtn.down, &self.mouse.mbtn.down,
           &self.in_managed_ctrl_down_state, &self.in_ctrl_tab_scroll_state, &self.in_right_btn_scroll_state,
        ] .into_iter() .for_each (|flag| flag.clear());
    }

    /// Utlity function to create a new Combo-generator (for combo-specification) <br>
    /// By default, it sets the modifier-keys to have mask-release (consumed), and mod-keys to have repeats suppressed (consumed)
    pub fn cg (&self, key:Key) -> ComboGen { ComboGen::new (key, &self) }

    /// Utility function to create a new Combo-Action generator (key-action output type) <br>
    /// By default, it WILL wrap the AF with modifier key guard actions, can be set to not do so w .mkg_nw()
    pub fn ag (&self, key:Key) -> ActionGen_wKey { ActionGen_wKey::new (key, &self) }

    /// Utlity function to create a new Combo-Action-generator (non-key action-function output type). <br>
    /// By default, it WILL NOT wrap the AF with modifier key guard actions, can be set to do so w .mkg_w()
    pub fn ag_af (&self, af:AF) -> ActionGen_wAF { ActionGen_wAF::new (af, &self) }

}





/// impl for Krusty data and state struct
impl Krusty {

    /// create a new Krusty object (holds the KrustyState, the combos-map, and a registry of keys to do default-bindings on)
    pub fn new() -> Krusty {
        Krusty {
            _private : (),
            ks    : KrustyState::instance(),
            cm    : CombosMap::instance(),
            iproc : InputProcessor::instance(),
        }
    }


    #[allow(dead_code)]
    /// utility fn to globally disable krusty functionality
    pub fn setup_global_disable (&self) {
        // todo: might not be straight-forward if we want to handle this at hook receipt
        // .. will need a global check there, as well as selective listening for the re-enable combo when disabled
    }

}





/// simple key-action utilities
pub mod key_utils {

    use std::sync::Arc;
    use crate::*;

    pub fn press                (key:Key) { key.press(); }
    pub fn release              (key:Key) { key.release(); }
    pub fn press_release        (key:Key) { key.press(); key.release(); }
    pub fn double_press_release (key:Key) { press_release(key); press_release(key); }

    pub fn wrapped_press_release (wrap_key:Key, key_action:fn(Key), key:Key) {
        wrap_key.press(); key_action(key); wrap_key.release();
    }
    pub fn wrapped_action (wrap_key:Key, af:AF) -> AF {
        Arc::new ( move || { wrap_key.press(); af(); wrap_key.release(); } )
    }

    // note that these are ONLY to be used when the mod key states DONT need to be tracked (e.g. in composition fallback actions)
    pub fn ctrl_press_release  (key:Key) { wrapped_press_release (Key::Ctrl,  press_release, key) }
    pub fn shift_press_release (key:Key) { wrapped_press_release (Key::Shift, press_release, key) }

    // we'll define some arc-wrapper util fns, but really, its just as easy to just use arcs directly
    /// wraps a given unitary function with NO input args into an Arc Fn
    pub fn action (f:fn()) -> AF { Arc::new (move || f()) }

    /// wraps a given unitary function with ONE input arg into an Arc Fn
    pub fn action_p1<T> (f:fn(T), t:T) -> AF where T: Copy + Send + Sync + 'static { Arc::new (move || f(t)) }

    pub fn no_action           () -> AF { Arc::new ( || {} ) }
    pub fn base_action  (key:Key) -> AF { action_p1 (press_release,        key) }
    pub fn fast_action  (key:Key) -> AF { action_p1 (double_press_release, key) }
    pub fn ctrl_action  (key:Key) -> AF { action_p1 (ctrl_press_release,   key) }
    pub fn shift_action (key:Key) -> AF { action_p1 (shift_press_release,  key) }

}


