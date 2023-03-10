
use std::{
    time::Instant,
    collections::HashSet,
    ops::Deref,
    sync::{
        Arc, RwLock,
        atomic::{AtomicBool, Ordering}
}  };

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
    pub fn check (&self) -> bool { self.0 .load (Ordering::SeqCst) }
    pub fn set   (&self) { self.0 .store (true,  Ordering::SeqCst) }
    pub fn clear (&self) { self.0 .store (false, Ordering::SeqCst) }
}





# [ derive (Debug) ]    // note that we def dont want this clonable (we'd rather clone its Arc than all underlying!)
/// KrustyState holds all our direct state flags, or encapsulating state objects like mode-states or modifier-keys collections
pub struct _KrustyState {
    // having this disallows direct instantiation
    _private : (),

    // used for toggling key processing .. should only listen to turn-back-on combo
    pub in_disabled_state: Flag,

    // mod-keys .. details in its declaration
    pub mod_keys : ModKeys,

    // mode states .. details in its declaration
    pub mode_states : ModeStates,

    // mouse btn states .. need to impl caps-as-ctrl, or right-mouse-scroll behavior etc
    pub mouse_left_btn_down:  Flag,
    pub mouse_right_btn_down: Flag,

    // for caps-ctrl eqv for caps-tab or caps-wheel, we'll send ctrl press/release at the right times, and will need to track that
    pub in_managed_ctrl_down_state: Flag,
    // and for right-mouse-btn-wheel switche support, we'll track that state too (and send switche specific keys)
    pub in_right_btn_scroll_state: Flag,

    pub last_wheel_stamp: RwLock<Instant>,
    pub is_wheel_spin_invalidated: Flag, // invalidated by e.g. mid-spin mod press, or spin stop (spacing > 120ms)
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
    // KrS is Arc<KrustyState>, holds all state flags
    pub ks: KrustyState,
    // we have the kbd and mouse bindings maps for per key/btn/wheel/pointer event action bindings
    pub kbb: KbdBindings,
    pub msb: MouseBindings,
    // we'll have a combos map to register all combos (key + modifiers + modes) to their mapped actions
    pub cm: CombosMap,
    // instead of polluting combos_map, we'll hold a registry for keys to gen default bindings for
    pub default_bind_keys : Arc <RwLock <HashSet <Key>>>,
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

                mouse_left_btn_down  : Flag::default(),
                mouse_right_btn_down : Flag::default(),

                in_managed_ctrl_down_state : Flag::default(),
                in_right_btn_scroll_state  : Flag::default(),

                last_wheel_stamp : RwLock::new(Instant::now()),
                is_wheel_spin_invalidated : Flag::default(),
            } ) )
        ) .clone()
    }

    pub fn unstick_all (&self) {
        self.mode_states.clear_flags();
        self.mod_keys.unstick_all();

        use MouseButton::*;
        LeftButton.release(); RightButton.release(); MiddleButton.release();
        X1Button.release(); X2Button.release();

        [   &self.mouse_left_btn_down, &self.mouse_right_btn_down,
            &self.in_right_btn_scroll_state, &self.in_managed_ctrl_down_state
        ] .into_iter() .for_each (|flag| flag.clear());
    }

}





/// impl for Krusty data and state struct
impl Krusty {

    /// create a new Krusty object (holds the KrustyState, the combos-map, and a registry of keys to do default-bindings on)
    pub fn new() -> Krusty {
        Krusty {
            _private : (),
            ks  : KrustyState::instance(),
            kbb : KbdBindings::instance(),
            msb : MouseBindings::instance(),
            cm  : CombosMap::instance(),
            default_bind_keys : Arc::new (RwLock::new (HashSet::new())),
        }
    }

    /// utlity function to create a new Combo-generator (key-output or combo-specification type)
    pub fn cg (&self, key:Key) -> ComboGen_wKey { ComboGen_wKey::new (key, &self.ks) }

    /// utlity function to create a new Combo-generator (non-key action-function output type)
    pub fn cg_af (&self, af:AF) -> ComboGen_wAF { ComboGen_wAF::new (af, &self.ks) }


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

}


