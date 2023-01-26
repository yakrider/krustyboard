use std::{thread, time, time::Instant, cell::RefCell};
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};
use std::thread::{sleep, spawn};

use derivative::Derivative;

use closure::closure;
// ^^ gives a nice macro that eliminates clutter from having to clone everywhere
// .. we'd use it a lot more, except IDE syntax highlighting doesnt work inside the macro .. which is waay too big a loss for the benefit!

use crate::{BlockInput, KbdEvent, KbdEvntCbMapKeyType::*, KbdKey, MouseButton, MouseEvent, MouseWheel, utils};



// we'll define some easier type aliases (CallBack, ArcFn, Arc-L2S etc) to pass around triggered actions and so on
type CB  = Box <dyn Fn(KbdEvent) + Send + Sync + 'static> ;
type AF  = Arc <dyn Fn() + Send + Sync + 'static> ;

type L2S = Arc <Layer2State> ;
type Key = KbdKey ;


#[derive(Default, Debug)]
pub struct Flag (RwLock<bool>);
// ^^ simple sugar that helps reduce a lot of clutter in code

impl Flag {
    pub fn check (&self) -> bool { *self.0.read().unwrap() }
    pub fn set   (&self) { *self.0.write().unwrap() = true }
    pub fn clear (&self) { *self.0.write().unwrap() = false }

    pub fn set_if_clear (&self) { if !self.check() { self.set() } }
    pub fn clear_if_set (&self) { if self.check() { self.clear() } }
}

#[derive(Derivative)]
#[derivative(Default, Debug)]
pub struct Layer2State {
    // used for toggling key processing .. should only listen to turn-back-on combo
    in_disabled_state: Flag,

    // caps will be tracked for internal reference, and we'll assume we'll ALWAYS operate with caps-lock off
    is_caps_down: Flag,

    // for alt, gotta separately track outside logical state, as sometimes we wanna suppress its down going out (instead it'll go out of sync)
    // this will explicitly track whether the outside seen last alt-key-event was down or not, and whether its consumed so as to mask its release
    is_lalt_down:          Flag,
    is_alt_active:         Flag,
    is_down_lalt_consumed: Flag,

    // since win listeners often act on release (e.g. start menu), its annoying after we've done some win combo to have it pop up
    // so where possible, we'll try and suppress its up if we consumed it internally ourselves
    is_lwin_down:          Flag,
    is_win_active:         Flag,
    is_down_lwin_consumed: Flag,

    // right-alt is completely suppressed, and just its state tracked so it can be used in our own combos
    is_ralt_down:  Flag,

    // tracking shift and ctrl helps avoid querying at runtime, and eases preserving e.g. ctrl-alt-? when alt-? is remapped
    is_lctrl_down:  Flag,
    is_rctrl_down:  Flag,
    is_lshift_down: Flag,
    is_rshift_down: Flag,

    // these three need indiv cloning into spawned threads, hence separate Arcs for them
    in_caret_sel_mode:  Arc<Flag>,
    in_caret_del_mode:  Arc<Flag>,
    in_caret_word_mode: Arc<Flag>,
    in_caret_fast_mode: Arc<Flag>,

    is_mouse_left_btn_down:  Flag,
    is_mouse_right_btn_down: Flag,

    // for caps-ctrl eqv for caps-tab or caps-wheel, we'll send ctrl press/release at the right times, and will need to track that
    in_managed_ctrl_down_state: Flag,
    // and for right-mouse-btn-wheel switche support, we'll track that state too (and send switche specific keys)
    in_right_btn_scroll_state: Flag,

    #[derivative(Default(value = "RwLock::new(Instant::now())"))]
    last_wheel_stamp: RwLock<Instant>,
    is_wheel_spin_invalidated: Flag, // invalidated by e.g. mid-spin mod press, or spin stop (spacing > 120ms)
}

impl Layer2State {
    fn clear_caret_mode_flags (&self) {
        self.in_caret_sel_mode.clear();
        self.in_caret_del_mode.clear();
        self.in_caret_word_mode.clear();
        self.in_caret_fast_mode.clear();
    }
    fn some_shift_down (&self) -> bool { self.is_lshift_down.check() || self.is_rshift_down.check() }
    fn some_ctrl_down (&self)  -> bool { self.is_lctrl_down.check()  || self.is_rctrl_down.check() }
}




#[derive(Default)]
pub struct ActionMaps {
    // this allows us to build maps to hold l2, win, l3 etc by group instead of spread around all over by key
    // the caps and caps-alt mapping comprise the l2 functionality, the win and caps-win comprise l3
    // the idea is to auto compose actual callbacks by key after all the mapping is filled out
    // note ofc that direct key callbacks can still be set for keys not put in these maps
    pub base_act_map       : HashMap<Key,AF>,
    pub caps_act_map       : HashMap<Key,AF>,
    pub lalt_act_map       : HashMap<Key,AF>,
    pub ralt_act_map       : HashMap<Key,AF>,
    pub win_act_map        : HashMap<Key,AF>,
    pub caps_win_act_map   : HashMap<Key,AF>,
    pub caps_lalt_act_map  : HashMap<Key,AF>,
    pub caps_ralt_act_map  : HashMap<Key,AF>,
    pub caps_shift_act_map : HashMap<Key,AF>,
}


#[derive(Default)]
pub struct Krusty {
    // this is mostly just a utility wrapper sugar to pass things around
    _private : (),              // this is to prevent direct instantiation of this struct
    pub l2s : Arc <Layer2State>,
    pub am  : RefCell <ActionMaps>,
    // we'll also keep a mapping of caret-mode keys and their associated flags for l2 impl
    pub cmks: RefCell <HashMap <Key, Arc<Flag>>>
}


impl Krusty {

    // NOTE: we've decided to mostly impl things grouped by related functionality in disparate modules rather than under impl here
    // .. mostly because doing that allows easier piecemeal imports, as well as better pub/private management

    pub fn new() -> Krusty {
        Krusty {
            _private : (),
            l2s  : Arc::new (Layer2State::default()),
            am   : RefCell::new (ActionMaps::default()),
            cmks : RefCell::new (HashMap::new())
    } }

    fn setup_global_disable (&self) {
        // todo: wont be straight-forward if we want to handle this at hook receipt
        // .. will need a global check there, as well as allowing selectively for the re-enable combo!
    }

}


pub mod key_utils {

    use crate::krusty::*;

    pub fn press_release        (key:Key) { key.press(); key.release(); }
    pub fn double_press_release (key:Key) { press_release(key); press_release(key); }

    fn wrapped_press_release (wrap_key:Key, key_action:fn(Key), key:Key) {
        wrap_key.press(); key_action(key); wrap_key.release();
    }
    // ctrl and shift are untracked pure mod keys, so we can blindly send those presses on demand
    pub fn ctrl_press_release       (key:Key) { wrapped_press_release (Key::Ctrl,  press_release, key) }
    pub fn shift_press_release      (key:Key) { wrapped_press_release (Key::Shift, press_release, key) }
    pub fn ctrl_shift_press_release (key:Key) { wrapped_press_release (Key::Ctrl,  shift_press_release, key) }

    // note: ^^ we dont have these versions for alt/win and ctrl-alt/win, as we need to check state before blindly sending those
    // .. instead, can use the smart state restoring action wrappers further below

    // we'll define some arc-wrapper util fns, but really, its just as easy to just use arcs directly
    /// wraps a given unitary function with NO input args into an Arc Fn
    pub fn action (f:fn()) -> AF { Arc::new (move || f()) }

    /// wraps a given unitary function with ONE input arg into an Arc Fn
    pub fn action_p1<T> (f:fn(T), t:T) -> AF where T: Copy + Send + Sync + 'static { Arc::new (move || f(t)) }

    pub fn no_action                () -> AF { Arc::new ( || {} ) }
    pub fn base_action       (key:Key) -> AF { action_p1 (press_release,            key) }
    pub fn fast_action       (key:Key) -> AF { action_p1 (double_press_release,     key) }

    pub fn ctrl_action       (key:Key) -> AF { action_p1 (ctrl_press_release,       key) }
    pub fn shift_action      (key:Key) -> AF { action_p1 (shift_press_release,      key) }
    pub fn ctrl_shift_action (key:Key) -> AF { action_p1 (ctrl_shift_press_release, key) }








}



pub mod mod_keys_setups { // tracking for tracked modifier keys e.g. capslock, lAlt, rAlt, lWin, lShift, rShift, lCtrl, rCtrl

    use crate::krusty::*;


    pub fn setup_caps_tracking (l2sr:&L2S) {
        // note that for caps, we completely block it from ever being sent up, and just manage internally!
        if Key::CapsLock.is_toggled() {
            Key::CapsLock.press();  // toggle off first if necessary (to clear key light)
        }
        let l2s = l2sr.clone();
        Key::CapsLock.block_bind(KeyDownCallback, move |_| {
            if !l2s.is_caps_down.check() {  // capslock can come as repeats like other mod keys
                l2s.is_caps_down.set();
                l2s.is_wheel_spin_invalidated.set_if_clear();
            }
            if l2s.is_mouse_left_btn_down.check() && !l2s.in_managed_ctrl_down_state.check() {
                l2s.in_managed_ctrl_down_state.set();
                spawn ( || Key::LControl.press() );
            }
            if l2s.is_lalt_down.check() && l2s.is_alt_active.check() { // immediately invalidate and clear any down lalt found upon caps activation!
                l2s.is_alt_active.clear();   // note that internally tracked physical is_alt_down will continue to be down!
                l2s.is_down_lalt_consumed.set_if_clear();
                let l2s = l2s.clone();
                spawn (move || release_alt_w_ctrl_masking(&l2s) );
            }
            if l2s.is_lwin_down.check() && l2s.is_win_active.check() { // immediately invalidate and clear any down lwin found upon caps activation!
                l2s.is_win_active.clear();   // note that internally tracked physical is_win_down will continue to be down!
                l2s.is_down_lwin_consumed.set_if_clear();
                let l2s = l2s.clone();
                spawn (move || release_win_w_ctrl_masking(&l2s) );
            }
        });
        let l2s = l2sr.clone();
        Key::CapsLock.block_bind(KeyUpCallback, move |_| {
           l2s.is_caps_down.clear_if_set();
            if l2s.in_managed_ctrl_down_state.check() {
                l2s.in_managed_ctrl_down_state.clear();
                spawn ( || Key::LControl.release() );
            }
            // the following isnt strictly necessary, but useful in case some keyup falls through
            l2s.clear_caret_mode_flags();
            // since we deactivate alt/win on caps press, check to see if we want to reactivate them
            // note: we'll setup a delay for activation to allow for some sloppy combo releases etc
            if l2s.is_lalt_down.check() {
                let l2s = l2s.clone();
                spawn ( move || {
                    sleep(time::Duration::from_millis(200));
                    if l2s.is_lalt_down.check() && !l2s.is_alt_active.check() {
                        Key::LAlt.press(); l2s.is_alt_active.set(); l2s.is_down_lalt_consumed.set_if_clear();
                } } );
            }
            if l2s.is_lwin_down.check() {
                let l2s = l2s.clone();
                spawn ( move || {
                    sleep(time::Duration::from_millis(200));
                    if l2s.is_lwin_down.check() && !l2s.is_win_active.check() {
                        Key::LWin.press(); l2s.is_win_active.set(); l2s.is_down_lwin_consumed.set_if_clear();
                } } );
            }
            // also, now might be a good time to try and get any out-of-sync l-alt states back in line if necessary
            //ensure_alt_inactive(&l2s);
            //ensure_win_inactive(&l2s);
            // ^^ nah we updated to actually ensure they become active via a delayed key press instead
        });
    }



    pub fn setup_left_alt_tracking (l2sr:&L2S) {
        // for left-alt, we'll let it go through, but instead of letting it spam on repeat on long presses, we just preserve the logical
        // >  state but suppress the spams .. that helps us reason safely about its logical state in all the other combos, esp non-caps ones,
        // >  plus, it makes alt-combos for keys not handled here continue to work as expected
        // ugh, however, letting alt out unblocked, does cause issues at times (eg. when it moves focus to menu items .. so we'll track both its
        // >  internal, as well as external state for when we've had to suppress it while caps was down etc
        let l2s = l2sr.clone();
        Key::LAlt.blockable_bind(KeyDownCallback, move |_| {
            // so goal here is, any presses with caps active, we suppress alt going outside
            // and since we can have caps come in AFTER alt is already down, we'll have to capture disparity states, as well as to deal
            // >  with restoring that when either caps/alt gets released etc
            // so, if caps is down, suppress alt, capture state, let specific combos deal with the disparity
            // plus, even when caps isnt down, suppress the repeated events
            // plus, we allow a small grace period after caps is released during which we dont register prior pressed alt (allows sloppy combo release)
            if l2s.is_caps_down.check() {
                // caps is down, record alt being down if not already, but either way block it (so no change to alt-active state)
                l2s.is_lalt_down.set_if_clear();
                l2s.is_down_lalt_consumed.clear_if_set();
                BlockInput::Block
            } else {
                if l2s.is_lalt_down.check() {
                    // caps isnt down, but alt was, so its repeat .. we'll block it even if out-of-sync or its coming after combo caps release
                    BlockInput::Block
                } else {
                    // caps isnt down, and alt wasnt down, so record states and let it through
                    l2s.is_lalt_down.set();
                    l2s.is_alt_active.set_if_clear();
                    l2s.is_down_lalt_consumed.clear_if_set();
                    l2s.is_wheel_spin_invalidated.set_if_clear();
                    BlockInput::DontBlock
                }
            }
        });
        let l2s = l2sr.clone();
        Key::LAlt.blockable_bind(KeyUpCallback, move |_| {
            // if caps is pressed, or alt is already inactive (via masked-rel, press-rel etc), we block it
            // else if win was consumed, we release with mask, else we can actually pass it through unblocked
            l2s.is_lalt_down.clear_if_set();
            if !l2s.is_alt_active.check() || l2s.is_caps_down.check() {
                BlockInput::Block
            } else {
                l2s.is_alt_active.clear();
                if !l2s.is_down_lalt_consumed.check() {
                    BlockInput::DontBlock
                } else {
                    let l2s = l2s.clone();
                    spawn (move || release_alt_w_ctrl_masking(&l2s) );
                    BlockInput::Block
                }
            }
        });
    }
    // note that we're making everything cnosume down lalt/lwin .. which makes sense, as really any key press action should do so
    pub fn consume_down_lalt (l2s:&L2S) {
        // utility for any key with alt/win action to consume down state to mask its keyup later .. (which should be everyone always)
        l2s.is_down_lalt_consumed.set_if_clear();
    }
    pub fn release_alt_w_ctrl_masking (l2s:&L2S) {
        // masking w ctrl helps avoid/reduce focus loss to menu
        Key::LControl.press(); Key::LAlt.release(); Key::LControl.release();
        if l2s.some_ctrl_down() { Key::Ctrl.press() }
    }
    pub fn ensure_alt_inactive (l2s:&L2S) {
        // utility to ensure lalt is inactive regardless if held down
        // shouldnt really be necessary since there are action wrappers available to set/restore alt/win for any need at any alt/win state
        l2s.is_down_lalt_consumed.set_if_clear();
        if l2s.is_alt_active.check() { l2s.is_alt_active.clear(); release_alt_w_ctrl_masking(l2s); }
    }
    pub fn ensure_alt_active (l2s:&L2S) {
        // utility to get alt out reliably whether its currently pressed or not, while keeping state tracking updated
        // this should really ONLY be necessary where we want alt to be left hanging on until later .. e.g. to simulate alt-tab
        l2s.is_down_lalt_consumed.set_if_clear();
        if !l2s.is_alt_active.check() { l2s.is_alt_active.set(); Key::LAlt.press(); }
    }




    pub fn setup_left_win_tracking (l2sr:&L2S) {
        // setup should mostly mirror lalt .. we track state, suppress repeats, block it if caps is also held
        // (and in caps, if caps comes in while lwin is down, we immediately invalidate it)
        // different from lalt, we mark the lwin-dn as consumed if we use it for our mappings, and mask its later release w ctrl
        // (this avoids triggering win start menu etc when the lwin is released after use for our shortcuts)
        let l2s = l2sr.clone();
        Key::LWin.blockable_bind(KeyDownCallback, move |_| {
            if l2s.is_caps_down.check() {
                // caps is down, record lwin being down if not already, but either way block it (so no change to alt-active state)
                l2s.is_lwin_down.set_if_clear();
                l2s.is_down_lwin_consumed.clear_if_set();
                BlockInput::Block
            } else {
                if l2s.is_lwin_down.check() {
                    // caps isnt down but lwin was, so its a repeat .. we'll block it even if out-of-sync or coming after combo caps release
                    BlockInput::Block
                } else {
                    // caps isnt down, lwin wasnt down, so record states and let through
                    l2s.is_lwin_down.set();
                    l2s.is_win_active.set_if_clear();
                    l2s.is_down_lwin_consumed.clear_if_set();
                    l2s.is_wheel_spin_invalidated.set_if_clear();
                    BlockInput::DontBlock
                }
            }
        });
        let l2s = l2sr.clone();
        Key::LWin.blockable_bind(KeyUpCallback, move |_| {
            // if caps is pressed, or win is already inactive, we block it
            // else if win was consumed, we release with mask, else we can actually pass it through unblocked
            l2s.is_lwin_down.clear_if_set();
            if !l2s.is_win_active.check() || l2s.is_caps_down.check() {
                BlockInput::Block
            } else { // so its active, and no caps
                l2s.is_win_active.clear();
                if !l2s.is_down_lwin_consumed.check() {
                    BlockInput::DontBlock
                } else {
                    let l2s = l2s.clone();
                    spawn (move || release_win_w_ctrl_masking(&l2s) );
                    BlockInput::Block
                }
            }
        });
    }
    // note that we're making everything cnosume down lalt/lwin .. which makes sense, as really any key press action should do so
    pub fn consume_down_lwin (l2s:&L2S) {
        // utility for any key with alt/win action to consume down state to mask its keyup later .. (which should be everyone always)
        l2s.is_down_lwin_consumed.set_if_clear();
    }
    pub fn release_win_w_ctrl_masking (l2s:&L2S) {
        // masking w ctrl helps avoid/reduce focus to win-start-menu etc
        // .. in theory the release doesnt have to interspersed, just needs a ctrl between the press and release of win, but meh, might as well
        Key::LControl.press(); Key::LWin.release(); Key::LControl.release();
        if l2s.some_ctrl_down() { Key::Ctrl.press() }
    }
    pub fn ensure_win_inactive (l2s:&L2S) {
        // utility to ensure lwin is inactive regardless if held down
        // shouldnt really be necessary since there are action wrappers available to set/restore alt/win for any need at any alt/win state
        l2s.is_down_lwin_consumed.set_if_clear();
        if l2s.is_win_active.check() { l2s.is_win_active.clear(); release_win_w_ctrl_masking(l2s); }
    }
    pub fn ensure_win_active (l2s:&L2S) {
        // utility to get win out reliably whether its currently pressed or not, while keeping state tracking updated
        // this should really ONLY be necessary where we want win to be left hanging on until later .. e.g. for alt to simulate alt-tab
        l2s.is_down_lwin_consumed.set_if_clear();
        if !l2s.is_win_active.check() { l2s.is_win_active.set(); Key::LWin.press(); }
    }
    // todo: most of these can now be removed since we have the guarded action wrappers that are much better to use than these
    //       .. not least because these are open-ended operations after which while the state are still tracked/synced, it could
    //       .. leave it changed from what it was previously ..




    pub fn setup_right_alt_tracking (l2s:&L2S) {
        // we'll completely disable its natural action other than tracking state so our own combos can use it as a separate key
        // usage goals: as shift for keys on board-left-half, as l2 fast mode, ralt-space as enter, and other combos for board-right
        Key::RAlt.block_bind (KeyDownCallback, closure! ( clone l2s, |_| { l2s.is_ralt_down.set_if_clear() } ) );
        Key::RAlt.block_bind (KeyUpCallback,   closure! ( clone l2s, |_| { l2s.is_ralt_down.clear_if_set() } ) );
    }


    // for the rest of the lshift/rshift, lctrl/rctrl .. we just want to mark the flag and let them through
    pub fn setup_left_shift_tracking (l2s:&L2S) {
        Key::LShift.non_blocking_bind ( KeyDownCallback, closure! ( clone l2s, |_| l2s.is_lshift_down.set_if_clear() ) );
        Key::LShift.non_blocking_bind ( KeyUpCallback,   closure! ( clone l2s, |_| l2s.is_lshift_down.clear_if_set() ) );
    }
    pub fn setup_right_shift_tracking (l2s:&L2S) {
        Key::RShift.non_blocking_bind ( KeyDownCallback, closure! ( clone l2s, |_| l2s.is_rshift_down.set_if_clear() ) );
        Key::RShift.non_blocking_bind ( KeyUpCallback,   closure! ( clone l2s, |_| l2s.is_rshift_down.clear_if_set() ) );
    }
    pub fn setup_left_ctrl_tracking (l2s:&L2S) {
        Key::LControl.non_blocking_bind ( KeyDownCallback, closure! ( clone l2s, |_| l2s.is_lctrl_down.set_if_clear() ) );
        Key::LControl.non_blocking_bind ( KeyUpCallback,   closure! ( clone l2s, |_| l2s.is_lctrl_down.clear_if_set() ) );
    }
    pub fn setup_right_ctrl_tracking (l2s:&L2S) {
        Key::RControl.non_blocking_bind ( KeyDownCallback, closure! ( clone l2s, |_| l2s.is_rctrl_down.set_if_clear() ) );
        Key::RControl.non_blocking_bind ( KeyUpCallback,   closure! ( clone l2s, |_| l2s.is_rctrl_down.clear_if_set() ) );
    }

}



pub mod mouse_btn_setups { // setups for mouse btn handling

    use crate::krusty::{*, key_utils::*, action_map_utils::*};

    pub fn setup_mouse_left_btn_handling (l2sr:&L2S) {
        let l2s = l2sr.clone();
        MouseButton::LeftButton.block_bind(true, move |_| {
            l2s.is_mouse_left_btn_down.set_if_clear();
            if l2s.is_caps_down.check() {
                l2s.in_managed_ctrl_down_state.set_if_clear();
                Key::LControl.press(); // this allows caps-as-ctrl for drag drop etc
            }
            MouseButton::LeftButton.press()
        });
        let l2s = l2sr.clone();
        MouseButton::LeftButton.block_bind(false, move |_| {
            l2s.is_mouse_left_btn_down.clear_if_set();
            sleep(time::Duration::from_millis(10));
            MouseButton::LeftButton.release();
        });
        // ^^ in theory could do non-blocking bind for release, but that can occasionally expose us to timing issues as press is on block
        //      and that will be slower to send the actual press from the spawned thread (so making release slower too helps)
    }

    pub fn setup_mouse_right_btn_handling (l2sr:&L2S) {
        // tracking btn down is for ctrl-wheel (zoom) etc, and right-btn-down-state is for switche scroll (via F21/F22/F23)
        let l2s = l2sr.clone();
        MouseButton::RightButton.non_blocking_bind(true, move |_| {
            l2s.is_mouse_right_btn_down.set_if_clear();
        });
        let l2s = l2sr.clone();
        let switche_action = alt_action (Key::F23, l2sr.clone());
        MouseButton::RightButton.non_blocking_bind(false, move |_| {
            l2s.is_mouse_right_btn_down.clear_if_set();
            if l2s.in_right_btn_scroll_state.check() {
                l2s.in_right_btn_scroll_state.clear();
                switche_action();
            }
        });
    }

    pub fn setup_mouse_x_btn_1_handling () {
        // turns out just doing press/release on initial press works snappier/more-reliable than trying to be true to btn-holds
        MouseButton::X1Button.block_bind(true, |_| {
            MouseButton::MiddleButton.press(); MouseButton::MiddleButton.release();
        });
        MouseButton::X1Button.block_bind(false, |_| { });
    }

    pub fn setup_mouse_x_btn_2_handling () {
        MouseButton::X2Button.block_bind(true, |_| {
            MouseButton::MiddleButton.press(); MouseButton::MiddleButton.release();
        });
        MouseButton::X2Button.block_bind(false, |_| { });
    }

}



pub mod mouse_wheel_setups { // setups for mouse wheel handling

    use crate::krusty::{*, key_utils::*, action_map_utils::*, mod_keys_setups::*};
    use windows::Win32::UI::WindowsAndMessaging::WHEEL_DELTA;
    use crate::utils::window_utils::get_fgnd_win_class;

    pub fn handle_wheel_guarded (delta:i32, l2sr:&L2S) {
        // this is mostly to make the super-fast inertial smooth-scroll wheel on my (MX3) mouse a lil more usable by spacing things out
        // also, the invalidation setup prevents things like caps down when wheel is still unintentionally inertially spinning to trigger zooms etc
        let last_stamp = *l2sr.last_wheel_stamp.read().unwrap();
        *l2sr.last_wheel_stamp.write().unwrap() = Instant::now();
        //let gap = l2sr.last_wheel_stamp.read().unwrap().duration_since(last_stamp);
        //println!("{:#?}", dur.as_millis());
        const GUARD_DUR_MS: u128 = 120;  // from dur printouts above, looked like max inertial gap is 120 (min 7ms, usually <100)
        if !l2sr.is_wheel_spin_invalidated.check() {
            handle_wheel_action(delta, l2sr);
        } else if GUARD_DUR_MS < l2sr.last_wheel_stamp.read().unwrap().duration_since(last_stamp).as_millis() {
            l2sr.is_wheel_spin_invalidated.clear();
            handle_wheel_action(delta, l2sr);
        } else {
            // if its invalidated AND wheel-spin spacing is below guard-dur, we suppress the wheel event
        }
    }

    pub fn handle_wheel_action (delta:i32, l2sr:&L2S) {
        let incr = delta / WHEEL_DELTA as i32;
        if l2sr.is_mouse_right_btn_down.check() {
            // right-mouse-btn-wheel support for switche task switching
            l2sr.in_right_btn_scroll_state.set_if_clear();
            let key = if incr.is_positive() { Key::F22 } else { Key::F21 };
            //alt_press_release(key, l2sr);
            alt_press_release_guarded (key, l2sr);
        } else  if l2sr.is_lalt_down.check() {
            // wheel support for scrolling in windows native alt-tab task-switching screen
            // this requires a system call to check alt-tab window, so push it out to thread
            l2sr.is_down_lalt_consumed.set_if_clear();
            let l2s = l2sr.clone();
            spawn ( move || {
                if get_fgnd_win_class() == "MultitaskingViewFrame" { // alt-tab states
                    ensure_alt_active(&l2s);
                    handle_alt_tab_wheel(incr)
                } else { incr_volume(incr); } // simple alt wheel for volume
            } );
        } else if l2sr.is_lwin_down.check() {
            l2sr.is_down_lwin_consumed.set_if_clear();
            incr_brightness(incr); // win-wheel for brightness
        } else if l2sr.is_caps_down.check() {
            l2sr.in_managed_ctrl_down_state.set_if_clear();
            Key::LControl.press();
            MouseWheel::DefaultWheel.scroll(delta); // caps-wheel as ctrl-wheel (zoom etc)
        } else if l2sr.some_shift_down() {
            //handle_horiz_scroll_wheel(incr);
            // ^^ todo:: .. (and for now, just let default pass through)
            MouseWheel::DefaultWheel.scroll(delta);
        } else {
            MouseWheel::DefaultWheel.scroll(delta);
        }
    }

    pub fn incr_volume(incr:i32) {
        let key = if incr > 0 { Key::VolumeUp } else { Key::VolumeDown };
        (0 .. incr.abs()) .for_each(|_| key.press());
    }

    pub fn incr_brightness (incr:i32) {
        static INCR_STEP:i32 = 1;
        //utils::brightness_ps_wmi::incr_brightness(INCR_STEP*incr);
        //utils::brightness_utils::incr_brightness(INCR_STEP*incr);
        spawn (move || utils::brightness_utils::incr_brightness(INCR_STEP*incr));
    }

    pub fn handle_alt_tab_wheel (incr:i32) {
        // todo potentially impl additional separate timer-spacing here, to slow this down even more than regular wheel spacing
        // note that for these we DONT want to release Alt key .. presumably, expecting it to be physically released later
        if incr.is_positive() { shift_press_release(Key::Tab) }
        else { press_release(Key::Tab) }
    }

    pub fn handle_horiz_scroll_wheel (incr:i32) {
    }

    pub fn setup_mouse_wheel_handling (l2sr:&L2S) {
        let l2s = l2sr.clone();
        MouseWheel::DefaultWheel.block_bind(true, move |ev| {
            ev .wheel_ev_data .iter() .for_each (|d| handle_wheel_guarded(d.delta, &l2s) );
        });
        let l2s = l2sr.clone();
        MouseWheel::DefaultWheel.block_bind(false, move |ev| {
            ev .wheel_ev_data .iter() .for_each (|d| handle_wheel_guarded(d.delta, &l2s) );
        })
    }

}



pub mod special_keys_setups { // special keys handling e.g. Space, Tab


    // NOTE: so far we have decided not to hold mappings for ctrl or shift (and cascadingly for ctrl-alt, ctrl-win, shift-alt, shift-win etc)
    // further, we make the action map composition default to pure ctrl/shift action when those natural combos are pressed
    // >  which is better than not checking for those, as this will allow for natural ctrl-alt, shift-alt, ctrl-win etc to pass through
    // however, that means if we need to setup combos that want to use shift or ctrl keys, we'll have to do it directly

    use crate::krusty::{*, key_utils::*};

    pub fn setup_direct_binding_keys (k:&Krusty) {

        // currently, we dont need to do it for anything
        // .. even for shift-tick, its handled by checking for shift in that base-action itself

        // HOWEVER, for anything that needs ctrl/shift while alt/win/ralt is down, we have no easy way currently to do it via action maps
        // .. and thats coz during am cb composition, shift/ctrl check defaulting to base needs to be before alt/ralt/win conditionals
        // the messy alternative if we wanted to do it via action maps, we'd have base action check alt/ctrl/shift etc etc and set that for all actions

        // ALSO, for things that require actual action in key-up callbacks, we'd do it direclty here because key-dn actions arent registered in maps
        // >  and so action-maps composed callbacks can only either set key-up to do nothing, or at most clear caret flags

        // IMPORTANT : note that whatever we put here, we will NOT want to include them in ANY action maps !!


    }


}




pub mod action_map_utils {

    use crate::krusty::{*, key_utils::*, mod_keys_setups::*};

    /// note: registering a caret mode key auto sets their caps and caps-lalt/ralt actions to nothing, others combos can be set as usual
    pub fn register_caret_mode_key (k: &Krusty, key:Key, mode_flag_r:&Arc<Flag>) {
        k.cmks.borrow_mut() .insert(key, mode_flag_r.clone());
        add_caps_mapping      (k, key, no_action());  // caret mode keys trigger w caps down, cant send anything during that
        add_caps_lalt_mapping (k, key, no_action());  // lalt is typically word-nav caret-mode, so this gotta be silent too
        add_caps_ralt_mapping (k, key, no_action());  // ralt is typically fast-nav caret-mode, so this gotta be silent too
    }

    /// if the key is registered for caret mode, this will generate the (key-dn, key-up) caret-flag setting actions
    fn gen_key_caret_mode_actions (k:&Krusty, key:Key) -> (AF,AF) {
        if let Some(cmfr) = k.cmks.borrow().get(&key) {
            let (cmf_c1, cmf_c2) = (cmfr.clone(), cmfr.clone()); // gotta clone to move into AF
            return ( Arc::new (move || cmf_c1.set_if_clear()), Arc::new (move || cmf_c2.clear_if_set()) )
            // ^^ key-dn and key-up caret-mode-actions for a caret-mode-key is to set and clear its caret-mode-flag
        }
        (no_action(), no_action()) // empty (dn,up) caret-flag-actions for non-caret keys
    }

    fn compose_action_maps_cb (k:&Krusty, key:Key) -> (CB, CB) {
        // lets make a utility closure that extracts the AF for this key from some specified am map
        let af = move |m:&HashMap<Key,AF>| { m.get(&key).map(|afr| afr.clone()) };
        // then retrieve all the registered actions for this key
        let am = &k.am.borrow();
        let (mba, mca, mlaa, mraa, mwa, mcwa, mclaa, mcraa, mcsa) = (
            af(&am.base_act_map), af(&am.caps_act_map), af(&am.lalt_act_map), af(&am.ralt_act_map),
            af(&am.win_act_map), af(&am.caps_win_act_map), af(&am.caps_lalt_act_map), af(&am.caps_ralt_act_map), af(&am.caps_shift_act_map)
        );
        // we cant move local options to closure, just the Arcs inside, so do that part of composition before making closure
        let (na, pba, ba) = (no_action(), base_action(key), mba.unwrap_or(base_action(key))); // pure-base-action vs possibly mapped base-action
        // for alt/win, we'll default to base to give natural alt/win-key combos (as the mod key alt/win press will already be out)
        // however, the actual alt/win state might not be active, so we'll wrap them to ensure active states
        let laa = mlaa.unwrap_or (alt_active_action (k.l2s.clone(), base_action(key)));
        let wa  = mwa.unwrap_or  (win_active_action (k.l2s.clone(), base_action(key)));;
        // ralt is by default mapped to shift unless specifically set otherwise!
        let raa = mraa.unwrap_or(shift_action(key));
        // we'll set caps-as-ctrl and caps-shift as ctrl-shift unless specifically set otherwise
        let (ca, csa) = ( mca.unwrap_or(ctrl_action(key)), mcsa.unwrap_or(ctrl_shift_action(key)) );
        // we'll set caps-as-ctrl to work with lalt/lwin combos unless specifically set otherwise!
        let (claa, cwa) = ( mclaa.unwrap_or(ctrl_alt_action(key,k.l2s.clone())), mcwa.unwrap_or(ctrl_win_action(key,k.l2s.clone())) );
         // for our remaining combo of caps with ralt (which is always suppressed), we'll default to no-action
        let craa = mcraa.unwrap_or(no_action());
        // for caret-mode support, we'll check if the key is in caret-mode keys, and create flag update actions
        let (cma_dn, cma_up) = gen_key_caret_mode_actions (k, key);
        // now we can finally compose these Arc action clones into the key action closure we need
        let l2s = k.l2s.clone();
        let cb_dn = Box::new ( move |_| { // note that we're ignoring the KbdEvent passed in (it'd have KbdEventType and key vk/sc codes)
            cma_dn();
            if l2s.is_caps_down.check() {
                // supports caps-mod combos .. if need caps-mod1-mod2 combos, can code the third mod check directly in that key's caps-mod action
                if      l2s.is_lwin_down.check() { cwa  () }
                else if l2s.is_lalt_down.check() { claa () }
                else if l2s.is_ralt_down.check() { craa () }
                else if l2s.some_shift_down()    { csa  () }
                else if l2s.some_ctrl_down()     { ba   () }
                else                             { ca   () }
            } // note: for the rest (lwin, lalt, ralt), we wont define multi-combos for now .. can add if need arises
            else if l2s.is_ralt_down.check() { raa() }
            // ^^ dont care about natural action for this, its completely suppressed
            else if l2s.some_ctrl_down() || l2s.some_shift_down() { ba() }  // might as well do ba() here instead of pba()
            // ^ for natural combos with ctrl or shift (both for alt and win), this ensures mapped alt-? combos dont mess those
            //   .. and sending base action is adequate since the ctrl/shift/alt will already be active
            else if l2s.is_lalt_down.check() && l2s.is_lwin_down.check() { ba() }
            // ^^ similarly, this allows natural win-alt combos to go through
            else if l2s.is_lalt_down.check() { laa() }
            else if l2s.is_lwin_down.check() { wa() }
            else { ba() }
        } );
        // and finally, the key up action, which is really just caret mode flag action if any
        let cb_up = Box::new ( move |_| { cma_up() } );
        (cb_dn, cb_up)
    }

    pub fn bind_all_from_action_maps (k:&Krusty) {
        // first we want to register all the keys in all the action maps into a keyset
        let keys: &RefCell<HashSet<Key>> = &RefCell::new(HashSet::new()); // using as ref avoids need for clones
        let reg = move |m:&HashMap<Key,AF>| { m.keys() .for_each (|kr| { keys.borrow_mut().insert(*kr); } ) };
        let am = &k.am.borrow();
        reg(&am.base_act_map); reg(&am.caps_act_map); reg(&am.lalt_act_map); reg(&am.ralt_act_map);
        reg(&am.win_act_map); reg(&am.caps_win_act_map); reg(&am.caps_lalt_act_map); reg(&am.caps_ralt_act_map);

        // then for each key in the set, we'll gen the composed action and bind it
        for key in keys.borrow().iter() {
            let (cb_dn, cb_up) = compose_action_maps_cb (k, *key);
            key.block_bind (KeyDownCallback, cb_dn);
            key.block_bind (KeyUpCallback,   cb_up);
        }
    }



    pub fn add_base_mapping (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .base_act_map .insert (key, action); }

    pub fn add_ralt_mapping (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .ralt_act_map .insert (key, action); }


    // for the caps mapping, and all of its combos that we support (dont have to worry about state, as w/ caps alt/win are masked/suppressed)
    // NOTE: we dont need one for caps-ctrl, as caps default is ctrl ... (though if we really ran out of combos, lol(!), we could add it)
    pub fn add_caps_mapping       (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .caps_act_map       .insert (key, action); }
    pub fn add_caps_ralt_mapping  (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .caps_ralt_act_map  .insert (key, action); }
    pub fn add_caps_lalt_mapping  (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .caps_lalt_act_map  .insert (key, action); }
    pub fn add_caps_win_mapping   (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .caps_win_act_map   .insert (key, action); }
    pub fn add_caps_shift_mapping (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .caps_shift_act_map .insert (key, action); }



    // for alt/win combos, since an alt/win-dn goes out on first alt/win press, we'll make util fns to map w that alt/win-dn either kept active or inactivated
    // we'll also mark the down lalt/lwin as consumed and mask its release with ctrl so it doesnt activate menus/start-btn etc


    fn checked_masked_alt_release (l2s:&L2S) {
        if !l2s.is_lalt_down.check() { Key::LAlt.release() }
        else { release_alt_w_ctrl_masking(l2s) }
    }
    pub fn alt_key_action_guarded (key:Key, key_action:fn(Key), l2s:&L2S) {
        l2s.is_down_lalt_consumed.set_if_clear();
        if l2s.is_alt_active.check() { key_action(key) }
        else { Key::LAlt.press(); key_action(key); checked_masked_alt_release(&l2s); }
    }
    pub fn alt_press_release_guarded      (key:Key, l2s:&L2S) { alt_key_action_guarded (key, press_release, l2s) }
    pub fn ctrl_alt_press_release_guarded (key:Key, l2s:&L2S) { alt_key_action_guarded (key, ctrl_press_release, l2s) }

    pub fn alt_action      (key:Key, l2s:L2S) -> AF { Arc::new (move || { alt_press_release_guarded      (key, &l2s); }) }
    pub fn ctrl_alt_action (key:Key, l2s:L2S) -> AF { Arc::new (move || { ctrl_alt_press_release_guarded (key, &l2s); }) }

    fn down_lalt_consuming_action (l2s:L2S, af:AF) -> AF {
        Arc::new ( move || { consume_down_lalt(&l2s); af(); } )
    }
    pub fn alt_active_action (l2s:L2S, af:AF) -> AF {
        Arc::new (move || {
            l2s.is_down_lalt_consumed.set_if_clear();
            if l2s.is_alt_active.check() { af() }
            else { Key::LAlt.press(); af(); checked_masked_alt_release(&l2s); }
        })
    }
    pub fn alt_inactive_action (l2s:L2S, af:AF) -> AF {
        // no fancy timing delay needed for this, unlike the attempt to pacify the win-start popout for win key below
        Arc::new (move || {
            l2s.is_down_lalt_consumed.set_if_clear();
            if !l2s.is_alt_active.check() { af() }
            else { checked_masked_alt_release(&l2s); af(); Key::LAlt.press(); }
        })
    }
    pub fn add_lalt_mapping (k:&Krusty, key:Key, action:AF) {
        k.am.borrow_mut() .lalt_act_map .insert (key, down_lalt_consuming_action(k.l2s.clone(), action));
    }
    pub fn add_lalt_mapping_w_alt_active (k:&Krusty, key:Key, action:AF) {
        k.am.borrow_mut() .lalt_act_map .insert (key, alt_active_action(k.l2s.clone(), action));
    }
    pub fn add_lalt_mapping_w_alt_inactive (k:&Krusty, key:Key, action:AF) {
        k.am.borrow_mut() .lalt_act_map .insert (key, alt_inactive_action(k.l2s.clone(), action));
    }





    // lwin is now basically mirror of lalt .. so track whether active, mark consumed, masked releases etc as above

    fn checked_masked_win_release (l2s:&L2S) {
        // not sure if these checks are necessary or useful .. maybe we could just mask everything all the time
        if !l2s.is_lwin_down.check() { Key::LWin.release() }
        else { release_win_w_ctrl_masking(l2s) }
    }
    pub fn win_key_action_guarded (key:Key, key_action:fn(Key), l2s:&L2S) {
        l2s.is_down_lwin_consumed.set_if_clear();
        if l2s.is_win_active.check() { key_action(key) }
        else { Key::LWin.press(); key_action(key); checked_masked_win_release(&l2s); }
    }
    pub fn win_press_release_guarded      (key:Key, l2s:&L2S) { win_key_action_guarded (key, press_release, l2s) }
    pub fn ctrl_win_press_release_guarded (key:Key, l2s:&L2S) { win_key_action_guarded (key, ctrl_press_release, l2s) }

    pub fn win_action      (key:Key, l2s:L2S) -> AF { Arc::new (move || { win_press_release_guarded      (key, &l2s); }) }
    pub fn ctrl_win_action (key:Key, l2s:L2S) -> AF { Arc::new (move || { ctrl_win_press_release_guarded (key, &l2s); }) }


    fn down_lwin_consuming_action (l2s:L2S, af:AF) -> AF {
        Arc::new ( move || { consume_down_lwin(&l2s); af(); } )
    }
    pub fn win_active_action (l2s:L2S, af:AF) -> AF {
        Arc::new (move || {
            l2s.is_down_lwin_consumed.set_if_clear();
            if l2s.is_win_active.check() { af() }
            else { Key::LWin.press(); af(); checked_masked_win_release(&l2s); }
        })
    }
    pub fn win_inactive_action (l2s:L2S, af:AF) -> AF {
        Arc::new (move || {
            l2s.is_down_lwin_consumed.set_if_clear();
            if !l2s.is_win_active.check() { af() }
            else {
                //checked_masked_win_release(&l2s); af(); Key::LWin.press();
                // ^^ nu-uh, apparently win start menu triggers unless there's some timing gap between the masked release and another press
                // .. and from quick expts apparently even 80ms is sometimes too little .. not sure if also machine dependent
                let l2s = l2s.clone(); let af = af.clone();
                spawn (move || {
                    checked_masked_win_release(&l2s); af();
                    sleep(time::Duration::from_millis(100));
                    Key::LWin.press();
                } );
            }
        })
    }
    pub fn add_win_mapping (k:&Krusty, key:Key, action:AF) {
        k.am.borrow_mut() .win_act_map .insert (key, down_lwin_consuming_action(k.l2s.clone(), action));
    }
    pub fn add_win_mapping_w_win_active (k:&Krusty, key:Key, action:AF) {
        k.am.borrow_mut() .win_act_map .insert (key, win_active_action(k.l2s.clone(), action));
    }
    pub fn add_win_mapping_w_win_inactive (k:&Krusty, key:Key, action:AF) {
        k.am.borrow_mut() .win_act_map .insert (key, win_inactive_action(k.l2s.clone(), action));
    }


}



pub mod l2_utils {

    /* setup options summary:
     - only j/k for left/right get d-for-word-nav mode speedup (native word nav by sending ctrl)
     - those and i/comma for up/down get e-for-double-speed nav mode (2x nav) .. i/comma get that for d too
     - h/l/u/m for home/end/pgup/pgdown get no speedup modes
     - r/f do sel/del modes, and those can be freely combined with the d/e word/fast nav modes
     - in del mode, left/home/up/pgup get ExtBackspace, right/end/down/pgdn get ExtDelete
     - in del mode, left/right do direct bksp/del, but others get select then bksp/del
     */
    use crate::krusty::{*, key_utils::*, action_map_utils::*};

    /// action-function generator type for l2 configuration
    pub type AFG = fn(Key) -> AF ;

    /// composes the navigation action for the l2 key, incl fast/word nav options
    fn l2_nav_afg (l2s:L2S, l2k:Key, wafg:AFG, fafg:AFG) -> AF {
        // note: this ^^ cant take k:&Krusty because k.am would be not 'Send' while k.l2s is, and so only can be sent here from AFs
        let (base_af, word_af, fast_af) = (base_action(l2k), wafg(l2k), fafg(l2k));
        // todo .. figure out below whether to keep either or both of lalt/ralt as mode keys
        Arc::new ( move || {
            //if l2s.is_lalt_down.check() || l2s.in_caret_word_mode.check() { word_af() }
            if l2s.in_caret_word_mode.check() { word_af() }
            //else if l2s.is_ralt_down.check() || l2s.in_caret_fast_mode.check() { fast_af() }
            else if l2s.in_caret_fast_mode.check() { fast_af() }
            // todo: hopefully can switch to this after some finger retraining on other changes first
            else { base_af() }
    } ) }

    /// delete action with specified bksp/del key, supports the fast/word nav options
    pub fn l2_del_afg (l2s:L2S, del_key:Key, _:AF) -> AF {
        // note : this is here instead of using del-sel for everything because this gives better undo/redo behavior in editors
        l2_nav_afg (l2s, del_key, ctrl_action, fast_action)
    }
    /// delete action as first select and then delete w the specified bksp/del key
    pub fn l2_del_sel_afg (_:L2S, del_key:Key, nav_af:AF) -> AF {
        Arc::new ( move || {
            Key::Shift.press(); nav_af(); Key::Shift.release();
            press_release(del_key);
    } ) }
    /// generator type for deletion AFs
    pub type DAFG = fn(L2S, Key, AF) -> AF ;

    /// l2 selection action, supports any configured fast/word nav options for the key
    fn l2_sel_afg (nav_af:AF) -> AF {
        Arc::new ( move || { Key::Shift.press(); nav_af(); Key::LShift.release() } )
    }

    /// composes the complete l2 action for the key
    fn l2_afg (l2s:L2S, nav_af:AF, sel_af:AF, del_af:AF) -> AF {
        Arc::new ( move || {
            if l2s.in_caret_sel_mode.check() { sel_af() }
            else if l2s.in_caret_del_mode.check() { del_af() }
            else { nav_af() }
    } ) }

    /// setup layer-2 behavior for a given 'key' <br>
    /// *l2k* : layer-2 eqv key to press for regular/nav actions <br>
    /// *dk* : key to use for delete action (backspace or delete) <br>
    /// *wafg* : word-action-gen,  specifies regular/fast/word-nav when word-mode-key (d) held <br>
    /// *fafg* : fast-action-gen, specifies regular/fast nav when fast-mode-key (e) held <br>
    /// *dafg* : del-action-gen,  specifies del action when 'f' held, either direct 'dk' or select-then-dk <br>
    /// (note that there's default selection action when 'r' is held)
    pub fn setup_l2k (k:&Krusty, key:Key, l2k:Key, dk:Key, aafg:AFG, fafg:AFG, dafg:DAFG) {
        // first lets assemble our component actions
        let nav_af = l2_nav_afg (k.l2s.clone(), l2k, aafg, fafg);
        let sel_af = l2_sel_afg (nav_af.clone());
        let del_af = dafg (k.l2s.clone(), dk, nav_af.clone());

        // then compose the complete l2 action for the key
        let l2_af  = l2_afg (k.l2s.clone(), nav_af, sel_af, del_af);

        // and since l2 behavior encompasses (caps, lalt, ralt), we'll set the same behavior for all three
        add_caps_mapping      (k, key, l2_af.clone());
        //add_caps_lalt_mapping (k, key, l2_af.clone());
        add_caps_lalt_mapping (k, key, no_action());
        // ^^ lalt is no longer l2s key, but to train for a bit, we'll set it to do nothing for these keys instead
        // todo: ^^ remove training-disabled things like these when no longer useful (so can use those high value combos for other purposes)
        add_caps_ralt_mapping (k, key, l2_af);
    }

    // note: actual setup for l2k keys can be done (importing utils this mod) along with other krustyboard setup steps

}


pub fn setup_krusty_board () {

    use crate::krusty::{*, key_utils::*, mod_keys_setups::*, special_keys_setups::*,
                        mouse_btn_setups::*, mouse_wheel_setups::*, action_map_utils::*};

    use crate::utils::{window_utils::*, process_utils::*};


    let k = Krusty::new();


    // setup capslock, we'll completely disable it other than for krusty use
    setup_caps_tracking (&k.l2s);
    // setup left-alt, this will be monitored, but allowed to get out-of-sync between pressed state and external state
    setup_left_alt_tracking (&k.l2s);
    // setup tracking for right-alt too .. (except Space etc), we use it as shift on left-keyboard-keys
    setup_right_alt_tracking (&k.l2s);
    // and for l-win .. this will track, but also modify win behavior to suppress repeats, and if consumed, its key-ups
    setup_left_win_tracking (&k.l2s);
    // and shift .. this is simple non-blocking tracking, only tracked to enable a few combos with caps
    setup_left_shift_tracking (&k.l2s);
    setup_right_shift_tracking (&k.l2s);
    // and even ctrl .. again simple non-blocking, just tracking flags to allow for natural ctrl-alt-? when alt-? is remapped
    setup_left_ctrl_tracking (&k.l2s);
    setup_right_ctrl_tracking (&k.l2s);


    // handling for mouse left btn, mostly to allow caps-as-ctrl behavior during drag drops and clicks
    setup_mouse_left_btn_handling (&k.l2s);
    // also for mouse right btn, mostly to allow switche scrolling w right-btn-wheel combo
    setup_mouse_right_btn_handling (&k.l2s);
    // also setup both Xbutton srcs to act as middle btns (used for link clicks, closing tabs etc)
    setup_mouse_x_btn_1_handling ();
    setup_mouse_x_btn_2_handling ();

    // setup handling for mouse wheel .. complex overloading over alt-tab, switche, volume, brightness etc !!
    setup_mouse_wheel_handling (&k.l2s);




    // we'll setup most keys via key-combo action maps that we'll compose into relevant callbacks after all mapping is registered
    // HOWEVER, there are some keys (incl those that look for shift/ctrl) that will be set directly below after all the action-map setups

    // lets start out by setting up bulk keys (which might get overriden further down, though we'd prefer to not rely on that)

    // IMPORTANT: default action maps composition per key, if no explicit mapping, sets ralt-as-shift, caps-as-ctrl, caps-alt--as--ctrl-alt
    // HOWEVER, action maps only come into play for a key if SOME mapping is made
    // and by now, we pretty much want to map all keys in some way or another, so might as well put em all here ourselves
    // .. EXCEPT for cases we find we need to directly bind some keys (special-keys-setups) .. which we have none so far !!

    let char_keys:  Vec<Key> = "qwertasdfgzxcvb`123456yuiop[]\\hjkl;\'nm,./7890-=" .chars() .map (|c| Key::from_char(c)) .flatten() .collect();
    let f_num_keys: Vec<Key> = (u64::from(Key::F1) .. u64::from(Key::F24)) .map (|v| Key::from(v)) .collect();
    let nav_keys   = vec![Key::Left, Key::Right, Key::Up, Key::Down, Key::PageUp, Key::PageDown, Key::Home, Key::End];
    let spcl_keys  = vec![Key::Backspace, Key::Delete, Key::Space, Key::Tab, Key::Enter, Key::Escape, Key::Insert, Key::Apps];
    //let media_keys = vec![Key::BrowserBack, Key::BrowserForward, Key::BrowserRefresh, Key::VolumeMute, Key::VolumeDown, Key::VolumeUp,
    //                      Key::MediaNextTrack, Key::MediaPrevTrack, Key::MediaStop, Key::MediaPlayPause];

    vec![char_keys, f_num_keys, nav_keys, spcl_keys] .concat() .into_iter() .for_each ( |key| {
        add_base_mapping (&k, key, base_action(key))
    } );
    // ^^ we can ofc override these later .. as we will for some like backquote-as-delete F1-as-F21 etc


    // a bunch of r-alt as shift mappings .. gonna be almost all char keys, only leaving behind F<num> keys etc
    // note that 'r' and 'd' are registered as caret mode keys too, but these can still work independently here!
    // note also that even when we're setting alt as shift for all char keys, its still valuable to not globally set it in
    // >  alt key itself as this will allow combos for other non char keys (like F<num> keys), or other three-key combos etc
    // >  .. plus, means we can use ralt as fast-mode in l2 setups (as not sending shift at alt allows separation of caps-ralt)
    //"qwertasdfgzxcvb`123456" .chars() .for_each ( |c| {                                  // only left side char keys
    //"qwertasdfgzxcvb123456yuiop[]\\hjkl;\'nm,./7890-=" .chars() .for_each ( |c| {          // all char keys !!
    //    Key::from_char(c) .into_iter() .for_each ( |key| add_ralt_mapping (&k, key, shift_action(key)) )
    //} );
    // ^^ this is no longer needed, as we now add ALL keys to mapping, and cb composition on keys without ralt mapping defaults to shift


    // a bunch of caps-as-shift mappings, basically nums or kbd-right symbols not otherwise involved in l2
    // note: Period '.' is reserved for caret-mode 'fast' accelerator --> not anymore since ralt now also does fast-mode
    "1234567890-=[]\\;\'/." .chars() .for_each ( |c| {
        Key::from_char(c) .into_iter() .for_each ( |key| add_caps_mapping (&k, key, shift_action(key)) )
    } );


    // a bunch of caps-as-ctrl mappings, basically char keys not set to l2, so natural ctrl-combos can work with caps
    //"qwtasfgzxcvbnyop" .chars() .for_each ( |c| {
    //    Key::from_char(c) .into_iter() .for_each ( |key| add_caps_mapping (&k, key, ctrl_action(key)) )
    //} );
    // ^^ also no longer necessary, as caps now defaults to ctrl during cb composition for keys without caps mapping


    // setup caps-lalt as ctrl-alt for almost all keys other than those set otherwise or overwritten (even for those with caps-as-shift)
    // note ofc that l2 and caret-mode keys cant be here .. some other might be overwritten elsewhere e.g. 'f', 'w' etc
    //"qtasfgzxcvb1234567890-=yop[]\\\'./" .chars() .for_each ( |c| {
    //    Key::from_char(c) .into_iter() .for_each ( |key| add_caps_lalt_mapping (&k, key, ctrl_alt_action(key)) )
    //} );
    // ^^ again, no longer needed, as caps-alt defaults to ctrl-alt during composition if no caps-alt mapping specified for key


    // lets set up the F<?> keys similarly too, giving them caps-as-ctrl and caps-lalt as ctrl-alt
    // we know at least caps-alt-F2 from here will get overwritten later for media keys .. which is fine
    //(u64::from(Key::F1) .. u64::from(Key::F12)) .map (|v| Key::from(v)) .for_each ( |key| {
    //    add_caps_mapping      (&k, key, ctrl_action(key));
    //    add_caps_lalt_mapping (&k, key, ctrl_alt_action(key));
    //} );
    // ^^ same here, happens auto via cb composition default


    // lets also setup actual arrow etc nav keys
    //vec![Key::Left, Key::Right, Key::Up, Key::Down, Key::PageUp, Key::PageDown, Key::Home, Key::End] .into_iter() .for_each ( |key| {
    //    add_caps_mapping      (&k, key, ctrl_action(key));
    //    add_caps_lalt_mapping (&k, key, ctrl_alt_action(key));
    //} );
    // ^^ same here, happens auto via cb composition default


    // lets also disable the win-number combos as they annoyingly activate/minimize items from taskbar
    "1234567890" .chars() .map (|c| Key::from_char(c)) .flatten() .for_each ( |key| {
        add_win_mapping      (&k, key, no_action());
        add_caps_win_mapping (&k, key, no_action());
    } ); // some of ^^ these will get overwritten by specific win-combos added later .. which is fine

    // note that at least for now, we're choosing to ignore caps-shift, caps-ctrl etc combos, though ofc could impl if need arises





    // setup keys for layer-2 caret nav sel/del/fast modes
    // note: registering as caret-mode key will set caps and caps-lalt/ralt actions to nothing, other combos can be set as usual elsewhere
    register_caret_mode_key (&k, Key::R, &k.l2s.in_caret_sel_mode);
    //register_caret_mode_key (&k, Key::D, &k.l2s.in_caret_del_mode);
    register_caret_mode_key (&k, Key::F, &k.l2s.in_caret_del_mode);
    //register_caret_mode_key (&k, Key::Period, &k.l2s.in_caret_fast_mode);
    // ^^ disabled '.' for fast-mode, as we started letting r-alt do fast mode too (l-alt does word-nav mode)
    //register_caret_mode_key (&k, Key::A,         &k.l2s.in_caret_word_mode);
    register_caret_mode_key (&k, Key::D,         &k.l2s.in_caret_word_mode);
    //register_caret_mode_key (&k, Key::Semicolon, &k.l2s.in_caret_fast_mode);
    register_caret_mode_key (&k, Key::E,         &k.l2s.in_caret_fast_mode);
    // ^^ trying out non-mod keys for word/fast mode to see if we can free up alt keys for all other possible combos!


    // caret mode support .. since we're using a as mode key, we'll setup caps-alt-a to give ctrl-a instead of deftault ctrl-alt-a
    //add_caps_lalt_mapping (&k, Key::A, alt_action(Key::A));
    add_caps_lalt_mapping (&k, Key::F, alt_action(Key::F, k.l2s.clone()));
    //add_lalt_mapping_w_alt_inactive (&k, Key::A, ctrl_action(Key::A));
    add_lalt_mapping_w_alt_inactive (&k, Key::F, ctrl_action(Key::F));


    // now for semicolon, we used to use caps as shift, so instead we'll set lalt as shift, lets see if it sees use
    //add_lalt_mapping      (&k, Key::Semicolon, shift_action(Key::Semicolon));
    //add_caps_lalt_mapping (&k, Key::Semicolon, shift_action(Key::Semicolon));
    //add_lalt_mapping_w_alt_inactive (&k, Key::Semicolon, shift_action(Key::Semicolon));

    // todo ^^ all the experimental sections above, clean up when sufficiently trained/settled






    // setup backquote .. make normal case be Delete, caps do back-tick, and shift do its tilde, alt will do quick switch via Alt-F20
    // note that since base is remapped, it will change ctrl/shift combos w/ alt/win etc too .. e.g. ctrl-alt-tick gives ctrl-alt-delete !
    let l2s = k.l2s.clone();
    add_base_mapping (&k, Key::Backquote, Arc::new ( move || {
        if l2s.some_shift_down() { press_release(Key::Backquote) } // uses already down shift to give tilde
        else { press_release(Key::ExtDelete) }
    } ) );
    add_caps_mapping (&k, Key::Backquote, base_action(Key::Backquote));
    add_lalt_mapping_w_alt_active (&k, Key::Backquote, base_action(Key::F20));
    //add_ralt_mapping (&k, Key::Backquote, shift_action(Key::Backquote));
    // ^^ not strictly necessary as cb composition now defaults to this, but also useful to see here for reference



    // setup tab .. caps-as-ctrl for caps-tab switching, ralt-tab for shift-tab
    let l2s = k.l2s.clone();
    add_caps_mapping (&k, Key::Tab, Arc::new (move || {
        if l2s.is_caps_down.check() {
            l2s.in_managed_ctrl_down_state.set_if_clear();
            Key::LControl.press();  // this enables caps-as-ctrl for caps-tab switching
            // ^^ we're not gonna release ctrl immediately, but keep track and release when caps is released
            press_release(Key::Tab)
        }
    } ) );
    //add_ralt_mapping (&k, Key::Tab, shift_action(Key::Tab));
    // ^^ not strictly necessary, as cb compostion now defaults to this, but also useful to see here for reference


    // setup space key .. ralt-space as enter, caps-space as ctrl-space, caps-lalt-space as alt-enter for intellij
    add_ralt_mapping      (&k, Key::Space, base_action(Key::Enter));
    add_caps_lalt_mapping (&k, Key::Space, alt_action(Key::Enter, k.l2s.clone()));
    //add_caps_mapping      (&k, Key::Space, ctrl_action(Key::Space));
    // ^^ not strictly necessary as cb composition now defaults to this, but also useful to see here for reference



    // win-m by default minimized all windows .. we just want to disable it
    add_win_mapping (&k, Key::M, no_action());

    // win-i should start irfanview
    add_win_mapping (&k, Key::I, Arc::new (|| start_irfanview()));

    // win-n should start chrome-incognito
    add_win_mapping (&k, Key::N, Arc::new (|| start_chrome_incognito()));

    // we'll setup win-T to quickly bring up chrome Tabs-Outliner via switche Alt-F24 hotkey
    //add_win_mapping_w_win_inactive (&k, Key::T, alt_action(Key::F24, k.l2s.clone()));
    add_win_mapping_w_win_inactive (&k, Key::C, alt_action(Key::F24, k.l2s.clone()));
    //add_caps_lalt_mapping (&k, Key::T, alt_action(Key::F24, k.l2s.clone()));     // try out caps-alt-t too, we'll see
    add_caps_lalt_mapping (&k, Key::C, alt_action(Key::F24, k.l2s.clone()));     // this one is one-handed

    // todo: test code here
    add_caps_mapping (&k, Key::LBracket, alt_action(Key::Minus, k.l2s.clone()));
    add_caps_mapping (&k, Key::RBracket, win_action(Key::Period, k.l2s.clone()));
    add_win_mapping_w_win_inactive (&k, Key::LBracket, alt_action(Key::Backslash, k.l2s.clone()));
    add_win_mapping_w_win_active (&k, Key::RBracket, win_action(Key::Minus, k.l2s.clone()));
    add_win_mapping_w_win_active (&k, Key::Backslash, alt_action(Key::Minus, k.l2s.clone()));


    // in cur laptop, Fn-F6/F7 do brightness, but at +10 incrs .. set them to do small incrs with win combos
    add_win_mapping (&k, Key::F6, Arc::new (|| incr_brightness(-1)));
    add_win_mapping (&k, Key::F7, Arc::new (|| incr_brightness(1)));

    // might as well do that for alt as well, since we use alt for most other such shortcuts
    add_lalt_mapping (&k, Key::F6, Arc::new (|| incr_brightness(-1)));
    add_lalt_mapping (&k, Key::F7, Arc::new (|| incr_brightness(1)));

    // actually, since we use alt-2/3 as vol down/up, might as well also set win-2/3 for brightness down/up
    add_win_mapping (&k, Key::Numrow_2, Arc::new (|| incr_brightness(-1)));
    add_win_mapping (&k, Key::Numrow_3, Arc::new (|| incr_brightness(1)));


    // alt-2 is vol down, alt-3 is vol up
    add_lalt_mapping_w_alt_inactive (&k, Key::Numrow_2, base_action(Key::VolumeDown));
    add_lalt_mapping_w_alt_inactive (&k, Key::Numrow_3, base_action(Key::VolumeUp));

    // alt-f1 play/pause, caps-f1 toggle mute, base-case F21 for switche caller, ralt for actual F1
    add_base_mapping (&k, Key::F1, alt_action(Key::F21, k.l2s.clone()));
    add_ralt_mapping (&k, Key::F1, base_action(Key::F1));
    add_caps_mapping (&k, Key::F1, base_action(Key::VolumeMute));
    add_lalt_mapping_w_alt_inactive (&k, Key::F1, base_action(Key::MediaPlayPause));


    // want al-f2 for next with some initial skip .. we'll use caps-alt-f2 for prev, so we'll set it up for both
    // note that our mechanism for wrapping alt-state restoring guards operates via AFs, hence setting those up (instead of fns)

    // skips work by alt-ctrl-volUp, so gotta wrap with alt_active_action
    fn media_skips_action (n_skips:u32, l2s:L2S) -> AF { alt_active_action (l2s, Arc::new (move || {
        (0 .. n_skips) .into_iter() .for_each (|_| { ctrl_press_release(Key::VolumeUp) });
    } ) ) }
    // ^^ gives an AF with specified number of skips

    // media next key shouldnt have alt on it, so should use alt_inactive-action .. (esp given we're on alt combo)
    let l2s = k.l2s.clone();
    let media_next_action = alt_inactive_action (k.l2s.clone(), Arc::new (move || {
        if !l2s.is_caps_down.check() { press_release(Key::MediaNextTrack) }
        else { press_release(Key::MediaPrevTrack) }
        let l2s = l2s.clone(); // gotta clone again to allow moving into spawned thread closure
        spawn ( move || { sleep(time::Duration::from_millis(2000));  media_skips_action(3,l2s)(); } );
    } ) );

    // al-f2 for next with some initial skip
    add_lalt_mapping      (&k, Key::F2, media_next_action.clone());
    add_caps_lalt_mapping (&k, Key::F2, media_next_action);

    // alt-f3 for skip forward a bit
    add_lalt_mapping (&k, Key::F3, media_skips_action(1,k.l2s.clone()));



    // caps-e is Enter .. specifically as left-hand-only enter while the right hand might be at mouse (else theres also ralt-space)
    // caps-alt-E as alt-enter as its freq used in intelliJ, but we have it globally captured by 'Everything' indexer
    //add_caps_mapping      (&k, Key::E, base_action(Key::Enter));
    add_lalt_mapping_w_alt_inactive (&k, Key::E, base_action(Key::Enter));
    //add_caps_lalt_mapping (&k, Key::E, alt_action(Key::Enter, k.l2s.clone()));
    add_caps_lalt_mapping (&k, Key::E, base_action(Key::Enter));
    // ^^ trying out E as caret fast mode instead .. so setting caps-alt-e as enter if desperate for left hand enter (most enter is ralt-space anyway)

    // escape is just escape, but we just want it to do press-release immediately (so switche is faster)
    add_base_mapping (&k, Key::Escape, base_action(Key::Escape));

    // use the apps key to send shift-escape
    add_base_mapping (&k, Key::Apps, shift_action(Key::Escape));

    // similar to backquote, lets also add alt-1 for switche next-win (via its Alt-F20 hotkey)
    add_lalt_mapping (&k, Key::Numrow_1, alt_action (Key::F20, k.l2s.clone()));




    // caps-lalt-F for full screen ..(but from bulk mapping above, caps-f should still give ctrl-f)
    //add_caps_lalt_mapping (&k, Key::F, base_action(Key::F11));
    // ^^ no longer available as we made F a caret mode key, and set caps-alt-F to alt-F instead

    // 'w' should have caps-ctrl mapping, but when w/ alt, send alt-f4 (to close all-tabs, windows etc)
    //add_caps_shift_mapping (&k, Key::W, alt_action(Key::F4));
    // ^^ note: initially we wanted this with caps-shift-w, but turns out (at least on my kbd, turns out caps+shift+[F1, 2, w, s, x]
    // >  dont produce any key event at the hook at all .. nothing .. its like the keyboard driver not sending those out
    // funnily enough, there's a bunch of complaints about specifically those keys for dell/hp laptops .. looks like hardware
    // >  appears to be a common kbd pcb layout issue .. heres from 2007: (https://www.joachim-breitner.de/blog/250-Shift-Caps-2)
    // sooo .. to makeup, we'll do alt-caps-w do the alt-f4 business instead
    add_caps_lalt_mapping (&k, Key::W, alt_action(Key::F4, k.l2s.clone()));




    // filling out l2 actions (incl w caps-alt combos) .. the l2_utils module and setup_l2k fn below have more details in doc-comments
    use crate::krusty::l2_utils::*;
    setup_l2k ( &k,  Key::J,     Key::ExtLeft,   Key::Backspace,   ctrl_action,   fast_action,   l2_del_afg );
    setup_l2k ( &k,  Key::K,     Key::ExtRight,  Key::ExtDelete,   ctrl_action,   fast_action,   l2_del_afg );
    setup_l2k ( &k,  Key::I,     Key::ExtUp,     Key::Backspace,   fast_action,   fast_action,   l2_del_sel_afg );
    setup_l2k ( &k,  Key::Comma, Key::ExtDown,   Key::ExtDelete,   fast_action,   fast_action,   l2_del_sel_afg );
    setup_l2k ( &k,  Key::H,     Key::ExtHome,   Key::Backspace,   base_action,   base_action,   l2_del_sel_afg );
    setup_l2k ( &k,  Key::L,     Key::ExtEnd,    Key::ExtDelete,   base_action,   base_action,   l2_del_sel_afg );
    setup_l2k ( &k,  Key::U,     Key::ExtPgUp,   Key::Backspace,   base_action,   base_action,   l2_del_sel_afg );
    setup_l2k ( &k,  Key::M,     Key::ExtPgDn,   Key::ExtDelete,   base_action,   base_action,   l2_del_sel_afg );



    // then the caps-win combo l3 actions :

    // caps-win-U should vert-max (via shift-win-up) if not already, or else restore window from vert-max
    add_caps_win_mapping (&k, Key::U, Arc::new (|| win_fgnd_toggle_vertmax()));
    // caps-win-m should maximize (via win-m) if not, else restore from max
    add_caps_win_mapping (&k, Key::M, Arc::new (|| win_fgnd_toggle_max()));

    // caps-win-[j,k,i,,] should  move window [left, right, top, bottom] respectively
    add_caps_win_mapping (&k, Key::J,     Arc::new (|| win_fgnd_move(-80, 0) ));
    add_caps_win_mapping (&k, Key::K,     Arc::new (|| win_fgnd_move(80, 0) ));
    add_caps_win_mapping (&k, Key::I,     Arc::new (|| win_fgnd_move(0, -50) ));
    add_caps_win_mapping (&k, Key::Comma, Arc::new (|| win_fgnd_move(0, 50) ));

    // caps-win-[h,;,.,o] should stretch window [narrower, wider, shorter, taller] respectively
    // .. note that caps-win-L gets unavoidably captured by windows to lock laptop, hence the semicolon
    add_caps_win_mapping (&k, Key::H,       Arc::new (|| win_fgnd_stretch(-30, 0) ));
    add_caps_win_mapping (&k, Key::O,       Arc::new (|| win_fgnd_stretch(0, -30) ));
    add_caps_win_mapping (&k, Key::Period,  Arc::new (|| win_fgnd_stretch(0, 30) ));
    //add_caps_win_mapping (&k, Key::L,     Arc::new (|| win_fgnd_stretch(30, 0) ));
    // ^^ any win-L combo is hardcoded at OS level to lock machine, cant override that, so we'll make semicolon do that instead
    add_caps_win_mapping (&k, Key::Semicolon, Arc::new (|| win_fgnd_stretch(30, 0) ));


    // some additional caps-win combos
    // caps-win-c being used to launch winmerge diff from last two clipboard entries
    add_caps_win_mapping (&k, Key::C, Arc::new (|| start_winmerge_clipboard() ));
    // gaah we'll just throw in iDEA diff for drag-drop diffing (just coz winmerge doesnt do dark mode)
    //add_win_mapping (&k, Key::C, Arc::new (|| start_idea_diff() ));
    // ^^ cant do from here, turns out idea diff from cmd line can ONLY be opened with two files pointed, unlike empty from Idea shortcut!





    // we could also set up specific r-alt combos (for F<?> keys etc other than the default ralt-as-shift)
    // hmm.. cant think of any so far?


    // could also setup caps-ralt combos (for non l2/caret keys), which can be separate from caps-lalt combos!
    // hah .. nothing here yet either huh .. well these are two hand combos, so not preferable anyway


    // if really want/need to, could do completely independent lalt_ralt_<key> combos too (with minimal extra coding)
    // not yet implemented as dont see any need or much utlity for doing so given there are other simpler unused combos avalable


    // also fyi re free combos: caps-win-<non-l3>, win-<num>, caps-alt-<num>, caps-ralt<non-l2>
    // .. even for caps-lalt-<?> defaulting to ctr-alt-<?> most are still free (other than l2, caret, e, f, w, space, f2)
    // .. and almost all F<num> combos with caps, caps-win, caps-lalt, ralt, caps-ralt, even w just lalt


    // plus, for additional l3+ setup, (e.g. moving windows across monitors), could impl 'mode' in l3 (w/ caps-win) like for l2 (w/ caps-alt)
    // .. or add additional mode keys in l2 (caps-alt) .. although not a lot of free keys there .. maybe q .. could reuse mod keys w/ caps-win though




    // finally bind everything from action-maps !!
    bind_all_from_action_maps (&k);



    // and we'll put any direct special key setups after all this
    // > which is for safety in case anything above accidently included those, although ofc we dont want to rely on that!
    setup_direct_binding_keys (&k);


    // note: the handle_input_events to start the whole shebang should be being called somewhere in main after this setup

}

