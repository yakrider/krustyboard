use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};
use std::thread;
use std::time;
use std::time::Instant;

use derivative::Derivative;

use crate::{BlockInput, KbdEvent, KbdEvntCbMapKeyType::*, KbdKey, MouseButton, MouseEvent, MouseWheel, utils};



// we'll define some easier type aliases (CallBack, ArcFn, Arc-L2S etc) to pass around triggered actions and so on
type CB  = Box <dyn Fn(KbdEvent) + Send + Sync + 'static> ;
type AF  = Arc <dyn Fn() + Send + Sync + 'static> ;

type L2S = Arc <Layer2State> ;
type Key = KbdKey ;



#[derive(Derivative)]
#[derivative(Default, Debug)]
pub struct Layer2State {
    // used for toggling key processing .. should only listen to turn-back-on combo
    in_disabled_state: RwLock<bool>,

    // note that this is specifically only for use for our caps-combos .. so dont have to worry too much about whether
    // >  it'll always be in sync .. just that its in sync for our caps combo periods! (which is a LOT easier)
    is_caps_down:   RwLock<bool>,
    is_lalt_down:   RwLock<bool>,
    is_ralt_down:   RwLock<bool>,
    is_lwin_down:   RwLock<bool>,
    is_lshift_down: RwLock<bool>,

    // for alt, gotta separately track outside logical state, as sometimes we wanna suppress its down going out (instead it'll go out of sync)
    // this will explicitly track whether the outside seen last alt-key-event was down or not
    is_alt_active : RwLock<bool>,

    // further, when the common caps-alt combo is released, if caps is released just a tiny bit earlier, we dont want the alt to trigger ..
    // so we'll track the caps release to allow for some grace period of sorts before we start accepting alt as activated
    #[derivative(Default(value = "RwLock::new(Instant::now())"))]
    last_caps_up_stamp: RwLock<Instant>,

    // since win listeners often act on release (e.g. start menu), its annoying after we've done some win combo to have it pop up
    // so where possible, we'll try and suppress its up if we consumed it internally ourselves
    is_down_lwin_consumed : RwLock<bool>,

    // these three need indiv cloning into spawned threads, hence separate Arcs for them
    in_caret_sel_mode:  Arc<RwLock<bool>>,
    in_caret_del_mode:  Arc<RwLock<bool>>,
    in_caret_fast_mode: Arc<RwLock<bool>>,

    is_mouse_left_btn_down:  RwLock<bool>,
    is_mouse_right_btn_down: RwLock<bool>,

    // for caps-ctrl eqv for caps-tab or caps-wheel, we'll send ctrl press/release at the right times, and will need to track that
    in_managed_ctrl_down_state: RwLock<bool>,
    // and for right-mouse-btn-wheel switche support, we'll track that state too (and send switche specific keys)
    in_right_btn_scroll_state:  RwLock<bool>,

    #[derivative(Default(value = "RwLock::new(Instant::now())"))]
    last_wheel_stamp: RwLock<Instant>,
    is_wheel_spin_invalidated: RwLock<bool>, // invalidated by e.g. mid-spin mod press, or spin stop (spacing > 120ms)
}

impl Layer2State {
    fn clear_caret_mode_flags (&self) {
        *self.in_caret_sel_mode.write().unwrap() = false;
        *self.in_caret_del_mode.write().unwrap() = false;
        *self.in_caret_fast_mode.write().unwrap() = false;
    }
    fn caps_up_grace_active (&self) -> bool {
        const GRACE_DUR_MS: u128 = 200;
        let t = Instant::now().duration_since(*self.last_caps_up_stamp.read().unwrap()).as_millis();
        //std::thread::spawn(move || println!("{} {}", t<GRACE_DUR_MS, t));
        t < GRACE_DUR_MS
    }
}




#[derive(Default)]
pub struct ActionMaps {
    // this allows us to build maps to hold l2, win, l3 etc by group instead of spread around all over by key
    // the caps and caps-alt mapping comprise the l2 functionality, the win and caps-win comprise l3
    // the idea is to auto compose actual callbacks by key after all the mapping is filled out
    // note ofc that direct key callbacks can still be set for keys not put in these maps
    pub base_act_map      : HashMap<Key,AF>,
    pub caps_act_map      : HashMap<Key,AF>,
    pub lalt_act_map      : HashMap<Key,AF>,
    pub ralt_act_map      : HashMap<Key,AF>,
    pub win_act_map       : HashMap<Key,AF>,
    pub caps_win_act_map  : HashMap<Key,AF>,
    pub caps_lalt_act_map : HashMap<Key,AF>,
    pub caps_ralt_act_map : HashMap<Key,AF>,
}


#[derive(Default)]
pub struct Krusty {
    // this is mostly just a utility wrapper sugar to pass things around
    _private : (),              // this is to prevent direct instantiation of this struct
    pub l2s : Arc <Layer2State>,
    pub am  : RefCell <ActionMaps>,
    // we'll also keep a mapping of caret-mode keys and their associated flags for l2 impl
    pub cmks: RefCell <HashMap <Key, Arc <RwLock <bool>>>>
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

    pub fn press_release       (key:Key) { key.press(); key.release() }
    pub fn shift_press_release (key:Key) { Key::Shift.press(); press_release(key); Key::Shift.release() }
    pub fn ctrl_press_release  (key:Key) { Key::Ctrl.press();  press_release(key); Key::Ctrl.release() }

    pub fn no_action () -> AF { Arc::new ( || {} ) }
    pub fn base_action  (key:Key) -> AF { Arc::new ( move || { press_release(key); } ) }
    pub fn ctrl_action  (key:Key) -> AF { Arc::new ( move || { ctrl_press_release(key); } ) }
    pub fn shift_action (key:Key) -> AF { Arc::new ( move || { shift_press_release(key); } ) }
    pub fn fast_action  (key:Key) -> AF { Arc::new ( move || { press_release(key); press_release(key); } ) }


    // we'll define some arc-wrapper util fns, but really, its just as easy to just use arcs directly
    /// wraps a given unitary function with NO input args into an Arc Fn
    pub fn arc_fn (f:fn()) -> AF { Arc::new (move || f()) }

    /// wraps a given unitary function with ONE input arg into an Arc Fn
    pub fn arc_fn_p1<T> (f:fn(T), t:T) -> AF where T: Copy + Send + Sync + 'static { Arc::new (move || f(t)) }

}



pub mod lalt_setups { // left-alt tracking business

    use crate::krusty::*;

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
            if *l2s.is_caps_down.read().unwrap() {
                // caps is down, record alt being down if not already, but either way block it (so no change to alt-active state)
                if !*l2s.is_lalt_down.read().unwrap() {
                    *l2s.is_lalt_down.write().unwrap() = true;
                }
                BlockInput::Block
            } else {
                if *l2s.is_lalt_down.read().unwrap() {
                    // caps isnt down, but alt was, so its repeat, so if in-post-caps-up-grace or not-out-of-sync then should still block it
                    if *l2s.is_alt_active.read().unwrap() || l2s.caps_up_grace_active() {
                        BlockInput::Block
                    } else {
                        *l2s.is_alt_active.write().unwrap() = true;
                        BlockInput::DontBlock
                    }
                } else {
                    // caps isnt down, and alt wasnt down, so record states and let it through
                    *l2s.is_lalt_down.write().unwrap() = true;
                    *l2s.is_alt_active.write().unwrap() = true;
                    *l2s.is_wheel_spin_invalidated.write().unwrap() = true;
                    BlockInput::DontBlock
                }
            }
        });
        let l2s = l2sr.clone();
        Key::LAlt.blockable_bind(KeyUpCallback, move |_| {
            // here again, if caps is active, cant help but suppress it, esp as most menu trigger on alt key-up!!
            //   we'll just record state updates for others to deal with
            // unless if no caps, where we can update states as necessary and let it through
            *l2s.is_lalt_down.write().unwrap() = false;
            // ^^ assumes we get this ev only when we were tracking it (else either way couldnt do much else anyway)

            if !*l2s.is_caps_down.read().unwrap() && *l2s.is_alt_active.read().unwrap() {
                *l2s.is_alt_active.write().unwrap() = false;
                BlockInput::DontBlock
            } else {
                // either caps was down, or things were already in sync and can only be made worse!
                BlockInput::Block
            }
        });
    }


    pub fn ensure_held_lalt_active (l2s:&L2S) {
        // utility to set held l-alt active (sync it) while tracking state (useful for media shortcuts etc)
        if *l2s.is_lalt_down.read().unwrap() && !*l2s.is_alt_active.read().unwrap() {
            Key::LAlt.press();
            *l2s.is_alt_active.write().unwrap() = true;
        }
    }
    pub fn ensure_alt_active (l2s:&L2S) {
        // utility to get alt out reliably whether its currently pressed or not, while keeping state tracking updated
        if !*l2s.is_alt_active.read().unwrap() {
            Key::LAlt.press();
            *l2s.is_alt_active.write().unwrap() = true;
        }
    }
    pub fn ensure_not_held_lalt_inactive (l2s:&L2S) {
        // utility to set not-held alt inactive (sync it) while tracking (useful for alt-combos that produce non-alt outputs)
        if !*l2s.is_lalt_down.read().unwrap() && *l2s.is_alt_active.read().unwrap() {
            Key::LAlt.release();
            *l2s.is_alt_active.write().unwrap() = false;
        }
    }
    pub fn ensure_held_lalt_inactive (l2s:&L2S) {
        // utility to hard-clear alt state for alt-combos (e.g. for media shortcuts etc)
        if *l2s.is_lalt_down.read().unwrap() && *l2s.is_alt_active.read().unwrap() {
            Key::LAlt.release();
            *l2s.is_alt_active.write().unwrap() = false;
        }
    }
    pub fn ensure_alt_inactive (l2s:&L2S) {
        // utility to ensure lalt is inactive regardless if held down
        // (expectation is l-2 keys should ensure that, and l-alt handler should never activate it when l2/caps is active either)
        if *l2s.is_alt_active.read().unwrap() {
            *l2s.is_alt_active.write().unwrap() = false;
            Key::LAlt.release();
        }
    }

    pub fn try_sync_lalt_if_no_caps (l2s:&L2S) {
        // well .. this fn .. either way, it can only fix when no-caps, so users will have to check first, and act differently ..
        // >  at which point, in most cases why bother, might as well just skip trying to (partially) fix it and handle it themselves!
        // >  plus, for them, they can selectively only send something IF the mismatch type was actually problematic for them
        // >  hence the various partial utility fns to cover those cases explicitly ONLY when absolutely necessary
        // so maybe we'll only use it upon capslock release, but its separately here in case it makes sense for others any other time
        if *l2s.is_caps_down.read().unwrap() {
            // cant safely try to do blindly do anything while caps is down, as we dont wanna interfere w any active layer2 modes
        } else {
            try_sync_lalt(l2s);
        }
    }
    pub fn try_sync_lalt (l2s:&L2S) {
        if *l2s.is_lalt_down.read().unwrap() {
            if !*l2s.is_alt_active.read().unwrap() {
                // alt key down, but not active .. so send a press
                Key::LAlt.press();
                *l2s.is_alt_active.write().unwrap() = true;
            }
        } else {
            if *l2s.is_alt_active.read().unwrap() {
                // alt key up but still active .. so send a release
                Key::LAlt.release();
                *l2s.is_alt_active.write().unwrap() = false;
            }
        }
    }

}



pub mod mod_keys_setups { // tracking for other tracked modifier keys e.g. capslock, rAlt, lWin, lShift

    use crate::krusty::{*, lalt_setups::*};

    pub fn setup_caps_tracking (l2sr:&L2S) {
        // note that for caps, we completely block it from ever being sent up, and just manage internally!
        if Key::CapsLock.is_toggled() {
            Key::CapsLock.press();  // toggle off first if necessary (to clear key light)
        }
        let l2s = l2sr.clone();
        Key::CapsLock.block_bind(KeyDownCallback, move |_| {
            if !*l2s.is_caps_down.read().unwrap() {
                *l2s.is_caps_down.write().unwrap() = true;
                *l2s.is_wheel_spin_invalidated.write().unwrap() = true;
            }
            if *l2s.is_mouse_left_btn_down.read().unwrap() && !*l2s.in_managed_ctrl_down_state.read().unwrap() {
                *l2s.in_managed_ctrl_down_state.write().unwrap() = true;
                Key::LControl.press();
            }
        });
        let l2s = l2sr.clone();
        Key::CapsLock.block_bind(KeyUpCallback, move |_| {
           *l2s.is_caps_down.write().unwrap() = false;
            if *l2s.in_managed_ctrl_down_state.read().unwrap() {
                *l2s.in_managed_ctrl_down_state.write().unwrap() = false;
                Key::LControl.release();
            }
            // tracking caps release stamp allows for a grace between caps release and alt release from combos
            *l2s.last_caps_up_stamp.write().unwrap() = Instant::now();
            // the following isnt strictly necessary, but useful in case some keyup falls through
            l2s.clear_caret_mode_flags();
            // also, now might be a good time to try and get any out-of-sync l-alt states back in line if necessary
            //try_sync_lalt(&l2s);
            // this ^^ is incompatible w caps grace ..
            // >  infact its worse than just no grace because it immediately triggers instead of natural grace waiting for alt's key repeat
            // >  so instead we'll just do a 'sync' if we needed an alt release
            ensure_not_held_lalt_inactive(&l2s);
        });
    }


    pub fn setup_right_alt_tracking (l2sr:&L2S) {
        // we'll completely disable its natural action other than tracking state so our own combos can use it as a separate key
        // usage goals: as shift for keys on board-left-half, as l2 fast mode, ralt-space as enter, and other combos for board-right
        let l2s = l2sr.clone();
        Key::RAlt.block_bind(KeyDownCallback, move |_| {
            *l2s.is_ralt_down.write().unwrap() = true;
        });
        let l2s = l2sr.clone();
        Key::RAlt.block_bind(KeyUpCallback, move |_| {
            *l2s.is_ralt_down.write().unwrap() = false;
        });
    }


    pub fn setup_left_win_tracking (l2sr:&L2S) {
        // other than tracking state, we also try to avoid effect of its key-up afterwards if we acted on it ourselves while down
        // also, to make that work, we'll suppress all repeats too, and ofc wont reset was-consumed flag for repeats either
        // however, ofc cant just block lwin-up, so instead will have to mask it w a ctrl down/up as ctrl-win doesnt trigger anything
        let l2s = l2sr.clone();
        Key::LWin.blockable_bind(KeyDownCallback, move |_| {
            if !*l2s.is_lwin_down.read().unwrap() { // new physical press
                *l2s.is_lwin_down.write().unwrap() = true;
                if *l2s.is_down_lwin_consumed.read().unwrap() {
                    *l2s.is_down_lwin_consumed.write().unwrap() = false;
                }
                BlockInput::DontBlock
            } else { // its a key repeat
                BlockInput::Block
            }
        });
        let l2s = l2sr.clone();
        Key::LWin.blockable_bind(KeyUpCallback, move |_| {
            *l2s.is_lwin_down.write().unwrap() = false;
            if *l2s.is_down_lwin_consumed.read().unwrap() {
                thread::spawn ( || {
                    Key::Ctrl.press(); Key::Ctrl.release();
                    Key::LWin.release(); // doesnt actually have to be interspersed between ctrl press/release
                } );
                BlockInput::Block
            } else { BlockInput::DontBlock }
        });
    }
    pub fn consume_down_lwin (l2s:&L2S) {
        // utility for any key with lwin action to consume down state if they want to (to suppress its keyup later)
        if *l2s.is_lwin_down.read().unwrap() && !*l2s.is_down_lwin_consumed.read().unwrap() {
            *l2s.is_down_lwin_consumed.write().unwrap() = true;
        }
    }


    pub fn setup_left_shift_tracking (l2sr:&L2S) {
        // for left-shift, we really only care for a few combos with caps etc .. could query it as necessary too, but might as well
        let l2s = l2sr.clone();
        Key::LShift.non_blocking_bind(KeyDownCallback, move |_| {
            if !*l2s.is_lshift_down.read().unwrap() { *l2s.is_lshift_down.write().unwrap() = true; }
        });
        let l2s = l2sr.clone();
        Key::LShift.non_blocking_bind(KeyUpCallback, move |_| {
            *l2s.is_lshift_down.write().unwrap() = false;
        });
    }

}



pub mod mouse_btn_setups { // setups for mouse btn handling

    use crate::krusty::{*, key_utils::*};

    pub fn setup_mouse_left_btn_handling (l2sr:&L2S) {
        let l2s = l2sr.clone();
        MouseButton::LeftButton.block_bind(true, move |_| {
            *l2s.is_mouse_left_btn_down.write().unwrap() = true;
            if *l2s.is_caps_down.read().unwrap() {
                *l2s.in_managed_ctrl_down_state.write().unwrap() = true;
                Key::LControl.press(); // this allows caps-as-ctrl for drag drop etc
            }
            MouseButton::LeftButton.press()
        });
        let l2s = l2sr.clone();
        MouseButton::LeftButton.block_bind(false, move |_| {
            *l2s.is_mouse_left_btn_down.write().unwrap() = false;
            thread::sleep(time::Duration::from_millis(10));
            MouseButton::LeftButton.release();
        });
        // ^^ in theory could do non-blocking bind for release, but that can occasionally expose us to timing issues as press is on block
        //      and that will be slower to send the actual press from the spawned thread (so making release slower too helps)
    }

    pub fn setup_mouse_right_btn_handling (l2sr:&L2S) {
        // tracking btn down is for ctrl-wheel (zoom) etc, and right-btn-down-state is for switche scroll (via F21/F22/F23)
        let l2s = l2sr.clone();
        MouseButton::RightButton.non_blocking_bind(true, move |_| {
            *l2s.is_mouse_right_btn_down.write().unwrap() = true;
        });
        let l2s = l2sr.clone();
        MouseButton::RightButton.non_blocking_bind(false, move |_| {
            *l2s.is_mouse_right_btn_down.write().unwrap() = false;
            if *l2s.in_right_btn_scroll_state.read().unwrap() {
                *l2s.in_right_btn_scroll_state.write().unwrap() = false;
                press_release(Key::F23);
            }
        });
    }

    pub fn setup_mouse_x_btn_1_handling () {
        // turns out just doing press/release on initial press works snappier/more-reliable than trying to be true to btn-holds
        MouseButton::X1Button.block_bind(true, move |_| {
            MouseButton::MiddleButton.press(); MouseButton::MiddleButton.release();
        });
        MouseButton::X1Button.block_bind(false, move |_| { });
    }

    pub fn setup_mouse_x_btn_2_handling () {
        MouseButton::X2Button.block_bind(true, move |_| {
            MouseButton::MiddleButton.press(); MouseButton::MiddleButton.release();
        });
        MouseButton::X2Button.block_bind(false, move |_| { });
    }

}



pub mod mouse_wheel_setups { // setups for mouse wheel handling

    use crate::krusty::{*, key_utils::*, lalt_setups::*};
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
        if !*l2sr.is_wheel_spin_invalidated.read().unwrap() {
            handle_wheel_action(delta, l2sr);
        } else if GUARD_DUR_MS < l2sr.last_wheel_stamp.read().unwrap().duration_since(last_stamp).as_millis() {
            *l2sr.is_wheel_spin_invalidated.write().unwrap() = false;
            handle_wheel_action(delta, l2sr);
        } else {
            // if its invalidated AND wheel-spin spacing is below guard-dur, we suppress the wheel event
        }
    }

    pub fn handle_wheel_action (delta:i32, l2sr:&L2S) {
        let incr = delta / WHEEL_DELTA as i32;
        if *l2sr.is_mouse_right_btn_down.read().unwrap() {
            // right-mouse-btn-wheel support for switche task switching
            if !*l2sr.in_right_btn_scroll_state.read().unwrap() { *l2sr.in_right_btn_scroll_state.write().unwrap() = true; }
            let key = if incr.is_positive() { Key::F22 } else { Key::F21 };
            press_release(key);
        } else  if *l2sr.is_lalt_down.read().unwrap() {
            // wheel support for scrolling in windows native alt-tab task-switching screen
            if *l2sr.is_caps_down.read().unwrap() || get_fgnd_win_class() == "MultitaskingViewFrame" { // alt-tab states
                ensure_alt_active(l2sr);
                handle_alt_tab_wheel(incr)
            } else { // simple alt wheel for volume
                handle_volume_wheel(incr);
            }
        } else if *l2sr.is_lwin_down.read().unwrap() {
            *l2sr.is_down_lwin_consumed.write().unwrap() = true;
            handle_brightness_wheel(incr); // win-wheel for brightness
        } else if *l2sr.is_caps_down.read().unwrap() {
            *l2sr.in_managed_ctrl_down_state.write().unwrap() = true;
            Key::LControl.press();
            MouseWheel::DefaultWheel.scroll(delta); // caps-wheel as ctrl-wheel (zoom etc)
        } else if *l2sr.is_lshift_down.read().unwrap() {
            //handle_horiz_scroll_wheel(incr);
            // ^^ todo:: .. (and for now, just let default pass through)
            MouseWheel::DefaultWheel.scroll(delta);
        } else {
            MouseWheel::DefaultWheel.scroll(delta);
        }
    }

    pub fn handle_volume_wheel (incr:i32) {
        let key = if incr > 0 { Key::VolumeUp } else { Key::VolumeDown };
        (0 .. incr.abs()) .for_each(|_| key.press());
    }

    pub fn handle_brightness_wheel (incr:i32) {
        static INCR_STEP:i32 = 1;
        //utils::brightness_ps_wmi::incr_brightness(INCR_STEP*incr);
        utils::brightness_utils::incr_brightness(INCR_STEP*incr);
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

    // used to be used for setting up things like space or tab, but after expanding action maps for ralt etc, even the more
    // >  complex setups that were done individually can now be covered via action maps
    // leaving this here just as reminder this option is always available in case we need it for anything in the future


}




pub mod action_map_utils {

    use crate::krusty::{*, key_utils::*, lalt_setups::*, mod_keys_setups::*};

    /// note: registering a caret mode key auto sets their caps and caps-lalt/ralt actions to nothing, others combos can be set as usual
    pub fn register_caret_mode_key (k: &Krusty, key:Key, mode_flag_r:&Arc<RwLock<bool>>) {
        k.cmks.borrow_mut() .insert(key, mode_flag_r.clone());
        add_caps_mapping      (k, key, no_action());  // caret mode keys trigger w caps down, cant send anything during that
        add_caps_lalt_mapping (k, key, no_action());  // lalt is typically word-nav caret-mode, so this gotta be silent too
        add_caps_ralt_mapping (k, key, no_action());  // ralt is typically fast-nav caret-mode, so this gotta be silent too
    }

    /// if the key is registered for caret mode, this will generate the (key-dn, key-up) caret-flag setting actions
    fn gen_key_caret_mode_actions (k:&Krusty, key:Key) -> (AF,AF) {
        if let Some(cmfr) = k.cmks.borrow().get(&key) {
            let (cmf_c1, cmf_c2) = (cmfr.clone(), cmfr.clone()); // gotta clone to move into AF
            return ( Arc::new ( move || { *cmf_c1.write().unwrap() = true } ),     // key-dn action, set mode-flag
                     Arc::new ( move || { *cmf_c2.write().unwrap() = false } ) )   // key-up action, clear mode-flag
        }
        (no_action(), no_action()) // empty (dn,up) caret-flag-actions for non-caret keys
    }

    fn compose_action_maps_cb (k:&Krusty, key:Key) -> (CB, CB) {
        // lets make a utility closure that extracts the AF for this key from some specified am map
        let af = move |m:&HashMap<Key,AF>| { m.get(&key).map(|afr| afr.clone()) };
        // then retrieve all the registered actions for this key
        let am = &k.am.borrow();
        let (mba, mca, mlaa, mraa, mwa, mcwa, mclaa, mcraa) = (
            af(&am.base_act_map), af(&am.caps_act_map), af(&am.lalt_act_map), af(&am.ralt_act_map),
            af(&am.win_act_map), af(&am.caps_win_act_map), af(&am.caps_lalt_act_map), af(&am.caps_ralt_act_map)
        );
        // we cant move local options to closure, just the Arcs inside, so do that part of composition before making closure
        let ba = mba.unwrap_or(base_action(key));
        // we'll derive actions for the rest of options, defaulting to base if unspecified
        // note that for lalt and win, initial presses go out, so picking base action will cover the alt or win action
        // .. however for ralt, caps, and caps-combos, this defaults to pure base action (for ralt as its up/down is always suppressed)
        let (ca, laa, raa, wa, cwa, claa, craa) = (
            mca.unwrap_or(ba.clone()), mlaa.unwrap_or(ba.clone()), mraa.unwrap_or(ba.clone()), mwa.unwrap_or(ba.clone()),
            mcwa.unwrap_or(ba.clone()), mclaa.unwrap_or(ba.clone()), mcraa.unwrap_or(ba.clone())
        );
        // for caret-mode support, we'll check if the key is in caret-mode keys, and create flag update actions
        let (cma_dn, cma_up) = gen_key_caret_mode_actions (k, key);
        // now we can finally compose these Arc action clones into the key action closure we need
        let l2s = k.l2s.clone();
        let cb_dn = Box::new ( move |_| { // note that we're ignoring the KbdEvent passed in (it'd have KbdEventType and key vk/sc codes)
            cma_dn();
            if *l2s.is_caps_down.read().unwrap() {
                if *l2s.is_lwin_down.read().unwrap() { cwa() }
                else if *l2s.is_lalt_down.read().unwrap() { claa() }
                else if *l2s.is_ralt_down.read().unwrap() { craa() }
                else { ca() }
            } // note: for the rest (lwin, lalt, ralt), we wont define multi-combos for now .. can add if need arises
            else if *l2s.is_lwin_down.read().unwrap() { wa() }
            else if *l2s.is_lalt_down.read().unwrap() { laa() }
            else if *l2s.is_ralt_down.read().unwrap() { raa() }
            else { ba() }
        } );
        // and finally, the btn up action, which is really just caret mode flag action if any
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
            key.block_bind (KeyUpCallback, cb_up);
        }
    }



    pub fn add_base_mapping (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .base_act_map .insert (key, action); }

    pub fn add_caps_mapping (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .caps_act_map .insert (key, action); }
    pub fn add_caps_as_shift_mapping (k:&Krusty, key:Key)   { k.am.borrow_mut() .caps_act_map .insert (key, shift_action(key)); }
    pub fn add_caps_as_ctrl_mapping (k:&Krusty, key:Key)    { k.am.borrow_mut() .caps_act_map .insert (key, ctrl_action(key)); }

    pub fn add_lalt_mapping (k:&Krusty, key:Key, action:AF)      { k.am.borrow_mut() .lalt_act_map.insert (key, action); }
    pub fn add_caps_lalt_mapping (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .caps_lalt_act_map.insert (key, action); }

    fn held_lalt_active_action   (l2s:L2S, af:AF) -> AF { Arc::new ( move || { ensure_held_lalt_active(&l2s); af() } ) }
    fn held_lalt_inactive_action (l2s:L2S, af:AF) -> AF { Arc::new ( move || { ensure_held_lalt_inactive(&l2s); af() } ) }

    pub fn add_caps_lalt_mapping_w_alt_active (k:&Krusty, key:Key, action:AF) {
        add_caps_lalt_mapping (k, key, held_lalt_active_action (k.l2s.clone(), action))
    }
    pub fn add_caps_lalt_mapping_w_alt_inactive (k:&Krusty, key:Key, action:AF) {
        add_caps_lalt_mapping (k, key, held_lalt_inactive_action (k.l2s.clone(), action))
    }

    pub fn add_ralt_mapping (k:&Krusty, key:Key, action:AF)      { k.am.borrow_mut() .ralt_act_map.insert (key, action); }
    pub fn add_caps_ralt_mapping (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .caps_ralt_act_map.insert (key, action); }
    pub fn add_ralt_as_shift_mapping (k:&Krusty, key:Key)        { k.am.borrow_mut() .ralt_act_map .insert (key, shift_action(key)); }

    fn lwin_consuming_action (l2s:L2S, af:AF) -> AF { Arc::new ( move || { consume_down_lwin(&l2s); af() } ) }

    pub fn add_win_mapping (k:&Krusty, key:Key, action:AF) {
        k.am.borrow_mut() .win_act_map .insert (key, lwin_consuming_action(k.l2s.clone(), action));
    }
    pub fn add_caps_win_mapping (k:&Krusty, key:Key, action:AF) {
        k.am.borrow_mut() .caps_win_act_map .insert (key, lwin_consuming_action(k.l2s.clone(), action));
    }

}



pub mod l2_utils {

    /* setup options summary:
     - only j/k for left/right get alt-for-ctrl speedup (native word nav mode)
     - those and i/comma for up/down get dot-for-double-speed mode (2x nav) .. i/comma get that for alt too
     - h/l/u/m for home/end/pgup/pgdown get no speedup modes
     - in del mode, left/home/up/pgup get ExtBackspace, right/end/down/pgdn get ExtDelete
     - in del mode, left/right do direct bksp/del, but others get select then bksp/del
     */
    use crate::krusty::{*, lalt_setups::*, key_utils::*, action_map_utils::*};

    /// action-function generator type for l2 configuration
    pub type AFG = fn(Key) -> AF ;

    /// composes the navigation action for the l2 key, incl fast/word nav options
    fn l2_nav_afg (l2s:L2S, l2k:Key, aafg:AFG, fafg:AFG) -> AF {
        // note: this ^^ cant take k:&Krusty because k.am would be not 'Send' while k.l2s is, and so only can be sent here from AFs
        let (base_af, alt_af, fast_af) = (base_action(l2k), aafg(l2k), fafg(l2k));
        Arc::new ( move || {
            if *l2s.is_lalt_down.read().unwrap() { alt_af() }
            else if *l2s.is_ralt_down.read().unwrap() || *l2s.in_caret_fast_mode.read().unwrap() { fast_af() }
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
            ensure_alt_inactive(&l2s);
            // ^^ NOTE that for ALL l2, lAlt is suppressed other than for l2 behavior
            if *l2s.in_caret_sel_mode.read().unwrap() { sel_af() }
            else if *l2s.in_caret_del_mode.read().unwrap() { del_af() }
            else { nav_af() }
    } ) }

    /// setup layer-2 behavior for a given 'key' <br>
    /// *l2k* : layer-2 eqv key to press for regular/nav actions <br>
    /// *dk* : key to use for delete action (backspace or delete) <br>
    /// *aafg* : alt-action-gen,  specifies regular/fast/word-nav when alt held <br>
    /// *fafg* : fast-action-gen, specifies regular/fast nav when fast-mode-key (.) held <br>
    /// *dafg* : del-action-gen,  specifies del action when 'd' held, either direct 'dk' or select-then-dk <br>
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
        add_caps_lalt_mapping (k, key, l2_af.clone());
        add_caps_ralt_mapping (k, key, l2_af);
    }

    // note: actual setup for l2k keys can be done (importing utils this mod) along with other krustyboard setup steps

}


pub fn setup_krusty_board () {

    use crate::krusty::{*, key_utils::*, lalt_setups::*, mod_keys_setups::*, special_keys_setups::*,
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
    // and left-shift .. this is simple non-blocking tracking, only tracked to enable a few combos with caps
    setup_left_shift_tracking (&k.l2s);


    // handling for mouse left btn, mostly to allow caps-as-ctrl behavior during drag drops and clicks
    setup_mouse_left_btn_handling (&k.l2s);
    // also for mouse right btn, mostly to allow switche scrolling w right-btn-wheel combo
    setup_mouse_right_btn_handling (&k.l2s);
    // also setup both Xbutton srcs to act as middle btns (used for link clicks, closing tabs etc)
    setup_mouse_x_btn_1_handling ();
    setup_mouse_x_btn_2_handling ();

    // setup handling for mouse wheel .. complex overloading over alt-tab, switche, volume, brightness etc !!
    setup_mouse_wheel_handling (&k.l2s);



    // setup keys for layer-2 caret nav sel/del/fast modes
    // note: registering as caret-mode key will set caps and caps-lalt/ralt actions to nothing, other combos can be set as usual elsewhere
    register_caret_mode_key (&k, Key::R, &k.l2s.in_caret_sel_mode);
    register_caret_mode_key (&k, Key::D, &k.l2s.in_caret_del_mode);
    //register_caret_mode_key (&k, Key::Period, &k.l2s.in_caret_fast_mode);
    // ^^ disabled '.' for fast-mode, as we started letting r-alt do fast mode too (l-alt does word-nav mode)




    // setting up all the other keys via by-key-combo action maps that we'll compose at the end to relevant callbacks

    // setup tab .. caps-as-ctrl for caps-tab switching, ralt-tab for shift-tab
    let l2s = k.l2s.clone();
    add_caps_mapping (&k, Key::Tab, Arc::new (move || {
        if *l2s.is_caps_down.read().unwrap() {
            Key::LControl.press();  // this enables caps-as-ctrl for caps-tab switching
            if !*l2s.in_managed_ctrl_down_state.read().unwrap() {
                *l2s.in_managed_ctrl_down_state.write().unwrap() = true;
            } // ^^ we're not gonna release ctrl immediately, but keep track and release when caps is released
            press_release(Key::Tab)
        }
    } ) );
    add_ralt_mapping (&k, Key::Tab, shift_action(Key::Tab));


    // setup space key .. ralt-space as enter, caps-space as ctrl-space, caps-lalt-space as alt-enter for intellij
    let l2s = k.l2s.clone();
    add_ralt_mapping (&k, Key::Space, base_action(Key::Enter));
    //add_caps_mapping (&k, Key::Space, ctrl_action(Key::Space));
    add_caps_mapping (&k, Key::Space, Arc::new (move || { ensure_not_held_lalt_inactive(&l2s); ctrl_press_release(Key::Space)}));
    // ^^ while caps held, there might be suppressed alt-ups esp w l2 usage, so ensure they are cleared first
    add_caps_lalt_mapping_w_alt_active (&k, Key::Space, base_action(Key::Enter));



    // win-m by default minimized all windows .. we just want to disable it
    add_win_mapping (&k, Key::M, no_action());

    // win-i should start irfanview
    add_win_mapping (&k, Key::I, Arc::new (|| start_irfanview()));

    // win-n should start chrome-incognito
    add_win_mapping (&k, Key::N, Arc::new (|| start_chrome_incognito()));

    // in cur laptop, Fn-F6/F7 do brightness, but at +10 incrs .. set them to do small incrs with win combos
    add_win_mapping (&k, Key::F6, Arc::new (|| handle_brightness_wheel(-1)));
    add_win_mapping (&k, Key::F7, Arc::new (|| handle_brightness_wheel(1)));

    // might as well do that for alt as well, since we use alt for most other such shortcuts
    add_lalt_mapping (&k, Key::F6, Arc::new (|| handle_brightness_wheel(-1)));
    add_lalt_mapping (&k, Key::F7, Arc::new (|| handle_brightness_wheel(1)));

    // actually, since we use alt-2/3 as vol down/up, might as well also set win-2/3 for brightness down/up
    add_win_mapping (&k, Key::Numrow_2, Arc::new (|| handle_brightness_wheel(-1)));
    add_win_mapping (&k, Key::Numrow_3, Arc::new (|| handle_brightness_wheel(1)));

    // disable the rest of the win-number combos as they annoyingly activate/minimize items from taskbar
    vec!['0', '1', /* 2, 3, */ '4', '5', '6', '7', '8', '9'] .iter() .for_each ( |n|
        Key::from_char(*n) .into_iter() .for_each ( |key| add_win_mapping (&k, key, no_action()) )
    );


    // alt-f1 play/pause, caps-f1 toggle mute, base-case, overload F1 for switche caller (F21)
    let l2s = k.l2s.clone();
    add_base_mapping (&k, Key::F1, base_action(Key::F21));
    add_caps_mapping (&k, Key::F1, base_action(Key::VolumeMute));
    add_lalt_mapping (&k, Key::F1, Arc::new ( move || {
        // media keys only work while alt is inactive, so gotta set/restore that etc
        ensure_held_lalt_inactive(&l2s);
        press_release(Key::MediaPlayPause);
    } ) );
    // we'll set ralt-F1 to actually send F1 which is otherwise overloaded with a pile of other stuff
    add_ralt_mapping (&k, Key::F1, Arc::new (|| press_release(Key::F1)));
    // ^^ hah, wont to much while we continue to run switche with it directly globally registering F1 for itself!!


    fn do_media_skips (n_skips:u32, l2sr:&L2S) {
        // sending ^!{Right} which we traditionally set on winamp to skip forward a bit
        //ensure_held_lalt_inactive(l2sr);
        // note that ^^ we'll instead send alts below explicitly so it works even if alt was quickly released
        ensure_alt_active(l2sr);
        (0 .. n_skips) .into_iter() .for_each (|_| { ctrl_press_release(Key::ExtRight) });
        ensure_not_held_lalt_inactive(l2sr);
    }

    // al-f2 for next with some initial skip
    let l2s = k.l2s.clone();
    add_lalt_mapping (&k, Key::F2, Arc::new ( move || {
        ensure_held_lalt_inactive(&l2s);
        if !*l2s.is_lshift_down.read().unwrap() { press_release(Key::MediaNextTrack) }
        else { Key::LShift.release(); press_release(Key::MediaPrevTrack) } // note: its safer not to press it back again
        let l2s = l2s.clone(); // gotta clone again to allow moving into spawned thread closure!
        thread::spawn ( move || { thread::sleep(time::Duration::from_millis(2000));  do_media_skips(3,&l2s); } );
    } ) );

    // alt-f3 for skip forward a bit (skips is via alt-ctrl-Right, not via media keys)
    let l2s = k.l2s.clone();
    add_lalt_mapping (&k, Key::F3, Arc::new (move || do_media_skips(1, &l2s)));
    // alt-2 is vol down, alt-3 is vol up
    add_lalt_mapping (&k, Key::Numrow_2, base_action(Key::VolumeDown));
    add_lalt_mapping (&k, Key::Numrow_3, base_action(Key::VolumeUp));



    // caps-e is Enter .. specifically as left-hand-only enter while the right hand might be at mouse (else theres also ralt-space)
    add_caps_mapping (&k, Key::E, base_action(Key::Enter));

    // escape is just escape, but we just want it to do press-release immediately (so switche is faster)
    add_base_mapping (&k, Key::Escape, base_action(Key::Escape));

    // use the apps key to send shift-escape
    add_base_mapping (&k, Key::Apps, shift_action(Key::Escape));

    // for back-tick, make normal case be Delete, caps do back-tick, and shifts do its ~, alt will do quick switch via F20/Alt-F20
    // note that this means the shift-delete action which usually maps to 'cut' action wont be available on this key
    // also note that ralt-backquote to tilde is covered separately in the ralt-as-shift mappings below
    let l2s = k.l2s.clone();
    add_base_mapping (&k, Key::Backquote, Arc::new ( move || {
        if *l2s.is_lshift_down.read().unwrap() { press_release(Key::Backquote) } // uses already down shift to give tilde
        else { press_release(Key::ExtDelete) }
    }));
    add_caps_mapping (&k, Key::Backquote, base_action(Key::Backquote));
    add_lalt_mapping (&k, Key::Backquote, base_action(Key::F20));

    // lets also add alt-1 for switche next-win (via its F20/Alt-F20 hotkey)
    add_lalt_mapping (&k, Key::Numrow_1, base_action(Key::F20));




    // a bunch of caps-as-ctrl mappings, basically all char keys not set to shift, l2, or other specific functionality
    // note that there might be further overloaded caps-alt-? etc combos added elsewhere too (e.g. 'f', 'w' etc)
    "qwtyasfzxcvbnop12345" .chars() .for_each ( |c|
        Key::from_char(c) .into_iter() .for_each ( |key| add_caps_as_ctrl_mapping (&k, key) )
    );


    // a bunch of caps-as-shift mappings, basically kbd-right chars not otherwise involved in l2 or needed as ctrl keys (e.g. o, p)
    // note: Period '.' is reserved for caret-mode 'fast' accelerator --> not anymore since ralt now also does fast-mode
    "67890-=[]\\;\'/." .chars() .for_each ( |c|
        Key::from_char(c) .into_iter() .for_each ( |key| add_caps_as_shift_mapping (&k, key) )
    );


    // and a bunch of r-alt as shift mappings .. gonna be almost all char keys, only leaving behind F<num> keys etc
    // note that 'r' and 'd' are registered as caret mode keys too, but these can still work independently here!
    // note also that even when we're setting alt as shift for all char keys, its still valuable to not globally set it in
    // >  alt key itself as this will allow combos for other non char keys (like F<num> keys), or other three-key combos etc
    // >  .. plus, means we can use ralt as fast-mode in l2 setups (as not sending shift at alt allows separation of caps-ralt)
    //"qwertasdfgzxcvb`123456" .chars() .for_each ( |c|                                 // only left side char keys
    "qwertasdfgzxcvb`123456yuiop[]\\hjkl;\'nm,./7890-=" .chars() .for_each ( |c|         // all char keys !!
        Key::from_char(c) .into_iter() .for_each ( |key| add_ralt_as_shift_mapping (&k, key) )
    );


    // other specific r-alt combos
    // hmm.. cant think of any so far? hah


    // and caps-ralt combos which can be separate from caps-lalt combos!
    // hah .. nothing here yet either huh .. well these are two hand combos, so not preferable anyway


    // if really want/need to, could do completely independent lalt_ralt_<key> combos too (with minimal extra coding)
    // not yet implemented as dont see any need or much utlity for doing so given there are other simpler unused combos avalable



    // caps-lalt-F for full screen, but also the expected caps-f for ctrl-f
    add_caps_lalt_mapping_w_alt_inactive (&k, Key::F, Arc::new (|| press_release(Key::F11)));

    // 'w' should have caps-ctrl mapping, but when w/ alt, send alt-f4 (to close all-tabs, windows etc)
    // note: initially we wanted this with caps-shift-w, but turns out (at least on my kbd, turns out caps+shift+[F1, 2, w, s, x]
    // >  dont produce any key event at the hook at all .. nothing .. its like the keyboard driver not sending those out
    // funnily enough, there's a bunch of complaints about specifically those keys for dell/hp laptops .. looks like hardware
    // >  appears to be a common kbd pcb layout issue .. heres from 2007: (https://www.joachim-breitner.de/blog/250-Shift-Caps-2)
    // sooo .. to makeup, we'll do alt-caps-w do the alt-f4 business instead
    add_caps_lalt_mapping_w_alt_active (&k, Key::W, Arc::new (|| press_release(Key::F4)));

    // caps-alt-F10 just to pass through as ctrl-alt-F10 for intelliJ
    add_caps_lalt_mapping_w_alt_active (&k, Key::F10, Arc::new (|| ctrl_press_release(Key::F10)));



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



    // finally bind everything from action-maps !!
    bind_all_from_action_maps (&k);

    // note: the handle_input_events to start the whole shebang should be being called somewhere in main after this setup

}

