use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
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
    // it'll always be in sync .. just that its in sync for our caps combo periods! (which is a LOT easier)
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
    pub base_act_map     : HashMap<Key,AF>,
    pub caps_act_map     : HashMap<Key,AF>,
    pub alt_act_map      : HashMap<Key,AF>,
    pub win_act_map      : HashMap<Key,AF>,
    pub caps_win_act_map : HashMap<Key,AF>,
    pub caps_alt_act_map : HashMap<Key,AF>,
}



#[derive(Default)]
pub struct Krusty {
    // this is mostly just a utility wrapper sugar to pass things around
    pub l2s : Arc <Layer2State>,
    pub am  : Rc <RefCell <ActionMaps>>
}


impl Krusty {

    // NOTE: we've decided to mostly impl things grouped by related functionality in disparate modules rather than under impl here
    // .. mostly because doing that allows easier piecemeal imports, as well as better pub/private management

    fn setup_global_disable (&self) {
        // todo: wont be straight-forward if we want to handle this at hook receipt
        // .. will need a global check there, as well as allowing selectively for the re-enable combo!
    }

}


pub mod key_utils {

    use crate::krusty::*;

    pub fn press_release (key:Key) {
        key.press(); key.release();
    }
    pub fn shift_press_release (key:Key) {
        Key::Shift.press(); key.press(); key.release(); Key::Shift.release();
    }
    pub fn ctrl_press_release (key:Key) {
        Key::Ctrl.press(); key.press(); key.release(); Key::Ctrl.release();
    }

    pub fn no_action () -> AF { Arc::new ( || {} ) }
    pub fn normal_action (key:Key) -> AF { Arc::new ( move || { press_release(key) } ) }
    pub fn ctrl_action   (key:Key) -> AF { Arc::new ( move || { ctrl_press_release(key) } ) }
    pub fn shift_action  (key:Key) -> AF { Arc::new ( move || { shift_press_release(key) } ) }

}



pub mod lalt_setups { // left-alt tracking business

    use crate::krusty::*;

    pub fn setup_left_alt_tracking (l2sr:&L2S) {
        // for left-alt, we'll let it go through, but instead of letting it spam on repeat on long presses, we just preserve the logical
        //     state but suppress the spams .. that helps us reason safely about its logical state in all the other combos, esp non-caps ones,
        //     .. plus, it makes alt-combos for keys not handled here continue to work as expected
        // ugh, however, letting alt out unblocked, does cause issues at times (eg. when it moves focus to menu items .. so we'll track both its
        //     internal, as well as external state for when we've had to suppress it while caps was down etc
        let l2s = l2sr.clone();
        Key::LAlt.blockable_bind(KeyDownCallback, move |_| {
            // so goal here is, any presses with caps active, we suppress alt going outside
            // and since we can have caps come in AFTER alt is already down, we'll have to capture disparity states, as well as to deal
            // with restoring that when either caps/alt gets released etc
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
    pub fn ensure_not_held_alt_inactive (l2s:&L2S) {
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
    pub fn ensure_lalt_inactive (l2s:&L2S) {
        // utility to ensure lalt is inactive regardless if held down
        // (expectation is l-2 keys should ensure that, and l-alt handler should never activate it when l2/caps is active either)
        if *l2s.is_alt_active.read().unwrap() {
            *l2s.is_alt_active.write().unwrap() = false;
            Key::LAlt.release();
        }
    }

    pub fn try_sync_lalt_if_no_caps (l2s:&L2S) {
        // well .. this fn .. either way, it can only fix when no-caps, so users will have to check first, and act differently ..
        //    at which point, in most cases why bother, might as well just skip trying to (partially) fix it and handle it themselves!
        //    .. plus, for them, they can selectively only send something IF the mismatch type was actually problematic for them
        //    .. hence the various partial utility fns to cover those cases explicitly ONLY when absolutely necessary
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
            // ^^ this is incompatible w caps grace ..
            //    .. infact its worse than just no grace because it immediately triggers instead of natural grace waiting for alt's key repeat
            //    .. so instead we'll just do a 'sync' if we needed an alt release
            ensure_not_held_alt_inactive(&l2s);
        });
    }


    pub fn setup_right_alt_tracking (l2sr:&L2S) {
        /// for most cases (except e.g. Space etc), we map it to RShift
        let l2s = l2sr.clone();
        Key::RAlt.block_bind(KeyDownCallback, move |_| {
            *l2s.is_ralt_down.write().unwrap() = true;
            Key::RShift.press();
        });
        let l2s = l2sr.clone();
        Key::RAlt.block_bind(KeyUpCallback, move |_| {
            *l2s.is_ralt_down.write().unwrap() = false;
            Key::RShift.release();
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
        // note: explicitly takes l2s so it can be moved into threads (instead of moving self/krusty which has non mobile action maps)
        if *l2s.is_lwin_down.read().unwrap() && !*l2s.is_down_lwin_consumed.read().unwrap() {
            *l2s.is_down_lwin_consumed.write().unwrap() = true;
        }
    }


    pub fn setup_left_shift_tracking (l2sr:&L2S) {
        // for left-shift, we really only care for a few combos with caps .. could query it as necessary too, but might as well
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

    use crate::krusty::*;

    pub fn setup_mouse_left_btn_handling (l2sr:&L2S) {
        let l2s = l2sr.clone();
        MouseButton::LeftButton.block_bind(true, move |_| {
            *l2s.is_mouse_left_btn_down.write().unwrap() = true;
            if *l2s.is_caps_down.read().unwrap() {
                *l2s.in_managed_ctrl_down_state.write().unwrap() = true;
                Key::LControl.press();
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
        // tracking btn down is for ctrl-wheel (zoom) etc, adn right-btn-down-wheel is for switche scroll (via F21/F22/F23)
        let l2s = l2sr.clone();
        MouseButton::RightButton.non_blocking_bind(true, move |_| {
            *l2s.is_mouse_right_btn_down.write().unwrap() = true;
        });
        let l2s = l2sr.clone();
        MouseButton::RightButton.non_blocking_bind(false, move |_| {
            *l2s.is_mouse_right_btn_down.write().unwrap() = false;
            if *l2s.in_right_btn_scroll_state.read().unwrap() {
                *l2s.in_right_btn_scroll_state.write().unwrap() = false;
                Key::F23.press(); Key::F23.release();
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

    use crate::krusty::*;
    use windows::Win32::UI::WindowsAndMessaging::WHEEL_DELTA;
    use crate::utils::window_utils::get_fgnd_win_class;

    pub fn handle_wheel_guarded (delta:i32, l2sr:&L2S) {
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
            // support for switche task switching
            if !*l2sr.in_right_btn_scroll_state.read().unwrap() { *l2sr.in_right_btn_scroll_state.write().unwrap() = true; }
            let key = if incr.is_positive() { Key::F22 } else { Key::F21 };
            key.press(); key.release();
        } else  if *l2sr.is_lalt_down.read().unwrap() {
            if *l2sr.is_caps_down.read().unwrap() || get_fgnd_win_class() == "MultitaskingViewFrame" { // alt-tab states
                // ensure alt is externally active first
                if !*l2sr.is_alt_active.read().unwrap() {
                    *l2sr.is_alt_active.write().unwrap() = true;
                    Key::LAlt.press();
                }
                handle_alt_tab_wheel(incr)
            } else {
                handle_volume_wheel(incr);
            }
        } else if *l2sr.is_lwin_down.read().unwrap() {
            *l2sr.is_down_lwin_consumed.write().unwrap() = true;
            handle_brightness_wheel(incr);
        } else if *l2sr.is_caps_down.read().unwrap() {
            *l2sr.in_managed_ctrl_down_state.write().unwrap() = true;
            Key::LControl.press();
            MouseWheel::DefaultWheel.scroll(delta);
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
        // todo impl timer-spacing here
        // note that for these we DONT want to release Alt key .. presumably, expecting it to be physically released later
        if incr.is_positive() {
            Key::LShift.press(); Key::Tab.press(); Key::Tab.release(); Key::LShift.release();
        } else {
            Key::Tab.press(); Key::Tab.release();
        }
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

    use crate::krusty::{*, lalt_setups::*};

    pub fn setup_space_key_handling (l2sr:&L2S) {
        let l2s = l2sr.clone();
        Key::Space.block_bind(KeyDownCallback, move |_| {
            if *l2s.is_ralt_down.read().unwrap() {
                // generic remapping of RAlt_Space to Enter
                // note that ralt is mapped separately to shift .. so that will be 'down' when we're here
                Key::RShift.release();
                Key::Enter.press(); Key::Enter.release();
                Key::RShift.press();
            } else if *l2s.is_caps_down.read().unwrap() {
                if *l2s.is_lalt_down.read().unwrap() {
                    // remapping Caps_LAlt_Space to Alt_Enter, mostly for IntelliJ
                    ensure_held_lalt_active(&l2s);
                    Key::Enter.press(); Key::Enter.release();
                } else {
                    // else if caps treat as ctrl-combo, should cover when shift is pressed too!
                    ensure_not_held_alt_inactive(&l2s);
                    Key::LControl.press();
                    Key::Space.press(); Key::Space.release();
                    Key::LControl.release();
                }
            } else {
                // nominal fallback .. dont try to check/fix alt states, as we might even have non-tracked alts active!
                Key::Space.press(); Key::Space.release();
            }
        });
        Key::Space.block_bind(KeyUpCallback, |_| {}); // just block it
    }

    pub fn setup_tab_key_handling (l2sr:&L2S) {
        let l2s = l2sr.clone();
        Key::Tab.block_bind(KeyDownCallback, move |_| {
            if *l2s.is_caps_down.read().unwrap() {
                //if !*l2s.in_managed_ctrl_down_state.read().unwrap() {
                    // ^^ disabling this check, makes it more robust w/ no real cost
                    *l2s.in_managed_ctrl_down_state.write().unwrap() = true;
                    Key::LControl.press();
                //}
            }
            Key::Tab.press(); Key::Tab.release();
            // note that this ^^ will make it instant press/release for tab, even w/o caps held (and that's good)
        });
        Key::Tab.block_bind(KeyUpCallback, move |_| {}); // just gotta block it
    }

}



pub mod caret_mode_setups {  // caret modes setups

    use crate::krusty::*;

    pub fn setup_caret_mode_key (l2sr:&L2S, key:Key, mode_flag_r:&Arc<RwLock<bool>>, ov_fn:AF) {
        // note that we only really just set/clear the particular caret-mode flag here, plus any non-caret related overloads
        // .. the actual caret-mode functionality ofc has to be impld by every key that supports it by checking this flag
        let mf = mode_flag_r.clone();
        let l2s = l2sr.clone();
        key.block_bind(KeyDownCallback, move |_| {
           *mf.write().unwrap() = true;
            if *l2s.is_caps_down.read().unwrap() {
                ov_fn(); // any overload functionality (since they cant be put in regular action maps like for other keys)
            } else  {
                key.press(); key.release()
            }
        });
        let mf = mode_flag_r.clone();
        key.block_bind(KeyUpCallback, move |_| {
            *mf.write().unwrap() = false;
        });
    }

}


pub mod action_map_utils {

    use crate::krusty::{*, key_utils::*, lalt_setups::*, mod_keys_setups::*};

    fn compose_action_maps_cb (k:&Krusty, key:Key) -> CB {
        let (mba, mca, maa, mwa, mcwa, mcaa) = (
            // hashmap returns option of refs to Arcs in there, we'll need to clone each one inside the Option to use them outside
            k.am.borrow().base_act_map     .get(&key) .map(|e| e.clone()),
            k.am.borrow().caps_act_map     .get(&key) .map(|e| e.clone()),
            k.am.borrow().alt_act_map      .get(&key) .map(|e| e.clone()),
            k.am.borrow().win_act_map      .get(&key) .map(|e| e.clone()),
            k.am.borrow().caps_win_act_map .get(&key) .map(|e| e.clone()),
            k.am.borrow().caps_alt_act_map .get(&key) .map(|e| e.clone()),
        );
        // we cant move local options to closure either, just the Arcs inside, so do that part of composition before making closure
        // first, the base value cant be option, use normal action as default
        let ba = mba.unwrap_or(normal_action(key));
        // next derive actual equivalent actions for the rest of options, defaulting to base if unspecified
        let (ca, aa, wa, cwa) = (mca.unwrap_or(ba.clone()), maa.unwrap_or(ba.clone()), mwa.unwrap_or(ba.clone()), mcwa.unwrap_or(ba.clone()) );
        // caa is special for l2 support, so if no explicit mcaa, we'll use ca composed above (instead of allowing alt-action via base)
        let caa = mcaa.unwrap_or(ca.clone());
        // now we can finally move these Arc action clones into the closure we need
        let l2s = k.l2s.clone();
        Box::new ( move |_| { // note that we're ignoring the KbdEvent passed in (it'd have KbdEventType and key vk/sc codes)
            if *l2s.is_caps_down.read().unwrap() {
                if *l2s.is_lwin_down.read().unwrap() { cwa() }
                else if *l2s.is_lalt_down.read().unwrap() { caa() }
                else { ca() }
            } else if *l2s.is_lwin_down.read().unwrap() { wa() }
            else if *l2s.is_lalt_down.read().unwrap() { aa() }
            else { ba() }
        } )
    }

    fn get_composition_maps_keys_union (k:&Krusty) -> Rc<HashSet<Key>> {
        let mut keys = HashSet::new();
        k.am.borrow().base_act_map       .keys().for_each ( |k| { keys.insert(*k); } );
        k.am.borrow().caps_act_map       .keys().for_each ( |k| { keys.insert(*k); } );
        k.am.borrow().alt_act_map        .keys().for_each ( |k| { keys.insert(*k); } );
        k.am.borrow().win_act_map        .keys().for_each ( |k| { keys.insert(*k); } );
        k.am.borrow().caps_win_act_map   .keys().for_each ( |k| { keys.insert(*k); } );
        k.am.borrow().caps_alt_act_map   .keys().for_each ( |k| { keys.insert(*k); } );
        Rc::new(keys)
    }

    pub fn bind_all_from_action_maps (k:&Krusty) {
        for key in get_composition_maps_keys_union(k).iter() {
            let cb = compose_action_maps_cb (k, *key);
            key.block_bind (KeyDownCallback, cb);
            key.block_bind (KeyUpCallback, |_| { });
            // ^^ note that key-up is disabled for EVERYTHING bound from action-maps! (shouldnt be a big deal at all though)
        }
    }



    pub fn add_base_mapping (k:&Krusty, key:Key, action:AF) {
        k.am.borrow_mut() .base_act_map .insert (key, action);
    }
    pub fn add_caps_mapping (k:&Krusty, key:Key, action:AF) {
        k.am.borrow_mut() .caps_act_map .insert (key, action);
    }
    pub fn add_caps_as_shift_mapping (k:&Krusty, key:Key) {
        k.am.borrow_mut() .caps_act_map .insert (key, shift_action(key));
    }
    pub fn add_caps_as_ctrl_mapping (k:&Krusty, key:Key) {
        k.am.borrow_mut() .caps_act_map .insert (key, ctrl_action(key));
    }

    pub fn add_alt_mapping (k:&Krusty, key:Key, action:AF) {
        k.am.borrow_mut() .alt_act_map .insert (key, action);
    }
    pub fn add_caps_alt_mapping (k:&Krusty, key:Key, action:AF) {
        k.am.borrow_mut() .caps_alt_act_map .insert (key, action);
    }
    pub fn add_caps_alt_mapping_w_alt_active (k:&Krusty, key:Key, action:AF) {
        let l2s = k.l2s.clone();
        let action = Arc::new ( move || { ensure_held_lalt_active(&l2s); action() } );
        k.am.borrow_mut() .caps_alt_act_map .insert (key, action);
    }
    pub fn add_caps_alt_mapping_w_alt_inactive (k:&Krusty, key:Key, action:AF) {
        let l2s = k.l2s.clone();
        let action = Arc::new ( move || { ensure_held_lalt_inactive(&l2s); action() } );
        k.am.borrow_mut() .caps_alt_act_map .insert (key, action);
    }

    pub fn add_win_mapping (k:&Krusty, key:Key, action:AF) {
        let l2s = k.l2s.clone();
        let action = Arc::new (move || { consume_down_lwin(&l2s); action(); });
        k.am.borrow_mut() .win_act_map .insert (key, action);
    }
    pub fn add_caps_win_mapping (k:&Krusty, key:Key, action:AF) {
        let l2s = k.l2s.clone();
        let action = Arc::new (move || { consume_down_lwin(&l2s); action(); });
        k.am.borrow_mut() .caps_win_act_map .insert (key, action);
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
    use crate::krusty::{*, lalt_setups::*};

    /// typical nav action as press/release of key
    pub fn base_afg (l2k:Key) -> AF {
        Arc::new ( move || { l2k.press(); l2k.release(); } )
    }
    /// sends nav actions wrapped with ctrl presses (which should trigger word navigation)
    pub fn l2_ctrl_afg (l2k:Key) -> AF {
        Arc::new ( move || { Key::Ctrl.press(); base_afg(l2k)(); Key::Ctrl.release(); } )
    }
    /// sends two nav actions sequentially (instead of the typical single nav action)
    pub fn l2_fast_afg (l2k:Key) -> AF {
        Arc::new ( move || { base_afg(l2k)(); base_afg(l2k)(); } )
    }
    pub type AFG = fn(Key) -> AF ;

    fn l2_nav_afg (l2s:L2S, l2k:Key, aafg:AFG, fafg:AFG) -> AF {
        // note: this ^^ cant take k:&Krusty because k.am would be not 'Send' while k.l2s is, and so only can be sent here from AFs
        let (base_af, alt_af, fast_af) = (base_afg(l2k), aafg(l2k), fafg(l2k));
        Arc::new ( move || {
            if *l2s.is_lalt_down.read().unwrap() { alt_af() }
            else if *l2s.in_caret_fast_mode.read().unwrap() { fast_af() }
            else { base_af() }
    } ) }

    /// delete action with specified bksp/del key, supports the fast/word nav options
    pub fn l2_del_afg (l2s:L2S, del_key:Key, _:AF) -> AF {
        Arc::new ( move || {
            l2_nav_afg (l2s.clone(), del_key, l2_ctrl_afg, l2_fast_afg) ()
    } ) }
    /// delete action as first select and then delete w the specified bksp/del key
    pub fn l2_del_sel_afg (_:L2S, del_key:Key, nav_af:AF) -> AF {
        Arc::new ( move || {
            Key::Shift.press(); nav_af(); Key::Shift.release();
            base_afg(del_key)();
    } ) }
    pub type DAFG = fn(L2S, Key, AF) -> AF ;

    fn l2_sel_afg (nav_af:AF) -> AF {
        Arc::new ( move || {
            Key::Shift.press(); nav_af(); Key::LShift.release()
    } ) }

    fn l2_afg (l2s:L2S, nav_af:AF, sel_af:AF, del_af:AF) -> AF {
        Arc::new ( move || {
            ensure_lalt_inactive(&l2s);
            // ^^ NOTE that for ALL l2, lAlt is suppressed other than for l2 behavior
            if *l2s.in_caret_sel_mode.read().unwrap() { sel_af() }
            else if *l2s.in_caret_del_mode.read().unwrap() { del_af() }
            else { nav_af() }
    } ) }

    /// setup layer-2 behavior for a given 'key'
    /// *l2k* : layer-2 eqv key to press for regular/nav actions
    /// *dk* : key to use for delete action (backspace or delete)
    /// *aafg* : alt-action-gen,  specifies regular/fast/word-nav when alt held
    /// *fafg* : fast-action-gen, specifies regular/fast nav when fast-mode-key (.) held
    /// *dafg* : del-action-gen,  specifies del action when 'd' held, either direct 'dk' or select-then-dk
    /// (note that there's default selection action when 'r' is held)
    pub fn setup_l2k (k:&Krusty, key:Key, l2k:Key, dk:Key, aafg:AFG, fafg:AFG, dafg:DAFG) {
        let nav_af = l2_nav_afg (k.l2s.clone(), l2k, aafg, fafg);
        let sel_af = l2_sel_afg (nav_af.clone());
        let del_af = dafg (k.l2s.clone(), dk, nav_af.clone());
        let l2_af  = l2_afg (k.l2s.clone(), nav_af, sel_af, del_af);
        // note that we'll set the composed callback action in caps map, but alt-caps will also call it (if undefined)
        k.am.borrow_mut().caps_act_map .insert (key, l2_af);
    }

    // note: actual setup for l2k keys can be done (importing utils this mod) along with other krustyboard setup steps

}


pub fn setup_krusty_board () {

    use crate::krusty::{*, key_utils::*, lalt_setups::*, mod_keys_setups::*, special_keys_setups::*,
                        caret_mode_setups::*, mouse_btn_setups::*, mouse_wheel_setups::*, action_map_utils::*};

    use crate::utils::{window_utils::*, process_utils::*};


    let k = Krusty {
        l2s : Arc::new(Layer2State::default()),
        am  : Rc::new(RefCell::new(ActionMaps::default())),
    };


    // setup capslock, we'll completely disable it other than for krusty use
    setup_caps_tracking (&k.l2s);
    // setup left-alt, this will be monitored, but allowed to get out-of-sync between pressed state and external state
    setup_left_alt_tracking (&k.l2s);
    // setup tracking for right-alt too .. for most cases (except e.g. Space etc), we completely map it to RShift
    setup_right_alt_tracking (&k.l2s);
    // and for l-win .. this will track, but also modify win behavior to suppress repeats, and if consumed, its key-ups
    setup_left_win_tracking (&k.l2s);
    // and left-shift .. this is simple non-blocking tracking, only tracked to enable a few combos with caps
    setup_left_shift_tracking (&k.l2s);


    // setup handling for space key .. overloads into enter with r-alt etc
    setup_space_key_handling (&k.l2s);
    // tab has special management to support alt-tab, but also turning it instant press/release for all cases
    setup_tab_key_handling (&k.l2s);


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
    // .. note: before setting up caret mode, we'll need to define any non-caret overloads (during caps-caret) for them
    // .. and we only need that for 'period' for caps-win-period as window-vert-stretch
    setup_caret_mode_key (&k.l2s, Key::R, &k.l2s.in_caret_sel_mode, no_action());
    setup_caret_mode_key (&k.l2s, Key::D, &k.l2s.in_caret_del_mode, no_action());
    let l2s = k.l2s.clone();
    let ov_fn = Arc::new ( move || {
        if *l2s.is_lwin_down.read().unwrap() { win_fgnd_stretch(0,30); }
    } );
    setup_caret_mode_key (&k.l2s, Key::Period, &k.l2s.in_caret_fast_mode, ov_fn);




    // setting up the all other keys via by-key-combo action maps that we'll compose at the end to relevant callbacks

    // win-m by default minimized all windows .. we just want to disable it
    add_win_mapping (&k, Key::M, Arc::new (|| {} ));

    // win-i should start irfanview
    add_win_mapping (&k, Key::I, Arc::new (|| { start_irfanview(); } ));

    // win-n should start chrome-incognito
    add_win_mapping (&k, Key::N, Arc::new (|| { start_chrome_incognito(); } ));

    // in cur laptop, Fn-F6/F7 do brightness, but at +10 incrs .. set them to do small incrs with win combos
    add_win_mapping (&k, Key::F6, Arc::new (|| { handle_brightness_wheel(-1) } ));
    add_win_mapping (&k, Key::F7, Arc::new (|| { handle_brightness_wheel(1) } ));

    // might as well do that for alt as well, since we use alt for most other such shortcuts
    add_alt_mapping (&k, Key::F6, Arc::new ( || { handle_brightness_wheel(-1); } ) );
    add_alt_mapping (&k, Key::F7, Arc::new ( || { handle_brightness_wheel(1); } ) );


    // actually, since we use alt-2/3 as vol down/up, might as well also set win-2/3 for brightness down/up
    add_win_mapping (&k, Key::Numrow_2, Arc::new (|| { handle_brightness_wheel(-1) } ));
    add_win_mapping (&k, Key::Numrow_3, Arc::new (|| { handle_brightness_wheel(1) } ));


    // alt-f1 play/pause, caps-f1 toggle mute, base-case, overload F1 for switche caller (F21)
    let l2s = k.l2s.clone();
    add_base_mapping (&k, Key::F1, normal_action(Key::F21));
    add_caps_mapping (&k, Key::F1, normal_action(Key::VolumeMute));
    add_alt_mapping  (&k, Key::F1, Arc::new ( move || {
        // media keys only work while alt is inactive, so gotta set/restore that etc
        ensure_held_lalt_inactive(&l2s);
        Key::MediaPlayPause.press(); Key::MediaPlayPause.release();
    } ) );

    fn do_media_skips (n_skips:u32, l2sr:&L2S) {
        // sending ^!{Right} which we traditionally set on winamp to skip forward a bit
        //ensure_held_lalt_inactive(l2sr);
        // note that we'll send alts explicitly so it works even if alt was quickly released
        ensure_alt_active(l2sr);
        Key::LControl.press();
        (0 ..n_skips) .into_iter().for_each(|_| { Key::ExtRight.press(); Key::ExtRight.release() });
        Key::LControl.release();
        ensure_not_held_alt_inactive(l2sr);
    }

    // al-f2 for next with some initial skip
    let l2s = k.l2s.clone();
    add_alt_mapping (&k, Key::F2, Arc::new ( move || {
        ensure_held_lalt_inactive(&l2s);
        if !*l2s.is_lshift_down.read().unwrap() {
            Key::MediaNextTrack.press();  Key::MediaNextTrack.release();
        } else {
            Key::LShift.release(); Key::MediaPrevTrack.press();  Key::MediaPrevTrack.release();
        };
        let l2s = l2s.clone(); // gotta clone again to allow moving into spawned thread closure!
        thread::spawn ( move || { thread::sleep(time::Duration::from_millis(2000));  do_media_skips(3,&l2s); } );
    } ) );

    // alt-f3 for skip forward a bit (skips is via alt-ctrl-Right, not via media keys)
    let l2s = k.l2s.clone();
    add_alt_mapping (&k, Key::F3, Arc::new ( move || { do_media_skips(1,&l2s) } ) );
    // alt-2 is vol down, alt-3 is vol up
    add_alt_mapping (&k, Key::Numrow_2, normal_action(Key::VolumeDown));
    add_alt_mapping (&k, Key::Numrow_3, normal_action(Key::VolumeUp));

    // trying out alt-1 for switche next-win (via its F20/Alt-F20 hotkey)
    add_alt_mapping (&k, Key::Numrow_1, normal_action(Key::F20));



    // caps-e is Enter
    add_caps_mapping (&k, Key::E, normal_action(Key::Enter));

    // escape is just escape, but we just want it to do press-release immediately (so switche is faster)
    add_base_mapping (&k, Key::Escape, normal_action(Key::Escape));

    // use the apps key to send shift-escape
    add_base_mapping (&k, Key::Apps, shift_action(Key::Escape));

    // for back-tick, make normal case be Delete, caps do back-tick, and shifts do its ~, alt will do quick switch via F20/Alt-F20
    // note that this means the shift-delete action which usually maps to 'cut' action wont be available on this key
    let l2s = k.l2s.clone();
    add_base_mapping (&k, Key::Backquote, Arc::new ( move || {
        press_release (
            if *l2s.is_lshift_down.read().unwrap() || *l2s.is_ralt_down.read().unwrap() { Key::Backquote }
            else { Key::ExtDelete }
        );
    }));
    add_caps_mapping (&k, Key::Backquote, normal_action(Key::Backquote));
    add_alt_mapping  (&k, Key::Backquote, normal_action(Key::F20));



    // number of caps-as-shift mappings
    add_caps_as_shift_mapping (&k, Key::Numrow_6);
    add_caps_as_shift_mapping (&k, Key::Numrow_7);
    add_caps_as_shift_mapping (&k, Key::Numrow_8);
    add_caps_as_shift_mapping (&k, Key::Numrow_9);
    add_caps_as_shift_mapping (&k, Key::Numrow_0);
    add_caps_as_shift_mapping (&k, Key::Minus);
    add_caps_as_shift_mapping (&k, Key::Equal);
    add_caps_as_shift_mapping (&k, Key::LBracket);
    add_caps_as_shift_mapping (&k, Key::RBracket);
    add_caps_as_shift_mapping (&k, Key::Semicolon);
    add_caps_as_shift_mapping (&k, Key::Quote);
    add_caps_as_shift_mapping (&k, Key::Slash);
    add_caps_as_shift_mapping (&k, Key::Backslash);
    //add_caps_as_shift_mapping (&k, Key::Period);  // can't do as used as 'fast' accelerator in caret mode!


    // number of caps-as-ctrl mappings
    add_caps_as_ctrl_mapping (&k, Key::A);
    add_caps_as_ctrl_mapping (&k, Key::S);
    add_caps_as_ctrl_mapping (&k, Key::F);  // further overloading below for caps-alt-f for full-screen
    add_caps_as_ctrl_mapping (&k, Key::W);  // further overloading below for caps-alt-w to close windows
    add_caps_as_ctrl_mapping (&k, Key::T);
    add_caps_as_ctrl_mapping (&k, Key::Y);
    add_caps_as_ctrl_mapping (&k, Key::Z);
    add_caps_as_ctrl_mapping (&k, Key::X);
    add_caps_as_ctrl_mapping (&k, Key::C);
    add_caps_as_ctrl_mapping (&k, Key::V);
    add_caps_as_ctrl_mapping (&k, Key::N);
    add_caps_as_ctrl_mapping (&k, Key::O);
    add_caps_as_ctrl_mapping (&k, Key::P);



    // caps-alt-F for full screen, but also the expected caps-f for ctrl-f
    add_caps_alt_mapping_w_alt_inactive (&k, Key::F, Arc::new ( move || { press_release(Key::F11) } ) );

    // 'w' should have caps-ctrl mapping, but when w/ alt, send alt-f4 (to close all-tabs, windows etc)
    // note: initially we wanted this with caps-shift-w, but turns out (at least on my kbd, turns out caps+shift+[F1, 2, w, s, x]
    //      dont produce any key event at the hook at all .. nothing .. its like the keyboard driver not sending those out
    //  .. funnily enough, there's a bunch of complaints about specifically those keys for dell/hp laptops .. looks like hardware
    //  .. appears to be a common kbd pcb layout issue .. heres from 2007: (https://www.joachim-breitner.de/blog/250-Shift-Caps-2)
    // ... sooo .. to makeup, we'll do alt-caps-w do the alt-f4 business instead
    add_caps_alt_mapping_w_alt_active (&k, Key::W, Arc::new ( move || { press_release(Key::F4) } ) );

    // caps-alt-F10 just to pass through as ctrl-alt-F10 for intelliJ
    add_caps_alt_mapping_w_alt_active (&k, Key::F10, Arc::new ( move || { ctrl_press_release(Key::F10) } ) );



    // filling out l2 actions (incl w caps-alt combos)
    use crate::krusty::l2_utils::*;
    setup_l2k ( &k,  Key::J,     Key::ExtLeft,  Key::Backspace,   l2_ctrl_afg,  l2_fast_afg,  l2_del_afg );
    setup_l2k ( &k,  Key::K,     Key::ExtRight, Key::ExtDelete,   l2_ctrl_afg,  l2_fast_afg,  l2_del_afg );
    setup_l2k ( &k,  Key::I,     Key::ExtUp,    Key::Backspace,   l2_fast_afg,  l2_fast_afg,  l2_del_sel_afg );
    setup_l2k ( &k,  Key::Comma, Key::ExtDown,  Key::ExtDelete,   l2_fast_afg,  l2_fast_afg,  l2_del_sel_afg );
    setup_l2k ( &k,  Key::H,     Key::ExtHome,  Key::Backspace,   base_afg,     base_afg,     l2_del_sel_afg );
    setup_l2k ( &k,  Key::L,     Key::ExtEnd,   Key::ExtDelete,   base_afg,     base_afg,     l2_del_sel_afg );
    setup_l2k ( &k,  Key::U,     Key::ExtPgUp,  Key::Backspace,   base_afg,     base_afg,     l2_del_sel_afg );
    setup_l2k ( &k,  Key::M,     Key::ExtPgDn,  Key::ExtDelete,   base_afg,     base_afg,     l2_del_sel_afg );



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
    add_caps_win_mapping (&k, Key::H,         Arc::new (|| win_fgnd_stretch(-30, 0) ));
    add_caps_win_mapping (&k, Key::O,         Arc::new (|| win_fgnd_stretch(0, -30) ));
    //add_caps_win_mapping (&k, Key::Period,  Arc::new (|| win_fgnd_stretch(0, 30) ));
    // ^^ cant to 'Period' here, as thats caret key w/ key-up handling .. gotta add any overloads right there
    //add_caps_win_mapping (&k, Key::L,         Arc::new (|| win_fgnd_stretch(30, 0) ));
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


}

