use std::alloc::LayoutErr;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::process::Command;
use std::rc::Rc;
use std::sync::{Arc, RwLock};
use std::thread;
use std::time;
use std::time::Instant;

use derivative::Derivative;
use windows::Win32::UI::WindowsAndMessaging::WHEEL_DELTA;

use crate::{BlockInput, KbdEvent, KbdEvntCbMapKeyType::*, KbdKey, KeySequence, MouseButton, MouseEvent, MouseWheel, utils};
use crate::utils::process_utils;
use crate::utils::window_utils::get_fgnd_win_class;



// we'll define some easier type aliases (CallBack, ArcFn, Arc-L2S etc) to pass around triggered actions etc
type CB  = Box <dyn Fn(KbdEvent) + Send + Sync + 'static> ;
type AF  = Arc <dyn Fn() + Send + Sync + 'static> ;
type L2S = Arc <Layer2State> ;
type Key = KbdKey ;



#[derive(Derivative)]
#[derivative(Default, Debug)]
struct Layer2State {
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
struct ActionMaps {
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
struct Krusty {
    // we'll use this as the global struct/object encapsulating all our krusty-board functionality
    pub l2s : Arc<Layer2State>,
    pub am  : Rc<RefCell<ActionMaps>>
}



impl Krusty { // basic key utils

    fn setup_global_disable (&self) {
        // todo: wont be straight-forward if we want to handle this at hook receipt
        // .. will need a global check there, as well as allowing selectively for the re-enable combo!
    }

    fn press_release (key:Key) {
        key.press(); key.release();
    }
    fn shift_press_release (key:Key) {
        Key::Shift.press(); key.press(); key.release(); Key::Shift.release();
    }
    fn ctrl_press_release (key:Key) {
        Key::Ctrl.press(); key.press(); key.release(); Key::Ctrl.release();
    }

    fn no_action () -> AF { Arc::new ( || {} ) }
    fn normal_action (key:Key) -> AF { Arc::new ( move || { Krusty::press_release(key) } ) }
    fn ctrl_action   (key:Key) -> AF { Arc::new ( move || { Krusty::ctrl_press_release(key) } ) }
    fn shift_action  (key:Key) -> AF { Arc::new ( move || { Krusty::shift_press_release(key) } ) }

}



impl Krusty { // left-alt tracking business

    fn setup_left_alt_tracking (&self) {
        /// for left-alt, we'll let it go through, but instead of letting it spam on repeat on long presses, we just preserve the logical
        ///     state but supress the spams .. that helps us reason safely about its logical state in all the other combos, esp non-caps ones,
        ///     .. plus, it makes alt-combos for keys not handled here continue to work as expected
        /// ugh, however, letting alt out unblocked, does cause issues at times (eg. when it moves focus to menu items .. so we'll track both its
        ///     internal, as well as external state for when we've had to suppress it while caps was down etc
        let l2s = self.l2s.clone();
        Key::LAlt.blockable_bind(KeyDownCallback, move |_| {
            // so goal here is, any presses with caps active, we supress alt going outside
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
        let l2s = self.l2s.clone();
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


    fn ensure_held_lalt_active(l2s:&L2S) {
        // utility to set held l-alt active (sync it) while tracking state (useful for media shortcuts etc)
        if *l2s.is_lalt_down.read().unwrap() && !*l2s.is_alt_active.read().unwrap() {
            Key::LAlt.press();
            *l2s.is_alt_active.write().unwrap() = true;
        }
    }
    fn ensure_alt_active(l2s:&L2S) {
        // utility to get alt out reliably whether its currently pressed or not, while keeping state tracking updated
        if !*l2s.is_alt_active.read().unwrap() {
            Key::LAlt.press();
            *l2s.is_alt_active.write().unwrap() = true;
        }
    }
    fn ensure_not_held_alt_inactive(l2s:&L2S) {
        // utility to set not-held alt inactive (sync it) while tracking (useful for alt-combos that prdouce non-alt outputs)
        if !*l2s.is_lalt_down.read().unwrap() && *l2s.is_alt_active.read().unwrap() {
            Key::LAlt.release();
            *l2s.is_alt_active.write().unwrap() = false;
        }
    }
    fn ensure_held_lalt_inactive(l2s:&L2S) {
        // utility to hard-clear alt state for alt-combos (e.g. for media shortcuts etc)
        if *l2s.is_lalt_down.read().unwrap() && *l2s.is_alt_active.read().unwrap() {
            Key::LAlt.release();
            *l2s.is_alt_active.write().unwrap() = false;
        }
    }
    fn ensure_lalt_inactive(l2s:&L2S) {
        // utility to ensure lalt is inactive regardless if held down
        // (expectation is l-2 keys should ensure that, and l-alt handler should never activate it when l2/caps is active either)
        if *l2s.is_alt_active.read().unwrap() {
            *l2s.is_alt_active.write().unwrap() = false;
            Key::LAlt.release();
        }
    }

    fn try_sync_lalt_if_no_caps (l2s:&L2S) {
        // well .. this fn .. either way, it can only fix when no-caps, so users will ahve to check first, and act differently ..
        //    at which point, in most cases why bother, might as well just skip trying to (partially) fix it and handle it themselves!
        //    .. plus, for them, they can selectively only send something IF the mismatch type was actually problematic for them
        //    .. hence the various partial utility fns to cover those cases explicitly ONLY when absolutely necessary
        // so maybe we'll only use it upon capslock release, but its separately here in case it makes sense for others any other time
        if *l2s.is_caps_down.read().unwrap() {
            // cant safely try to do blindly do anything while caps is down, as we dont wanna interfere w any active layer2 modes
        } else {
            Krusty::try_sync_lalt(l2s);
        }
    }
    fn try_sync_lalt (l2s:&L2S) {
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



impl Krusty { // tracking for other tracked modifier keys e.g. capslock, rAlt, lWin, lShift

    fn setup_caps_tracking (&self) {
        // note that for caps, we completely block it from ever being sent up, and just manage internally!
        if Key::CapsLock.is_toggled() {
            Key::CapsLock.press();  // toggle off first if necessary (to clear key light)
        }
        let l2s = self.l2s.clone();
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
        let l2s = self.l2s.clone();
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
            Krusty::ensure_not_held_alt_inactive(&l2s);
        });
    }
    fn caps_up_grace_active (&self) -> bool { self.l2s.caps_up_grace_active() }



    fn setup_right_alt_tracking (&self) {
        /// for most cases (except e.g. Space etc), we map it to RShift
        let l2s = self.l2s.clone();
        Key::RAlt.block_bind(KeyDownCallback, move |_| {
            *l2s.is_ralt_down.write().unwrap() = true;
            Key::RShift.press();
        });
        let l2s = self.l2s.clone();
        Key::RAlt.block_bind(KeyUpCallback, move |_| {
            *l2s.is_ralt_down.write().unwrap() = false;
            Key::RShift.release();
        });
    }


    fn setup_left_win_tracking (&self) {
        /// other than tracking state, we also try to avoid effect of its key-up afterwards if we acted on it ourselves while down
        /// also, to make that work, we'll suppress all repeats too, and ofc wont reset was-consumed flag for repeats either
        /// however, ofc cant just block lwin-up, so instead will have to mask it w a ctrl down/up as ctrl-win doesnt trigger anything
        let l2s = self.l2s.clone();
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
        let l2s = self.l2s.clone();
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
    fn consume_down_lwin (l2s:&L2S) {
        // utility for any key with lwin action to consume down state if they want to (to suppress its keyup later)
        // note: explicitly takes l2s so it can be moved into threads (instead of moving self/krusty which has non mobile action maps)
        if *l2s.is_lwin_down.read().unwrap() && !*l2s.is_down_lwin_consumed.read().unwrap() {
            *l2s.is_down_lwin_consumed.write().unwrap() = true;
        }
    }


    fn setup_left_shift_tracking (&self) {
        // for left-shift, we really only care for a few combos with caps .. could query it as necessary too, but might as well
        let l2s = self.l2s.clone();
        Key::LShift.non_blocking_bind(KeyDownCallback, move |_| {
            if !*l2s.is_lshift_down.read().unwrap() { *l2s.is_lshift_down.write().unwrap() = true; }
        });
        let l2s = self.l2s.clone();
        Key::LShift.non_blocking_bind(KeyUpCallback, move |_| {
            *l2s.is_lshift_down.write().unwrap() = false;
        });
    }

}



impl Krusty { // setups for mouse btn handling

    fn setup_mouse_left_btn_handling (&self) {
        let l2s = self.l2s.clone();
        MouseButton::LeftButton.block_bind(true, move |_| {
            *l2s.is_mouse_left_btn_down.write().unwrap() = true;
            if *l2s.is_caps_down.read().unwrap() {
                *l2s.in_managed_ctrl_down_state.write().unwrap() = true;
                Key::LControl.press();
            }
            MouseButton::LeftButton.press()
        });
        let l2s = self.l2s.clone();
        MouseButton::LeftButton.block_bind(false, move |_| {
            *l2s.is_mouse_left_btn_down.write().unwrap() = false;
            thread::sleep(time::Duration::from_millis(10));
            MouseButton::LeftButton.release();
        });
        // ^^ in theory could do non-blocking bind for release, but that can occasionally expose us to timing issues as press is on block
        //      and that will be slower to send the actual press from the spawned thread (so making release slower too helps)
    }

    fn setup_mouse_right_btn_handling (&self) {
        // tracking btn down is for ctrl-wheel (zoom) etc, adn right-btn-down-wheel is for switche scroll (via F21/F22/F23)
        let l2s = self.l2s.clone();
        MouseButton::RightButton.non_blocking_bind(true, move |_| {
            *l2s.is_mouse_right_btn_down.write().unwrap() = true;
        });
        let l2s = self.l2s.clone();
        MouseButton::RightButton.non_blocking_bind(false, move |_| {
            *l2s.is_mouse_right_btn_down.write().unwrap() = false;
            if *l2s.in_right_btn_scroll_state.read().unwrap() {
                *l2s.in_right_btn_scroll_state.write().unwrap() = false;
                Key::F23.press(); Key::F23.release();
            }
        });
    }

    fn setup_mouse_x_btn_1_handling (&self) {
        // turns out just doing press/release on initial press works snappier/more-reliable than trying to be true to btn-holds
        MouseButton::X1Button.block_bind(true, move |_| {
            MouseButton::MiddleButton.press(); MouseButton::MiddleButton.release();
        });
        MouseButton::X1Button.block_bind(false, move |_| { });
    }

    fn setup_mouse_x_btn_2_handling (&self) {
        MouseButton::X2Button.block_bind(true, move |_| {
            MouseButton::MiddleButton.press(); MouseButton::MiddleButton.release();
        });
        MouseButton::X2Button.block_bind(false, move |_| { });
    }

}



impl Krusty { // setups for mouse wheel handling

    fn handle_wheel_guarded (delta:i32, l2s:&L2S) {
        let last_stamp = *l2s.last_wheel_stamp.read().unwrap();
        *l2s.last_wheel_stamp.write().unwrap() = Instant::now();
        //let gap = l2s.last_wheel_stamp.read().unwrap().duration_since(last_stamp);
        //println!("{:#?}", dur.as_millis());
        const GUARD_DUR_MS: u128 = 120;  // from dur printouts above, looked like max inertial gap is 120 (min 7ms, usually <100)
        if !*l2s.is_wheel_spin_invalidated.read().unwrap() {
            Krusty::handle_wheel_action(delta, l2s);
        } else if GUARD_DUR_MS < l2s.last_wheel_stamp.read().unwrap().duration_since(last_stamp).as_millis() {
            *l2s.is_wheel_spin_invalidated.write().unwrap() = false;
            Krusty::handle_wheel_action(delta, l2s);
        } else {
            // if its invalidated AND wheel-spin spacing is below guard-dur, we supress the wheel event
        }
    }

    fn handle_wheel_action (delta:i32, l2s:&L2S) {
        let incr = delta / WHEEL_DELTA as i32;
        if *l2s.is_mouse_right_btn_down.read().unwrap() {
            // support for switche task switching
            if !*l2s.in_right_btn_scroll_state.read().unwrap() { *l2s.in_right_btn_scroll_state.write().unwrap() = true; }
            let key = if incr.is_positive() { Key::F22 } else { Key::F21 };
            key.press(); key.release();
        } else  if *l2s.is_lalt_down.read().unwrap() {
            if *l2s.is_caps_down.read().unwrap() || get_fgnd_win_class() == "MultitaskingViewFrame" { // alt-tab states
                // ensure alt is externally active first
                if !*l2s.is_alt_active.read().unwrap() {
                    *l2s.is_alt_active.write().unwrap() = true;
                    Key::LAlt.press();
                }
                Krusty::handle_alt_tab_wheel(incr)
            } else {
                Krusty::handle_volume_wheel(incr);
            }
        } else if *l2s.is_lwin_down.read().unwrap() {
            *l2s.is_down_lwin_consumed.write().unwrap() = true;
            Krusty::handle_brightness_wheel(incr);
        } else if *l2s.is_caps_down.read().unwrap() {
            *l2s.in_managed_ctrl_down_state.write().unwrap() = true;
            Key::LControl.press();
            MouseWheel::DefaultWheel.scroll(delta);
        } else if *l2s.is_lshift_down.read().unwrap() {
            //handle_horiz_scroll_wheel(incr);
            // ^^ todo:: .. (and for now, just let default pass through)
            MouseWheel::DefaultWheel.scroll(delta);
        } else {
            MouseWheel::DefaultWheel.scroll(delta);
        }
    }

    fn handle_volume_wheel (incr:i32) {
        let key = if incr > 0 { Key::VolumeUp } else { Key::VolumeDown };
        (0 .. incr.abs()) .for_each(|_| key.press());
    }

    fn handle_brightness_wheel (incr:i32) {
        static INCR_STEP:i32 = 1;
        //utils::brightness_ps_wmi::incr_brightness(INCR_STEP*incr);
        utils::brightness_utils::incr_brightness(INCR_STEP*incr);
    }

    fn handle_alt_tab_wheel (incr:i32) {
        // todo impl timer-spacing here
        // note that for these we DONT want to release Alt key .. presumably, expecting it to be physically released later
        if incr.is_positive() {
            Key::LShift.press(); Key::Tab.press(); Key::Tab.release(); Key::LShift.release();
        } else {
            Key::Tab.press(); Key::Tab.release();
        }
    }

    fn handle_horiz_scroll_wheel (incr:i32) {
    }

    fn setup_mouse_wheel_handling (&self) {
        let l2s = self.l2s.clone();
        MouseWheel::DefaultWheel.block_bind(true, move |ev| {
            ev .wheel_ev_data .iter() .for_each (|d| Krusty::handle_wheel_guarded(d.delta, &l2s) );
        });
        let l2s = self.l2s.clone();
        MouseWheel::DefaultWheel.block_bind(false, move |ev| {
            ev .wheel_ev_data .iter() .for_each (|d| Krusty::handle_wheel_guarded(d.delta, &l2s) );
        })
    }

}



impl Krusty { // special keys handling e.g. Space, Tab

    fn setup_space_key_handling (&self) {
        let l2s = self.l2s.clone();
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
                    Krusty::ensure_held_lalt_active(&l2s);
                    Key::Enter.press(); Key::Enter.release();
                } else {
                    // else if caps treat as ctrl-combo, should cover when shift is pressed too!
                    Krusty::ensure_not_held_alt_inactive(&l2s);
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

    fn setup_tab_key_handling (&self) {
        let l2s = self.l2s.clone();
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



impl Krusty {  // caret modes setups

    fn setup_caret_mode_key (&self, key: Key, is_mode_active:Arc<RwLock<bool>>, ovFn:AF) {
        // note that we only really just set/clear the particular caret-mode flag here, plus any non-caret realted overloads
        // .. the actual caret-mode functionality ofc has to be impld by every key that supports it by checking this flag
        let is_mode_active_c = is_mode_active.clone();
        let l2s = self.l2s.clone();
        key.block_bind(KeyDownCallback, move |_| {
           *is_mode_active_c.write().unwrap() = true;
            if *l2s.is_caps_down.read().unwrap() {
                ovFn(); // any overload functionality (since they cant be put in regular action maps like for other keys)
            } else  {
                key.press(); key.release()
            }
        });
        key.block_bind(KeyUpCallback, move |_| {
            *is_mode_active.write().unwrap() = false;
        });
    }

    fn clear_caret_mode_flags (&self) { self.l2s.clear_caret_mode_flags() }

}


impl Krusty { // action-maps related fns

    fn compose_action_maps_cb (&self, key:Key) -> CB {
        let (mba, mca, maa, mwa, mcwa, mcaa) = (
            // hashmap returns option of refs to Arcs in there, we'll need to clone each one inside the Option to use them outside
            self.am.borrow().base_act_map     .get(&key) .map(|e| e.clone()),
            self.am.borrow().caps_act_map     .get(&key) .map(|e| e.clone()),
            self.am.borrow().alt_act_map      .get(&key) .map(|e| e.clone()),
            self.am.borrow().win_act_map      .get(&key) .map(|e| e.clone()),
            self.am.borrow().caps_win_act_map .get(&key) .map(|e| e.clone()),
            self.am.borrow().caps_alt_act_map .get(&key) .map(|e| e.clone()),
        );
        // we cant move local options to closure either, just the Arcs inside, so do that part of composition before making closure
        // first, the base value cant be option, use normal action as default
        let ba = mba.unwrap_or(Krusty::normal_action(key.clone()));
        // next derive actual equivalent actions for the rest of options, defaulting to base if unspecified
        let (ca, aa, wa, cwa) = (mca.unwrap_or(ba.clone()), maa.unwrap_or(ba.clone()), mwa.unwrap_or(ba.clone()), mcwa.unwrap_or(ba.clone()) );
        // caa is special for l2 support, so if no explicit mcaa, we'll use ca composed above (instead of allowing alt-action via base)
        let caa = mcaa.unwrap_or(ca.clone());
        // now we can finally move these Arc action clones into the closure we need
        let l2s = self.l2s.clone();
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

    fn get_composition_maps_keys_union (&self) -> Rc<HashSet<Key>> {
        let mut keys = HashSet::new();
        self.am.borrow().base_act_map       .keys().for_each ( |k| { keys.insert(*k); } );
        self.am.borrow().caps_act_map       .keys().for_each ( |k| { keys.insert(*k); } );
        self.am.borrow().alt_act_map        .keys().for_each ( |k| { keys.insert(*k); } );
        self.am.borrow().win_act_map        .keys().for_each ( |k| { keys.insert(*k); } );
        self.am.borrow().caps_win_act_map   .keys().for_each ( |k| { keys.insert(*k); } );
        self.am.borrow().caps_alt_act_map   .keys().for_each ( |k| { keys.insert(*k); } );
        Rc::new(keys)
    }

    fn bind_all_from_action_maps (&self) {
        for key in self.get_composition_maps_keys_union().iter() {
            let cb = self.compose_action_maps_cb (key.clone());
            key.block_bind (KeyDownCallback, cb);
            key.block_bind (KeyUpCallback, |_| { });
            // ^^ note that key-up is disabled for EVERYTHING bound from action-maps! (shouldnt be a big deal at all though)
        }
    }



    fn add_base_mapping (&self, key:Key, action:AF) {
        self.am.borrow_mut() .base_act_map .insert (key, action);
    }
    fn add_caps_mapping (&self, key:Key, action:AF) {
        self.am.borrow_mut() .caps_act_map .insert (key, action);
    }
    fn add_caps_as_shift_mapping (&self, key:Key) {
        self.am.borrow_mut() .caps_act_map .insert (key, Krusty::shift_action(key));
    }
    fn add_caps_as_ctrl_mapping (&self, key:Key) {
        self.am.borrow_mut() .caps_act_map .insert (key, Krusty::ctrl_action(key));
    }

    fn add_alt_mapping (&self, key:Key, action:AF) {
        self.am.borrow_mut() .alt_act_map .insert (key, action);
    }
    fn add_caps_alt_mapping (&self, key:Key, action:AF) {
        self.am.borrow_mut() .caps_alt_act_map .insert (key, action);
    }
    fn add_caps_alt_mapping_w_alt_active (&self, key:Key, action:AF) {
        let l2s = self.l2s.clone();
        let action = Arc::new ( move || { Krusty::ensure_held_lalt_active(&l2s); action() } );
        self.am.borrow_mut() .caps_alt_act_map .insert (key, action);
    }
    fn add_caps_alt_mapping_w_alt_inactive (&self, key:Key, action:AF) {
        let l2s = self.l2s.clone();
        let action = Arc::new ( move || { Krusty::ensure_held_lalt_inactive(&l2s); action() } );
        self.am.borrow_mut() .caps_alt_act_map .insert (key, action);
    }

    fn add_win_mapping (&self, key:Key, action:AF) {
        let l2s = self.l2s.clone();
        let action = Arc::new (move || { Krusty::consume_down_lwin(&l2s); action(); });
        self.am.borrow_mut() .win_act_map .insert (key, action);
    }
    fn add_caps_win_mapping (&self, key:Key, action:AF) {
        let l2s = self.l2s.clone();
        let action = Arc::new (move || { Krusty::consume_down_lwin(&l2s); action(); });
        self.am.borrow_mut() .caps_win_act_map .insert (key, action);
    }

}



impl Krusty { // l2k-setup

    fn do_l2k_setups (&self) {
        /* setup options summary:
         - only j/k for left/right get alt-for-ctrl speedup (native word nav mode)
         - those and i/comma for up/down get dot-for-double-speed mode (2x nav) .. i/comma get that for alt too
         - h/l/u/m for home/end/pgup/pgdown get no speedup modes
         - in del mode, left/home/up/pgup get ExtBackspace, right/end/down/pgdn get ExtDelete
         - in del mode, left/right do direct bksp/del, but others get select then bksp/del
         */

        fn base_afg (l2k:Key) -> AF {
            Arc::new ( move || { l2k.press(); l2k.release(); } )
        }
        fn l2_ctrl_afg (l2k:Key) -> AF {
            Arc::new ( move || { Key::Ctrl.press(); base_afg(l2k)(); Key::Ctrl.release(); } )
        }
        fn l2_fast_afg (l2k:Key) -> AF {
            Arc::new ( move || { base_afg(l2k)(); base_afg(l2k)(); } )
        }
        type AFG = fn(Key) -> AF ;

        fn l2_nav_afg (l2s:L2S, l2k:Key, aafg:AFG, fafg:AFG) -> AF {
            // note: this ^^ cant take k:&Krusty because k.am would be not 'Send' while k.l2s is, and so only can be sent here from AFs
            let (base_af, alt_af, fast_af) = (base_afg(l2k), aafg(l2k), fafg(l2k));
            Arc::new ( move || {
                if *l2s.is_lalt_down.read().unwrap() { alt_af() }
                else if *l2s.in_caret_fast_mode.read().unwrap() { fast_af() }
                else { base_af() }
        } ) }

        fn l2_del_afg (l2s:L2S, del_key:Key, _:AF) -> AF {
            Arc::new ( move || {
                l2_nav_afg (l2s.clone(), del_key, l2_ctrl_afg, l2_fast_afg) ()
        } ) }
        fn l2_del_sel_afg (_:L2S, del_key:Key, nav_af:AF) -> AF {
            Arc::new ( move || {
                Key::Shift.press(); nav_af(); Key::Shift.release();
                base_afg(del_key)();
        } ) }
        type DAFG = fn(L2S, Key, AF) -> AF ;

        fn l2_sel_afg (nav_af:AF) -> AF {
            Arc::new ( move || {
                Key::Shift.press(); nav_af(); Key::LShift.release()
        } ) }

        fn l2_afg (l2s:L2S, nav_af:AF, sel_af:AF, del_af:AF) -> AF {
            Arc::new ( move || {
                Krusty::ensure_lalt_inactive(&l2s);
                // ^^ NOTE that for ALL l2, lAlt is suppressed other than for l2 behavior
                if *l2s.in_caret_sel_mode.read().unwrap() { sel_af() }
                else if *l2s.in_caret_del_mode.read().unwrap() { del_af() }
                else { nav_af() }
        } ) }

        fn setup_l2k (k:&Krusty, key:Key, l2k:Key, dk:Key, aafg:AFG, fafg:AFG, dafg:DAFG) {
            let nav_af = l2_nav_afg (k.l2s.clone(), l2k, aafg, fafg);
            let sel_af = l2_sel_afg (nav_af.clone());
            let del_af = dafg (k.l2s.clone(), dk, nav_af.clone());
            let l2_af  = l2_afg (k.l2s.clone(), nav_af, sel_af, del_af);
            // note that we'll set the composed callback action in caps map, but alt-caps will also call it (if undefined)
            k.am.borrow_mut().caps_act_map .insert (key, l2_af);
        }

        // now we can do actual assignments for l2 keys
        setup_l2k ( self, Key::J,     Key::ExtLeft,  Key::Backspace,   l2_ctrl_afg,  l2_fast_afg,  l2_del_afg );
        setup_l2k ( self, Key::K,     Key::ExtRight, Key::ExtDelete,   l2_ctrl_afg,  l2_fast_afg,  l2_del_afg );
        setup_l2k ( self, Key::I,     Key::ExtUp,    Key::Backspace,   l2_fast_afg,  l2_fast_afg,  l2_del_sel_afg );
        setup_l2k ( self, Key::Comma, Key::ExtDown,  Key::ExtDelete,   l2_fast_afg,  l2_fast_afg,  l2_del_sel_afg );
        setup_l2k ( self, Key::H,     Key::ExtHome,  Key::Backspace,   base_afg,     base_afg,     l2_del_sel_afg );
        setup_l2k ( self, Key::L,     Key::ExtEnd,   Key::ExtDelete,   base_afg,     base_afg,     l2_del_sel_afg );
        setup_l2k ( self, Key::U,     Key::ExtPgUp,  Key::Backspace,   base_afg,     base_afg,     l2_del_sel_afg );
        setup_l2k ( self, Key::M,     Key::ExtPgDn,  Key::ExtDelete,   base_afg,     base_afg,     l2_del_sel_afg );

    }

}



pub fn setup_krusty_board () {

    let k = Krusty {
        l2s : Arc::new(Layer2State::default()),
        am  : Rc::new(RefCell::new(ActionMaps::default())),
    };
    type K = Krusty;

    // setup capslock, we'll completely disable it other than for krusty use
    k.setup_caps_tracking();

    // setup left-alt, this will be monitored, but allowed to get out-of-sync between pressed state and external state
    k.setup_left_alt_tracking();

    // setup tracking for right-alt too .. for most cases (except e.g. Space etc), we completely map it to RShift
    k.setup_right_alt_tracking();

    // and for l-win .. this will track, but also modify win behavior to supress repeats, and if consumed, its key-ups
    k.setup_left_win_tracking();

    // and left-shift .. this is simple non-blocking tracking, only tracked to enable a few combos with caps
    k.setup_left_shift_tracking();

    // setup handling for space key .. overloads into enter with r-alt etc
    k.setup_space_key_handling();

    // tab has special management to support alt-tab, but also turning it instant press/release for all cases
    k.setup_tab_key_handling();


    // handling for mouse left btn, mostly to allow caps-as-ctrl behavior during drag drops and clicks
    k.setup_mouse_left_btn_handling();
    // also for mouse right btn, mostly to allow switche scrolling w right-btn-wheel combo
    k.setup_mouse_right_btn_handling();
    // also setup both Xbutton srcs to act as middle btns (used for link clicks, closing tabs etc)
    k.setup_mouse_x_btn_1_handling();
    k.setup_mouse_x_btn_2_handling();

    // setup handling for mouse wheel .. complex overloading over alt-tab, switche, volume, brigheness etc !!
    k.setup_mouse_wheel_handling();



    // setup keys for layer-2 caret nav sel/del/fast modes
    // .. note: before setting up caret mode, we'll need to define any non-caret overloads (during caps-caret) for them
    // .. and we only need that for 'period' for caps-win-period as window-vert-stretch
    k.setup_caret_mode_key (Key::R, k.l2s.in_caret_sel_mode.clone(), K::no_action());
    k.setup_caret_mode_key (Key::D, k.l2s.in_caret_del_mode.clone(), K::no_action());
    let l2s = k.l2s.clone();
    let ovFn = Arc::new ( move || {
        if *l2s.is_lwin_down.read().unwrap() { win_fgnd_stretch(0,30); }
    } );
    k.setup_caret_mode_key (Key::Period, k.l2s.in_caret_fast_mode.clone(), ovFn);




    // setting up the all other keys via by-key-combo action maps that we'll compose at the end to relevant callbacks
    use utils::window_utils::*;
    use utils::process_utils::*;

    // win-m by default minimized all windows .. we just want to disable it
    k.add_win_mapping (Key::M, Arc::new (|| {} ));

    // win-i should start irfanview
    k.add_win_mapping (Key::I, Arc::new (|| { start_irfanview(); } ));

    // win-n should start chrome-incognito
    k.add_win_mapping (Key::N, Arc::new (|| { start_chrome_incognito(); } ));

    // in cur laptop, Fn-F6/F7 do brightness, but at +10 incrs .. set them to do small incrs with win combos
    k.add_win_mapping (Key::F6, Arc::new (|| { K::handle_brightness_wheel(-1) } ));
    k.add_win_mapping (Key::F7, Arc::new (|| { K::handle_brightness_wheel(1) } ));

    // might as well do that for alt as well, since we use alt for most other such shortcuts
    k.add_alt_mapping (Key::F6, Arc::new ( || { K::handle_brightness_wheel(-1); } ) );
    k.add_alt_mapping (Key::F7, Arc::new ( || { K::handle_brightness_wheel(1); } ) );


    // actually, since we use alt-2/3 as vol down/up, might as well also set win-2/3 for brightness down/up
    k.add_win_mapping (Key::Numrow_2, Arc::new (|| { K::handle_brightness_wheel(-1) } ));
    k.add_win_mapping (Key::Numrow_3, Arc::new (|| { K::handle_brightness_wheel(1) } ));


    // alt-f1 play/pause, caps-f1 toggle mute, base-case, overload F1 for switche caller (F21)
    let l2s = k.l2s.clone();
    k.add_base_mapping (Key::F1, K::normal_action(Key::F21));
    k.add_caps_mapping (Key::F1, K::normal_action(Key::VolumeMute));
    k.add_alt_mapping  (Key::F1, Arc::new ( move || {
        // media keys only work while alt is inactive, so gotta set/restore that etc
        K::ensure_held_lalt_inactive(&l2s);
        Key::MediaPlayPause.press(); Key::MediaPlayPause.release();
    } ) );

    fn do_media_skips (n_skips:u32, l2s:&L2S) {
        // sending ^!{Right} which we traditionally set on winamp to skip forward a bit
        //ensure_held_lalt_inactive(l2s);
        // note that we'll send alts explicitly so it works even if alt was quickly released
        K::ensure_alt_active(l2s);
        Key::LControl.press();
        (0 ..n_skips) .into_iter().for_each(|_| { Key::ExtRight.press(); Key::ExtRight.release() });
        Key::LControl.release();
        K::ensure_not_held_alt_inactive(l2s);
    };

    // al-f2 for next with some initial skip
    let l2s = k.l2s.clone();
    k.add_alt_mapping (Key::F2, Arc::new ( move || {
        K::ensure_held_lalt_inactive(&l2s);
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
    k.add_alt_mapping (Key::F3, Arc::new ( move || { do_media_skips(1,&l2s) } ) );
    // alt-2 is vol down, alt-3 is vol up
    k.add_alt_mapping (Key::Numrow_2, K::normal_action(Key::VolumeDown));
    k.add_alt_mapping (Key::Numrow_3, K::normal_action(Key::VolumeUp));

    // trying out alt-1 for switche next-win (via its F20/Alt-F20 hotkey)
    k.add_alt_mapping (Key::Numrow_1, K::normal_action(Key::F20));



    // caps-e is Enter
    k.add_caps_mapping (Key::E, K::normal_action(Key::Enter));

    // escape is just escape, but we just want it to do press-release immediately (so switche is faster)
    k.add_base_mapping (Key::Escape, K::normal_action(Key::Escape));

    // use the apps key to send shift-escape
    k.add_base_mapping (Key::Apps, K::shift_action(Key::Escape));

    // for back-tick, make normal case be Delete, caps do back-tick, and shifts do its ~, alt will do quick switch via F20/Alt-F20
    // note that this means the shift-delete action which usually maps to 'cut' action wont be available on this key
    let l2s = k.l2s.clone();
    k.add_base_mapping (Key::Backquote, Arc::new ( move || {
        K::press_release (
            if *l2s.is_lshift_down.read().unwrap() || *l2s.is_ralt_down.read().unwrap() { Key::Backquote }
            else { Key::ExtDelete }
        );
    }));
    k.add_caps_mapping (Key::Backquote, K::normal_action(Key::Backquote));
    k.add_alt_mapping  (Key::Backquote, K::normal_action(Key::F20));



    // number of caps-as-shift mappings
    k.add_caps_as_shift_mapping (Key::Numrow_6);
    k.add_caps_as_shift_mapping (Key::Numrow_7);
    k.add_caps_as_shift_mapping (Key::Numrow_8);
    k.add_caps_as_shift_mapping (Key::Numrow_9);
    k.add_caps_as_shift_mapping (Key::Numrow_0);
    k.add_caps_as_shift_mapping (Key::Minus);
    k.add_caps_as_shift_mapping (Key::Equal);
    k.add_caps_as_shift_mapping (Key::LBracket);
    k.add_caps_as_shift_mapping (Key::RBracket);
    k.add_caps_as_shift_mapping (Key::Semicolon);
    k.add_caps_as_shift_mapping (Key::Quote);
    k.add_caps_as_shift_mapping (Key::Slash);
    k.add_caps_as_shift_mapping (Key::Backslash);
    //add_caps_shift_mapping.clone() (Key::Period); // can't do as used as 'fast' accelerator in caret mode!


    // number of caps-as-ctrl mappings
    k.add_caps_as_ctrl_mapping (Key::A);
    k.add_caps_as_ctrl_mapping (Key::S);
    k.add_caps_as_ctrl_mapping (Key::F);  // further overloading below for caps-alt-f for full-screen
    k.add_caps_as_ctrl_mapping (Key::W);  // further overloading below for caps-alt-w to close windows
    k.add_caps_as_ctrl_mapping (Key::T);
    k.add_caps_as_ctrl_mapping (Key::Y);
    k.add_caps_as_ctrl_mapping (Key::Z);
    k.add_caps_as_ctrl_mapping (Key::X);
    k.add_caps_as_ctrl_mapping (Key::C);
    k.add_caps_as_ctrl_mapping (Key::V);
    k.add_caps_as_ctrl_mapping (Key::N);
    k.add_caps_as_ctrl_mapping (Key::O);
    k.add_caps_as_ctrl_mapping (Key::P);



    // caps-alt-F for full screen, but also the expected caps-f for ctrl-f
    k.add_caps_alt_mapping_w_alt_inactive (Key::F, Arc::new ( move || { K::press_release(Key::F11) } ) );

    // 'w' should have caps-ctrl mapping, but when w/ alt, send alt-f4 (to close all-tabs, windows etc)
    // note: intiially we wanted this with caps-shift-w, but turns out (at least on my kbd, turns out caps+shift+[F1, 2, w, s, x]
    //      dont produce any key event at the hook at all .. nothing .. its like the keyboard driver not sending those out
    //  .. funnily enough, there's a bunch of complaints about specifically those keys for dell/hp laptops .. looks like hardware
    //  .. appears to be a common kbd pcb layout issue .. heres from 2007: (https://www.joachim-breitner.de/blog/250-Shift-Caps-2)
    // ... sooo .. to makeup, we'll do alt-caps-w do the alt-f4 business instead
    k.add_caps_alt_mapping_w_alt_active (Key::W, Arc::new ( move || { K::press_release(Key::F4) } ) );

    // caps-alt-F10 just to pass through as ctrl-alt-F10 for intelliJ
    k.add_caps_alt_mapping_w_alt_active (Key::F10, Arc::new ( move || { K::ctrl_press_release(Key::F10) } ) );



    // filling out l2 actions (incl w caps-alt combos)
    k.do_l2k_setups();



    // then the caps-win combo l3 actions :

    // caps-win-U should vert-max (via shift-win-up) if not already, or else restore window from vert-max
    k.add_caps_win_mapping (Key::U, Arc::new (|| win_fgnd_toggle_vertmax()));
    // caps-win-m should maximize (via win-m) if not, else restore from max
    k.add_caps_win_mapping (Key::M, Arc::new (|| win_fgnd_toggle_max()));

    // caps-win-[j,k,i,,] should  move window [left, right, top, bottom] respectively
    k.add_caps_win_mapping (Key::J,     Arc::new (|| win_fgnd_move(-80, 0) ));
    k.add_caps_win_mapping (Key::K,     Arc::new (|| win_fgnd_move(80, 0) ));
    k.add_caps_win_mapping (Key::I,     Arc::new (|| win_fgnd_move(0, -50) ));
    k.add_caps_win_mapping (Key::Comma, Arc::new (|| win_fgnd_move(0, 50) ));

    // caps-win-[h,;,.,o] should stretch window [narroer, wider, shorter, taller] respectively
    // .. note that caps-win-L gets unavoidably captured by windows to lock laptop, hence the semicolon
    k.add_caps_win_mapping (Key::H,         Arc::new (|| win_fgnd_stretch(-30, 0) ));
    k.add_caps_win_mapping (Key::O,         Arc::new (|| win_fgnd_stretch(0, -30) ));
    //k.add_caps_win_mapping (Key::Period,  Arc::new (|| win_fgnd_stretch(0, 30) ));
    // ^^ cant to 'Period' here, as thats caret key w/ key-up handling .. gotta add any overloads right there
    //k.add_caps_win_mapping (Key::L,         Arc::new (|| win_fgnd_stretch(30, 0) ));
    // ^^ any win-L combo is hardcoded at OS level to lock machine, cant override that, so we'll make semicolon do that instead
    k.add_caps_win_mapping (Key::Semicolon, Arc::new (|| win_fgnd_stretch(30, 0) ));


    // some additional caps-win combos
    // caps-win-c being used to launch winmerge diff from last two clipboard entries
    k.add_caps_win_mapping (Key::C, Arc::new (|| start_winmerge_clipboard() ));
    // gaah we'll just throw in iDEA diff for drag-drop diffing (just coz winmerge doesnt do dark mode)
    //k.add_win_mapping (Key::C, Arc::new (|| start_idea_diff() ));
    // ^^ cant do from here, turns out idea diff from cmd line can ONLY be opened with two files pointed, unlike empty from Idea shortcut!



    // finally bind everytihng from action-maps !!
    k.bind_all_from_action_maps();


}

