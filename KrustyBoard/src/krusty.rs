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

    in_managed_ctrl_down_state: RwLock<bool>,
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


fn setup_global_disable (l2s:Arc<Layer2State>) {
    // todo: wont be straight-forward if we want to handle this at hook receipt
    // .. will need a global check there, as well as allowing selectively for the re-enable combo!
}


fn setup_caps_tracking (l2s:Arc<Layer2State>) {
    // note that for caps, we completely block it from ever being sent up, and just manage internally!
    if KbdKey::CapsLock.is_toggled() {
        KbdKey::CapsLock.press();  // toggle off first if necessary (to clear key light)
    }
    let l2sc = l2s.clone();
    KbdKey::CapsLock.block_bind(KeyDownCallback, move |_| {
        if !*l2sc.is_caps_down.read().unwrap() {
            *l2sc.is_caps_down.write().unwrap() = true;
            *l2sc.is_wheel_spin_invalidated.write().unwrap() = true;
        }
        if *l2sc.is_mouse_left_btn_down.read().unwrap() && !*l2sc.in_managed_ctrl_down_state.read().unwrap() {
            *l2sc.in_managed_ctrl_down_state.write().unwrap() = true;
            KbdKey::LControl.press();
        }
    });

    // dont need a clone here, just consume the passed in copy
    KbdKey::CapsLock.block_bind(KeyUpCallback, move |_| {
       *l2s.is_caps_down.write().unwrap() = false;
        if *l2s.in_managed_ctrl_down_state.read().unwrap() {
            *l2s.in_managed_ctrl_down_state.write().unwrap() = false;
            KbdKey::LControl.release();
        }
        // tracking caps release stamp allows for a grace between caps release and alt release from combos
        *l2s.last_caps_up_stamp.write().unwrap() = Instant::now();
        // the following isnt strictly necessary, but useful in case some keyup falls through
        l2s.clear_caret_mode_flags();
        // also, now might be a good time to try and get any out-of-sync l-alt states back in line if necessary
        //try_sync_lalt(l2s.clone());
        // ^^ this is incompatible w caps grace ..
        //    .. infact its worse than just no grace because it immediately triggers instead of natural grace waiting for alt's key repeat
        //    .. so instead we'll just do a 'sync' if we needed an alt release
        ensure_not_held_alt_inactive(l2s.clone());
    });
}

fn setup_left_alt_tracking (l2s:Arc<Layer2State>) {
    /// for left-alt, we'll let it go through, but instead of letting it spam on repeat on long presses, we just preserve the logical
    ///     state but supress the spams .. that helps us reason safely about its logical state in all the other combos, esp non-caps ones,
    ///     .. plus, it makes alt-combos for keys not handled here continue to work as expected
    /// ugh, however, letting alt out unblocked, does cause issues at times (eg. when it moves focus to menu items .. so we'll track both its
    ///     internal, as well as external state for when we've had to suppress it while caps was down etc
    let l2sc = l2s.clone();
    KbdKey::LAlt.blockable_bind(KeyDownCallback, move |_| {
        // so goal here is, any presses with caps active, we supress alt going outside
        // and since we can have caps come in AFTER alt is already down, we'll have to capture disparity states, as well as to deal
        // with restoring that when either caps/alt gets released etc
        // so, if caps is down, suppress alt, capture state, let specific combos deal with the disparity
        // plus, even when caps isnt down, suppress the repeated events
        // plus, we allow a small grace period after caps is released during which we dont register prior pressed alt (allows sloppy combo release)
        if *l2sc.is_caps_down.read().unwrap() {
            // caps is down, record alt being down if not already, but either way block it (so no change to alt-active state)
            if !*l2sc.is_lalt_down.read().unwrap() {
                *l2sc.is_lalt_down.write().unwrap() = true;
            }
            BlockInput::Block
        } else {
            if *l2sc.is_lalt_down.read().unwrap() {
                // caps isnt down, but alt was, so its repeat, so if in-post-caps-up-grace or not-out-of-sync then should still block it
                if *l2sc.is_alt_active.read().unwrap() || l2sc.caps_up_grace_active() {
                    BlockInput::Block
                } else {
                    *l2sc.is_alt_active.write().unwrap() = true;
                    BlockInput::DontBlock
                }
            } else {
                // caps isnt down, and alt wasnt down, so record states and let it through
                *l2sc.is_lalt_down.write().unwrap() = true;
                *l2sc.is_alt_active.write().unwrap() = true;
                *l2sc.is_wheel_spin_invalidated.write().unwrap() = true;
                BlockInput::DontBlock
            }
        }
    });
    KbdKey::LAlt.blockable_bind(KeyUpCallback, move |_| {
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


fn ensure_held_lalt_active(l2s:Arc<Layer2State>) {
    // utility to set held l-alt active (sync it) while tracking state (useful for media shortcuts etc)
    if *l2s.is_lalt_down.read().unwrap() && !*l2s.is_alt_active.read().unwrap() {
        KbdKey::LAlt.press();
        *l2s.is_alt_active.write().unwrap() = true;
    }
}
fn ensure_alt_active(l2s:Arc<Layer2State>) {
    // utility to get alt out reliably whether its currently pressed or not, while keeping state tracking updated
    if !*l2s.is_alt_active.read().unwrap() {
        KbdKey::LAlt.press();
        *l2s.is_alt_active.write().unwrap() = true;
    }
}
fn ensure_not_held_alt_inactive(l2s:Arc<Layer2State>) {
    // utility to set not-held alt inactive (sync it) while tracking (useful for alt-combos that prdouce non-alt outputs)
    if !*l2s.is_lalt_down.read().unwrap() && *l2s.is_alt_active.read().unwrap() {
        KbdKey::LAlt.release();
        *l2s.is_alt_active.write().unwrap() = false;
    }
}
fn ensure_held_lalt_inactive(l2s:Arc<Layer2State>) {
    // utility to hard-clear alt state for alt-combos (e.g. for media shortcuts etc)
    if *l2s.is_lalt_down.read().unwrap() && *l2s.is_alt_active.read().unwrap() {
        KbdKey::LAlt.release();
        *l2s.is_alt_active.write().unwrap() = false;
    }
}
fn ensure_lalt_inactive(l2s:Arc<Layer2State>) {
    // utility to ensure lalt is inactive regardless if held down
    // (expectation is l-2 keys should ensure that, and l-alt handler should never activate it when l2/caps is active either)
    if *l2s.is_alt_active.read().unwrap() {
        *l2s.is_alt_active.write().unwrap() = false;
        KbdKey::LAlt.release();
    }
}

fn try_sync_lalt_if_no_caps (l2s:Arc<Layer2State>) {
    // well .. this fn .. either way, it can only fix when no-caps, so users will ahve to check first, and act differently ..
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
fn try_sync_lalt (l2s:Arc<Layer2State>) {
    if *l2s.is_lalt_down.read().unwrap() {
        if !*l2s.is_alt_active.read().unwrap() {
            // alt key down, but not active .. so send a press
            KbdKey::LAlt.press();
            *l2s.is_alt_active.write().unwrap() = true;
        }
    } else {
        if *l2s.is_alt_active.read().unwrap() {
            // alt key up but still active .. so send a release
            KbdKey::LAlt.release();
            *l2s.is_alt_active.write().unwrap() = false;
        }
    }
}


fn setup_right_alt_tracking (l2s:Arc<Layer2State>) {
    /// for most cases (except e.g. Space etc), we map it to RShift
    let l2sc = l2s.clone();
    KbdKey::RAlt.block_bind(KeyDownCallback, move |_| {
        *l2sc.is_ralt_down.write().unwrap() = true;
        KbdKey::RShift.press();
    });
    KbdKey::RAlt.block_bind(KeyUpCallback, move |_| {
        *l2s.is_ralt_down.write().unwrap() = false;
        KbdKey::RShift.release();
    });
}

fn setup_left_win_tracking (l2s:Arc<Layer2State>) {
    /// other than tracking state, we also try to avoid effect of its key-up afterwards if we acted on it ourselves while down
    /// also, to make that work, we'll suppress all repeats too, and ofc wont reset was-consumed flag for repeats either
    /// however, ofc cant just block lwin-up, so instead will have to mask it w a ctrl down/up as ctrl-win doesnt trigger anything
    let l2sc = l2s.clone();
    KbdKey::LWin.blockable_bind(KeyDownCallback, move |_| {
        if !*l2sc.is_lwin_down.read().unwrap() { // new physical press
            *l2sc.is_lwin_down.write().unwrap() = true;
            if *l2sc.is_down_lwin_consumed.read().unwrap() {
                *l2sc.is_down_lwin_consumed.write().unwrap() = false;
            }
            BlockInput::DontBlock
        } else { // its a key repeat
            BlockInput::Block
        }
    });
    KbdKey::LWin.blockable_bind(KeyUpCallback, move |_| {
        *l2s.is_lwin_down.write().unwrap() = false;
        if *l2s.is_down_lwin_consumed.read().unwrap() {
            thread::spawn ( || {
                KbdKey::Ctrl.press(); KbdKey::Ctrl.release();
                KbdKey::LWin.release(); // doesnt actually have to be interspersed between ctrl press/release
            } );
            BlockInput::Block
        } else { BlockInput::DontBlock }
    });
}
fn consume_down_lwin(l2s:Arc<Layer2State>) {
    // utility for any key with lwin action to consume down state if they want to (to suppress its keyup later)
    if *l2s.is_lwin_down.read().unwrap() && !*l2s.is_down_lwin_consumed.read().unwrap() {
        *l2s.is_down_lwin_consumed.write().unwrap() = true;
    }
}

fn setup_left_shift_tracking(l2s:Arc<Layer2State>) {
    // for left-shift, we really only care for a few combos with caps .. could query it as necessary too, but might as well
    let l2sc = l2s.clone();
    KbdKey::LShift.non_blocking_bind(KeyDownCallback, move |_| {
        if !*l2sc.is_lshift_down.read().unwrap() { *l2sc.is_lshift_down.write().unwrap() = true; }
    });
    KbdKey::LShift.non_blocking_bind(KeyUpCallback, move |_| {
        *l2s.is_lshift_down.write().unwrap() = false;
    });
}


fn setup_mouse_left_btn_handling (l2s:Arc<Layer2State>) {
    let l2sc = l2s.clone();
    MouseButton::LeftButton.block_bind(true, move |_| {
        *l2sc.is_mouse_left_btn_down.write().unwrap() = true;
        if *l2sc.is_caps_down.read().unwrap() {
            *l2sc.in_managed_ctrl_down_state.write().unwrap() = true;
            KbdKey::LControl.press();
        }
        MouseButton::LeftButton.press()
    });
    MouseButton::LeftButton.block_bind(false, move |_| {
        *l2s.is_mouse_left_btn_down.write().unwrap() = false;
        thread::sleep(time::Duration::from_millis(10));
        MouseButton::LeftButton.release();
    });
    // ^^ in theory could do non-blocking bind for release, but that can occasionally expose us to timing issues as press is on block
    //      and that will be slower to send the actual press from the spawned thread (so making release slower too helps)
}

fn setup_mouse_right_btn_handling (l2s:Arc<Layer2State>) {
    let l2sc = l2s.clone();
    MouseButton::RightButton.non_blocking_bind(true, move |_| {
        *l2sc.is_mouse_right_btn_down.write().unwrap() = true;
    });
    MouseButton::RightButton.non_blocking_bind(false, move |_| {
        *l2s.is_mouse_right_btn_down.write().unwrap() = false;
        if *l2s.in_right_btn_scroll_state.read().unwrap() {
            *l2s.in_right_btn_scroll_state.write().unwrap() = false;
            KbdKey::F23.press(); KbdKey::F23.release();
        }
    });
}

fn setup_mouse_x_btn_1_handling () {
    // turns out just doing press/release on initial press works snappier/more-reliable than trying to be true to btn-holds
    MouseButton::X1Button.block_bind(true, move |_| {
        MouseButton::MiddleButton.press(); MouseButton::MiddleButton.release();
    });
    MouseButton::X1Button.block_bind(false, move |_| { });
}

fn setup_mouse_x_btn_2_handling () {
    MouseButton::X2Button.block_bind(true, move |_| {
        MouseButton::MiddleButton.press(); MouseButton::MiddleButton.release();
    });
    MouseButton::X2Button.block_bind(false, move |_| { });
}




fn handle_wheel_guarded (delta:i32, l2s:Arc<Layer2State>) {
    let last_stamp = *l2s.last_wheel_stamp.read().unwrap();
    *l2s.last_wheel_stamp.write().unwrap() = Instant::now();
    //let gap = l2s.last_wheel_stamp.read().unwrap().duration_since(last_stamp);
    //println!("{:#?}", dur.as_millis());
    const GUARD_DUR_MS: u128 = 120;  // from dur printouts above, looked like max inertial gap is 120 (min 7ms, usually <100)
    if !*l2s.is_wheel_spin_invalidated.read().unwrap() {
        handle_wheel_action(delta, l2s);
    } else if GUARD_DUR_MS < l2s.last_wheel_stamp.read().unwrap().duration_since(last_stamp).as_millis() {
        *l2s.is_wheel_spin_invalidated.write().unwrap() = false;
        handle_wheel_action(delta, l2s);
    } else {
        // if its invalidated AND wheel-spin spacing is below guard-dur, we supress the wheel event
    }
}

fn handle_wheel_action (delta:i32, l2s:Arc<Layer2State>) {
    let incr = delta / WHEEL_DELTA as i32;
    if *l2s.is_mouse_right_btn_down.read().unwrap() {
        // support for switche task switching
        if !*l2s.in_right_btn_scroll_state.read().unwrap() { *l2s.in_right_btn_scroll_state.write().unwrap() = true; }
        let key = if incr.is_positive() { KbdKey::F22 } else { KbdKey::F21 };
        key.press(); key.release();
    } else  if *l2s.is_lalt_down.read().unwrap() {
        if *l2s.is_caps_down.read().unwrap() || get_fgnd_win_class() == "MultitaskingViewFrame" { // alt-tab states
            // ensure alt is externally active first
            if !*l2s.is_alt_active.read().unwrap() {
                *l2s.is_alt_active.write().unwrap() = true;
                KbdKey::LAlt.press();
            }
            handle_alt_tab_wheel(incr)
        } else {
            handle_volume_wheel(incr);
        }
    } else if *l2s.is_lwin_down.read().unwrap() {
        *l2s.is_down_lwin_consumed.write().unwrap() = true;
        handle_brightness_wheel(incr);
    } else if *l2s.is_caps_down.read().unwrap() {
        *l2s.in_managed_ctrl_down_state.write().unwrap() = true;
        KbdKey::LControl.press();
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
    let key = if incr > 0 { KbdKey::VolumeUp } else { KbdKey::VolumeDown };
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
        KbdKey::LShift.press(); KbdKey::Tab.press(); KbdKey::Tab.release(); KbdKey::LShift.release();
    } else {
        KbdKey::Tab.press(); KbdKey::Tab.release();
    }
}

fn handle_horiz_scroll_wheel (incr:i32) {
}

fn setup_mouse_wheel_handling (l2s:Arc<Layer2State>) {
    let l2sc = l2s.clone();
    MouseWheel::DefaultWheel.block_bind(true, move |ev| {
        ev .wheel_ev_data .iter() .for_each (|d| handle_wheel_guarded(d.delta, l2sc.clone()) );
    });
    MouseWheel::DefaultWheel.block_bind(false, move |ev| {
        ev .wheel_ev_data .iter() .for_each (|d| handle_wheel_guarded(d.delta, l2s.clone()) );
    })
}




fn setup_space_key_handling (l2s:Arc<Layer2State>) {
    KbdKey::Space.block_bind(KeyDownCallback, move |_| {
        if *l2s.is_ralt_down.read().unwrap() {
            // generic remapping of RAlt_Space to Enter
            // note that ralt is mapped separately to shift .. so that will be 'down' when we're here
            KbdKey::RShift.release();
            KbdKey::Enter.press(); KbdKey::Enter.release();
            KbdKey::RShift.press();
        } else if *l2s.is_caps_down.read().unwrap() {
            if *l2s.is_lalt_down.read().unwrap() {
                // remapping Caps_LAlt_Space to Alt_Enter, mostly for IntelliJ
                ensure_held_lalt_active(l2s.clone());
                KbdKey::Enter.press(); KbdKey::Enter.release();
            } else {
                // else if caps treat as ctrl-combo, should cover when shift is pressed too!
                ensure_not_held_alt_inactive(l2s.clone());
                KbdKey::LControl.press();
                KbdKey::Space.press(); KbdKey::Space.release();
                KbdKey::LControl.release();
            }
        } else {
            // nominal fallback .. dont try to check/fix alt states, as we might even have non-tracked alts active!
            KbdKey::Space.press(); KbdKey::Space.release();
        }
    });
    KbdKey::Space.block_bind(KeyUpCallback, |_| {}); // just block it
}

fn setup_tab_key_handling (l2s:Arc<Layer2State>) {
    KbdKey::Tab.block_bind(KeyDownCallback, move |_| {
        if *l2s.is_caps_down.read().unwrap() {
            //if !*l2s.in_managed_ctrl_down_state.read().unwrap() {
                // ^^ disabling this check, makes it more robust w/ no real cost
                *l2s.in_managed_ctrl_down_state.write().unwrap() = true;
                KbdKey::LControl.press();
            //}
        }
        KbdKey::Tab.press(); KbdKey::Tab.release();
        // note that this ^^ will make it instant press/release for tab, even w/o caps held (and that's good)
    });
    KbdKey::Tab.block_bind(KeyUpCallback, move |_| {}); // just gotta block it
}


fn setup_caret_mode_key (key: KbdKey, is_mode_active:Arc<RwLock<bool>>, l2s: Arc<Layer2State>, ovFn:AF) {
    // note that we only really just set/clear the particular caret-mode flag here, plus any non-caret realted overloads
    // .. the actual caret-mode functionality ofc has to be impld by every key that supports it by checking this flag
    let is_mode_active_c = is_mode_active.clone();
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




type CB = Box <dyn Fn(KbdEvent) + Send + Sync + 'static>;
type AF = Arc <dyn Fn() + Send + Sync + 'static>;

fn press_release (key:KbdKey) { key.press(); key.release(); }
fn shift_press_release (key:KbdKey) {
    KbdKey::Shift.press(); key.press(); key.release(); KbdKey::Shift.release();
}
fn ctrl_press_release (key:KbdKey) {
    KbdKey::Ctrl.press(); key.press(); key.release(); KbdKey::Ctrl.release();
}

fn no_action () -> AF { Arc::new ( || {} ) }
fn normal_action (key:KbdKey) -> AF { Arc::new ( move || { press_release(key) } ) }
fn ctrl_action   (key:KbdKey) -> AF { Arc::new ( move || { ctrl_press_release(key) } ) }
fn shift_action  (key:KbdKey) -> AF { Arc::new ( move || { shift_press_release(key) } ) }




// trying out building maps to hold l2, win, l3 etc by group instead of spread around by key
// the caps and caps-alt mapping comprise the l2 functionality, the win and caps-win comprise l3

#[derive(Default)]
struct ActionMaps {
    pub base_act_map     : HashMap<KbdKey,AF>,
    pub caps_act_map     : HashMap<KbdKey,AF>,
    pub alt_act_map      : HashMap<KbdKey,AF>,
    pub win_act_map      : HashMap<KbdKey,AF>,
    pub caps_win_act_map : HashMap<KbdKey,AF>,
    pub caps_alt_act_map : HashMap<KbdKey,AF>,

    // ^^ should try not to have caps-alt separate as alt has possible out-of-sync down vs active states, esp w caps combo
    // .. for now, prob better to just keep anything with alt combined with caps and check there (maybe we'll never need anytihng else)
}

fn compose_action_maps_cb (key:KbdKey, l2s:Arc<Layer2State>, am: Rc<RefCell<ActionMaps>>) -> CB {
    let (mba, mca, maa, mwa, mcwa, mcaa) = (
        // hashmap returns option of refs to Arcs in there, we'll need to clone each one inside the Option to use them outside
        am.borrow().base_act_map     .get(&key) .map(|e| e.clone()),
        am.borrow().caps_act_map     .get(&key) .map(|e| e.clone()),
        am.borrow().alt_act_map      .get(&key) .map(|e| e.clone()),
        am.borrow().win_act_map      .get(&key) .map(|e| e.clone()),
        am.borrow().caps_win_act_map .get(&key) .map(|e| e.clone()),
        am.borrow().caps_alt_act_map .get(&key) .map(|e| e.clone()),
    );
    // we cant move local options to closure either, just the Arcs inside, so do that part of composition before making closure
    // first, the base value cant be option, use normal action as default
    let ba = mba.unwrap_or(normal_action(key.clone()));
    // next derive actual equivalent actions for the rest of options, defaulting to base if unspecified
    let (ca, aa, wa, cwa) = (mca.unwrap_or(ba.clone()), maa.unwrap_or(ba.clone()), mwa.unwrap_or(ba.clone()), mcwa.unwrap_or(ba.clone()) );
    // caa is special for l2 support, so if no explicit mcaa, we'll use ca composed above (instead of allowing alt-action via base)
    let caa = mcaa.unwrap_or(ca.clone());
    // now we can finally move these Arc action clones into the closure we need
    Box::new ( move |_| {
        if *l2s.is_caps_down.read().unwrap() {
            if *l2s.is_lwin_down.read().unwrap() { cwa() }
            else if *l2s.is_lalt_down.read().unwrap() { caa() }
            else { ca() }
        } else if *l2s.is_lwin_down.read().unwrap() { wa() }
        else if *l2s.is_lalt_down.read().unwrap() { aa() }
        else { ba() }
    } )
}

fn get_composition_maps_keys_union (am: Rc<RefCell<ActionMaps>>) -> Rc<HashSet<KbdKey>> {
    let mut keys = HashSet::new();
    am.borrow().base_act_map       .keys().for_each ( |k| { keys.insert(*k); } );
    am.borrow().caps_act_map       .keys().for_each ( |k| { keys.insert(*k); } );
    am.borrow().alt_act_map        .keys().for_each ( |k| { keys.insert(*k); } );
    am.borrow().win_act_map        .keys().for_each ( |k| { keys.insert(*k); } );
    am.borrow().caps_win_act_map   .keys().for_each ( |k| { keys.insert(*k); } );
    am.borrow().caps_alt_act_map   .keys().for_each ( |k| { keys.insert(*k); } );
    Rc::new(keys)
}

fn bind_all_from_action_maps (am: Rc<RefCell<ActionMaps>>, l2s:Arc<Layer2State>) {
    for key in get_composition_maps_keys_union(am.clone()).iter() {
        let cb = compose_action_maps_cb (key.clone(), l2s.clone(), am.clone());
        key.block_bind (KeyDownCallback, cb);
        key.block_bind (KeyUpCallback, |_| { });
        // ^^ note that key-up is disabled for EVERYTHING bound from action-maps! (shouldnt be a big deal at all though)
    }
}





fn do_l2k_setups_old (am: Rc<RefCell<ActionMaps>>, l2s:Arc<Layer2State>) {

    fn handle_l2_key_req (l2key: KbdKey, l2s:&Layer2State, allow_fast:bool, allow_alt:bool) {
        if allow_alt && *l2s.is_lalt_down.read().unwrap() {
            // basically this is to make it go from char-nav to word-nav etc, in gen does ctrl-down when alt-down
            KbdKey::Ctrl.press(); l2key.press(); l2key.release(); KbdKey::Ctrl.release();
        } else if allow_fast && ( *l2s.in_caret_fast_mode.read().unwrap() || ( !allow_alt && *l2s.is_lalt_down.read().unwrap() ) ) {
            // in addn to fast-mode key held, we also do fast for alt-held if it was allow_fast but not allow_alt
            l2key.press(); l2key.release(); l2key.press(); l2key.release();
        } else {
            l2key.press(); l2key.release();
        }
    }

    fn l2_action (l2key:KbdKey, l2s:Arc<Layer2State>, allow_fast:bool, allow_alt:bool) -> AF { Arc::new ( move || {
        ensure_lalt_inactive(l2s.clone());
        if *l2s.in_caret_sel_mode.read().unwrap() {
            // simulating holding down shift during nav to trigger char/word selection
            KbdKey::Shift.press();
            handle_l2_key_req (l2key, &l2s, allow_fast, allow_alt);
            KbdKey::Shift.release();
        }
        else if *l2s.in_caret_del_mode.read().unwrap() {
            // for deletion, instead of sending del/bksp we could just do shift-sel then del to treat fwd/back identically
            // ^^ buttt that makes ctrl-z/y a little messy, so we'll just do some special treatment
            if l2key == KbdKey::ExtRight {
                handle_l2_key_req (KbdKey::ExtDelete, &l2s, allow_fast, allow_alt);
            } else if l2key == KbdKey::ExtLeft {
                handle_l2_key_req (KbdKey::Backspace, &l2s, allow_fast, allow_alt);
            } else {
                KbdKey::Shift.press();
                handle_l2_key_req (l2key, &l2s, allow_fast, allow_alt);
                KbdKey::Shift.release();
                if l2key == KbdKey::ExtEnd || l2key == KbdKey::ExtDown || l2key == KbdKey::ExtPgDn {
                    KbdKey::ExtDelete.press(); KbdKey::ExtDelete.release();
                } else  { //if l2key == KbdKey::ExtHome || l2key == KbdKey::ExtUp || l2key == KbdKey::ExtPgUp {
                    KbdKey::Backspace.press(); KbdKey::Backspace.release();
                }
            }
            // TODO : ^^ this setup is non-ideal as these conditionals for keys dont belong here, should be specifiable when called!
        }
        else { // no special caret mode, just layer-2 functionality
            handle_l2_key_req (l2key, &l2s, allow_fast, allow_alt);
        }
    } ) }

    // now we can finally fill out the l2 actions (incl caps-alt combos)
    am.borrow_mut().caps_act_map .insert (KbdKey::J,     l2_action (KbdKey::ExtLeft,  l2s.clone(), true,  true));
    am.borrow_mut().caps_act_map .insert (KbdKey::K,     l2_action (KbdKey::ExtRight, l2s.clone(), true,  true));
    am.borrow_mut().caps_act_map .insert (KbdKey::H,     l2_action (KbdKey::ExtHome,  l2s.clone(), false, false));
    am.borrow_mut().caps_act_map .insert (KbdKey::L,     l2_action (KbdKey::ExtEnd,   l2s.clone(), false, false));
    am.borrow_mut().caps_act_map .insert (KbdKey::I,     l2_action (KbdKey::ExtUp,    l2s.clone(), true,  false));
    am.borrow_mut().caps_act_map .insert (KbdKey::Comma, l2_action (KbdKey::ExtDown,  l2s.clone(), true,  false));
    am.borrow_mut().caps_act_map .insert (KbdKey::U,     l2_action (KbdKey::ExtPgUp,  l2s.clone(), false, false));
    am.borrow_mut().caps_act_map .insert (KbdKey::M,     l2_action (KbdKey::ExtPgDn,  l2s.clone(), false, false));

}

fn do_l2k_setups (am:Rc<RefCell<ActionMaps>>, l2s:Arc<Layer2State>) {
    /* setup options summary:
     - only j/k for left/right get alt-for-ctrl speedup (native word nav mode)
     - those and i/comma for up/down get dot-for-double-speed mode (2x nav) .. i/comma get that for alt too
     - h/l/u/m for home/end/pgup/pgdown get no speedup modes
     - in del mode, left/home/up/pgup get ExtBackspace, right/end/down/pgdn get ExtDelete
     - in del mode, left/right do direct bksp/del, but others get select then bksp/del
     */

    // we'll define some setup fns below as closures, simply to capture am/l2s and avoid having to clone and pass them all over

    fn l2_ctrl_af (l2k:KbdKey) -> AF {
        Arc::new ( move || {
            KbdKey::Ctrl.press(); l2k.press(); l2k.release(); KbdKey::Ctrl.release();
    } ) }
    fn l2_fast_af (l2k:KbdKey) -> AF {
        Arc::new ( move || {
            l2k.press(); l2k.release(); l2k.press(); l2k.release();
    } ) }
    fn l2_del_sel_af (del_key:KbdKey, l2af:AF) -> AF {
        Arc::new ( move || {
            KbdKey::Shift.press(); l2af(); KbdKey::Shift.release();
            normal_action(del_key)();
    } ) }

    let l2sc = l2s.clone();
    let l2_af = move |l2a:AF, aa:AF, fa:AF| {
        Arc::new ( move || {
            if *l2sc.is_lalt_down.read().unwrap() { aa() }
            else if *l2sc.in_caret_fast_mode.read().unwrap() { fa() }
            else { l2a() }
    } ) } ;

    let l2_af_c = l2_af.clone();
    let l2_del_af = move |delKey:KbdKey| {
        Arc::new ( move || {
            l2_af_c.clone() (normal_action(delKey), l2_ctrl_af(delKey), l2_fast_af(delKey))()
    } ) };

    let l2sc = l2s.clone();
    let l2_compose = move |l2a:AF, cda:AF| {
        Arc::new ( move || {
            ensure_lalt_inactive(l2sc.clone());
            if *l2sc.in_caret_sel_mode.read().unwrap() {
                KbdKey::Shift.press(); l2a(); KbdKey::LShift.release();
            } else if *l2sc.in_caret_del_mode.read().unwrap() { cda() }
            else { l2a() }
    } ) };

    let setup_l2k = {
        move |k:KbdKey, l2a:AF, cda:AF| {
            am.borrow_mut().caps_act_map .insert (k, l2_compose.clone()(l2a, cda));
    } };


    // now we can do actual assignments for l2 keys
    let (k, l2k, dk) = (KbdKey::J, KbdKey::ExtLeft, KbdKey::Backspace);
    let l2af = l2_af.clone() (normal_action(l2k), l2_ctrl_af(l2k), l2_fast_af(l2k)); // extra modes: alt, fast
    setup_l2k (k, l2af, l2_del_af.clone()(dk));

    let (k, l2k, dk) = (KbdKey::K, KbdKey::ExtRight, KbdKey::ExtDelete);
    let l2af = l2_af.clone() (normal_action(l2k), l2_ctrl_af(l2k), l2_fast_af(l2k)); // extra modes: alt, fast
    setup_l2k (k, l2af, l2_del_af.clone()(dk));

    let (k, l2k, dk) = (KbdKey::H, KbdKey::ExtHome, KbdKey::Backspace);
    let l2af = l2_af.clone() (normal_action(l2k), normal_action(l2k), normal_action(l2k)); // extra modes: NONE
    setup_l2k (k, l2af.clone(), l2_del_sel_af(dk, l2af));

    let (k, l2k, dk) = (KbdKey::L, KbdKey::ExtEnd, KbdKey::ExtDelete);
    let l2af = l2_af.clone() (normal_action(l2k), normal_action(l2k), normal_action(l2k)); // extra modes: NONE
    setup_l2k (k, l2af.clone(), l2_del_sel_af(dk, l2af));

    let (k, l2k, dk) = (KbdKey::I, KbdKey::ExtUp, KbdKey::Backspace);
    let l2af = l2_af.clone() (normal_action(l2k), l2_fast_af(l2k), l2_fast_af(l2k)); // extra modes: fast only
    setup_l2k (k, l2af.clone(), l2_del_sel_af(dk, l2af));

    let (k, l2k, dk) = (KbdKey::Comma, KbdKey::ExtDown, KbdKey::ExtDelete);
    let l2af = l2_af.clone() (normal_action(l2k), l2_fast_af(l2k), l2_fast_af(l2k)); // extra modes: fast only
    setup_l2k (k, l2af.clone(), l2_del_sel_af(dk, l2af));

    let (k, l2k, dk) = (KbdKey::U, KbdKey::ExtPgUp, KbdKey::Backspace);
    let l2af = l2_af.clone() (normal_action(l2k), normal_action(l2k), normal_action(l2k)); // extra modes: NONE
    setup_l2k (k, l2af.clone(), l2_del_sel_af(dk, l2af));

    let (k, l2k, dk) = (KbdKey::M, KbdKey::ExtPgDn, KbdKey::ExtDelete);
    let l2af = l2_af.clone() (normal_action(l2k), normal_action(l2k), normal_action(l2k)); // extra modes: NONE
    setup_l2k (k, l2af.clone(), l2_del_sel_af(dk, l2af));

}

pub fn setup_krusty_board () {

    let l2s = Arc::new(Layer2State::default());
    let am = Rc::new(RefCell::new(ActionMaps::default()));

    // setup capslock, we'll completely disable it other than for krusty use
    setup_caps_tracking(l2s.clone());

    // setup left-alt, this will be monitored, but allowed to get out-of-sync between pressed state and external state
    setup_left_alt_tracking(l2s.clone());

    // setup tracking for right-alt too .. for most cases (except e.g. Space etc), we completely map it to RShift
    setup_right_alt_tracking(l2s.clone());

    // and for l-win .. this one is just minimal simple tracking
    setup_left_win_tracking(l2s.clone());

    // and left-shift .. ehh could prob even ignore this
    setup_left_shift_tracking(l2s.clone());

    // setup handling for space key .. overloads into enter with r-alt etc
    setup_space_key_handling(l2s.clone());

    // tab has special management to support alt-tab, but also turning it instant press/release for all cases
    setup_tab_key_handling(l2s.clone());


    // handling for mouse left btn, mostly to allow caps-as-ctrl behavior during drag drops and clicks
    setup_mouse_left_btn_handling(l2s.clone());
    // also for mouse right btn, mostly to allow switche scrolling w right-btn-wheel combo
    setup_mouse_right_btn_handling(l2s.clone());
    // also setup both Xbutton srcs to act as middle btns (used for link clicks, closing tabs etc)
    setup_mouse_x_btn_1_handling();
    setup_mouse_x_btn_2_handling();

    // setup handling for mouse wheel .. complex overloading over alt-tab, switche, volume, brigheness etc !!
    setup_mouse_wheel_handling(l2s.clone());


    // setup keys for layer-2 caret nav sel/del/fast modes
    // .. note: before setting up caret mode, we'll need to define any non-caret overloads (during caps-caret) for them
    // .. and we only need that for 'period' for caps-win-period as window-vert-stretch
    setup_caret_mode_key (KbdKey::R, l2s.in_caret_sel_mode.clone(), l2s.clone(), no_action());
    setup_caret_mode_key (KbdKey::D, l2s.in_caret_del_mode.clone(), l2s.clone(), no_action());
    let l2sc = l2s.clone();
    let ovFn = Arc::new ( move || {
        if *l2sc.is_lwin_down.read().unwrap() { win_fgnd_stretch(0,30); }
    } );
    setup_caret_mode_key (KbdKey::Period, l2s.in_caret_fast_mode.clone(), l2s.clone(), ovFn);




    // setting up the rest via by-key-combo action maps that we'll compose at the end to relevant callbacks
    use utils::window_utils::*;
    use utils::process_utils::*;

    // we'll define some util fns below as closures just to capture l2s and am (so we dont need to keep sending them in)
    let amc = am.clone();
    let add_base_mapping = move |key:KbdKey, action:AF| {
        amc.borrow_mut() .base_act_map .insert (key, action)
    };
    let amc = am.clone();
    let add_caps_mapping = move |key:KbdKey, action:AF| {
        amc.borrow_mut() .caps_act_map .insert (key, action)
    };
    let amc = am.clone();
    let add_alt_mapping = move |key:KbdKey, action:AF| {
        amc.borrow_mut() .alt_act_map .insert (key, action)
        // note that this ^^ doesnt touch alt down/active states, so if those need taking care of, gotta do it directly
    };
    let amc = am.clone();
    let add_caps_alt_mapping = move |key:KbdKey, action:AF| {
        amc.borrow_mut() .caps_alt_act_map .insert (key, action)
    };
    let (amc, l2sc) = (am.clone(), l2s.clone());
    let add_win_mapping = move |key:KbdKey, action:AF| {
        let action = Arc::new (move || { consume_down_lwin(l2sc.clone()); action(); });
        amc.borrow_mut().win_act_map .insert (key, action);
    };
    let (amc, l2sc) = (am.clone(), l2s.clone());
    let add_caps_win_mapping = move |key:KbdKey, action:AF| {
        let action = Arc::new (move || { consume_down_lwin(l2sc.clone()); action(); });
        amc.borrow_mut().caps_win_act_map .insert (key, action);
    };
    let amc = am.clone();
    let add_caps_as_shift_mapping = move |key:KbdKey| {
        amc.borrow_mut() .caps_act_map .insert (key, shift_action(key))
    };
    let amc = am.clone();
    let add_caps_as_ctrl_mapping = move |key:KbdKey| {
        amc.borrow_mut() .caps_act_map .insert (key, ctrl_action(key))
    };


    // win-m by default minimized all windows .. we just want to disable it
    add_win_mapping.clone() (KbdKey::M, Arc::new (|| {} ));

    // win-i should start irfanview
    add_win_mapping.clone() (KbdKey::I, Arc::new (|| { start_irfanview(); } ));

    // win-n should start chrome-incognito
    add_win_mapping.clone() (KbdKey::N, Arc::new (|| { start_chrome_incognito(); } ));

    // in cur laptop, Fn-F6/F7 do brightness, but at +10 incrs .. set them to do small incrs with win combos
    add_win_mapping.clone() (KbdKey::F6, Arc::new (|| { handle_brightness_wheel(-1) } ));
    add_win_mapping.clone() (KbdKey::F7, Arc::new (|| { handle_brightness_wheel(1) } ));

    // might as well do that for alt as well, since we use alt for most other such shortcuts
    add_alt_mapping.clone() (KbdKey::F6, Arc::new ( || { handle_brightness_wheel(-1); } ) );
    add_alt_mapping.clone() (KbdKey::F7, Arc::new ( || { handle_brightness_wheel(1); } ) );


    // actually, since we use alt-2/3 as vol down/up, might as well also set win-2/3 for brightness down/up
    add_win_mapping.clone() (KbdKey::Numrow_2, Arc::new (|| { handle_brightness_wheel(-1) } ));
    add_win_mapping.clone() (KbdKey::Numrow_3, Arc::new (|| { handle_brightness_wheel(1) } ));


    // alt-f1 play/pause, caps-f1 toggle mute, base-case, overload F1 for switche caller (F21)
    let l2sc = l2s.clone();
    add_base_mapping.clone() (KbdKey::F1, normal_action(KbdKey::F21));
    add_caps_mapping.clone() (KbdKey::F1, normal_action(KbdKey::VolumeMute));
    add_alt_mapping.clone()  (KbdKey::F1, Arc::new ( move || {
        // media keys only work while alt is inactive, so gotta set/restore that etc
        ensure_held_lalt_inactive(l2sc.clone());
        KbdKey::MediaPlayPause.press(); KbdKey::MediaPlayPause.release();
    } ) );

    let l2sc = l2s.clone();
    let do_media_skips = move |n_skips:u32| {
        // sending ^!{Right} which we traditionally set on winamp to skip forward a bit
        //ensure_held_lalt_inactive(l2sc.clone());
        // note that we'll send alts explicitly so it works even if alt was quickly released
        ensure_alt_active(l2sc.clone());
        KbdKey::LControl.press();
        (0 ..n_skips) .into_iter().for_each(|_| { KbdKey::ExtRight.press(); KbdKey::ExtRight.release() });
        KbdKey::LControl.release();
        ensure_not_held_alt_inactive(l2sc.clone());
    };

    // al-f2 for next with some initial skip
    let l2sc = l2s.clone();
    let do_media_skips_c = do_media_skips.clone(); // gotta clone it to allow it to be moved into AF closure below
    add_alt_mapping.clone() (KbdKey::F2, Arc::new ( move || {
        ensure_held_lalt_inactive(l2sc.clone());
        if !*l2sc.is_lshift_down.read().unwrap() {
            KbdKey::MediaNextTrack.press();  KbdKey::MediaNextTrack.release();
        } else {
            KbdKey::LShift.release(); KbdKey::MediaPrevTrack.press();  KbdKey::MediaPrevTrack.release();
        };
        let do_media_skips_c = do_media_skips_c.clone(); // gotta clone again to allow moving into spawned thread closure!
        thread::spawn ( move || { thread::sleep(time::Duration::from_millis(2000));  do_media_skips_c.clone()(2); } );
    } ) );

    // alt-f3 for skip forward a bit (skips is via alt-ctrl-Right, not via media keys)
    add_alt_mapping.clone() (KbdKey::F3, Arc::new ( move || { do_media_skips.clone()(1) } ) );
    // alt-2 is vol down, alt-3 is vol up
    add_alt_mapping.clone() (KbdKey::Numrow_2, normal_action(KbdKey::VolumeDown));
    add_alt_mapping.clone() (KbdKey::Numrow_3, normal_action(KbdKey::VolumeUp));

    // trying out alt-1 for switche next-win (via its F20/Alt-F20 hotkey)
    add_alt_mapping.clone() (KbdKey::Numrow_1, normal_action(KbdKey::F20));



    // caps-e is Enter
    add_caps_mapping.clone() (KbdKey::E, normal_action(KbdKey::Enter));

    // escape is just escape, but we just want it to do press-release immediately (so switche is faster)
    add_base_mapping.clone() (KbdKey::Escape, normal_action(KbdKey::Escape));

    // use the apps key to send shift-escape
    add_base_mapping (KbdKey::Apps, shift_action(KbdKey::Escape));

    // for back-tick, make normal case be Delete, caps do back-tick, and shifts do its ~, alt will do quick switch via F20/Alt-F20
    // note that this means the shift-delete action which usually maps to 'cut' action wont be available on this key
    let l2sc = l2s.clone();
    add_base_mapping.clone() (KbdKey::Backquote, Arc::new ( move || {
        press_release (
            if *l2sc.is_lshift_down.read().unwrap() || *l2sc.is_ralt_down.read().unwrap() { KbdKey::Backquote }
            else { KbdKey::ExtDelete }
        );
    }));
    add_caps_mapping.clone() (KbdKey::Backquote, normal_action(KbdKey::Backquote));
    add_alt_mapping.clone()  (KbdKey::Backquote, normal_action(KbdKey::F20));



    // number of caps-as-shift mappings
    add_caps_as_shift_mapping.clone() (KbdKey::Numrow_6);
    add_caps_as_shift_mapping.clone() (KbdKey::Numrow_7);
    add_caps_as_shift_mapping.clone() (KbdKey::Numrow_8);
    add_caps_as_shift_mapping.clone() (KbdKey::Numrow_9);
    add_caps_as_shift_mapping.clone() (KbdKey::Numrow_0);
    add_caps_as_shift_mapping.clone() (KbdKey::Minus);
    add_caps_as_shift_mapping.clone() (KbdKey::Equal);
    add_caps_as_shift_mapping.clone() (KbdKey::LBracket);
    add_caps_as_shift_mapping.clone() (KbdKey::RBracket);
    add_caps_as_shift_mapping.clone() (KbdKey::Semicolon);
    add_caps_as_shift_mapping.clone() (KbdKey::Quote);
    add_caps_as_shift_mapping.clone() (KbdKey::Slash);
    add_caps_as_shift_mapping.clone() (KbdKey::Backslash);
    //add_caps_shift_mapping.clone() (KbdKey::Period); // can't do as used as 'fast' accelerator in caret mode!


    // number of caps-as-ctrl mappings
    add_caps_as_ctrl_mapping.clone() (KbdKey::A);
    add_caps_as_ctrl_mapping.clone() (KbdKey::S);
    add_caps_as_ctrl_mapping.clone() (KbdKey::F);  // further overloading below for caps-alt-f for full-screen
    add_caps_as_ctrl_mapping.clone() (KbdKey::W);  // further overloading below for caps-alt-w to close windows
    add_caps_as_ctrl_mapping.clone() (KbdKey::T);
    add_caps_as_ctrl_mapping.clone() (KbdKey::Y);
    add_caps_as_ctrl_mapping.clone() (KbdKey::Z);
    add_caps_as_ctrl_mapping.clone() (KbdKey::X);
    add_caps_as_ctrl_mapping.clone() (KbdKey::C);
    add_caps_as_ctrl_mapping.clone() (KbdKey::V);
    add_caps_as_ctrl_mapping.clone() (KbdKey::N);
    add_caps_as_ctrl_mapping.clone() (KbdKey::O);
    add_caps_as_ctrl_mapping.clone() (KbdKey::P);


    // caps-alt-F for full screen, but also the expected caps-f for ctrl-f
    let l2sc = l2s.clone();
    add_caps_alt_mapping.clone() (KbdKey::F, Arc::new (move || {
        ensure_held_lalt_inactive(l2sc.clone());
        KbdKey::F11.press(); KbdKey::F11.release();
    } ) );

    // 'w' should have caps-ctrl mapping, but when w/ alt, send alt-f4 (to close all-tabs, windows etc)
    let l2sc = l2s.clone();
    // note: intiially we wanted this with caps-shift-w, but turns out (at least on my kbd, turns out caps+shift+[F1, 2, w, s, x]
    //      dont produce any key event at the hook at all .. nothing .. its like the keyboard driver not sending those out
    //  .. funnily enough, there's a bunch of complaints about specifically those keys for dell/hp laptops .. looks like hardware
    //  .. appears to be a common kbd pcb layout issue .. heres from 2007: (https://www.joachim-breitner.de/blog/250-Shift-Caps-2)
    // ... sooo .. to makeup, we'll do alt-caps-w do the alt-f4 business instead
    add_caps_alt_mapping.clone() (KbdKey::W, Arc::new ( move || {
        ensure_held_lalt_active(l2sc.clone());
        KbdKey::F4.press(); KbdKey::F4.release();
    } ) );


    // filling out l2 actions (incl w caps-alt combos)
    //do_l2k_setups_old (am.clone(), l2s.clone());
    do_l2k_setups (am.clone(), l2s.clone());



    // then the caps-win combo l3 actions :

    // caps-win-U should vert-max (via shift-win-up) if not already, or else restore window from vert-max
    add_caps_win_mapping.clone() (KbdKey::U, Arc::new (|| win_fgnd_toggle_vertmax()));
    // caps-win-m should maximize (via win-m) if not, else restore from max
    add_caps_win_mapping.clone() (KbdKey::M, Arc::new (|| win_fgnd_toggle_max()));

    // caps-win-[j,k,i,,] should  move window [left, right, top, bottom] respectively
    add_caps_win_mapping.clone() (KbdKey::J,     Arc::new (|| win_fgnd_move(-80, 0) ));
    add_caps_win_mapping.clone() (KbdKey::K,     Arc::new (|| win_fgnd_move(80, 0) ));
    add_caps_win_mapping.clone() (KbdKey::I,     Arc::new (|| win_fgnd_move(0, -50) ));
    add_caps_win_mapping.clone() (KbdKey::Comma, Arc::new (|| win_fgnd_move(0, 50) ));

    // caps-win-[h,;,.,o] should stretch window [narroer, wider, shorter, taller] respectively
    // .. note that caps-win-L gets unavoidably captured by windows to lock laptop, hence the semicolon
    add_caps_win_mapping.clone() (KbdKey::H,         Arc::new (|| win_fgnd_stretch(-30, 0) ));
    add_caps_win_mapping.clone() (KbdKey::O,         Arc::new (|| win_fgnd_stretch(0, -30) ));
    //add_caps_win_mapping.clone() (KbdKey::Period,  Arc::new (|| win_fgnd_stretch(0, 30) ));
    // ^^ cant to 'Period' here, as thats caret key w/ key-up handling .. gotta add any overloads right there
    //add_caps_win_mapping.clone() (KbdKey::L,         Arc::new (|| win_fgnd_stretch(30, 0) ));
    // ^^ any win-L combo is hardcoded at OS level to lock machine, cant override that, so we'll make semicolon do that instead
    add_caps_win_mapping.clone() (KbdKey::Semicolon, Arc::new (|| win_fgnd_stretch(30, 0) ));


    // some additional caps-win combos
    // caps-win-c being used to launch winmerge diff from last two clipboard entries
    add_caps_win_mapping.clone() (KbdKey::C, Arc::new (|| start_winmerge_clipboard() ));
    // gaah we'll just throw in iDEA diff for drag-drop diffing (just coz winmerge doesnt do dark mode)
    //add_win_mapping.clone() (KbdKey::C, Arc::new (|| start_idea_diff() ));
    // ^^ cant do from here, turns out idea diff from cmd line can ONLY be opened with two files pointed, unlike empty from Idea shortcut!



    // finally bind everytihng from action-maps !!
    bind_all_from_action_maps (am.clone(), l2s.clone());


}

