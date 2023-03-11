

use std::{
    time::Instant,
    sync::Arc,
};
use windows::Win32::UI::WindowsAndMessaging::WHEEL_DELTA;

use crate::{
    *, key_utils::*, EventPropagationDirective::*,
    MouseEventCallbackFnType::*, MouseEventCbMapKeyAction::*,
};


/// sets up mouse left btn for msotly pass-through eqv w tracking, but allowing caps-as-ctrl behavior for the mouse
pub fn setup_mouse_left_btn_handling (k:&Krusty) {
    use crate::{MouseButton::*, MouseBtnEvent_T::*};
    let ks = k.ks.clone();
    k.msb .bind_btn_event ( LeftButton, BtnEventCb(BtnDown), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_SpawnedCallback ( Arc::new ( move |_| handle_mouse_left_btn_down(&ks) ) )
    } );
    let ks = k.ks.clone();
    k.msb .bind_btn_event ( LeftButton, BtnEventCb(BtnUp), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_SpawnedCallback ( Arc::new ( move |_| handle_mouse_left_btn_up(&ks) ) )
    } );
    // ^^ in theory could do non-blocking bind for release, but that can occasionally expose us to timing issues as press is on block-bind
    //      and that will be slower to send the actual press from the spawned thread (so making release slower too helps)
}

fn handle_mouse_left_btn_down (ks:&KrustyState) {
    ks.mouse_left_btn_down.set();
    if ks.mod_keys.caps.down.check() {
        ks.in_managed_ctrl_down_state.set();
        ks.mod_keys.lctrl.ensure_active();   // this allows caps-as-ctrl for drag drop etc
    }
    MouseButton::LeftButton.press()
}

fn handle_mouse_left_btn_up (ks:&KrustyState) {
    ks.mouse_left_btn_down.clear();
    MouseButton::LeftButton.release();
}




/// sets up mouse right btn for pass-through eqv w/ tracking, but allows right-held-scroll for switche tab-switching
// tracking right-btn-down-state is required for switche scroll (via F16/F17/Ctrl-F18)
// note that unlike other mouse btn/wheels, the right btn is set to passthrough!
pub fn setup_mouse_right_btn_handling (k:&Krusty) {
    use crate::{MouseButton::*, MouseBtnEvent_T::*};
    let ks = k.ks.clone();
    k.msb .bind_btn_event ( RightButton, BtnEventCb(BtnDown), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Continue,
        cb : MouseEvCbFn_SpawnedCallback ( Arc::new ( move |_| handle_mouse_right_btn_down(&ks) ) )
    } );
    let ks = k.ks.clone();
    k.msb .bind_btn_event ( RightButton, BtnEventCb(BtnUp), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Continue,
        cb : MouseEvCbFn_SpawnedCallback ( Arc::new ( move |_| handle_mouse_right_btn_up(&ks) ) )
    } );
}

fn handle_mouse_right_btn_down (ks:&KrustyState) {
    ks.mouse_right_btn_down.set();
}

fn handle_mouse_right_btn_up (ks:&KrustyState) {
    ks.mouse_right_btn_down.clear();
    if ks.in_right_btn_scroll_state.check() {
        ks.in_right_btn_scroll_state.clear();
        //k.ks.lalt.active_action(base_action(Key::F18))();
        //k.ks.lctrl.active_action(base_action(Key::F18))();
        //base_action(Key::F18)();
        ctrl_press_release(Key::F18);
    }
}




/// x1/x2 btns are set up to behave like they were middle btn too
// note: it turns out just doing press/release on initial press works snappier/more-reliable than default btn-holds
pub fn setup_mouse_x_btn_1_handling (k:&Krusty) {
    use crate::{MouseButton::*, MouseBtnEvent_T::*};
    k.msb .bind_btn_event ( X1Button, BtnEventCb(BtnDown), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_SpawnedCallback ( Arc::new ( move |_| {
            MiddleButton.press(); MiddleButton.release();
        } ) ),
    } );
    k.msb .bind_btn_event ( X1Button, BtnEventCb(BtnUp), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_InlineCallback ( Arc::new ( move |_| EventProp_Stop) )
    } );
}

/// x1/x2 btns are set up to behave like they were middle btn too
pub fn setup_mouse_x_btn_2_handling (k:&Krusty) {
    use crate::{MouseButton::*, MouseBtnEvent_T::*};
    k.msb .bind_btn_event ( X2Button, BtnEventCb(BtnDown), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_SpawnedCallback ( Arc::new ( move |_| {
            MiddleButton.press(); MiddleButton.release();
        } ) ),
    } );
    k.msb .bind_btn_event ( X2Button, BtnEventCb(BtnUp), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_InlineCallback ( Arc::new ( move |_| EventProp_Stop) )
    } );
}







/// sets up mouse wheel with various overloaded modes incl brightness, volume, tab-switching, caps-as-ctrl etc
pub fn setup_mouse_wheel_handling (k:&Krusty) {
    use crate::{MouseEvent::*, MouseWheel::*, MouseWheelEvent_T::*};
    let ks = k.ks.clone();
    k.msb .bind_wheel_event ( DefaultWheel, WheelEventCb(WheelForward), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_SpawnedCallback ( Arc::new ( move |ev| {
            if let wheel_event {src_wheel:_, delta} = ev { handle_wheel_guarded (delta, &ks) }
        } ) ),
    } );
    let ks = k.ks.clone();
    k.msb .bind_wheel_event( DefaultWheel, WheelEventCb(WheelBackward), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_SpawnedCallback ( Arc::new ( move |ev| {
            if let wheel_event {src_wheel:_, delta} = ev { handle_wheel_guarded (delta, &ks) }
        } ) ),
    } );
}

pub fn handle_wheel_guarded (delta:i32, ksr:&KrustyState) {
    // this is mostly to make the super-fast inertial smooth-scroll wheel on my (MX3) mouse a lil more usable by spacing things out
    // also, the invalidation setup prevents things like caps down when wheel is still unintentionally inertially spinning to trigger zooms etc
    let last_stamp = *ksr.last_wheel_stamp.read().unwrap();
    *ksr.last_wheel_stamp.write().unwrap() = Instant::now();
    //let gap = ksr.last_wheel_stamp.read().unwrap().duration_since(last_stamp);
    //println!("{:#?}", dur.as_millis());
    const GUARD_DUR_MS: u128 = 120;  // from dur printouts above, looked like max inertial gap is 120 (min 7ms, usually <100)
    if !ksr.is_wheel_spin_invalidated.check() {
        handle_wheel_action(delta, ksr);
    } else if GUARD_DUR_MS < ksr.last_wheel_stamp.read().unwrap().duration_since(last_stamp).as_millis() {
        ksr.is_wheel_spin_invalidated.clear();
        handle_wheel_action(delta, ksr);
    } else {
        // if its invalidated AND wheel-spin spacing is below guard-dur, we suppress the wheel event
    }
}

pub fn handle_wheel_action (delta:i32, ksr:&KrustyState) {
    let incr = delta / WHEEL_DELTA as i32;
    if ksr.mouse_right_btn_down.check() {
        // right-mouse-btn-wheel support for switche task switching
        ksr.in_right_btn_scroll_state.set();
        let key = if incr.is_positive() { Key::F17 } else { Key::F16 };
        //ksr.mod_keys.lalt.active_action(base_action(key))();       // not usable due to masking keys (so changed in hotkey switche)
        //ksr.mod_keys.lctrl.active_action(base_action(key))();      // not ideal as even non-masked wrapping causes slower/choppy switche scroll
        press_release(key);                                 // we'd rather use direct press hotkeys for best perf
    } else  if ksr.mod_keys.lalt.down.check() {
        // wheel support for scrolling in windows native alt-tab task-switching screen
        // this requires a system call to check alt-tab window, so push it out to thread?
        // .. naah, we're always spawned out from hook thread (for blocking binds), so theres no point
        ksr.mod_keys.lalt.consumed.set();
        if utils::get_fgnd_win_class() == "MultitaskingViewFrame" { // alt-tab states
            ksr.mod_keys.lalt.ensure_active();    // we're already down but just in case its down/inactive
            handle_alt_tab_wheel(incr)
        } else {
            // alt-wheel for (fine delta) control, caps-alt-wheel for larger adjustments
            //let mult = if ksr.mode_states.qks1.down.check() || ksr.mod_keys.caps.down.check() {5} else {1};
            let mult = if ksr.mode_states.qks1.down.check() {5} else {1};
            incr_brightness (incr * mult)
        }
    } else if ksr.mod_keys.lwin.down.check() {
        // win-wheel for (fine delta) control, caps-win-wheel for larger adjustments
        ksr.mod_keys.lwin.consumed.set();
        //let mult = if ksr.mode_states.qks1.down.check() || ksr.mod_keys.caps.down.check() {2} else {1};
        let mult = if ksr.mode_states.qks1.down.check() {2} else {1};
        incr_volume (incr * mult)
    } else if ksr.mod_keys.caps.down.check() {
        ksr.in_managed_ctrl_down_state.set();
        //Key::LCtrl.press();
        ksr.mod_keys.lctrl.ensure_active();
        MouseWheel::DefaultWheel.scroll(delta); // caps-wheel as ctrl-wheel (zoom etc)
    } else if ksr.mod_keys.some_shift_down() {
        //handle_horiz_scroll_wheel(incr);
        // ^^ todo:: .. (and for now, just let default pass through)
        MouseWheel::DefaultWheel.scroll(delta);
    } else {
        MouseWheel::DefaultWheel.scroll(delta);
    }
}

pub fn incr_volume (incr:i32) {
    let key = if incr > 0 { Key::VolumeUp } else { Key::VolumeDown };
    (0 .. incr.abs()) .for_each(|_| key.press());
}

pub fn incr_brightness (incr:i32) {
    static INCR_STEP:i32 = 1;
    // note again that we're always spawned out from hook thread, so slower tasks are also ok here
    //utils::brightness_ps_wmi::incr_brightness(INCR_STEP*incr);
    let _ = utils::incr_brightness(INCR_STEP*incr);
}

pub fn handle_alt_tab_wheel (incr:i32) {
    // todo potentially impl additional separate timer-spacing here, to slow this down even more than regular wheel spacing
    // note that for these we DONT want to release Alt key .. presumably, expecting it to be physically released later
    if incr.is_positive() { shift_press_release(Key::Tab) }
    else { press_release(Key::Tab) }
}

#[allow(dead_code)]
pub fn handle_horiz_scroll_wheel (_incr:i32) {
    // todo we could in theory impl this overriding the native horiz scroll behavior
}
