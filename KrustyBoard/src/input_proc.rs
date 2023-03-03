use crate::{common::*, public::*};
use once_cell::sync::Lazy;
use std::{
    mem::{size_of, MaybeUninit},
    ptr::null_mut,
    sync::atomic::AtomicPtr,
};
use std::os::raw::c_int;

use windows::Win32::{
    Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, WPARAM},
    UI::Input::KeyboardAndMouse::*,
    UI::WindowsAndMessaging::*,
};


// this is used for identifying the fake keypresses we insert, so we don't process them in an infinite loop
// note that 0xFFC3D44F is from ahk, though ahk uses further complex variations on it to signal more things
//const FAKE_EXTRA_INFO: ULONG_PTR = 0x14C;
const FAKE_EXTRA_INFO: usize = 0xFFC3D44F;

static KEYBD_HHOOK: Lazy<AtomicPtr<HHOOK>> = Lazy::new(AtomicPtr::default);
static MOUSE_HHOOK: Lazy<AtomicPtr<HHOOK>> = Lazy::new(AtomicPtr::default);

static KEY_SWAPS_MAP: Lazy<HashMap<KbdKey,u64>> = Lazy::new ( || {
    [   (KbdKey::RAlt, 0xE038 as u64), (KbdKey::RCtrl, 0xE01D as u64) , (KbdKey::RShift, 0x0036 as u64),
        (KbdKey::LAlt, 0x0038 as u64), (KbdKey::LCtrl, 0x001D as u64) , (KbdKey::LShift, 0x002A as u64)
    ] .into_iter() .collect::<HashMap<KbdKey,u64>>()
} );

impl KbdKey {
    /// Returns true if a given `KeybdKey` is currently pressed (in the down position).
    pub fn is_pressed(self) -> bool {
        (unsafe { GetAsyncKeyState(u64::from(self) as i32) } >> 15) != 0
    }

    /// Presses a given `KeybdKey`. Note: this means the key will remain in the down
    /// position. You must manually call release to create a full 'press'.
    pub fn press(self) { self.send_key_event(false) }

    /// Releases a given `KeybdKey`. This means the key would be in the up position.
    pub fn release(self) { self.send_key_event(true) }

    fn send_key_event (self, ev_is_up:bool) {
        // todo : prob build a separate sc-code mechanism so dont have to do hacks like these
        let (code, sc_not_vk) = KEY_SWAPS_MAP .get(&self) .map (|c| (*c, true))
            .or (Some((u64::from(self), false))) .map (|(c,b)| (c, b || c >= 0xE0)) .unwrap();

        send_keybd_input(code as u16, ev_is_up, sc_not_vk);
    }

    /// Returns true if a keyboard key which supports toggling (ScrollLock, NumLock,
    /// CapsLock) is on.
    pub fn is_toggled(self) -> bool {
        unsafe { GetKeyState(u64::from(self) as i32) & 15 != 0 }
    }
}

impl MouseButton {
    /// Returns true if a given `MouseButton` is currently pressed (in the down position).
    pub fn is_pressed(self) -> bool {
        (unsafe { GetAsyncKeyState(u32::from(self) as i32) } >> 15) != 0
    }

    /// Presses a given `MouseButton`. Note: this means the button will remain in the down
    /// position. You must manually call release to create a full 'click'.
    pub fn press(self) {
        match self {
            MouseButton::LeftButton   => send_mouse_input(MOUSEEVENTF_LEFTDOWN,   0, 0, 0),
            MouseButton::RightButton  => send_mouse_input(MOUSEEVENTF_RIGHTDOWN,  0, 0, 0),
            MouseButton::MiddleButton => send_mouse_input(MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0),
            _ => {}
        }
    }

    /// Releases a given `MouseButton`. This means the button would be in the up position.
    pub fn release(self) {
        match self {
            MouseButton::LeftButton   => send_mouse_input(MOUSEEVENTF_LEFTUP,   0, 0, 0),
            MouseButton::RightButton  => send_mouse_input(MOUSEEVENTF_RIGHTUP,  0, 0, 0),
            MouseButton::MiddleButton => send_mouse_input(MOUSEEVENTF_MIDDLEUP, 0, 0, 0),
            _ => {}
        }
    }
}

impl MousePointer {
    pub fn pos() -> (i32, i32) {
        unsafe {
            let mut point = MaybeUninit::uninit();
            GetCursorPos(point.as_mut_ptr());
            let point = point.assume_init();
            (point.x, point.y)
        }
    }

    /// Moves the mouse relative to its current position by a given amount of pixels.
    pub fn move_rel(dx: i32, dy: i32) {
        let (x, y) = Self::pos();
        Self::move_abs(x + dx, y + dy);
    }

    /// Moves the mouse to a given position based on absolute coordinates. The top left
    /// corner of the screen is (0, 0).
    pub fn move_abs(x: i32, y: i32) {
        unsafe {
            SetCursorPos(x, y);
        }
    }
}

impl MouseWheel {
    /// Scrolls the mouse wheel (whether default or horiz wheel) by given delta .. note that typically 120 delta is 1 incr
    pub fn scroll(self, delta: i32) {
        if let Some(flag) = match self {
            MouseWheel::DefaultWheel    => Some(MOUSEEVENTF_WHEEL),
            MouseWheel::HorizontalWheel => Some(MOUSEEVENTF_HWHEEL),
            _ => None
        } {
            send_mouse_input(flag, delta, 0, 0);
        }
    }
}




/// Starts listening for bound input events.
pub fn handle_input_events () {
    if !MOUSE_CALLBACKS.read().unwrap().is_empty() {
        set_hook(WH_MOUSE_LL, &*MOUSE_HHOOK, mouse_proc);
    };
    if COMBO_MAPS_PROCESSOR.read().unwrap().is_some() || !KEYBD_CALLBACKS.read().unwrap().is_empty() {
        set_hook(WH_KEYBOARD_LL, &*KEYBD_HHOOK, keybd_proc);
    };
    // win32 sends hook events to a thread with a 'message loop', but we dont create any windows,
    //  so we wont get any actual messages, so we can just leave a forever waiting GetMessage instead of setting up a msg-loop
    // .. basically while its waiting, the thread is awakened simply to call kbd hook (for an actual msg, itd awaken give the msg)
    let mut msg: MSG = unsafe { MaybeUninit::zeroed().assume_init() };
    unsafe { GetMessageW(&mut msg, HWND(0), 0, 0) };
}




/// Keyboard lower-level-hook processor
unsafe extern "system"
fn keybd_proc (code: c_int, w_param: WPARAM, l_param: LPARAM) -> LRESULT {

    use crate::{EventPropagationDirective::*, KbdEvCbComboProcDirective::*, KbdEventCallbackFnType::*};

    let return_call = || { CallNextHookEx(HHOOK(0), code, w_param, l_param) };

    if code < 0 { return return_call() }      // ms-docs says we MUST do this, so ig k fine

    /*
    // .. disabling this, as it's basically never applicable for our usage, and incurs runtime cost on every event
    if COMBO_MAPS_PROCESSOR.read().unwrap().is_none() && KEYBD_CALLBACKS.read().unwrap().is_empty() {
        unset_hook(&*KEYBD_HHOOK);
        return return_call();
    }*/

    let kb_struct = *(l_param.0 as *const KBDLLHOOKSTRUCT);

    /*
    println!("code: {:X}, w_param: {:X}, vk_code: {:#06X}, scanCode: {:#06X}, flags: {:X}, time: {}, dwExtraInfo: {:X}",
             code, w_param,
             kb_struct.vkCode, kb_struct.scanCode, kb_struct.flags, kb_struct.time, kb_struct.dwExtraInfo);
    */

    // if we injected this event ourselves, we should just bail
    if kb_struct.dwExtraInfo == FAKE_EXTRA_INFO { return return_call() }

    let (event, ev_is_key_down) = match w_param.0 as u32 {
        WM_KEYDOWN      => (KbdEventType::KbdEvent_KeyDown,      true  ),
        WM_SYSKEYDOWN   => (KbdEventType::KbdEvent_SysKeyDown,   true  ),
        WM_KEYUP        => (KbdEventType::KbdEvent_KeyUp,        false ),
        WM_SYSKEYUP     => (KbdEventType::KbdEvent_SysKeyUp,     false ),
        _               => (KbdEventType::KbdEvent_Unrecognized, false ),
    };
    let key = KbdKey::from(u64::from(kb_struct.vkCode));

    let kbd_event = KbdEvent { event, key, vk_code: kb_struct.vkCode as u32, sc_code: kb_struct.scanCode as u32 };

    let cb_lookup_key = KbdEventCbMapKey::from_key_down_state(key, ev_is_key_down);

    let (mut do_ev_prop, mut do_combo_proc) = (EventProp_Continue, ComboProc_Enable);

    // first route it through any per-key registered callbacks
    if let Some(cbe) = KEYBD_CALLBACKS.read().unwrap() .get(&cb_lookup_key) .as_ref() {
        do_ev_prop = cbe.event_prop_directive;
        do_combo_proc = cbe.combo_proc_directive;
        match &cbe.cb {
            KbdEvCbFn_InlineCallback {cb}  => {
                let ev_prop_drctv = cb(kbd_event);
                if cbe.event_prop_directive == EventProp_Undetermined { do_ev_prop = ev_prop_drctv }
            }
            KbdEvCbFn_SpawnedCallback {cb} => {
                let (cb, kbe) = (cb.clone(), kbd_event.clone());    // clone as we'll need the event later again
                spawn (move || cb(kbe));
            }
    } }

    // now lets call the bulk defaults/combos processor if its available, and if combo_proc for this event not disabled from above
    if do_combo_proc == ComboProc_Enable {
        let mut ev_prop = EventProp_Continue;
        if let Some (cbe) = COMBO_MAPS_PROCESSOR.read().unwrap().as_ref() {
            match &cbe.cb {
                KbdEvCbFn_InlineCallback {cb} => {
                    ev_prop = cb(kbd_event);
                }
                _ => {}
            }
        }
        do_ev_prop = if ev_prop == EventProp_Stop { EventProp_Stop } else { do_ev_prop }
    }
    if do_ev_prop == EventProp_Stop {
        return LRESULT(1);  // returning with non-zero code signals OS to block further processing on the input event
    }
    return return_call()
}


#[allow(non_snake_case)]
fn HIWORD(l: u32) -> u16 { ((l >> 16) & 0xffff) as u16 }

/// mouse lower-level-hook processor
unsafe extern "system"
fn mouse_proc(code: c_int, w_param: WPARAM, l_param: LPARAM) -> LRESULT {

    use crate::{MouseButton::*, MouseWheel::*, EventPropagationDirective::*, MouseEventCallbackFnType::*};

    let return_call = || { CallNextHookEx(HHOOK(0), code, w_param, l_param) };

    if code < 0 { return return_call() }      // ms-docs says we MUST do this, so ig k fine

    /*
    if MOUSE_CALLBACKS.read().unwrap().is_empty() {
        unset_hook(&*MOUSE_HHOOK);
        return return_call();
    }*/

    let mh_struct = &*(l_param.0 as *const MSLLHOOKSTRUCT);

    if mh_struct.dwExtraInfo == FAKE_EXTRA_INFO {
        // if we injected it, we should just bail (and call the next guy down the line)
        return return_call()
    }

    let ev_and_key_for_btn_ev = |btn:MouseButton, down_not_up:bool| {
        Some ( ( MouseEvent::from_btn_ev(btn, down_not_up), MouseEventCbMapKey::for_btn_down_state(btn, down_not_up) ) )
    };
    let ev_and_key_for_wheel_ev = |wheel:MouseWheel, delta:i32| {
        Some ( ( MouseEvent::from_wheel_ev(wheel, delta), MouseEventCbMapKey::for_wheel_fwd_state(wheel, delta >= 0) ) )
    };
    let ev_and_key_for_pointer_ev = |x:i32, y:i32| {
        Some ( ( MouseEvent::from_pointer_ev(x,y), MouseEventCbMapKey::for_pointer() ) )
    };

    //println!("{:#?}", mh_struct);

    if let Some ((event, lookup_key)) = match w_param.0 as u32 {
        WM_LBUTTONDOWN => ev_and_key_for_btn_ev (LeftButton,   true),
        WM_RBUTTONDOWN => ev_and_key_for_btn_ev (RightButton,  true),
        WM_MBUTTONDOWN => ev_and_key_for_btn_ev (MiddleButton, true),
        WM_XBUTTONDOWN => {
            match MOUSEHOOKSTRUCTEX_MOUSE_DATA ( HIWORD (mh_struct.mouseData.0) as u32) {
                XBUTTON1 => ev_and_key_for_btn_ev (X1Button, true),
                XBUTTON2 => ev_and_key_for_btn_ev (X2Button, true),
                _ => None,
            }
        }
        WM_LBUTTONUP => ev_and_key_for_btn_ev (LeftButton,   false),
        WM_RBUTTONUP => ev_and_key_for_btn_ev (RightButton,  false),
        WM_MBUTTONUP => ev_and_key_for_btn_ev (MiddleButton, false),
        WM_XBUTTONUP => {
            match MOUSEHOOKSTRUCTEX_MOUSE_DATA ( HIWORD (mh_struct.mouseData.0) as u32) {
                XBUTTON1 => ev_and_key_for_btn_ev (X1Button, false),
                XBUTTON2 => ev_and_key_for_btn_ev (X2Button, false),
                _ => None,
            }
        }
        WM_MOUSEWHEEL  => ev_and_key_for_wheel_ev (DefaultWheel,    HIWORD(mh_struct.mouseData.0) as i16 as i32),
        WM_MOUSEHWHEEL => ev_and_key_for_wheel_ev (HorizontalWheel, HIWORD(mh_struct.mouseData.0) as i16 as i32),

        //WM_MOUSEMOVE   => ev_and_key_for_pointer_ev (mh_struct.pt.x, mh_struct.pt.y),
        _ => None,
    } {
        //{ let ec = event.clone(); spawn(move || println!("{:?}", ec)); }   // for debug

        let mut do_ev_prop = EventProp_Continue;

        if let Some(cbe) = MOUSE_CALLBACKS.read().unwrap().get(&lookup_key).as_ref() {
            do_ev_prop = cbe.event_prop_directive;
            match &cbe.cb {
                MouseEvCbFn_InlineCallback {cb}  => {
                    let ev_prop_drctv = cb(event);
                    if cbe.event_prop_directive == EventProp_Undetermined { do_ev_prop = ev_prop_drctv }
                }
                MouseEvCbFn_SpawnedCallback {cb} => {
                    let cb = cb.clone();
                    spawn (move || cb(event));
                }
            }
        } else {
            /*spawn( move || {
                println!("no match for mouse event {:?} in mouse-cb-map", event.ev_src);
                println!("current mouse-cb map is :\n {:#?} ", (MOUSE_CALLBACKS.lock().unwrap()));
            });*/
        }
        if do_ev_prop == EventProp_Stop {
            return LRESULT(1);  // returning with non-zero code signals OS to block further processing on the input event
        }
    }
    return return_call();
}




/// set lower-level hook
fn set_hook(
    hook_id: WINDOWS_HOOK_ID,
    hook_ptr: &AtomicPtr<HHOOK>,
    hook_proc: unsafe extern "system" fn(c_int, WPARAM, LPARAM) -> LRESULT,
) {
    hook_ptr.store(
        unsafe { & mut SetWindowsHookExW (hook_id, Some(hook_proc), HINSTANCE(0), 0) .unwrap() },
        Ordering::Relaxed,
    );
}

/// unset lower-level hook
#[allow(dead_code)]
fn unset_hook(hook_ptr: &AtomicPtr<HHOOK>) {
    if !hook_ptr.load(Ordering::Relaxed).is_null() {
        unsafe { UnhookWindowsHookEx (*hook_ptr.load(Ordering::Relaxed)) };
        hook_ptr.store(null_mut(), Ordering::Relaxed);
    }
}


/// send simulated mouse events to system for injection into events-stream
fn send_mouse_input(flags: MOUSE_EVENT_FLAGS, data: i32, dx: i32, dy: i32) {
    let mut inputs = [ INPUT {
        r#type: INPUT_MOUSE,
        Anonymous: INPUT_0 {
            mi : MOUSEINPUT {
                dx,
                dy,
                mouseData: data,
                dwFlags: flags,
                time: 0,
                dwExtraInfo: FAKE_EXTRA_INFO
            }
        }
    } ];
    unsafe { SendInput(&mut inputs, size_of::<INPUT>() as c_int) };
}

/// send simulated keyboard events to system for injection into events-stream
fn send_keybd_input(key_code: u16, up_not_down:bool, sc_not_vk:bool) {
    let keyup_flag = if up_not_down { KEYEVENTF_KEYUP } else { KEYBD_EVENT_FLAGS(0) };
    let ext_key_flag = if sc_not_vk && key_code > 0xFF { KEYEVENTF_EXTENDEDKEY } else { KEYBD_EVENT_FLAGS(0) };
    let (w_vk, w_sc, sc_flag) = if sc_not_vk {(0, key_code, KEYEVENTF_SCANCODE)} else {(key_code, 0, KEYBD_EVENT_FLAGS(0))};
    let mut inputs = [ INPUT {
        r#type: INPUT_KEYBOARD,
        Anonymous: INPUT_0 {
            ki: KEYBDINPUT {
                wVk: VIRTUAL_KEY(w_vk),
                wScan: w_sc,
                dwFlags: ext_key_flag | sc_flag | keyup_flag,
                time: 0,
                dwExtraInfo: FAKE_EXTRA_INFO,
            }
        }
    } ];
    unsafe { SendInput(&mut inputs, size_of::<INPUT>() as c_int) };
}
