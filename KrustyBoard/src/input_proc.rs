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

impl KbdKey {
    /// Returns true if a given `KeybdKey` is currently pressed (in the down position).
    pub fn is_pressed(self) -> bool {
        (unsafe { GetAsyncKeyState(u64::from(self) as i32) } >> 15) != 0
    }

    /// Presses a given `KeybdKey`. Note: this means the key will remain in the down
    /// position. You must manually call release to create a full 'press'.
    pub fn press(self) {
        if u64::from(self) < 0xE0 {
            send_keybd_input(u64::from(self) as u16, false, false);
        } else {
            send_keybd_input(u64::from(self) as u16, false, true);
        }
    }

    /// Releases a given `KeybdKey`. This means the key would be in the up position.
    pub fn release(self) {
        if u64::from(self) < 0xE0 {
            send_keybd_input(u64::from(self) as u16, true, false);
        } else {
            send_keybd_input(u64::from(self) as u16, true, true);
        }
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
            MouseButton::LeftButton => send_mouse_input(MOUSEEVENTF_LEFTDOWN, 0, 0, 0),
            MouseButton::RightButton => send_mouse_input(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0),
            MouseButton::MiddleButton => send_mouse_input(MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0),
            _ => {}
        }
    }

    /// Releases a given `MouseButton`. This means the button would be in the up position.
    pub fn release(self) {
        match self {
            MouseButton::LeftButton => send_mouse_input(MOUSEEVENTF_LEFTUP, 0, 0, 0),
            MouseButton::RightButton => send_mouse_input(MOUSEEVENTF_RIGHTUP, 0, 0, 0),
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
            MouseWheel::DefaultWheel => Some(MOUSEEVENTF_WHEEL),
            MouseWheel::HorizontalWheel => Some(MOUSEEVENTF_HWHEEL),
            _ => None
        } {
            send_mouse_input(flag, delta, 0, 0);
        }
    }
}




/// Starts listening for bound input events.
pub fn handle_input_events() {
    if !MOUSE_CALLBACKS.lock().unwrap().is_empty() {
        set_hook(WH_MOUSE_LL, &*MOUSE_HHOOK, mouse_proc);
    };
    if !KEYBD_CALLBACKS.lock().unwrap().is_empty() {
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
fn keybd_proc(code: c_int, w_param: WPARAM, l_param: LPARAM) -> LRESULT {

    let return_call = || { CallNextHookEx(HHOOK(0), code, w_param, l_param) };

    if code < 0 { return return_call() }      // ms-docs says we MUST do this, so ig k fine

    if KEYBD_CALLBACKS.lock().unwrap().is_empty() {
        unset_hook(&*KEYBD_HHOOK);
        return return_call();
    }

    let kb_struct = *(l_param.0 as *const KBDLLHOOKSTRUCT);

    /*
    println!("code: {:X}, w_param: {:X}, vk_code: {:#06X}, scanCode: {:#06X}, flags: {:X}, time: {}, dwExtraInfo: {:X}",
             code, w_param,
             kb_struct.vkCode, kb_struct.scanCode, kb_struct.flags, kb_struct.time, kb_struct.dwExtraInfo);
    */

    // if we injected this event ourselves, we should just bail
    if kb_struct.dwExtraInfo == FAKE_EXTRA_INFO { return return_call() }

    let (event, ev_is_key_down) = match w_param.0 as u32 {
        WM_KEYDOWN      => ( KbdEvntType::KbdEvntKeyDown, true ),
        WM_SYSKEYDOWN   => ( KbdEvntType::KbdEvntSysKeyDown, true ),
        WM_KEYUP        => ( KbdEvntType::KbdEvntKeyUp, false ),
        WM_SYSKEYUP     => ( KbdEvntType::KbdEvntSysKeyUp, false ),
        _               => ( KbdEvntType::Unrecognized, false ),
    };
    let key = KbdKey::from(u64::from(kb_struct.vkCode));
    let cb_lookup_key = KbdEvntCbMapKey::from_key_state(key, KbdEvntCbMapKeyType::from_key_down_state(ev_is_key_down));

    if let Some (cbv) = KEYBD_CALLBACKS.lock().unwrap().get(&cb_lookup_key) {
        let kbd_event = KbdEvent { event, vk_code: kb_struct.vkCode as u32, sc_code: kb_struct.scanCode as u32 };
        //println!("{:?}", kbd_event);
        match cbv {
            KbdEvntCallback::NonBlockingCallback {cb} => {
                let cb = Arc::clone(cb);
                spawn(move || cb(kbd_event));
                // returns at end as usual
            }
            KbdEvntCallback::BlockingCallback {cb} => {
                let cb = Arc::clone(cb);
                spawn(move || cb(kbd_event));
                return LRESULT(1); // return with non-zero (blocking) code
            }
            KbdEvntCallback::BlockableCallback {cb} => {
                if let BlockInput::Block = cb(kbd_event) {
                    return LRESULT(1);
                } // else falls back and returns at end
            }
        }
    }
    return return_call()
}


fn HIWORD(l: u32) -> u16 { ((l >> 16) & 0xffff) as u16 }

/// mouse lower-level-hook processor
unsafe extern "system"
fn mouse_proc(code: c_int, w_param: WPARAM, l_param: LPARAM) -> LRESULT {

    use crate::{MouseButton::*, MouseWheel::*};

    let return_call = || { CallNextHookEx(HHOOK(0), code, w_param, l_param) };

    if code < 0 { return return_call() }      // ms-docs says we MUST do this, so ig k fine

    if MOUSE_CALLBACKS.lock().unwrap().is_empty() {
        unset_hook(&*MOUSE_HHOOK);
        return return_call();
    }

    let mh_struct = &*(l_param.0 as *const MSLLHOOKSTRUCT);

    if mh_struct.dwExtraInfo == FAKE_EXTRA_INFO {
        // if we injected it, we should just bail (and call the next guy down the line)
        return return_call()
    }

    let ev_and_key_for_btn_ev = |btn:MouseButton, down_not_up:bool| {
        Some ( ( MouseEvent::from_btn_ev(btn, down_not_up), MouseEventCbMapKey::for_btn_action(btn, down_not_up) ) )
    };
    let ev_and_key_for_wheel_ev = |wheel:MouseWheel, delta:i32| {
        Some ( ( MouseEvent::from_wheel_ev(wheel, delta), MouseEventCbMapKey::for_wheel_action(wheel, delta >= 0) ) )
    };

    //println!("{:#?}", mh_struct);

    if let Some ((event, lookup_key)) = match w_param.0 as u32 {
        WM_LBUTTONDOWN => ev_and_key_for_btn_ev(LeftButton, true),
        WM_RBUTTONDOWN => ev_and_key_for_btn_ev(RightButton, true),
        WM_MBUTTONDOWN => ev_and_key_for_btn_ev(MiddleButton, true),
        WM_XBUTTONDOWN => {
            match MOUSEHOOKSTRUCTEX_MOUSE_DATA ( HIWORD (mh_struct.mouseData.0) as u32) {
                XBUTTON1 => ev_and_key_for_btn_ev(X1Button, true),
                XBUTTON2 => ev_and_key_for_btn_ev(X2Button, true),
                _ => None,
            }
        }
        WM_LBUTTONUP => ev_and_key_for_btn_ev(LeftButton, false),
        WM_RBUTTONUP => ev_and_key_for_btn_ev(RightButton, false),
        WM_MBUTTONUP => ev_and_key_for_btn_ev(MiddleButton, false),
        WM_XBUTTONUP => {
            match MOUSEHOOKSTRUCTEX_MOUSE_DATA ( HIWORD (mh_struct.mouseData.0) as u32) {
                XBUTTON1 => ev_and_key_for_btn_ev(X1Button, false),
                XBUTTON2 => ev_and_key_for_btn_ev(X2Button, false),
                _ => None,
            }
        }
        WM_MOUSEWHEEL  => ev_and_key_for_wheel_ev (DefaultWheel, HIWORD(mh_struct.mouseData.0) as i16 as i32),
        WM_MOUSEHWHEEL => ev_and_key_for_wheel_ev (HorizontalWheel, HIWORD(mh_struct.mouseData.0) as i16 as i32),

        //WM_MOUSEMOVE   => MouseEvent::from_pointer_ev (??, ??)
        _ => None,
    } {
        //{ let ec = event.clone(); spawn(move || println!("{:?}", ec)); }   // for debug
        if let Some(cbv) = MOUSE_CALLBACKS.lock().unwrap().get(&lookup_key) {
            match cbv {
                MouseEventCallback::NonBlockingCallback {cb} => {
                    let cb = Arc::clone(cb);
                    spawn(move || cb(event));
                }
                MouseEventCallback::BlockingCallback {cb} => {
                    let cb = Arc::clone(cb);
                    spawn(move || cb(event));
                    return LRESULT(1);
                }
                MouseEventCallback::BlockableCallback {cb} => {
                    if let BlockInput::Block = cb(event) {
                        return LRESULT(1);
                    }
                }
            }
        } else {
            /*spawn( move || {
                println!("no match for mouse event {:?} in mouse-cb-map", event.ev_src);
                println!("current mouse-cb map is :\n {:#?} ", (MOUSE_CALLBACKS.lock().unwrap()));
            });*/
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
