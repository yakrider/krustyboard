
// allow non camel-case names for this entire file
#![allow(non_camel_case_types)]


use std::{
    thread::sleep,
    time::Duration,
    mem::size_of,
    os::raw::c_int,
};
use strum_macros::EnumIter;

use windows::Win32::Foundation::POINT;
use windows::Win32::UI::WindowsAndMessaging::{GetCursorPos, SetCursorPos};
use windows::Win32::UI::Input::KeyboardAndMouse::*;
use once_cell::sync::Lazy;
use rustc_hash::FxHashMap;


use crate::*;


/// Enum representation of all the Keyboard Keys
// we've put actual KbdKey def in kbd_codes just because its such a long enum, but we'll re-export it from here!
pub use super::kbd_codes::KbdKey;

/// representation for hotstrings that can be sent as a sequence of keys
pub struct KeySequence (pub &'static str);



/// The keyboard even type is the OS provided down/up or sys-down/up (which fires when Alt is held down etc)
#[allow(non_camel_case_types)]
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum KbdEvent_T {
    KbdEvent_KeyDown,
    KbdEvent_SysKeyDown,
    KbdEvent_KeyUp,
    KbdEvent_SysKeyUp,
}

/// Intended as a temp hack to send (possibly extended) scan-codes instead of virtual-key-codes for left/right modifier-keys.
/// .. In particular since machines dont seem to be consistent in what they send, and this ensures the L/R key nature is consistent
static KEY_SWAPS_MAP: Lazy<FxHashMap<KbdKey,u64>> = Lazy::new ( || {
    [   (KbdKey::RAlt, 0xE038 as u64), (KbdKey::RCtrl, 0xE01D as u64) , (KbdKey::RShift, 0x0036 as u64),
        (KbdKey::LAlt, 0x0038 as u64), (KbdKey::LCtrl, 0x001D as u64) , (KbdKey::LShift, 0x002A as u64)
    ] .into_iter() .collect::<FxHashMap<KbdKey,u64>>()
} );




/// Representation for left/right/middle and X1/X2 mouse buttons .. others are not fully supported
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter)]
pub enum MouseButton {
    LeftButton,
    MiddleButton,
    RightButton,
    X1Button,
    X2Button,

    #[strum(disabled)]
    OtherButton(u32),
}

/// Representation for the normal (vertical) and horizontal scroll wheels
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter)]
pub enum MouseWheel {
    DefaultWheel,
    HorizontalWheel,

    #[strum(disabled)]
    OtherWheel(u32),
}


/// The mouse-pointer type (with no other property, siimply to differentiate from other mouse-event sources)
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub struct MousePointer;




/// For the mouse-btn, event types can be btn-down, btn-up, or double-click
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum MouseBtnEv_T {
    BtnDown,
    BtnUp,
    //DblClick,
}


/// For the mouse-wheel, event types can be wheel-forwards or wheel-backwards
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum MouseWheelEv_T {
    WheelForwards,
    WheelBackwards,
}
impl From<i32> for MouseWheelEv_T {
    fn from (delta: i32) -> Self {
        if delta > 0 { Self::WheelForwards } else { Self::WheelBackwards }
    }
}




/// Input event can be a kbd-key-event, mouse-btn-event, mouse-wheel-event, or mouse-pointer-move (with their associated data)
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum EventDat {
    key_event   { key:KbdKey, ev_t: KbdEvent_T, vk_code:u32, sc_code:u32 },
    btn_event   { btn:MouseButton, ev_t:MouseBtnEv_T },
    wheel_event { wheel:MouseWheel, delta:i32 },
    move_event  { x_pos:i32, y_pos:i32 },
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub struct Event {
    pub stroke_id : usize,
    pub stamp : u32,
    pub injected : bool,
    pub extra_info : usize,
    pub dat : EventDat,
}





impl KbdKey {

    /// Returns true if a given `KeybdKey` is currently pressed (in the down position).
    pub fn is_pressed (self) -> bool {
        (unsafe { GetAsyncKeyState (u64::from(self) as i32) } >> 0x0F) != 0
    }

    /// Presses a given `KeybdKey`. Note: this means the key will remain in the down position.
    /// One must then call release to create to complete a typical key-down/key-up sequence
    pub fn press (self) { self.send_key_event(false) }

    /// Releases a given `KeybdKey`. This means the key would be in the up position.
    pub fn release (self) { self.send_key_event(true) }

    /// Pesses then Releases a given `KeybdKey`.
    pub fn press_release (self) { self.press(); self.release(); }

    fn send_key_event (self, ev_is_up:bool) {
        // todo : prob build a separate sc-code mechanism so dont have to do hacks like these
        let (code, sc_not_vk) = {
            KEY_SWAPS_MAP .get(&self) .map (|c| (*c, true)) .or (Some((u64::from(self), false)))
                .map (|(c,b)| (c, b || c >= 0xE0)) .unwrap()
        };
        send_keybd_input (code as u16, ev_is_up, sc_not_vk);
    }

    /// Returns true if a keyboard key which supports toggling (ScrollLock, NumLock, CapsLock) is on.
    pub fn is_toggled(self) -> bool {
        unsafe { GetKeyState (u64::from(self) as i32) & 0x0F != 0 }
    }

    /// Returns true if this key is a 'modifier' key (one of left/right/generic versions of [Alt, Ctrl, Shift, Win]
    pub fn is_modifier_key (self) -> bool {
        use KbdKey::*;
        match self {
            LAlt | RAlt | Alt | LCtrl | RCtrl | Ctrl | LShift | RShift | Shift | LWin | RWin => true,
            _ => false
        }
    }

}





/// Sends simulated keyboard events to OS for injection into events-stream
fn send_keybd_input (key_code: u16, up_not_down:bool, sc_not_vk:bool) {

    let keyup_flag = {
        if up_not_down { KEYEVENTF_KEYUP } else { KEYBD_EVENT_FLAGS(0) }
    };
    let ext_key_flag = {
        if sc_not_vk && key_code > 0xFF { KEYEVENTF_EXTENDEDKEY } else { KEYBD_EVENT_FLAGS(0) }
    };
    let (w_vk, w_sc, sc_flag) = {
        if sc_not_vk { (0, key_code, KEYEVENTF_SCANCODE) } else { (key_code, 0, KEYBD_EVENT_FLAGS(0)) }
    };
    let mut inputs = [ INPUT {
        r#type: INPUT_KEYBOARD,
        Anonymous: INPUT_0 {
            ki: KEYBDINPUT {
                wVk: VIRTUAL_KEY(w_vk),
                wScan: w_sc,
                dwFlags: ext_key_flag | sc_flag | keyup_flag,
                time: 0,
                dwExtraInfo: KRUSTY_INJECTED_IDENTIFIER_EXTRA_INFO,
        } }
    } ];

    unsafe { SendInput (&mut inputs, size_of::<INPUT>() as c_int) };

}





impl KeySequence {

    pub fn send(&self) {
        static UPPER_SYMBOLS: [char; 21] = [
            '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', '{', '}', '|',
            ':', '"', '<', '>', '?', '~',
        ];
        for c in self.0.chars() {
            if let Some(keybd_key) = KbdKey::from_char(c) {
                let uppercase = c.is_uppercase() || UPPER_SYMBOLS.contains(&c);
                if uppercase { KbdKey::LShift.press(); }
                keybd_key.press();
                sleep(Duration::from_millis(20));
                keybd_key.release();
                if uppercase { KbdKey::LShift.release(); }
            };
    } }

}





impl MouseButton {

    /// Returns true if a given `MouseButton` is currently pressed (in the down position).
    pub fn is_pressed(self) -> bool {
        (unsafe { GetAsyncKeyState (u32::from(self) as i32) } >> 0x0F) != 0
    }

    /// Presses a given `MouseButton`. Note: this will leave the button in the down position.
    /// One must then call release to complete a typical btn-down/btn-up sequence.
    fn _press (self, x:i32, y:i32, abs:bool) {
        use MouseButton::*;
        let (mut ev_flag, data) = match self {
            LeftButton   => (MOUSEEVENTF_LEFTDOWN,    0i32),
            RightButton  => (MOUSEEVENTF_RIGHTDOWN,   0),
            MiddleButton => (MOUSEEVENTF_MIDDLEDOWN,  0),
            X1Button     => (MOUSEEVENTF_XDOWN,       1),
            X2Button     => (MOUSEEVENTF_XDOWN,       2),
            _            => (MOUSE_EVENT_FLAGS(0),    0),
        };
        if abs { ev_flag = ev_flag | MOUSEEVENTF_ABSOLUTE }
        send_mouse_input (ev_flag, data, x, y)
    }
    pub fn press (self) { self._press (0, 0, false) }
    pub fn press_at (self, x:i32, y:i32) { self._press (x, y, true) }

    /// Releases a given `MouseButton`. This will leave the button in the up position.
    fn _release (self, x:i32, y:i32, abs:bool) {
        use MouseButton::*;
        let (mut ev_flag, data) = match self {
            LeftButton   => (MOUSEEVENTF_LEFTUP,    0i32),
            RightButton  => (MOUSEEVENTF_RIGHTUP,   0),
            MiddleButton => (MOUSEEVENTF_MIDDLEUP,  0),
            X1Button     => (MOUSEEVENTF_XUP,       1),
            X2Button     => (MOUSEEVENTF_XUP,       2),
            _            => (MOUSE_EVENT_FLAGS(0),  0),
        };
        if abs { ev_flag = ev_flag | MOUSEEVENTF_ABSOLUTE }
        send_mouse_input (ev_flag, data, x, y)
    }
    pub fn release (self) { self._release (0, 0, false) }
    pub fn release_at (self, x:i32, y:i32) { self._release (x, y, true) }

    pub fn press_release (self) { self.press(); self.release(); }

}


impl From<MouseButton> for u32 {

    fn from(button: MouseButton) -> u32 {
        use MouseButton::*;
        match button {
            LeftButton    => 0x01,
            RightButton   => 0x02,
            MiddleButton  => 0x04,
            X1Button      => 0x05,
            X2Button      => 0x06,
            OtherButton(code) => code,
        }
    }

}





impl MouseWheel {

    /// Scrolls the mouse wheel (whether default or horiz wheel) by given delta .. note that typically 120 delta is 1 incr
    pub fn scroll(self, delta: i32) {
        use MouseWheel::*;
        if let Some(flag) = match self {
            DefaultWheel    => Some (MOUSEEVENTF_WHEEL ),
            HorizontalWheel => Some (MOUSEEVENTF_HWHEEL),
            _ => None
        } {
            send_mouse_input(flag, delta, 0, 0);
        }
    }

}





impl MousePointer {

    pub fn pos() -> POINT {
        unsafe {
            let mut point = POINT::default();
            GetCursorPos (&mut point);
            point
        }
    }

    /// Moves the mouse relative to its current position by a given amount of pixels.
    pub fn move_rel (dx: i32, dy: i32) {
        let p = Self::pos();
        Self::move_abs (p.x + dx, p.y + dy);
    }

    /// Moves the mouse to a given position based on absolute coordinates. The top left
    /// corner of the screen is (0, 0).
    pub fn move_abs (x: i32, y: i32) {
        unsafe {
            SetCursorPos (x, y);
        }
    }

}





/// Send simulated mouse events to OS for injection into events-stream
fn send_mouse_input (flags: MOUSE_EVENT_FLAGS, data: i32, dx: i32, dy: i32) {

    let mut inputs = [ INPUT {
        r#type: INPUT_MOUSE,
        Anonymous: INPUT_0 {
            mi : MOUSEINPUT {
                dx,
                dy,
                mouseData: data,
                dwFlags: flags,
                time: 0,
                dwExtraInfo: KRUSTY_INJECTED_IDENTIFIER_EXTRA_INFO
        } }
    } ];

    unsafe {
        SendInput (&mut inputs, size_of::<INPUT>() as c_int)
    };

}

