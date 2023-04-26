
// allow non camel-case names for this entire file
#![allow(non_camel_case_types)]


use std::mem;
use std::os::raw::c_int;

use strum_macros::EnumIter;

use windows::Win32::Foundation::POINT;
use windows::Win32::UI::WindowsAndMessaging::{GetCursorPos, SetCursorPos};
use windows::Win32::UI::Input::KeyboardAndMouse::{
    GetAsyncKeyState, INPUT, INPUT_0, MOUSEINPUT, SendInput, MOUSE_EVENT_FLAGS, INPUT_MOUSE, MOUSEEVENTF_ABSOLUTE,
    MOUSEEVENTF_HWHEEL, MOUSEEVENTF_LEFTDOWN, MOUSEEVENTF_LEFTUP, MOUSEEVENTF_MIDDLEDOWN, MOUSEEVENTF_MIDDLEUP,
    MOUSEEVENTF_RIGHTDOWN, MOUSEEVENTF_RIGHTUP, MOUSEEVENTF_WHEEL,
};

use crate::*;


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
pub enum MouseBtnEvent_T {
    BtnDown,
    BtnUp,
    DblClick,
}

/// Mouse event can be a btn-event, wheel-event, or pointer-move (with their associated data)
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum MouseEvent {
    btn_event   { src_btn:MouseButton, ev_t:MouseBtnEvent_T, stamp:u32, injected:bool },
    wheel_event { src_wheel:MouseWheel, delta:i32, stamp:u32, injected:bool },
    move_event  { x_pos:i32, y_pos:i32, stamp:u32, injected:bool },
}
impl MouseEvent {
    pub fn get_stamp(&self) -> u32 {
        use MouseEvent::*;
        match self {
            btn_event   {stamp, ..} => *stamp,
            wheel_event {stamp, ..} => *stamp,
            move_event  {stamp, ..} => *stamp,
        }
    }
    pub fn check_injected (&self) -> bool {
        use MouseEvent::*;
        match self {
            btn_event   {injected, ..} => *injected,
            wheel_event {injected, ..} => *injected,
            move_event  {injected, ..} => *injected,
        }
    }
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
        let mut ev_flag = match self {
            LeftButton   => MOUSEEVENTF_LEFTDOWN,
            RightButton  => MOUSEEVENTF_RIGHTDOWN,
            MiddleButton => MOUSEEVENTF_MIDDLEDOWN,
            _ => MOUSE_EVENT_FLAGS(0),
        };
        if abs { ev_flag = ev_flag | MOUSEEVENTF_ABSOLUTE }
        send_mouse_input (ev_flag, 0, x, y)
    }
    pub fn press (self) { self._press (0, 0, false) }
    pub fn press_at (self, x:i32, y:i32) { self._press (x, y, true) }

    /// Releases a given `MouseButton`. This will leave the button in the up position.
    fn _release (self, x:i32, y:i32, abs:bool) {
        use MouseButton::*;
        let mut ev_flag = match self {
            LeftButton   => MOUSEEVENTF_LEFTUP,
            RightButton  => MOUSEEVENTF_RIGHTUP,
            MiddleButton => MOUSEEVENTF_MIDDLEUP,
            _ => MOUSE_EVENT_FLAGS(0),
        };
        if abs { ev_flag = ev_flag | MOUSEEVENTF_ABSOLUTE }
        send_mouse_input (ev_flag, 0, x, y)
    }
    pub fn release (self) { self._release (0, 0, false) }
    pub fn release_at (self, x:i32, y:i32) { self._release (x, y, true) }

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
                dwExtraInfo: FAKE_EXTRA_INFO
        } }
    } ];

    unsafe {
        SendInput (&mut inputs, mem::size_of::<INPUT>() as c_int)
    };

}

