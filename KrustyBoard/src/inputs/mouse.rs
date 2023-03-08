
// allow non camel-case names for this entire file
#![allow(non_camel_case_types)]


use std::{
    mem::{size_of, MaybeUninit},
    os::raw::c_int,
};
use windows::Win32::{
    UI::Input::KeyboardAndMouse::*,
    UI::WindowsAndMessaging::*,
};
use strum_macros::EnumIter;

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

/// For the mouse-wheel, event types can be wheel-forward, or wheel-backward
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum MouseWheelEvent_T {
    WheelForward,
    WheelBackward,
}

/// Mouse event can be a btn-event, wheel-event, or pointer-move (with their associated data)
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub enum MouseEvent {
    btn_event   { src_btn:MouseButton, ev_t:MouseBtnEvent_T },
    wheel_event { src_wheel:MouseWheel, delta:i32 },
    move_event  { x_pos:i32, y_pos:i32 },
}





impl MouseButton {

    /// Returns true if a given `MouseButton` is currently pressed (in the down position).
    pub fn is_pressed(self) -> bool {
        (unsafe { GetAsyncKeyState (u32::from(self) as i32) } >> 0x0F) != 0
    }

    /// Presses a given `MouseButton`. Note: this will leave the button in the down position.
    /// One must then call release to complete a typical btn-down/btn-up sequence.
    pub fn press(self) {
        use MouseButton::*;
        match self {
            LeftButton   => send_mouse_input (MOUSEEVENTF_LEFTDOWN,   0, 0, 0),
            RightButton  => send_mouse_input (MOUSEEVENTF_RIGHTDOWN,  0, 0, 0),
            MiddleButton => send_mouse_input (MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0),
            _ => {}
        }
    }

    /// Releases a given `MouseButton`. This will leave the button in the up position.
    pub fn release(self) {
        use MouseButton::*;
        match self {
            LeftButton   => send_mouse_input (MOUSEEVENTF_LEFTUP,   0, 0, 0),
            RightButton  => send_mouse_input (MOUSEEVENTF_RIGHTUP,  0, 0, 0),
            MiddleButton => send_mouse_input (MOUSEEVENTF_MIDDLEUP, 0, 0, 0),
            _ => {}
        }
    }

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
        SendInput (&mut inputs, size_of::<INPUT>() as c_int)
    };

}

