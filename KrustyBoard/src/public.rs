use crate::common::*;
use std::{thread::sleep, time::Duration};

use strum::IntoEnumIterator;
use strum_macros::EnumIter;

pub enum BlockInput {
    Block,
    DontBlock,
}


#[allow(non_camel_case_types)]
#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter)]
pub enum KbdKey {
    MouseLeftBtn,
    MouseRightBtn,
    MouseMiddleBtn,
    MouseX1Btn,
    MouseX2Btn,
    Backspace,
    Tab,
    Enter,
    Shift,
    Ctrl,
    Alt,
    Escape,
    Space,
    PageUp,
    PageDown,
    End,
    Home,
    Left,
    Up,
    Right,
    Down,
    Insert,
    Delete,
    Numrow_0,
    Numrow_1,
    Numrow_2,
    Numrow_3,
    Numrow_4,
    Numrow_5,
    Numrow_6,
    Numrow_7,
    Numrow_8,
    Numrow_9,
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
    P,
    Q,
    R,
    S,
    T,
    U,
    V,
    W,
    X,
    Y,
    Z,
    LWin,
    RWin,
    Apps,
    Sleep,
    Numpad_0,
    Numpad_1,
    Numpad_2,
    Numpad_3,
    Numpad_4,
    Numpad_5,
    Numpad_6,
    Numpad_7,
    Numpad_8,
    Numpad_9,
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    F11,
    F12,
    F13,
    F14,
    F15,
    F16,
    F17,
    F18,
    F19,
    F20,
    F21,
    F22,
    F23,
    F24,
    NumLock,
    ScrollLock,
    CapsLock,
    LShift,
    RShift,
    LControl,
    RControl,
    LAlt,
    RAlt,
    BrowserBack,
    BrowserForward,
    BrowserRefresh,
    VolumeMute,
    VolumeDown,
    VolumeUp,
    MediaNextTrack,
    MediaPrevTrack,
    MediaStop,
    MediaPlayPause,
    Backquote,
    Slash,
    Backslash,
    Comma,
    Period,
    Minus,
    Quote,
    Semicolon,
    LBracket,
    RBracket,
    Equal,

    // the following are extended keys (with e0 prefixed scan code) .. supposed to work regardless of Shift state
    // todo: these really shouldnt be here, as these are scan-codes, not v-codes .. we gotta split these up

    ExtHome,
    ExtUp,
    ExtPgUp,
    ExtLeft,
    ExtRight,
    ExtEnd,
    ExtDown,
    ExtPgDn,
    ExtInsert,
    ExtDelete,

    #[strum(disabled)]
    OtherKey(u64),
}



impl KbdKey {
    pub fn non_blocking_bind<F> (self, key_state: KbdEvntCbMapKeyType, callback: F)
        where F: Fn(KbdEvent) + Send + Sync + 'static,
    {
        KEYBD_CALLBACKS.lock().unwrap().insert(
            KbdEvntCbMapKey::from_key_state(self, key_state),
            KbdEvntCallback::NonBlockingCallback { cb: Arc::new(callback) }
        );
    }

    pub fn block_bind<F>(self, key_state: KbdEvntCbMapKeyType, callback: F)
        where F: Fn(KbdEvent) + Send + Sync + 'static,
    {
        KEYBD_CALLBACKS.lock().unwrap().insert(
            KbdEvntCbMapKey::from_key_state(self, key_state),
            KbdEvntCallback::BlockingCallback { cb : Arc::new(callback) }
        );
    }

    pub fn blockable_bind<F>(self, key_state: KbdEvntCbMapKeyType, callback: F)
        where F: Fn(KbdEvent) -> BlockInput + Send + Sync + 'static,
    {
        KEYBD_CALLBACKS.lock().unwrap().insert(
            KbdEvntCbMapKey::from_key_state(self, key_state),
            KbdEvntCallback::BlockableCallback { cb : Arc::new(callback) }
        );
    }

    pub fn unbind(self, key_state: KbdEvntCbMapKeyType) {
        let cb_map_key = KbdEvntCbMapKey::from_key_state(self, key_state);
        KEYBD_CALLBACKS.lock().unwrap().remove(&cb_map_key);
    }
}

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

impl MouseButton {
    pub fn non_blocking_bind<F>(self, down_not_up:bool, callback: F)
        where F: Fn(MouseEvent) + Send + Sync + 'static,
    {
        MOUSE_CALLBACKS .lock() .unwrap() .insert(
            MouseEventCbMapKey::for_btn_action(self, down_not_up),
            MouseEventCallback::NonBlockingCallback { cb : Arc::new(callback) }
        );
    }

    pub fn block_bind<F>(self, down_not_up:bool, callback: F)
        where F: Fn(MouseEvent) + Send + Sync + 'static,
    {
        MOUSE_CALLBACKS .lock() .unwrap() .insert(
            MouseEventCbMapKey::for_btn_action(self, down_not_up),
            MouseEventCallback::BlockingCallback { cb : Arc::new(callback) }
        );
    }

    pub fn blockable_bind<F>(self, down_not_up:bool, callback: F)
        where F: Fn(MouseEvent) -> BlockInput + Send + Sync + 'static,
    {
        MOUSE_CALLBACKS .lock() .unwrap() .insert(
            MouseEventCbMapKey::for_btn_action(self, down_not_up),
            MouseEventCallback::BlockableCallback { cb : Arc::new(callback) }
        );
    }

    pub fn unbind(self, down_not_up:bool) {
        let cb_map_key = MouseEventCbMapKey::for_btn_action(self, down_not_up);
        MOUSE_CALLBACKS.lock().unwrap().remove(&cb_map_key);
    }
}




#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter)]
pub enum MouseWheel {
    DefaultWheel,
    HorizontalWheel,

    #[strum(disabled)]
    OtherWheel(u32),
}


impl MouseWheel {
    pub fn non_blocking_bind<F>(self, fwd_not_bkwd:bool, callback: F)
        where F: Fn(MouseEvent) + Send + Sync + 'static,
    {
        MOUSE_CALLBACKS .lock() .unwrap() .insert(
            MouseEventCbMapKey::for_wheel_action(self, fwd_not_bkwd),
            MouseEventCallback::NonBlockingCallback { cb : Arc::new(callback) }
        );
    }

    pub fn block_bind<F>(self, fwd_not_bkwd:bool, callback: F)
        where F: Fn(MouseEvent) + Send + Sync + 'static,
    {
        MOUSE_CALLBACKS .lock() .unwrap() .insert(
            MouseEventCbMapKey::for_wheel_action(self, fwd_not_bkwd),
            MouseEventCallback::BlockingCallback { cb : Arc::new(callback) }
        );
    }

    pub fn blockable_bind<F>(self, fwd_not_bkwd:bool, callback: F)
        where F: Fn(MouseEvent) -> BlockInput + Send + Sync + 'static,
    {
        MOUSE_CALLBACKS .lock() .unwrap() .insert(
            MouseEventCbMapKey::for_wheel_action(self, fwd_not_bkwd),
            MouseEventCallback::BlockableCallback { cb : Arc::new(callback) }
        );
    }

    pub fn unbind(self, fwd_not_bkwd:bool) {
        let cb_map_key = MouseEventCbMapKey::for_wheel_action(self, fwd_not_bkwd);
        MOUSE_CALLBACKS.lock().unwrap().remove(&cb_map_key);
    }
}




#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
pub struct MousePointer;


impl MousePointer {
    pub fn non_blocking_bind<F>(self, callback: F)
        where F: Fn(MouseEvent) + Send + Sync + 'static,
    {
        MOUSE_CALLBACKS .lock() .unwrap() .insert(
            MouseEventCbMapKey::for_pointer(),
            MouseEventCallback::NonBlockingCallback { cb : Arc::new(callback) }
        );
    }

    pub fn unbind(self) {
        let cb_map_key = MouseEventCbMapKey::for_pointer();
        MOUSE_CALLBACKS.lock().unwrap().remove(&cb_map_key);
    }
}



impl KbdKey {
    pub fn get_char(&self) -> Option<char> {
        match self {
            KbdKey::A => Some('a'),
            KbdKey::B => Some('b'),
            KbdKey::C => Some('c'),
            KbdKey::D => Some('d'),
            KbdKey::E => Some('e'),
            KbdKey::F => Some('f'),
            KbdKey::G => Some('g'),
            KbdKey::H => Some('h'),
            KbdKey::I => Some('i'),
            KbdKey::J => Some('j'),
            KbdKey::K => Some('k'),
            KbdKey::L => Some('l'),
            KbdKey::M => Some('m'),
            KbdKey::N => Some('n'),
            KbdKey::O => Some('o'),
            KbdKey::P => Some('p'),
            KbdKey::Q => Some('q'),
            KbdKey::R => Some('r'),
            KbdKey::S => Some('s'),
            KbdKey::T => Some('t'),
            KbdKey::U => Some('u'),
            KbdKey::V => Some('v'),
            KbdKey::W => Some('w'),
            KbdKey::X => Some('x'),
            KbdKey::Y => Some('y'),
            KbdKey::Z => Some('z'),
            KbdKey::Numpad_0 => Some('0'),
            KbdKey::Numpad_1 => Some('1'),
            KbdKey::Numpad_2 => Some('2'),
            KbdKey::Numpad_3 => Some('3'),
            KbdKey::Numpad_4 => Some('4'),
            KbdKey::Numpad_5 => Some('5'),
            KbdKey::Numpad_6 => Some('6'),
            KbdKey::Numpad_7 => Some('7'),
            KbdKey::Numpad_8 => Some('8'),
            KbdKey::Numpad_9 => Some('9'),
            KbdKey::Numrow_0 => Some('0'),
            KbdKey::Numrow_1 => Some('1'),
            KbdKey::Numrow_2 => Some('2'),
            KbdKey::Numrow_3 => Some('3'),
            KbdKey::Numrow_4 => Some('4'),
            KbdKey::Numrow_5 => Some('5'),
            KbdKey::Numrow_6 => Some('6'),
            KbdKey::Numrow_7 => Some('7'),
            KbdKey::Numrow_8 => Some('8'),
            KbdKey::Numrow_9 => Some('9'),
            KbdKey::Backslash => Some('\\'),
            KbdKey::Slash => Some('/'),
            KbdKey::Comma => Some(','),
            KbdKey::Period => Some('.'),
            KbdKey::Minus => Some('-'),
            KbdKey::Quote => Some('"'),
            KbdKey::Semicolon => Some(';'),
            KbdKey::LBracket => Some('['),
            KbdKey::RBracket => Some(']'),
            KbdKey::Equal => Some('='),
            _ => None,
        }
    }

    pub fn from_char(c: char) -> Option<KbdKey> {
        match c {
            ' ' => Some(KbdKey::Space),
            'A' | 'a' => Some(KbdKey::A),
            'B' | 'b' => Some(KbdKey::B),
            'C' | 'c' => Some(KbdKey::C),
            'D' | 'd' => Some(KbdKey::D),
            'E' | 'e' => Some(KbdKey::E),
            'F' | 'f' => Some(KbdKey::F),
            'G' | 'g' => Some(KbdKey::G),
            'H' | 'h' => Some(KbdKey::H),
            'I' | 'i' => Some(KbdKey::I),
            'J' | 'j' => Some(KbdKey::J),
            'K' | 'k' => Some(KbdKey::K),
            'L' | 'l' => Some(KbdKey::L),
            'M' | 'm' => Some(KbdKey::M),
            'N' | 'n' => Some(KbdKey::N),
            'O' | 'o' => Some(KbdKey::O),
            'P' | 'p' => Some(KbdKey::P),
            'Q' | 'q' => Some(KbdKey::Q),
            'R' | 'r' => Some(KbdKey::R),
            'S' | 's' => Some(KbdKey::S),
            'T' | 't' => Some(KbdKey::T),
            'U' | 'u' => Some(KbdKey::U),
            'V' | 'v' => Some(KbdKey::V),
            'W' | 'w' => Some(KbdKey::W),
            'X' | 'x' => Some(KbdKey::X),
            'Y' | 'y' => Some(KbdKey::Y),
            'Z' | 'z' => Some(KbdKey::Z),
            '0' | ')' => Some(KbdKey::Numrow_0),
            '1' | '!' => Some(KbdKey::Numrow_1),
            '2' | '@' => Some(KbdKey::Numrow_2),
            '3' | '#' => Some(KbdKey::Numrow_3),
            '4' | '$' => Some(KbdKey::Numrow_4),
            '5' | '%' => Some(KbdKey::Numrow_5),
            '6' | '^' => Some(KbdKey::Numrow_6),
            '7' | '&' => Some(KbdKey::Numrow_7),
            '8' | '*' => Some(KbdKey::Numrow_8),
            '9' | '(' => Some(KbdKey::Numrow_9),
            '`' | '~' => Some(KbdKey::Backquote),
            '/' | '?' => Some(KbdKey::Slash),
            ',' | '<' => Some(KbdKey::Comma),
            '.' | '>' => Some(KbdKey::Period),
            '-' | '_' => Some(KbdKey::Minus),
            ';' | ':' => Some(KbdKey::Semicolon),
            '[' | '{' => Some(KbdKey::LBracket),
            ']' | '}' => Some(KbdKey::RBracket),
            '=' | '+' => Some(KbdKey::Equal),
            '\\' | '|' => Some(KbdKey::Backslash),
            '\'' | '"' => Some(KbdKey::Quote),
            _ => None,
        }
    }
}

pub struct KeySequence(pub &'static str);

impl KeySequence {
    pub fn send(&self) {
        for c in self.0.chars() {
            let mut uppercase = false;

            if let Some(keybd_key) = {
                if c.is_uppercase()
                    || [
                        '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', '{', '}', '|',
                        ':', '"', '<', '>', '?', '~',
                    ]
                    .contains(&c)
                {
                    uppercase = true;
                }

                KbdKey::from_char(c)
            } {
                if uppercase {
                    KbdKey::LShift.press();
                }

                keybd_key.press();
                sleep(Duration::from_millis(20));
                keybd_key.release();

                if uppercase {
                    KbdKey::LShift.release();
                }
            };
        }
    }
}
