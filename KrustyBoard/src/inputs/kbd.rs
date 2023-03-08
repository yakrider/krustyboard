

use std::{
    thread::sleep,
    time::Duration,
    mem::size_of,
    collections::HashMap,
    os::raw::c_int,
};
use windows::Win32::{
    UI::Input::KeyboardAndMouse::*,
};
use once_cell::sync::Lazy;


use crate::*;


/// Enum representation of all the Keyboard Keys
// we've put actual KbdKey def in kbd_codes just because its such a long enum, but we'll re-export it from here!
pub use super::kbd_codes::KbdKey;

/// representation for hotstrings that can be sent as a sequence of keys
pub struct KeySequence (pub &'static str);



/// The keyboard even type is the OS provided down/up or sys-down/up (which fires when Alt is held down etc)
#[allow(non_camel_case_types)]
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum KbdEventType {
    KbdEvent_KeyDown,
    KbdEvent_SysKeyDown,
    KbdEvent_KeyUp,
    KbdEvent_SysKeyUp,
}


/// The KbdEvent includes the OS provided scan-code and virtual-key-code as well as the KbdKey representation and KbdEventType
#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub struct KbdEvent {
    pub ev_t    : KbdEventType,
    pub key     : KbdKey,
    pub vk_code : u32,
    pub sc_code : u32,
}


/// Intended as a temp hack to send (possibly extended) scan-codes instead of virtual-key-codes for left/right modifier-keys.
/// .. In particular since machines dont seem to be consistent in what they send, and this ensures the L/R key nature is consistent
static KEY_SWAPS_MAP: Lazy<HashMap<KbdKey,u64>> = Lazy::new ( || {
    [   (KbdKey::RAlt, 0xE038 as u64), (KbdKey::RCtrl, 0xE01D as u64) , (KbdKey::RShift, 0x0036 as u64),
        (KbdKey::LAlt, 0x0038 as u64), (KbdKey::LCtrl, 0x001D as u64) , (KbdKey::LShift, 0x002A as u64)
    ] .into_iter() .collect::<HashMap<KbdKey,u64>>()
} );





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
                dwExtraInfo: FAKE_EXTRA_INFO,
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

