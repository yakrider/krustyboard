#![allow (non_camel_case_types, non_snake_case, non_upper_case_globals)]

use chrono::Local;
use colored::*;
use crossbeam_channel::{unbounded, Receiver, Sender};

use std::sync::LazyLock;
use std::{mem, thread, time::Instant};
use std::collections::HashSet;
use std::time::Duration;
use windows::Win32::{
    Foundation::{GetLastError, LPARAM, LRESULT, WPARAM},
    UI::Input::KeyboardAndMouse::*,
    UI::WindowsAndMessaging::*,
};


mod keys;
use keys::VKey;


#[derive (Debug, Default, Clone, Eq, PartialEq)]
struct Mods {
    alt   : bool,
    ctrl  : bool,
    win   : bool,
    shift : bool,
}


#[derive (Debug, Default, Clone, Copy, PartialEq, Eq)]
enum Injected {
    #[default]
    None,
    Repeat,
    Krusty, // Injected, Krusty dwExtraInfo
    Other,  // Injected, unrecogized dwExtraInfo
}


#[allow (dead_code)]
#[derive (Debug, Default, Clone, Eq, PartialEq)]
struct Event {
    vk_code  : u32,
    sc_code  : u32,
    is_press : bool,
    mods     : Mods,
    inj      : Injected,
}


static EVENT_CHANNEL: LazyLock <(Sender<Event>, Receiver<Event>)> = LazyLock::new (unbounded);


static MODIFIER_KEYS: LazyLock <HashSet <VKey>> = LazyLock::new (|| {
    use VKey::*;
    HashSet::from ( [
        Shift, LShift, RShift, Ctrl, LCtrl, RCtrl, Alt, LAlt, RAlt, LWin, RWin
    ] )
});



#[derive (Debug, Clone, Copy, Eq, PartialEq)]
enum Cls {
    Key,
    Gap,
    Grey,
}


#[derive (Debug, Clone, Copy, Eq, PartialEq)]
enum Colors {
    KeyPress,
    KeyRelease,
    ModPress,
    ModRelease,
    ModActive,
    Gap,
    Grey,

    //Highlight,
    // ^^ unfortunately, on terminals like powershell, at least while using the color-crate, bkg unavoidably brightens fgnd
    // .. which makes our subdued greys become white .. and no amount of tweaking colors got around that
    // .. hence, we'll just define highlight colors for each element instead of trying to do full line highlight .. oh well

    KeyPress_hl,
    ModActive_hl,
    Gap_hl,
    Grey_hl,

}


impl Colors {
    fn color(&self) -> CustomColor {
        match self {
            Colors::KeyPress    => CustomColor::new (120, 200, 100),   // greenish
            Colors::KeyRelease  => CustomColor::new (180, 140,  40),   // orangeish
            Colors::ModPress    => CustomColor::new (120, 180, 255),   // bluish
            Colors::ModRelease  => CustomColor::new (200, 110, 190),   // pinkish
            Colors::ModActive   => CustomColor::new (170, 140,   0),   // goldish
            Colors::Gap         => CustomColor::new (120, 150,  70),   // greenish
            Colors::Grey        => CustomColor::new ( 80,  80,  80),   // grey

            //Colors::Highlight   => CustomColor::new ( 60,  80,  40),   // greenish bkg highlight

            Colors::KeyPress_hl  => CustomColor::new (140, 255, 140),   // greenish
            Colors::ModActive_hl => CustomColor::new (220, 220,   0),   // yellowish
            Colors::Gap_hl       => CustomColor::new (160, 190, 110),   // greenish
            Colors::Grey_hl      => CustomColor::new (180, 180, 180),   // grey
        }
    }
}




fn main() {
    println!("Starting keyboard event monitor (Press Ctrl+C to exit)...");

    let receiver_clone = EVENT_CHANNEL.1.clone();
    let _printer_handle = thread::spawn (move || {
        print_events(receiver_clone);
    });

    unsafe {
        if SetWindowsHookExW ( WH_KEYBOARD_LL, Some(low_level_keyboard_proc), None, 0 ) .is_err() {
            eprintln!("Fatal: Failed to set hook. Error: {:?}", GetLastError());
            return;
        }
        let mut msg: MSG = mem::zeroed();
        loop {
            if GetMessageW (&mut msg, None, 0, 0) .as_bool() {
                //TranslateMessage(&msg);
                DispatchMessageW(&msg);
            } else { break }
        }
    }
}



fn mod_colorized (txt:&str, active:bool, hl:bool) -> ColoredString {
    match (active, hl) {
        (true,  false) => txt .custom_color (Colors::ModActive    .color()),
        (false, false) => txt .custom_color (Colors::Grey         .color()),
        //(true,  true)  => txt .custom_color (Colors::ModActive  .color()) .on_custom_color (Colors::Highlight.color()),
        //(false, true)  => txt .custom_color (Colors::Grey       .color()) .on_custom_color (Colors::Highlight.color()),
        (true,  true)  => txt .custom_color (Colors::ModActive_hl .color()),
        (false, true)  => txt .custom_color (Colors::Grey_hl      .color()),

    }
}
fn colorized (txt:&str, cls:Cls, press: bool, is_mod:bool, hl:bool) -> ColoredString {
    // note: bold doesnt seem to work on custom-colors (on powershell) .. though dimmed/italic work .. oh well
    match (cls,     is_mod,  press,   hl) {
        ( Cls::Key,  false,  true , false )  =>  txt .custom_color (Colors::KeyPress    .color()),
        ( Cls::Key,  false,  true ,  true )  =>  txt .custom_color (Colors::KeyPress_hl .color()),
        ( Cls::Key,  false,  false,     _ )  =>  txt .custom_color (Colors::KeyRelease  .color()),
        ( Cls::Key,  true ,  true ,     _ )  =>  txt .custom_color (Colors::ModPress    .color()),
        ( Cls::Key,  true ,  false,     _ )  =>  txt .custom_color (Colors::ModRelease  .color()),
        ( Cls::Gap,      _,      _, false )  =>  txt .custom_color (Colors::Gap         .color()),
        ( Cls::Gap,      _,      _,  true )  =>  txt .custom_color (Colors::Gap_hl      .color()),
        ( Cls::Grey,     _,      _, false )  =>  txt .custom_color (Colors::Grey        .color()),
        ( Cls::Grey,     _,      _,  true )  =>  txt .custom_color (Colors::Grey_hl     .color()),
    }
}




// This function runs in a separate thread, receiving events from the hook.
fn print_events (receiver: Receiver<Event>) {

    let mut last_ev = Event::default();
    let mut last_ev_stamp = Instant::now();
    let mut last_was_repeat = false;

    let header = format!(
        "{:>16} {:<2} {:<1}  {:<3}  {:<3}  {:<4}  {:<5}  {:<6}  {:<12} {:<6}  ",
        "Key", "Typ", "Inj", "Alt", "Win", "Ctrl", "Shift", "Code", "Timestamp", "Gap"
    );
    println! ("{}", header.bold() );
    println! ("{}", "-".repeat(75).dimmed());

    for event in receiver {

        let ev_stamp = Instant::now();
        let gap_dur = ev_stamp.duration_since(last_ev_stamp);
        last_ev_stamp = ev_stamp;

        let repeat = event == last_ev;
        if repeat && last_was_repeat && gap_dur < Duration::from_millis(100) {
            continue;
        }
        last_ev = event.clone();
        last_was_repeat = repeat;

        let key = VKey::from_vk_code (event.vk_code as u16);
        let is_mod = MODIFIER_KEYS .contains (&key);
        let hl = event.is_press && !is_mod && event.inj == Injected::Krusty;

        let key = format!("{:>16}", key.to_string().chars().take(16).collect::<String>());
        let key = colorized (&key, Cls::Key, event.is_press, is_mod, hl);

        let ev_t = if event.is_press { "dn" } else { "up" };
        let ev_t = colorized (& format! ("{:<2}", ev_t), Cls::Key, event.is_press, is_mod, hl);

        let inj = if repeat { Injected::Repeat } else { event.inj };
        let inj = match inj {
            Injected::None   => " ",
            Injected::Repeat => "R",
            Injected::Krusty => "k",
            Injected::Other  => "i",
        };
        let inj = colorized (& format!("{:<1}", inj), Cls::Grey, false, false, hl);

        let code = colorized (& format! ("{:>#06x}", event.vk_code), Cls::Grey, false, false, hl);

        let alt   = mod_colorized ( "Alt",   event.mods.alt,   hl );
        let win   = mod_colorized ( "Win",   event.mods.win,   hl );
        let ctrl  = mod_colorized ( "Ctrl",  event.mods.ctrl,  hl );
        let shift = mod_colorized ( "Shift", event.mods.shift, hl );

        // timestamp as HH:MM:SS.mss
        let stamp = Local::now();
        let stamp = stamp .format("%H:%M:%S%.3f").to_string();
        let stamp = colorized ( & format! ("{:<12}", &*stamp), Cls::Grey, false, false, hl);

        // Format gap duration as seconds.milliseconds, or blank if very large
        let gap_ms = gap_dur.as_millis();
        let gap_str = if gap_ms < 60000 { // Show gap if less than 60 seconds
            format!("{:02}.{:03}", gap_ms / 1000, gap_ms % 1000)
        } else {
            format!("{:>6}", "") // Blank if gap is large
        };
        let gap = colorized (&gap_str, Cls::Gap, false, false, hl);

        let line = format! (
            "{}  {}  {}  {}  {}  {}  {}  {}  {}  {}  ", // spacing should match the header
            key, ev_t, inj, alt, win, ctrl, shift, code, stamp, gap
        );
        //if hl {
        //    println! ("{}", line.on_custom_color (Colors::Highlight.color()));
        //} else { println! ("{}",line) }
        println! ("{}",line);
    }

    println!("Event channel closed. Printer thread exiting.");
}




unsafe extern "system" fn low_level_keyboard_proc(
    n_code: i32,     // Hook code (HC_ACTION means process the message)
    w_param: WPARAM, // Message identifier (WM_KEYDOWN, WM_KEYUP, etc.)
    l_param: LPARAM, // Pointer to KBDLLHOOKSTRUCT
) -> LRESULT {

    if n_code == HC_ACTION as i32 {
        // ^^ if n_code < 0, gotta forward it to CallNextHookEx straight up

        let hook_struct = *(l_param.0 as *const KBDLLHOOKSTRUCT);
        let vk_code = hook_struct.vkCode;
        let sc_code = hook_struct.scanCode;
        let flags = hook_struct.flags;
        let inj_stamp = hook_struct.dwExtraInfo;

        let is_press = match w_param.0 as u32 {
            WM_KEYDOWN | WM_SYSKEYDOWN => true,
            WM_KEYUP | WM_SYSKEYUP => false,
            _ => false,
        };

        const KRUSTY_STAMP: usize = 0xFFC3D44F;
        let inj = if flags.contains(LLKHF_INJECTED) {
            if inj_stamp == KRUSTY_STAMP { Injected::Krusty }
            else { Injected::Other }
        } else { Injected::None };

        let mods = Mods {
            alt   : (GetAsyncKeyState  (VK_MENU.0 as i32)    as u32 & 0x8000) != 0,
            ctrl  : (GetAsyncKeyState  (VK_CONTROL.0 as i32) as u32 & 0x8000) != 0,
            shift : (GetAsyncKeyState  (VK_SHIFT.0 as i32)   as u32 & 0x8000) != 0,
            win   : ((GetAsyncKeyState (VK_LWIN.0 as i32)    as u32 & 0x8000) != 0) ||
                    ((GetAsyncKeyState (VK_RWIN.0 as i32)    as u32 & 0x8000) != 0),
        };
        // ^^ GetAsyncKeyState is fast enough to not worry about calling here repeatedly

        let event = Event { vk_code, sc_code, is_press, mods, inj };

        if let Err(e) = EVENT_CHANNEL.0.send(event) {
            eprintln! ("[Hook Error] Failed to send event: {}", e);
        }
    }

    CallNextHookEx (None, n_code, w_param, l_param)
}



