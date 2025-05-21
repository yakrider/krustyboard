#![allow (non_camel_case_types, non_snake_case, non_upper_case_globals)]

use chrono::Local;
use colored::*;
use crossbeam_channel::{unbounded, Receiver, Sender};

use std::sync::LazyLock;
use std::{mem, thread, time::Instant};
use std::collections::HashSet;
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
    Repeat,   // Determined as repeat from cache comparison
    Krusty,   // Injected, Krusty dwExtraInfo
    Other,    // Injected, unrecogized dwExtraInfo
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


#[allow (clippy::type_complexity)]
static EVENT_CHANNEL: LazyLock <(Sender<(Instant,Event)>, Receiver<(Instant,Event)>)> = LazyLock::new (unbounded);


static MODIFIER_KEYS: LazyLock <HashSet <VKey>> = LazyLock::new (|| {
    use VKey::*;
    HashSet::from ( [
        CapsLock, Shift, LShift, RShift, Ctrl, LCtrl, RCtrl, Alt, LAlt, RAlt, LWin, RWin
    ] )
});



#[derive (Debug, Clone, Copy, Eq, PartialEq)]
#[allow (dead_code)]
enum Cls {
    Key,
    EvT,
    Mods,
    Gap,
    Grey,
}


#[derive (Debug, Clone, Copy, Eq, PartialEq)]
#[allow (dead_code)]
enum Colors {
    Grey,
    Grey_hl,

    KeyPress,
    KeyPress_hl,
    ModPress,

    KeyRelease,
    ModRelease,

    EvT,
    EvT_hl,

    ModActive,
    ModActive_hl,

    Gap,
    Gap_hl,

    Highlight_kdn,
    Highlight_kdn_kr,
}


/*
- Note that the colors below are designed for a Powershell-7 profile for windows-terminal that disables automatic color adjustment ..
    otherwise, the terminal tries to enforce contrast levels by brightening colors against bkg-highlight etc
- Plus, the 'colored' crate used requires $env:COLORTERM = 'truecolor' set, either in pwsh $PROFILE or directly in env-vars etc
- for the extract below, kbd-evs is defined as alias to our exe in $PROFILE
- Powershell-7 profile extract sample intended for use with this utility:
    {
        "name": "kbd-evs",
        "commandline": "\"C:\\Program Files\\PowerShell\\7\\pwsh.exe\" -NoExit -Command kbd-evs",
        "icon": "D:\\yakdat\\code\\git-repos\\krustyboard\\KbdEvsPrinter\\kbd_evs.ico",
        "startingDirectory": "%USERPROFILE%",
        "tabTitle": "kbd-evs .. Keyboard Events Printer"
        "intenseTextStyle": "all",
        "adjustIndistinguishableColors": "never",
        "colorScheme": "Campbell",
        "elevate": false,
        "hidden": true,
        "guid": "{50ac40c1-aefc-482a-8029-74f39149919d}",
    },
 */

impl Colors {

    fn color(&self) -> CustomColor {
        match self {
            Colors::Grey     => CustomColor::new (100, 100, 100),   // grey
            Colors::Grey_hl  => CustomColor::new (140, 140, 140),   // grey

            Colors::Highlight_kdn_kr => CustomColor::new ( 50,  70,  30),   // greenish bkg highlight
            Colors::Highlight_kdn    => CustomColor::new ( 60,  40,   0),   // brownish bkg highlight

            Colors::KeyPress     => CustomColor::new (120, 200, 100),   // greenish
            Colors::KeyPress_hl  => CustomColor::new (140, 255, 140),   // greenish
            Colors::ModPress     => CustomColor::new (120, 180, 255),   // bluish

            Colors::KeyRelease  => CustomColor::new (180, 140,  40),   // orangeish
            Colors::ModRelease  => CustomColor::new (200, 110, 190),   // pinkish

            Colors::EvT     => Colors::Grey.color(),
            Colors::EvT_hl  => Colors::Grey_hl.color(),

            Colors::ModActive    => CustomColor::new (170, 140,   0),   // goldish
            Colors::ModActive_hl => CustomColor::new (220, 220,   0),   // yellowish

            Colors::Gap     => CustomColor::new (120, 150,  70),   // greenish
            Colors::Gap_hl  => CustomColor::new (160, 190, 110),   // greenish

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



fn key_colorized (txt:&str, press:bool, is_mod:bool, hl:bool) -> ColoredString {
    match (is_mod,  press,   hl) {
        ( false,  true , false )  =>  txt .custom_color (Colors::KeyPress    .color()),
        ( false,  true ,  true )  =>  txt .custom_color (Colors::KeyPress_hl .color()) .bold(),
        ( false,  false,     _ )  =>  txt .custom_color (Colors::KeyRelease  .color()),

        ( true ,  true ,     _ )  =>  txt .custom_color (Colors::ModPress    .color()),
        ( true ,  false,     _ )  =>  txt .custom_color (Colors::ModRelease  .color()),
    }
}

fn mod_colorized (txt:&str, active:bool, hl:bool) -> ColoredString {
    // note: bold doesnt seem to work on custom-colors (on powershell) .. though dimmed/italic work .. oh well
    match (active, hl) {
        (true,  false) => txt .custom_color (Colors::ModActive    .color()),
        (false, false) => txt .custom_color (Colors::Grey         .color()),
        (true,  true)  => txt .custom_color (Colors::ModActive_hl .color()),
        (false, true)  => txt .custom_color (Colors::Grey_hl      .color()),
    }
}

fn colorized (txt:&str, cls:Cls, hl:bool) -> ColoredString {
    match (cls, hl) {
        ( Cls::EvT,  false )  =>  txt .custom_color (Colors::EvT    .color()),
        ( Cls::Gap,  false )  =>  txt .custom_color (Colors::Gap    .color()),
        ( Cls::Grey, false )  =>  txt .custom_color (Colors::Grey   .color()),

        ( Cls::EvT,  true )  =>  txt .custom_color (Colors::EvT_hl  .color()),
        ( Cls::Gap,  true )  =>  txt .custom_color (Colors::Gap_hl  .color()),
        ( Cls::Grey, true )  =>  txt .custom_color (Colors::Grey_hl .color()),

        _  => txt.into(),
    }
}


fn print_header() {
    let header = format!(
        "{:>16} {:<2} {:<1}  {:<3}  {:<3}  {:<4}  {:<5}  {:<6}  {:<12} {:<6}    ",
        "Key", "Typ", "Inj", "Alt", "Win", "Ctrl", "Shift", "Code", "Timestamp", "Gap"
    );
    println! ("{}", header.bold() );
}
fn print_spacer() {
    println! ("{}", "-".repeat(80).dimmed());
}


// This function runs in a separate thread, receiving events from the hook.
fn print_events (receiver: Receiver<(Instant,Event)>) {

    let mut last_ev = Event::default();
    let mut last_ev_stamp = Instant::now();
    let mut last_was_repeat = false;

    print_header();
    print_spacer();

    for (stamp, event) in receiver {

        let gap_dur = stamp.duration_since(last_ev_stamp);
        let repeat = event == last_ev;
        last_ev = event.clone();
        if repeat && last_was_repeat {
            continue;
        }
        last_was_repeat = repeat;
        if !repeat { last_ev_stamp = stamp; }

        let key = VKey::from_vk_code (event.vk_code as u16);
        let is_mod = MODIFIER_KEYS .contains (&key);

        // we'll highlight (in green) krusty sent key-presses, but ignoring modkeys etc
        let hl_kr  = event.is_press && !is_mod && event.inj == Injected::Krusty;

        // and separately highlight (in brown) actual non-repeat keypresses, i.e no injection, but modkeys ok
        let hl_kdn = event.is_press && !repeat && event.inj == Injected::None;

        let key = format!("{:>16}", key.to_string().chars().take(16).collect::<String>());
        let key = key_colorized (&key, event.is_press, is_mod, hl_kr);

        let ev_t = if event.is_press { "dn" } else { "up" };
        let ev_t = key_colorized (& format! ("{ev_t:<2}"), event.is_press, is_mod, hl_kr);

        let inj = if repeat { Injected::Repeat } else { event.inj };
        let inj = match inj {
            Injected::None   => " ",
            Injected::Repeat => "R",
            Injected::Krusty => "k",
            Injected::Other  => "j",
        };
        let inj = colorized (& format!("{inj:<1}"), Cls::EvT, hl_kr);

        let code = colorized (& format! ("{:>#06x}", event.vk_code), Cls::Grey, hl_kr);

        let alt   = mod_colorized ( "Alt",   event.mods.alt,   hl_kr );
        let win   = mod_colorized ( "Win",   event.mods.win,   hl_kr );
        let ctrl  = mod_colorized ( "Ctrl",  event.mods.ctrl,  hl_kr );
        let shift = mod_colorized ( "Shift", event.mods.shift, hl_kr );

        // timestamp as HH:MM:SS.mss
        let stamp = Local::now();
        let stamp = stamp .format("%H:%M:%S%.3f").to_string();
        let stamp = colorized ( & format! ("{:<12}", &*stamp), Cls::Grey, hl_kr);

        // Format gap duration as seconds.milliseconds, or blank if very large
        let gap_ms = gap_dur.as_millis();
        let gap_str = if gap_ms < 60000 { // Show gap if less than 60 seconds
            format!("{:02}.{:03}", gap_ms / 1000, gap_ms % 1000)
        } else {
            format!("{:>6}", "") // Blank if gap is large
        };
        let gap = colorized (&gap_str, Cls::Gap, hl_kr);

        if gap_ms > 20 {
            print_spacer();
        }

        let line = format! (
            "{key}  {ev_t}  {inj}  {alt}  {win}  {ctrl}  {shift}  {code}  {stamp}  {gap}    "
            // ^^spacing should match the header
        );
        let line = if hl_kr {
            line.on_custom_color (Colors::Highlight_kdn_kr.color())
        } else if hl_kdn {
            line.on_custom_color (Colors::Highlight_kdn.color())
        } else {
            line.into()
        };
        println! ("{line}")
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
        let stamp = Instant::now();
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

        let event = Event { vk_code, sc_code, is_press, mods, inj};

        if let Err(e) = EVENT_CHANNEL.0 .send ((stamp, event)) {
            eprintln! ("[Hook Error] Failed to send event: {e}");
        }
    }

    CallNextHookEx (None, n_code, w_param, l_param)
}



