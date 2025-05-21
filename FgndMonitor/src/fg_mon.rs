#![allow (non_upper_case_globals)]

use std::collections::VecDeque;
use std::ffi::OsString;
use std::mem::zeroed;
use std::os::windows::ffi::OsStringExt;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::LazyLock;
use windows::core::{BOOL, PWSTR};
use windows::Win32::Foundation::{CloseHandle, HANDLE, HWND, LPARAM, MAX_PATH, RECT, TRUE};
use windows::Win32::Security::{GetTokenInformation, TokenElevation, TOKEN_ELEVATION, TOKEN_QUERY};
use windows::Win32::System::Threading::{OpenProcess, OpenProcessToken, QueryFullProcessImageNameW, PROCESS_NAME_NATIVE, PROCESS_QUERY_LIMITED_INFORMATION};
use windows::Win32::UI::Accessibility::{SetWinEventHook, UnhookWinEvent, HWINEVENTHOOK};
use windows::Win32::UI::WindowsAndMessaging::{DispatchMessageW, EnumWindows, GetClassNameW, GetForegroundWindow, GetMessageW, GetWindowLongW, GetWindowRect, GetWindowTextW, GetWindowThreadProcessId, IsWindowVisible, EVENT_SYSTEM_FOREGROUND, GWL_EXSTYLE, MSG, WINEVENT_OUTOFCONTEXT, WS_EX_TOPMOST};



// --- ANSI Color Codes ---
// \x1b[38;2;{r;g;b}m for fgnd
// \x1b[48;2;{r;g;b}m for bkgd
const RESET_COLOR : &str = "\x1b[0m";
const COLOR_A     : &str = "\x1b[38;2;220;220;180m";
const COLOR_B     : &str = "\x1b[38;2;180;240;240m";
const COLOR_BG_A  : &str = "\x1b[48;2;00;00;00m";
const COLOR_BG_B  : &str = "\x1b[48;2;50;50;50m";



#[derive (Debug, Default)]
struct HwndInfo {
    hwnd : HWND,
    title : String,
    class : String,
    rect  : RECT,
    is_fgnd : bool,
    is_topmost : bool,
    proc_info : Option <ProcessInfo>,
}

#[derive (Debug, Default)]
struct ProcessInfo {
    #[allow (unused)]
    pid  : u32,
    elev : bool,
    exe  : String,
}


// Struct to pass data to the EnumWindows callback
struct SessionDat {
    data : VecDeque <HwndInfo>,
    fgnd : HWND,
}


// RAII guard for the win32 handles
struct HandleGuard (HANDLE);
impl Drop for HandleGuard {
    fn drop (&mut self) {
        if !self.0.is_invalid() {
            unsafe { let _ = CloseHandle(self.0); }
        }
    }
}



fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 && args[1] == "-l" {
        print_z_ordered_list();
    } else {
        live_monitoring_mode();
    }
}




fn live_monitoring_mode() { unsafe {

    let hook = SetWinEventHook (
        EVENT_SYSTEM_FOREGROUND, EVENT_SYSTEM_FOREGROUND,
        None, Some (win_event_proc), 0, 0, WINEVENT_OUTOFCONTEXT,
    );

    if hook.is_invalid() {
        eprintln!("Failed to set event hook!");
        return;
    }

    println! ();
    println! ("Starting live monitoring of fgnd HWND .. Press Ctrl+C to exit");
    println! ();

    print_window_info(true, &Default::default(), 0);

    let mut msg = MSG::default();
    while GetMessageW (&mut msg, None, 0, 0) .into() {
        DispatchMessageW (&msg);
    }

    let _ = UnhookWinEvent(hook);

} }




unsafe extern "system" fn win_event_proc (
    _hook: HWINEVENTHOOK, _event: u32, hwnd: HWND,
    _id_object: i32, _id_child: i32, _event_thread: u32, _event_time: u32,
) {
    if !IsWindowVisible(hwnd).as_bool() { return; }
    let info = gather_hwnd_dat (hwnd, true);
    print_window_info (false, &info, 0);
}





fn print_z_ordered_list() {

    let data: VecDeque<HwndInfo> = VecDeque::new();
    let fgnd = unsafe { GetForegroundWindow() };

    let mut cbd = SessionDat {data, fgnd};

    let _ = unsafe { EnumWindows (
        Some(enum_windows_proc), LPARAM (&mut cbd as *mut _ as isize)
    ) };

    println!("Total Windows Found: {}", cbd.data.len());
    println!(); // Blank line
    print_window_info (true, &HwndInfo::default(), 0);  // title line

    for (index, wi) in cbd.data.iter().rev().enumerate() {
        let z_order = index + 1;
        print_window_info (false, wi, z_order);
    }
}



// Callback function for EnumWindows
unsafe extern "system" fn enum_windows_proc(hwnd: HWND, lparam: LPARAM) -> BOOL {

    // Basic safety check
    if lparam.0 == 0 {
        eprintln!("EnumWindows callback received null LPARAM!");
        return TRUE; // Continue enumeration but log error
    }

    // Skip invisible windows
    if !IsWindowVisible(hwnd).as_bool() {
        return TRUE; // Continue enumeration
    }

    // Cast LPARAM back to our struct pointer
    let cbd = &mut *(lparam.0 as *mut SessionDat);

    let is_fgnd = hwnd == cbd.fgnd;
    let info = gather_hwnd_dat (hwnd, is_fgnd);

    // Add to the front of the VecDeque (EnumWindows lists top-down, front = bottom of Z-order)
    cbd.data.push_front(info);

    TRUE // Continue enumeration
}




fn gather_hwnd_dat (hwnd:HWND, is_fgnd: bool) -> HwndInfo { unsafe {

    // Get Window Title
    let mut title_buffer: [u16; 512] = [0; 512];
    let title_len = GetWindowTextW (hwnd, &mut title_buffer);
    let title = if title_len > 0 {
        OsString::from_wide(&title_buffer[..title_len as usize])
            .to_string_lossy()
            .into_owned()
    } else {
        String::new() // Empty string if no title or error
    };

    // Get Window Class Name
    let mut class_buffer: [u16; 256] = [0; 256]; // Buffer for class name
    let class_len = GetClassNameW (hwnd, &mut class_buffer);
    let class = if class_len > 0 {
        OsString::from_wide(&class_buffer[..class_len as usize])
            .to_string_lossy()
            .into_owned()
    } else {
        String::from("<class error>") // Placeholder if error
    };

    // Get Window Rect
    let mut rect = RECT::default();
    let _ = GetWindowRect (hwnd, &mut rect); // Ignore error for rect

    // Check if Topmost
    let ex_style = GetWindowLongW (hwnd, GWL_EXSTYLE) as u32;
    let is_topmost = (ex_style & WS_EX_TOPMOST.0) == WS_EX_TOPMOST.0;

    let proc_info = get_proc_info (hwnd);

    HwndInfo { hwnd, title, class, rect, is_fgnd, is_topmost, proc_info }

} }




unsafe fn get_proc_info (hwnd: HWND) -> Option <ProcessInfo> {

    let mut pid = 0u32;
    let _thread_id = GetWindowThreadProcessId (hwnd, Some(&mut pid));
    if pid == 0 { return None; }

    let h_proc = OpenProcess (PROCESS_QUERY_LIMITED_INFORMATION, false, pid) .ok() ?;
    let _ph_guard = HandleGuard (h_proc);

    let mut h_tok : HANDLE = HANDLE::default();
    OpenProcessToken (h_proc, TOKEN_QUERY, &mut h_tok) .ok()?;
    let _th_guard = HandleGuard (h_tok);

    let mut elevation = TOKEN_ELEVATION::default();
    let mut return_size: u32 = 0;

    let elev = GetTokenInformation (
        h_tok, TokenElevation,
        Some (&mut elevation as *mut _ as *mut std::ffi::c_void),
        size_of::<TOKEN_ELEVATION>() as u32, &mut return_size,
    )
    .ok() .map (|_| elevation.TokenIsElevated != 0) .unwrap_or(false);

    let mut buffer: [u16; MAX_PATH as usize] = zeroed();
    let mut size = buffer.len() as u32;

    let exe = QueryFullProcessImageNameW (
        h_proc, PROCESS_NAME_NATIVE, PWSTR(buffer.as_mut_ptr()), &mut size
    ) .ok() .and_then (|_| {
        let full_path = OsString::from_wide (&buffer[..size as usize]);
        full_path .to_string_lossy() .rsplit('\\') .next() .map(|s| s.to_string())
    } )
    .unwrap_or ("<error>".to_string());

    Some ( ProcessInfo { pid, elev, exe } )

}




fn print_window_info (title_only:bool, info: &HwndInfo, z_order: usize) {

    const w_z    : usize = 3;
    const w_hwnd : usize = "0x0000000  ".len();
    const w_fgnd : usize = 3;
    const w_top  : usize = 4;
    const w_elev : usize = 3;
    const w_pos  : usize = "-16000, -16000".len();
    const w_dim  : usize = "0000 x 0000   ".len();
    const w_proc : usize = 25;
    const w_class : usize = 30;
    const w_title : usize = 50;

    if title_only {
        // print header and separator
        println!(
            "{:<w_z$} {:<w_hwnd$} {:<w_fgnd$} {:<w_top$} {:<w_elev$} {:<w_pos$} {:<w_dim$} {:<w_proc$} {:<w_class$} {:<w_title$}",
            "@z", "HWND", "Fg", "Top ", "Elv", "      Loc", "     Dims", " Process", " Class", " Title",
        );
        println!(
            "{:-<w_z$} {:-<w_hwnd$} {:-<w_fgnd$} {:-<w_top$} {:-<w_elev$} {:-<w_pos$} {:-<w_dim$} {:-<w_proc$} {:-<w_class$} {:-<w_title$}",
            "", "", "", "", "", "", "", "", "", ""
        );
        return
    }

    let hwnd_str = format!("0x.{:<8x}", info.hwnd.0 as usize);
    let fgnd_str = if info.is_fgnd { "Fg" } else { "" };
    let top_str  = if info.is_topmost { "Top" } else { "" };
    let elev_str = if info.proc_info.as_ref() .map (|pi| pi.elev) .unwrap_or(false) { "Elv" } else { "" };
    let x = info.rect.left;
    let y = info.rect.top;
    let width = info.rect.right - info.rect.left;
    let height = info.rect.bottom - info.rect.top;
    let pos_str = format!("{x:7} {y:<7}");
    let dim_str = format!("{width:4} x {height:<4}");

    fn trunc (s: &str, max: usize) -> String {
        if max < 5 {
            s.chars().take(max).collect()
        } else if s.chars().count() >= max-2 {
            format! ("{}[..]", &s.chars().take(max-5).collect::<String>())
        } else {
            s.to_string()
        }
    }

    let proc  = info.proc_info.as_ref() .map (|pi| trunc (&pi.exe, w_proc)) .unwrap_or("<error>".to_string());
    let class = trunc (&info.class, w_class);
    let title = trunc (&info.title, w_title);

    static is_alt : LazyLock<AtomicBool> = LazyLock::new (AtomicBool::default);
    let (fg, bg) = if is_alt.fetch_xor(true, Ordering::Relaxed) { (COLOR_A, COLOR_BG_A) } else { (COLOR_B, COLOR_BG_B) };

    println!(
        "{bg}{fg}{z_order:<w_z$} {hwnd_str:<w_hwnd$} {fgnd_str:<w_fgnd$} {top_str:<w_top$} \
        {elev_str:<w_elev$} {pos_str:<w_pos$} {dim_str:<w_dim$} {proc:<w_proc$} {class:<w_class$} {title:<w_title$}{RESET_COLOR}"
    );
}

