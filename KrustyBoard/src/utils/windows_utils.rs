

use std::ffi::c_void;
use std::mem;
use std::mem::size_of;

use std::sync::{Arc, Mutex, RwLock};
use once_cell::sync::Lazy;

use windows::core::{PSTR, HSTRING, PCWSTR};
use windows::Win32::Foundation::{HINSTANCE, POINT, HWND, LPARAM, RECT, WPARAM, HANDLE, BOOL, CloseHandle};
use windows::Win32::Graphics::Dwm::{DwmGetWindowAttribute, DWMWA_CLOAKED, DWMWA_EXTENDED_FRAME_BOUNDS};
use windows::Win32::UI::HiDpi::{DPI_AWARENESS_CONTEXT_SYSTEM_AWARE, SetThreadDpiAwarenessContext};
use windows::Win32::UI::WindowsAndMessaging::*;
use windows::Win32::System::SystemServices::{APPCOMMAND_MICROPHONE_VOLUME_MUTE};
use windows::Win32::System::Threading::{OpenProcess, QueryFullProcessImageNameA, PROCESS_NAME_WIN32, PROCESS_QUERY_LIMITED_INFORMATION, SetPriorityClass, GetCurrentProcess, HIGH_PRIORITY_CLASS};


// we'll define our own new-type of Hwnd mostly because HWND doesnt implement trait Hash to put into maps etc

# [ derive (Debug, Default, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash) ]
pub struct Hwnd (pub(crate) isize);

impl From<HWND> for Hwnd {
    fn from (hwnd:HWND) -> Self { Hwnd(hwnd.0) }
}
impl From<Hwnd> for isize {
    fn from (hwnd:Hwnd) -> Self { hwnd.0 }
}
impl From<Hwnd> for HWND {
    fn from (hwnd:Hwnd) -> Self { HWND(hwnd.0) }
}


pub fn win_set_thread_dpi_aware() { unsafe {
    SetThreadDpiAwarenessContext (DPI_AWARENESS_CONTEXT_SYSTEM_AWARE);
} }

pub fn win_set_cur_process_priority_high() -> bool { unsafe {
    SetPriorityClass (GetCurrentProcess(), HIGH_PRIORITY_CLASS) .as_bool()
} }

pub fn dpi_conv_point (hwnd:Hwnd, p:POINT) -> POINT { unsafe {
    let mut p = POINT { x: p.x, y: p.y };
    PhysicalToLogicalPoint (hwnd, &mut p);
    p
} }


pub fn check_window_visible (hwnd:Hwnd) -> bool { unsafe {
    IsWindowVisible (hwnd) .as_bool()
} }

pub fn check_window_cloaked (hwnd:Hwnd) -> bool { unsafe {
    let mut cloaked_state: isize = 0;
    let out_ptr = &mut cloaked_state as *mut isize as *mut c_void;
    let _ = DwmGetWindowAttribute (hwnd, DWMWA_CLOAKED, out_ptr, size_of::<isize>() as u32);
    cloaked_state != 0
} }

pub fn check_if_tool_window (hwnd:Hwnd) -> bool { unsafe {
    GetWindowLongW (hwnd, GWL_EXSTYLE) & WS_EX_TOOLWINDOW.0 as i32 != 0
} }

pub fn check_window_has_owner (hwnd:Hwnd) -> bool { unsafe {
    GetAncestor (hwnd, GA_ROOTOWNER).0 != hwnd.0
} }

pub fn win_check_hwnd (hwnd:Hwnd) -> bool { unsafe {
    IsWindow (hwnd).as_bool()
} }


pub fn win_get_fgnd () -> Hwnd { unsafe {
    GetForegroundWindow().into()
} }
pub fn win_set_fgnd (hwnd:Hwnd) { unsafe {
    SetForegroundWindow (hwnd);
} }

pub fn get_pointer_loc () -> POINT { unsafe {
    let mut point = POINT::default();
    GetCursorPos (&mut point);
    point
} }

pub fn win_get_hwnd_from_point (point:POINT) -> Hwnd { unsafe {
    let hwnd = WindowFromPoint (point);
    let hwnd = GetAncestor (hwnd, GA_ROOT);
    hwnd.into()
} }
pub fn win_get_hwnd_from_pointer () -> Hwnd {
    win_get_hwnd_from_point (get_pointer_loc())
}

pub fn win_get_window_rect (hwnd:Hwnd) -> RECT { unsafe {
    let mut rect = RECT::default();
    GetWindowRect (hwnd, &mut rect as *mut RECT);
    rect
} }
pub fn win_get_window_frame (hwnd:Hwnd) -> RECT { unsafe {
    let mut rect = RECT::default();
    let _ = DwmGetWindowAttribute (hwnd, DWMWA_EXTENDED_FRAME_BOUNDS, &mut rect as *mut RECT as *mut c_void, mem::size_of::<RECT>() as u32);
    rect
} }

pub fn win_get_fgnd_rect () -> (Hwnd, RECT) { unsafe {
    let hwnd = GetForegroundWindow();
    let mut rect = RECT::default();
    GetWindowRect (hwnd, &mut rect as *mut RECT);
    (hwnd.into(), rect)
} }

pub fn win_get_work_area () -> RECT { unsafe {
    let mut rect = RECT::default();
    let _ = SystemParametersInfoW (SPI_GETWORKAREA, 0, Some (&mut rect as *mut RECT as *mut c_void), SYSTEM_PARAMETERS_INFO_UPDATE_FLAGS::default());
    rect
} }


pub fn win_activate (hwnd:Hwnd) { unsafe {     //println!("winapi activate {:?}",hwnd);
    //ShowWindowAsync (hwnd, SW_NORMAL);
    // ^^ this will cause minimized/maximized windows to be restored
    let mut win_state =  WINDOWPLACEMENT::default();
    GetWindowPlacement (hwnd, &mut win_state);
    if win_state.showCmd == SW_SHOWMINIMIZED {
        ShowWindowAsync (hwnd, SW_RESTORE);
    } else {
        ShowWindowAsync (hwnd, SW_SHOW);
    }
    //keybd_event (0, 0, KEYBD_EVENT_FLAGS::default(), 0);
    SetForegroundWindow (hwnd);
} }

pub fn win_send_to_back (hwnd:Hwnd) { unsafe {     //println!("winapi send to back {:?}",(&hwnd));
   SetWindowPos (hwnd, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE|SWP_NOSIZE|SWP_NOACTIVATE);
} }
pub fn win_hide (hwnd:Hwnd) { unsafe {      //println!("winapi hide {:?}",hwnd);
    ShowWindow (hwnd, SW_HIDE);
} }
pub fn win_close (hwnd:Hwnd) { unsafe {     //println!("winapi close {:?}",hwnd);
    //CloseWindow(hwnd);
    // note ^^ that the u32 'CloseWindow' cmd actually minimizes it, to close, send it a WM_CLOSE msg
    PostMessageA (hwnd, WM_CLOSE, WPARAM::default(), LPARAM::default());
} }
pub fn win_minimize (hwnd:Hwnd) { unsafe {
    ShowWindowAsync (hwnd, SW_MINIMIZE);
} }
pub fn win_maximize (hwnd:Hwnd) { unsafe {
    ShowWindowAsync (hwnd, SW_MAXIMIZE);
} }
pub fn win_toggle_maximize (hwnd:Hwnd) { unsafe {
    let mut win_state =  WINDOWPLACEMENT::default();
    GetWindowPlacement (hwnd, &mut win_state);
    if win_state.showCmd == SW_SHOWMAXIMIZED {
        ShowWindowAsync (hwnd, SW_RESTORE);
    } else if win_state.showCmd == SW_SHOWMINIMIZED {
        ShowWindowAsync (hwnd, SW_RESTORE);
    } else {
        ShowWindowAsync (hwnd, SW_SHOWMAXIMIZED);
    }

} }



pub fn set_cursor (cursor_style:PCWSTR) { unsafe {
    SetCursor (LoadCursorW (HINSTANCE::default(), cursor_style).unwrap());
} }

pub fn win_fgnd_move_rel (dx:i32, dy:i32) { unsafe {
    let (hwnd, r) = win_get_fgnd_rect();
    MoveWindow (hwnd, r.left + dx, r.top + dy, r.right-r.left, r.bottom-r.top, true);
} }
pub fn win_move_to (hwnd:Hwnd, x:i32, y:i32, width:i32, height:i32) { unsafe {
    MoveWindow (hwnd, x, y, width, height, true);    // the bool param at end flags whether to repaint or not
} }

pub fn win_find_by_win_class (cls:&str) -> Hwnd { unsafe {
   FindWindowW (&HSTRING::from(cls), PCWSTR::null()) .into()
} }



pub fn win_fgnd_stretch (dx:i32, dy:i32) { unsafe {
    let (hwnd, r) = win_get_fgnd_rect();
    MoveWindow (hwnd, r.left, r.top, r.right-r.left+dx, r.bottom-r.top+dy, true);
} }


pub fn win_fgnd_center () { unsafe {
    let scr_w = GetSystemMetrics(SM_CXSCREEN);
    let (hwnd, r) = win_get_fgnd_rect();
    MoveWindow (hwnd, (scr_w - (r.right - r.left))/2, r.top, r.right-r.left, r.bottom-r.top, true);
} }


pub fn win_fgnd_center_if_past_screen () { unsafe {
    let scr_w = GetSystemMetrics(SM_CXSCREEN);
    let (hwnd, r) = win_get_fgnd_rect();
    if r.right > (scr_w - 30) {
        MoveWindow (hwnd, (scr_w - (r.right - r.left))/2 + 100, r.top, r.right-r.left, r.bottom-r.top, true);
    }
} }


pub fn win_check_if_topmost (hwnd: Hwnd) -> bool { unsafe {
    GetWindowLongW (hwnd, GWL_EXSTYLE) as u32 & WS_EX_TOPMOST.0 == WS_EX_TOPMOST.0
} }

fn win_change_always_on_top (hwnd: Hwnd, z_val:HWND) { unsafe {
    SetWindowPos (hwnd, z_val, 0, 0, 0, 0, SWP_NOMOVE|SWP_NOSIZE|SWP_NOACTIVATE);
} }
pub fn win_set_always_on_top   (hwnd: Hwnd) { win_change_always_on_top (hwnd, HWND_TOPMOST) }
pub fn win_unset_always_on_top (hwnd: Hwnd) { win_change_always_on_top (hwnd, HWND_NOTOPMOST) }

fn win_fgnd_change_always_on_top (z_val:HWND) { unsafe {
    win_change_always_on_top (GetForegroundWindow().into(), z_val);
} }
pub fn win_fgnd_set_always_on_top ()   { win_change_always_on_top (win_get_fgnd(), HWND_TOPMOST) }
pub fn win_fgnd_unset_always_on_top () { win_change_always_on_top (win_get_fgnd(), HWND_NOTOPMOST) }

pub fn win_fgnd_toggle_always_on_top () { unsafe {
    let is_topmost = GetWindowLongW (GetForegroundWindow(), GWL_EXSTYLE) as u32 & WS_EX_TOPMOST.0 == WS_EX_TOPMOST.0;
    let z_tog_val = if is_topmost { HWND_NOTOPMOST } else { HWND_TOPMOST };
    win_fgnd_change_always_on_top (z_tog_val)
} }

pub fn win_fgnd_toggle_titlebar () { unsafe {
    // works but only for win32 apps, not for UWP etc
    let hwnd = GetForegroundWindow();
    let style = GetWindowLongW (hwnd, GWL_STYLE);
    //println! ( "{:#x?}", ( style, style as u32, (WS_CAPTION | WS_SYSMENU).0, style as u32 & (!(WS_CAPTION | WS_SYSMENU)).0 ) );
    //println! ( "{:#x?}", ( style as u32 & !((WS_CAPTION | WS_SYSMENU).0), style as u32 | (WS_CAPTION | WS_SYSMENU).0) );
    if style as u32 & WS_CAPTION.0 > 0 {
        SetWindowLongW (hwnd, GWL_STYLE, (style as u32 & (!(WS_CAPTION | WS_SYSMENU)).0) as i32);
    } else {
        SetWindowLongW (hwnd, GWL_STYLE, (style as u32 | (WS_CAPTION | WS_SYSMENU).0) as i32 );
    }
} }


pub fn win_fgnd_toggle_vertmax () { unsafe {
    // works by sending doubleclick at top of window (which also works manually)
    PostMessageW (GetForegroundWindow(), WM_NCLBUTTONDBLCLK, WPARAM(HTTOP as _), LPARAM(0));
} }


pub fn win_fgnd_toggle_max () {
    win_toggle_maximize (win_get_fgnd())
}

pub fn _win_get_screen_metrics () -> (i32, i32) { unsafe {
    (GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN))
} }

pub fn win_fgnd_min () { unsafe {
    let hwnd = GetForegroundWindow();
    PostMessageW (hwnd, WM_SYSCOMMAND, WPARAM(SC_MINIMIZE as _), LPARAM(0));
} }



pub fn get_fgnd_win_title () -> String { unsafe {
    get_win_title (GetForegroundWindow().into())
} }
pub fn get_win_title (hwnd:Hwnd) -> String { unsafe {
    const MAX_LEN : usize = 512;
    let mut lpstr : [u16; MAX_LEN] = [0; MAX_LEN];
    let copied_len = GetWindowTextW (hwnd, &mut lpstr);
    String::from_utf16_lossy (&lpstr[..(copied_len as _)])
} }


pub fn get_fgnd_win_class () -> String { unsafe {
    get_win_class_by_hwnd (GetForegroundWindow().into())
} }
pub fn get_win_class_by_hwnd (hwnd:Hwnd) -> String { unsafe {
    let mut lpstr: [u16; 120] = [0; 120];
    let len = GetClassNameW (hwnd, &mut lpstr);
    String::from_utf16_lossy(&lpstr[..(len as _)])
} }


pub fn get_fgnd_win_exe () -> Option<String> {
    get_exe_by_hwnd ( win_get_fgnd() )
}
pub fn get_exe_by_hwnd (hwnd:Hwnd) -> Option<String> {
    get_exe_by_pid ( get_pid_by_hwnd (hwnd))
}
pub fn get_exe_by_pid (pid:u32) -> Option<String> { unsafe {
    let handle = OpenProcess (PROCESS_QUERY_LIMITED_INFORMATION, BOOL::from(false), pid);
    let mut lpstr: [u8; 256] = [0; 256];
    let mut lpdwsize = 256u32;
    if handle.is_err() { return None }
    let _ = QueryFullProcessImageNameA ( HANDLE (handle.as_ref().unwrap().0), PROCESS_NAME_WIN32, PSTR::from_raw(lpstr.as_mut_ptr()), &mut lpdwsize );
    handle.iter().for_each ( |h| { CloseHandle(*h); } );
    PSTR::from_raw(lpstr.as_mut_ptr()).to_string() .ok() .map (|s| s.split("\\").last().map(|s| s.to_string())) .flatten() .into()
} }

pub fn get_pid_by_hwnd (hwnd:Hwnd) -> u32 { unsafe {
    let mut pid = 0u32;
    let _ = GetWindowThreadProcessId (hwnd, Some(&mut pid));
    pid
} }



pub fn mic_mute_toggle () { unsafe {
    // note that the 'mute' appcommands actually do toggle, both for mic and volume
    PostMessageW (GetForegroundWindow(), WM_APPCOMMAND, WPARAM(0), LPARAM((APPCOMMAND_MICROPHONE_VOLUME_MUTE.0 << 16) as _));
} }








// we'll use a static rwlocked vec to store enum-windows from callbacks, and a mutex to ensure only one enum-windows call is active
#[allow(non_upper_case_globals)]
static enum_hwnds_lock : Lazy <Arc <Mutex <()>>> = Lazy::new (|| Arc::new ( Mutex::new(())));
#[allow(non_upper_case_globals)]
static enum_hwnds : Lazy <Arc <RwLock <Vec <Hwnd>>>> = Lazy::new (|| Arc::new ( RwLock::new (vec!()) ) );



type WinEnumCb = unsafe extern "system" fn (HWND, LPARAM) -> BOOL;

fn win_get_hwnds_w_filt (filt_fn: WinEnumCb) -> Vec<Hwnd> { unsafe {
    let lock = enum_hwnds_lock.lock().unwrap();
    *enum_hwnds.write().unwrap() = Vec::with_capacity(50);   // setting up some excess capacity to reduce reallocations
    let _ = EnumWindows ( Some(filt_fn), LPARAM::default() );
    let hwnds = enum_hwnds.write().unwrap().drain(..).collect();
    drop(lock);
    hwnds
} }

pub fn win_get_switcher_filt_hwnds () -> Vec<Hwnd> {
    win_get_hwnds_w_filt (win_enum_cb_switcher_filt)
}
pub unsafe extern "system" fn win_enum_cb_switcher_filt (hwnd:HWND, _:LPARAM) -> BOOL {
    let retval = BOOL (true as i32);
    if !check_window_visible   (hwnd.into())  { return retval }
    if  check_window_cloaked   (hwnd.into())  { return retval }
    if  check_window_has_owner (hwnd.into())  { return retval }
    if  check_if_tool_window   (hwnd.into())  { return retval }
    enum_hwnds.write().unwrap() .push (hwnd.into());
    retval
}

pub fn win_get_ide_dialog_hwnds() -> Vec<Hwnd> {
    win_get_hwnds_w_filt (win_enum_cb_ide_dialog_filt)
}
pub unsafe extern "system" fn win_enum_cb_ide_dialog_filt (hwnd:HWND, _:LPARAM) -> BOOL {
    let retval = BOOL (true as i32);
    if !check_window_visible (hwnd.into())  { return retval }
    if  check_window_cloaked (hwnd.into())  { return retval }
    if  check_if_tool_window (hwnd.into())  { return retval }
    //if  check_window_has_owner (hwnd.into())  { return retval }
    if get_win_class_by_hwnd (hwnd.into()) != "SunAwtDialog" { return retval }
    if get_exe_by_hwnd (hwnd.into()) .filter (|s| s == "idea64.exe") .is_none() { return retval }
    enum_hwnds.write().unwrap() .push (hwnd.into());
    retval
}















#[cfg(test)] #[test]
pub fn test_mic_mute_toggle () {
    // todo: gotta cleanup these quick test portions
    use windows::Win32::System::SystemServices::{APPCOMMAND_VOLUME_MUTE};
    crate::utils::open_mic_cpl();
    //mic_mute_toggle();
    //let _ = std::process::Command::new("rundll32.exe").arg("user32.dll,MessageBeep").spawn();
    let _ = std::process::Command::new("rundll32.exe").arg("shell32.dll,Control_RunDLL").arg("mmsys.cpl,,1").spawn();
    unsafe{PostMessageW (GetForegroundWindow(), WM_APPCOMMAND, WPARAM(0), LPARAM((APPCOMMAND_VOLUME_MUTE.0 << 16) as _));}
    std::thread::sleep(std::time::Duration::from_millis(500));
    unsafe{PostMessageW (GetForegroundWindow(), WM_APPCOMMAND, WPARAM(0), LPARAM((APPCOMMAND_VOLUME_MUTE.0 << 16) as _));}
    std::thread::sleep(std::time::Duration::from_millis(100));
    //unsafe{PostMessageW (GetForegroundWindow(), WM_APPCOMMAND, WPARAM(0), LPARAM((APPCOMMAND_MEDIA_PLAY_PAUSE.0 << 16) as _));}
    //std::thread::sleep(std::time::Duration::from_millis(100));

    // quick fn hooked to main in examples/test to quickly rerun testing here
    println! ("todo..");
    //println!("{:?}",get_fgnd_win_class());

    //super::brightness::incr_brightness(1);
    //enum_windows_test();

}
