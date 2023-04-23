use std::ffi::c_void;
use std::mem;
#[allow(unused_imports)]

use windows::{
    core::PSTR,
    Win32::Foundation::{HWND, LPARAM, RECT, WPARAM, HANDLE, BOOL, CloseHandle},
    Win32::UI::WindowsAndMessaging::{
        GetForegroundWindow, GetSystemMetrics, GetWindowRect, HTCAPTION, HTTOP, MoveWindow, ShowWindow,
        SendMessageW, PostMessageW, GetClassNameW, GetWindowThreadProcessId, SM_CXSCREEN, SM_CYSCREEN, SW_NORMAL, WM_NCLBUTTONDBLCLK,
        HTMAXBUTTON, HTZOOM, SC_MAXIMIZE, SC_MINIMIZE, SC_RESTORE, WM_APPCOMMAND, WM_NCLBUTTONUP, WM_SYSCOMMAND
    },
    Win32::System::SystemServices::{
        APPCOMMAND_MEDIA_PLAY_PAUSE, APPCOMMAND_MEDIA_NEXTTRACK, APPCOMMAND_MEDIA_PREVIOUSTRACK,
        APPCOMMAND_VOLUME_MUTE, APPCOMMAND_VOLUME_UP, APPCOMMAND_VOLUME_DOWN,
        APPCOMMAND_MICROPHONE_VOLUME_MUTE, APPCOMMAND_MIC_ON_OFF_TOGGLE,  APPCOMMAND_MICROPHONE_VOLUME_UP, APPCOMMAND_MICROPHONE_VOLUME_DOWN,
    },
    Win32::System::Threading::{
        OpenProcess, QueryFullProcessImageNameA, PROCESS_NAME_WIN32, PROCESS_QUERY_LIMITED_INFORMATION
    },
    Win32::System::ProcessStatus::{ K32GetProcessImageFileNameA },
};
use windows::core::PCWSTR;
use windows::Win32::Foundation::{HINSTANCE, POINT};
use windows::Win32::Graphics::Dwm::{DwmGetWindowAttribute, DWMWA_EXTENDED_FRAME_BOUNDS};
use windows::Win32::UI::HiDpi::{DPI_AWARENESS_CONTEXT_SYSTEM_AWARE, SetThreadDpiAwarenessContext};
use windows::Win32::UI::WindowsAndMessaging::{GetWindowLongW, GetWindowTextW, GWL_EXSTYLE, HWND_NOTOPMOST, HWND_TOPMOST, LoadCursorW, PhysicalToLogicalPoint, SetCursor, SetWindowPos, SPI_GETWORKAREA, SWP_NOMOVE, SWP_NOSIZE, SYSTEM_PARAMETERS_INFO_UPDATE_FLAGS, SystemParametersInfoW, WS_EX_TOPMOST};


pub fn win_set_thread_dpi_aware() { unsafe {
    SetThreadDpiAwarenessContext (DPI_AWARENESS_CONTEXT_SYSTEM_AWARE);
} }

pub fn win_get_window_rect (hwnd:HWND) -> RECT { unsafe {
    let mut rect = RECT::default();
    GetWindowRect (hwnd, &mut rect as *mut RECT);
    rect
} }
pub fn win_get_window_frame (hwnd:HWND) -> RECT { unsafe {
    let mut rect = RECT::default();
    DwmGetWindowAttribute (hwnd, DWMWA_EXTENDED_FRAME_BOUNDS, &mut rect as *mut RECT as *mut c_void, mem::size_of::<RECT>() as u32);
    rect
} }

pub fn win_get_fgnd_rect () -> (HWND, RECT) { unsafe {
    let hwnd = GetForegroundWindow();
    let mut rect = RECT::default();
    GetWindowRect (hwnd, &mut rect as *mut RECT);
    (hwnd, rect)
} }

pub fn win_get_work_area () -> RECT { unsafe {
    let mut rect = RECT::default();
    let _ = SystemParametersInfoW (SPI_GETWORKAREA, 0, Some (&mut rect as *mut RECT as *mut c_void), SYSTEM_PARAMETERS_INFO_UPDATE_FLAGS::default());
    rect
} }

pub fn dpi_conv_point (hwnd:HWND, p:POINT) -> POINT { unsafe {
    let mut p = POINT { x: p.x, y: p.y };
    PhysicalToLogicalPoint (hwnd, &mut p);
    p
} }

pub fn set_cursor (cursor_style:PCWSTR) { unsafe {
    SetCursor (LoadCursorW (HINSTANCE::default(), cursor_style).unwrap());
} }

pub fn win_fgnd_move_rel (dx:i32, dy:i32) { unsafe {
    let (hwnd, r) = win_get_fgnd_rect();
    MoveWindow (hwnd, r.left + dx, r.top + dy, r.right-r.left, r.bottom-r.top, true);
} }
pub fn win_move_to (hwnd:HWND, x:i32, y:i32, width:i32, height:i32, do_repaint:bool) { unsafe {
    MoveWindow (hwnd, x, y, width, height, do_repaint);

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

fn win_fgnd_change_always_on_top (z_val:HWND) { unsafe {
    //let (hwnd, r) = win_get_fgnd_rect();
    //SetWindowPos (hwnd, z_val, r.top, r.left, r.right-r.left, r.bottom-r.top, SWP_SHOWWINDOW);
    SetWindowPos (GetForegroundWindow(), z_val, 0, 0, 0, 0, SWP_NOMOVE|SWP_NOSIZE);
} }
pub fn win_fgnd_set_always_on_top ()   { win_fgnd_change_always_on_top (HWND_TOPMOST) }
pub fn win_fgnd_unset_always_on_top () { win_fgnd_change_always_on_top (HWND_NOTOPMOST) }

pub fn win_fgnd_toggle_always_on_top () { unsafe {
    let z_tog_val = if GetWindowLongW (GetForegroundWindow(), GWL_EXSTYLE) & (WS_EX_TOPMOST.0 as i32) == 0 { HWND_TOPMOST } else { HWND_NOTOPMOST };
    win_fgnd_change_always_on_top (z_tog_val)
} }


pub fn win_fgnd_toggle_vertmax () { unsafe {
    // works by sending doubleclick at top of window (which also works manually)
    PostMessageW (GetForegroundWindow(), WM_NCLBUTTONDBLCLK, WPARAM(HTTOP as _), LPARAM(0));
} }



pub fn win_fgnd_toggle_max () {
    // should in theory work by sending doubleclick at window titlebar (which works manually)
    //PostMessageW (GetForegroundWindow(), WM_NCLBUTTONDBLCLK, WPARAM(HTCAPTION as _), LPARAM(0));
    // ^^ except, for win32 windows, it works to max, but when already max it CLOSES them (coz it sends it at left-top icon?)
    //PostMessageW (GetForegroundWindow(), WM_NCLBUTTONUP, WPARAM(HTMAXBUTTON as _), LPARAM(0));
    // ^^ doesnt work
    //PostMessageW (GetForegroundWindow(), WM_SYSCOMMAND, WPARAM(SC_MAXIMIZE as _), LPARAM(0));
    // ^^ only maximizes not toggle, and no easy way to know if its maxed other than guessing from dimensions
    win_fgnd_toggle_max_guess();
    // ^^ so we'll finally just do it based on window rect dimensions .. should work for vast majority cases
}

fn win_fgnd_toggle_max_guess () { unsafe {
    let (scr_w, scr_h) = win_get_screen_metrics();
    let (hwnd, r) = win_get_fgnd_rect();
    // complication here is where and how thick the taskbar might me ..
    // we'll try and get at guesswork by requiring at least two sides and either width/height to be maxed?
    if ( r.left <= 0 && r.top <= 0 && (r.right >= scr_w || r.bottom >= scr_h) ) ||
        // ^^ covers taskbar in bottom or right
        ( r.right >= scr_w && r.bottom >= scr_h && (r.left <= 0 || r.top <= 0) )
        // ^^ covers taskbar in left or top
    {
        PostMessageW (hwnd, WM_SYSCOMMAND, WPARAM(SC_RESTORE as _), LPARAM(0));
    } else {
        PostMessageW (hwnd, WM_SYSCOMMAND, WPARAM(SC_MAXIMIZE as _), LPARAM(0));
    }
} }

pub fn win_get_screen_metrics () -> (i32, i32) { unsafe {
    (GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN))
} }

pub fn win_fgnd_min () { unsafe {
    let hwnd = GetForegroundWindow();
    PostMessageW (hwnd, WM_SYSCOMMAND, WPARAM(SC_MINIMIZE as _), LPARAM(0));
} }




pub fn get_fgnd_win_title () -> String { unsafe {
    const MAX_LEN : usize = 512;
    let mut lpstr : [u16; MAX_LEN] = [0; MAX_LEN];
    let copied_len = GetWindowTextW (GetForegroundWindow(), &mut lpstr);
    String::from_utf16_lossy (&lpstr[..(copied_len as _)])
} }

pub fn get_fgnd_win_class () -> String { unsafe {
    let mut lpstr: [u16; 120] = [0; 120];
    let len = GetClassNameW (GetForegroundWindow(), &mut lpstr);
    String::from_utf16_lossy(&lpstr[..(len as _)])
} }


pub fn get_fgnd_win_exe () -> Option<String> { unsafe {
    let mut pid = 0u32;
    let _ = GetWindowThreadProcessId (GetForegroundWindow(), Some(&mut pid));
    let handle = OpenProcess (PROCESS_QUERY_LIMITED_INFORMATION, BOOL::from(false), pid);
    let mut lpstr: [u8; 256] = [0; 256];
    let mut lpdwsize = 256u32;
    if handle.is_err() { return None }
    let _ = QueryFullProcessImageNameA ( HANDLE (handle.as_ref().unwrap().0), PROCESS_NAME_WIN32, PSTR::from_raw(lpstr.as_mut_ptr()), &mut lpdwsize );
    handle.iter().for_each ( |h| { CloseHandle(*h); } );
    PSTR::from_raw(lpstr.as_mut_ptr()).to_string() .ok() .map (|s| s.split("\\").last().map(|s| s.to_string())) .flatten()
} }



pub fn mic_mute_toggle () { unsafe {
    // note that the 'mute' appcommands actually do toggle, both for mic and volume
    PostMessageW (GetForegroundWindow(), WM_APPCOMMAND, WPARAM(0), LPARAM((APPCOMMAND_MICROPHONE_VOLUME_MUTE.0 << 16) as _));
    //PostMessageW (GetForegroundWindow(), WM_APPCOMMAND, WPARAM(0), LPARAM((APPCOMMAND_VOLUME_MUTE.0 << 16) as _));
    //PostMessageW (GetForegroundWindow(), WM_APPCOMMAND, WPARAM(0), LPARAM((APPCOMMAND_MEDIA_PLAY_PAUSE.0 << 16) as _));
} }


#[test]
pub fn test () {
    // todo: gotta cleanup these quick test portions
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

    //unsafe {
        //
    //}

}
