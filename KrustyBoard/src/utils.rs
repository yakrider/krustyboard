

/// deprecated
pub mod brightness_ps_wmi {
    use std::process::Command;

    static PS_LOC: &str = r#"C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe"#;
    static BRIGHT_Q_CMD: &str = "(Get-WmiObject -Namespace root/WMI -Class WmiMonitorBrightness).CurrentBrightness";

    fn set_cmd (v:i32) -> String { format!("(Get-WmiObject -Namespace root/WMI -Class WmiMonitorBrightnessMethods).WmiSetBrightness(1,{})", v) }

    pub fn incr_brightness (incr:i32) {
        let cur_b = Command::new(PS_LOC).arg(BRIGHT_Q_CMD).output().ok()
            .map (|o| String::from_utf8(o.stdout).ok()) .flatten()
            .map (|s| s.lines().next().map(|s| s.to_owned().parse::<i32>().ok()).flatten()) .flatten();

        let set_b_cmd = |v:i32| { Command::new(PS_LOC).arg(set_cmd(v)).spawn(); };
        cur_b .iter() .for_each(|v| set_b_cmd((v + incr).abs()));
    }
}


pub mod brightness_utils {
    use brightness::{Brightness};
    use futures::{executor::block_on, TryStreamExt};

    fn incr_b_limited (v:u32, incr:i32) -> u32 {
        let res = v as i64 + incr as i64;
        let res = if res < 0 { 0 } else if res > 100 { 100 } else { res };
        res as u32
    }
    async fn incr_disp_brightness_async (incr: i32) -> Result<(), brightness::Error> {
        brightness::brightness_devices().try_for_each(|mut dvc| async move {
            let v = dvc.get().await?;
            let v_new = incr_b_limited(v, incr);
            if v != v_new {
                dvc.set(v_new).await
            } else { Ok(()) }
        }).await
    }

    pub fn incr_brightness(incr: i32) -> Result<(), brightness::Error> {
        block_on (incr_disp_brightness_async(incr))
    }
}

pub mod process_utils {
    use std::process::Command;
    use std::thread;
    use std::time::Duration;

    use crate::utils::window_utils;

    static APP_RUNNER_LOC: &str = r#"C:\cygwin64\bin\run.exe"#;
    static CHROME_LOC: &str = r#"C:\Program Files\Google\Chrome\Application\chrome.exe"#;
    static IRFAN_VIEW_LOC: &str = r#"C:\Program Files\IrfanView\i_view64.exe"#;
    static WINMERGE_LOC: &str = r#"C:\Program Files\WinMerge\WinMergeU.exe"#;
    static IDEA_LOC: &str = r#"C:\Program Files\JetBrains\IntelliJ IDEA Community Edition 2021.1.1\bin\idea64.exe"#;

    pub fn start_chrome() {
        Command::new(APP_RUNNER_LOC).arg(CHROME_LOC).arg(r#"--profile-directory="Default""#) .spawn();
        setup_opened_window("Chrome");
    }
    pub fn start_chrome_incognito() {
        Command::new(APP_RUNNER_LOC).arg(CHROME_LOC).arg(r#"--profile-directory="Default" -incognito"#) .spawn();
        setup_opened_window("Chrome");
    }

    pub fn start_irfanview() {
        Command::new(APP_RUNNER_LOC).arg(IRFAN_VIEW_LOC) .spawn();
        setup_opened_window("IrfanView");
    }

    pub fn start_winmerge_clipboard() {
        Command::new(APP_RUNNER_LOC).arg(WINMERGE_LOC).arg("/clipboard-compare").spawn();
    }

    pub fn start_idea_diff() {
        Command::new(APP_RUNNER_LOC).arg(IDEA_LOC).arg("diff").spawn();
    }

    fn setup_opened_window (win_class_part_match: &str) {
        thread::sleep(Duration::from_millis(1000));
        if window_utils::get_fgnd_win_class().contains(win_class_part_match) {
            window_utils::win_fgnd_center_if_past_screen();
            thread::sleep(Duration::from_millis(100));
            window_utils::win_fgnd_toggle_vertmax();
        }
    }

}


pub mod window_utils {

    use windows::{
        Win32::Foundation::{BOOL, HWND, LPARAM, RECT, WPARAM},
        Win32::UI::WindowsAndMessaging::{
            GetForegroundWindow, GetSystemMetrics, GetWindowRect, HTCAPTION, HTTOP, MoveWindow,
            PostMessageW, ShowWindow, SM_CXSCREEN, SM_CYSCREEN, SW_NORMAL, WM_NCLBUTTONDBLCLK
        },
    };
    use windows::Win32::UI::WindowsAndMessaging::{GetClassNameW, HTMAXBUTTON, HTZOOM, SC_MAXIMIZE, SC_RESTORE, WM_NCLBUTTONUP, WM_SYSCOMMAND};

    pub fn win_get_fgnd_rect () -> (HWND, RECT) { unsafe {
        let hwnd = GetForegroundWindow();
        let mut rect = RECT::default();
        GetWindowRect (hwnd, &mut rect as *mut RECT);
        (hwnd, rect)
    } }

    pub fn win_fgnd_move (dx:i32, dy:i32) { unsafe {
        let (hwnd, r) = win_get_fgnd_rect();
        MoveWindow (hwnd, r.left + dx, r.top + dy, r.right-r.left, r.bottom-r.top, true);
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


    pub fn win_fgnd_toggle_vertmax () { unsafe {
        // works by sending doubleclick at top of window (which also works manually)
        PostMessageW (GetForegroundWindow(), WM_NCLBUTTONDBLCLK, WPARAM(HTTOP as _), LPARAM(0));
    } }


    pub fn win_fgnd_toggle_max () { unsafe {
        // should in theory work by sending doubleclick at window titlebar (which works manually)
        //PostMessageW (GetForegroundWindow(), WM_NCLBUTTONDBLCLK, WPARAM(HTCAPTION as _), LPARAM(0));
        // ^^ except, for win32 windows, it works to max, but when already max it CLOSES them (coz it sends it at left-top icon?)
        //PostMessageW (GetForegroundWindow(), WM_NCLBUTTONUP, WPARAM(HTMAXBUTTON as _), LPARAM(0));
        // ^^ doesnt work
        //PostMessageW (GetForegroundWindow(), WM_SYSCOMMAND, WPARAM(SC_MAXIMIZE as _), LPARAM(0));
        // ^^ only maximizes not toggle, and no easy way to know if its maxed other than guessing from dimensions
        win_fgnd_toggle_max_guess();
        // ^^ so we'll finally just do it based on window rect dimensions .. should work for vast majority cases
    } }
    fn win_fgnd_toggle_max_guess () { unsafe {
        let (scr_w, scr_h) = (GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN));
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


    pub fn get_fgnd_win_class () -> String { unsafe {
        let mut lpstr: [u16; 120] = [0; 120];
        let len = GetClassNameW(GetForegroundWindow(), &mut lpstr);
        String::from_utf16_lossy(&lpstr[..(len as _)])
    } }
    // todo
    pub fn get_fgnd_win_exe () -> String { "".to_string() }
    pub fn get_fgnd_win_title () -> String { "".to_string() }



    pub fn test () {
        // todo: remove
        // quick fn hooked to main in examples/test to quickly rerun testing here
        println! ("todo..");
        //println!("{:?}",get_fgnd_win_class());

        //super::brightness::incr_brightness(1);
        //enum_windows_test();

        unsafe {
            //
        }

    }

}

