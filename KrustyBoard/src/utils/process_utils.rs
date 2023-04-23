

use std::process::Command;
use std::thread;
use std::time::Duration;

use super::windows_utils::*;


static APP_RUNNER_LOC : &str = r#"C:\cygwin64\bin\run.exe"#;
static CHROME_LOC     : &str = r#"C:\Program Files\Google\Chrome\Application\chrome.exe"#;
static IRFAN_VIEW_LOC : &str = r#"C:\Program Files\IrfanView\i_view64.exe"#;
static WINMERGE_LOC   : &str = r#"C:\Program Files\WinMerge\WinMergeU.exe"#;
static IDEA_LOC       : &str = r#"C:\Program Files\JetBrains\IntelliJ IDEA Community Edition 2022.3.3\bin\idea64.exe"#;


pub fn start_chrome() {
    let _ = Command::new(APP_RUNNER_LOC).arg(CHROME_LOC).arg(r#"--profile-directory="Default""#) .spawn();
    setup_opened_window("Chrome");
}

pub fn start_chrome_incognito() {
    let _ = Command::new(APP_RUNNER_LOC).arg(CHROME_LOC).arg(r#"--profile-directory="Default" -incognito"#) .spawn();
    setup_opened_window("Chrome");
}

pub fn start_irfanview() {
    let _ = Command::new(APP_RUNNER_LOC).arg(IRFAN_VIEW_LOC) .spawn();
    setup_opened_window("IrfanView");
}

pub fn start_winmerge_clipboard() {
    let _ = Command::new(APP_RUNNER_LOC).arg(WINMERGE_LOC).arg("/clipboard-compare").spawn();
}

pub fn start_idea_diff() {
    let _ = Command::new(APP_RUNNER_LOC).arg(IDEA_LOC).arg("diff").spawn();
}

fn setup_opened_window (win_class_part_match: &str) {
    thread::sleep(Duration::from_millis(2000));
    if get_fgnd_win_class().contains(win_class_part_match) {
        win_fgnd_center_if_past_screen();
        thread::sleep(Duration::from_millis(300));
        win_fgnd_toggle_vertmax();
    }
}

pub fn open_mic_cpl () {
    let _ = Command::new("rundll32.exe").arg("shell32.dll,Control_RunDLL").arg("mmsys.cpl,,1").spawn();
}

