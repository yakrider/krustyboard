

use std::process::Command;
use std::thread;
use std::time::Duration;

use super::windows_utils::*;


static APP_RUNNER_LOC : &str = r#"D:\cygwin64\bin\run.exe"#;
static EXPLORER_LOC   : &str = r#"C:\Windows\explorer.exe"#;
static CHROME_LOC     : &str = r#"C:\Program Files\Google\Chrome\Application\chrome.exe"#;
static IRFAN_VIEW_LOC : &str = r#"C:\Program Files\IrfanView\i_view64.exe"#;
static VLC_LOC        : &str = r#"C:\Program Files\VideoLAN\VLC\vlc.exe"#;
static WINMERGE_LOC   : &str = r#"C:\Program Files\WinMerge\WinMergeU.exe"#;
static Q_DIR_LOC      : &str = r#"C:\Users\yakrider\AppData\Roaming\Q-Dir\Q-Dir.exe"#;
static IDEA_LOC       : &str = r#"C:\Program Files\JetBrains\IntelliJ IDEA Community Edition 2022.3.3\bin\idea64.exe"#;
static CLICK_MONITOR_CDC_LOC : &str = r#"D:\yakdat\downloads\ins-bin\_MONITOR_BRIGHTNESS_UTILS\ClickMonitorDDC_7_2\ClickMonitorDDC_7_2.exe"#;


/* reminder - re UAC elevation when starting processes while running krusy elevated (as we often want to do)
    - since processes started here directly
*/

pub fn start_chrome() {
    let _ = Command::new(APP_RUNNER_LOC) .arg(CHROME_LOC) .spawn();
    setup_opened_window("Chrome");
}

pub fn start_chrome_incognito() {
    let _ = Command::new(APP_RUNNER_LOC) .arg(CHROME_LOC) .arg(r#"--profile-directory="Default" -incognito"#) .spawn();
    setup_opened_window("Chrome");
}

pub fn start_chrome_app (app_id:&str) {
    let _ = Command::new(APP_RUNNER_LOC) .arg(CHROME_LOC) .arg(format!(r#"--profile-directory="Default" --app-id={}"#, app_id)) .spawn();
}
pub fn start_chrome_bard () { start_chrome_app ("nohacooabmgpjcdeajcfjgkpfibiffjf") }

pub fn start_alt_file_explorer() {
    let _ = Command::new(EXPLORER_LOC) .arg(Q_DIR_LOC) .spawn();
}

pub fn start_irfanview() {
    let _ = Command::new(EXPLORER_LOC) .arg(IRFAN_VIEW_LOC) .spawn();
    setup_opened_window("IrfanView");
}

pub fn start_vlc() {
    let _ = Command::new(EXPLORER_LOC) .arg(VLC_LOC) .spawn();
}

pub fn start_winmerge_clipboard() {
    let _ = Command::new(APP_RUNNER_LOC) .arg(WINMERGE_LOC) .arg("/clipboard-compare").spawn();
}

pub fn start_idea_diff() {
    let _ = Command::new(APP_RUNNER_LOC) .arg(IDEA_LOC) .arg("diff").spawn();
}

pub fn open_mic_cpl () {
    let _ = Command::new("rundll32.exe") .arg("shell32.dll,Control_RunDLL") .arg("mmsys.cpl,,1").spawn();
}

pub fn bringup_click_monitor_cdc () {
    let _ = Command::new(APP_RUNNER_LOC) .arg(CLICK_MONITOR_CDC_LOC) .spawn();
}

fn setup_opened_window (win_class_part_match_str: &str) {
    use crate::*;
    let match_str = win_class_part_match_str.to_string();
    thread::spawn ( move || {
        fn attempt_setup (cls_str: &String) -> bool {
            if get_fgnd_win_class().contains(cls_str) {
                //win_fgnd_center_if_past_screen();
                //win_fgnd_place_right_if_past_screen();
                let ks = KrustyState::instance();
                snap_closest_edge_side(&ks, RectEdgeSide::Right);
                //snap_closest_edge_side(&ks, RectEdgeSide::Top);
                //thread::sleep(Duration::from_millis(300));
                win_fgnd_toggle_vertmax();
                true
            } else { false }
        }
        thread::sleep(Duration::from_millis(500 )); if attempt_setup(&match_str) == true { return }
        thread::sleep(Duration::from_millis(1000)); if attempt_setup(&match_str) == true { return }
        thread::sleep(Duration::from_millis(2000)); if attempt_setup(&match_str) == true { return }
    } );
}

