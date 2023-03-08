


// we deprecated the older brightness module based on windows powershell via wmi
//.. in favor of a cargo crate that seems to fit our bill well
//.. so while we keep the older code below, we'll re-export out the actual intended module from here
pub use self::brightness_utils::*;



/// !!  deprecated  !!
# [ allow (dead_code) ]
mod brightness_ps_wmi {

    use std::process::Command;

    static PS_LOC: &str = r#"C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe"#;
    static BRIGHT_Q_CMD: &str = "(Get-WmiObject -Namespace root/WMI -Class WmiMonitorBrightness).CurrentBrightness";

    fn set_cmd (v:i32) -> String { format!("(Get-WmiObject -Namespace root/WMI -Class WmiMonitorBrightnessMethods).WmiSetBrightness(1,{})", v) }

    pub fn incr_brightness (incr:i32) {
        let cur_b = Command::new(PS_LOC).arg(BRIGHT_Q_CMD).output().ok()
            .map (|o| String::from_utf8(o.stdout).ok()) .flatten()
            .map (|s| s.lines().next().map(|s| s.to_owned().parse::<i32>().ok()).flatten()) .flatten();

        let set_b_cmd = |v:i32| { let _ = Command::new(PS_LOC).arg(set_cmd(v)).spawn(); };
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

