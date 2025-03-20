
use std::sync::Arc;

use serde::Serialize;
use serde_json;

use windows::core::HSTRING;
use windows::Win32::Foundation::CloseHandle;
use windows::Win32::UI::WindowsAndMessaging::{AllowSetForegroundWindow, ASFW_ANY};
use windows::Win32::Storage::FileSystem::{CreateFileW, WriteFile, FILE_GENERIC_WRITE, FILE_SHARE_WRITE, OPEN_EXISTING, FILE_FLAGS_AND_ATTRIBUTES};

use crate::AF;


const PIPE_NAME: &str = r"\\.\pipe\switche_krusty_cmd_pipe";


#[derive (Debug, Clone, Serialize)]
pub enum SwitchePipeCmd {
    Invoke,
    ScrollDown,
    ScrollUp,
    ScrollEnd,
    ScrollEndDisarm,
    SwitchNextNonMinimized,
    SwitchZIndex(usize),
    SnapListRefresh,
    SnapListSwitchNext,
    SnapListSwitchPrev,
    SnapListSwitchTop,
    SnapListSwitchBottom,
    SwitchApp {
        exes: Vec<String>,
        title: Option<String>,
        partial: bool,
    },
}


impl SwitchePipeCmd {

    pub fn send (&self) -> windows::core::Result<()> { unsafe {
        // lets get the pipe first (and bail if we cant)
        let h_pipe = CreateFileW (
            &HSTRING::from(PIPE_NAME),
            FILE_GENERIC_WRITE.0,
            FILE_SHARE_WRITE,
            None,
            OPEN_EXISTING,
            FILE_FLAGS_AND_ATTRIBUTES::default(),
            None,
        )?;

        // we'll try and give away fgnd setting privileges
        // todo: here we'll give it up widely, but could limit it to only the sw process (but we'd have to find/query its pid first)
        let _ = AllowSetForegroundWindow (ASFW_ANY);

        // then we'll try and send the cmd
        let cmd_json = serde_json::to_string(&self).unwrap();
        let mut bytes_written = 0;
        let _ = WriteFile(
            h_pipe,
            Some (cmd_json.as_ref()),
            Some (&mut bytes_written),
            None,
        );
        let _ = CloseHandle(h_pipe);
        Ok(())
    } }

    // and for syntactic sugar
    pub fn send_af (&self) -> AF {
        let cmd = self.clone();
        Arc::new (move || { let _ = cmd.send(); } )
    }

    // and some static syntactic sugar methods
    pub fn sw_exe_af (exes:&[String]) -> AF {
        Self::SwitchApp { exes: exes.to_vec(), title:None, partial:false } .send_af()
    }
    pub fn sw_exe_title_af (exe:&str, title:&str, partial:bool) -> AF {
        Self::SwitchApp { exes: vec!(exe.into()), title: Some(title.into()), partial } .send_af()
    }

}
