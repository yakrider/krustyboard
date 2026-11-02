
use std::sync::OnceLock;
use std::thread::spawn;
use windows::core::{w, Result};
use windows::Win32::Foundation::{HWND, LPARAM, LRESULT, WPARAM};
use windows::Win32::UI::WindowsAndMessaging::{CreateWindowExW, DispatchMessageW, GetMessageW, RegisterClassW, TranslateMessage, DEVICE_NOTIFY_WINDOW_HANDLE, HWND_MESSAGE, MSG, WNDCLASSW};
use windows::Win32::System::LibraryLoader::GetModuleHandleW;
use windows::Win32::System::Power::{RegisterPowerSettingNotification, RegisterSuspendResumeNotification, POWERBROADCAST_SETTING};
use windows::Win32::System::RemoteDesktop::{WTSRegisterSessionNotification, NOTIFY_FOR_THIS_SESSION};
use windows::Win32::System::SystemServices::GUID_SESSION_DISPLAY_STATUS;

use crate::{Flag, InputProcessor};


pub struct SysEventsListener {

    is_hooked : Flag,

    // we'll grab an InputProcessor ref during init to use later to pump sys events
    iproc : &'static InputProcessor
}


impl SysEventsListener {

    pub fn instance () -> &'static SysEventsListener {
        static INSTANCE: OnceLock <SysEventsListener> = OnceLock::new();
        let inst = INSTANCE .get_or_init ( || {
            SysEventsListener {
                is_hooked : Flag::default(),
                iproc     : InputProcessor::instance(),
            }
        } );
        // we could in theory, set up the listener here directly, but we'll go with convention in rest of code and let user do it
        //inst.setup_system_events_listener();
        inst
    }


    pub fn setup_system_events_listener (&'static self) {

        // if we already tried to start once, we dont try to do so again
        if self.is_hooked.is_clear() { self.is_hooked.set(); } else { return }

        tracing::info! ("Starting System Events Listener..");

        spawn ( move || {
            if let Err(e) = self.attempt_system_events_listener_setup() {
                tracing::error! ("FAILURE setting up system events listener : {}", e);
            }
        } );
    }
    fn attempt_system_events_listener_setup (&'static self) -> Result<()> { unsafe {
        let hinst = GetModuleHandleW (None)?;
        let class = w!("krusty_sysevents_listener");
        RegisterClassW (&WNDCLASSW {
            lpfnWndProc: Some (Self::win_evs_proc_cb),
            hInstance: hinst.into(),
            lpszClassName: class,
            ..Default::default()
        });
        let hwnd = CreateWindowExW (
            Default::default(), class, w!(""), Default::default(),
            0, 0, 0, 0, Some(HWND_MESSAGE), None, Some(hinst.into()), None,
        )?;

        WTSRegisterSessionNotification (hwnd, NOTIFY_FOR_THIS_SESSION)?;
        let _susp = RegisterSuspendResumeNotification (hwnd.into(), DEVICE_NOTIFY_WINDOW_HANDLE)?;
        let _disp = RegisterPowerSettingNotification (
            hwnd.into(), &GUID_SESSION_DISPLAY_STATUS, DEVICE_NOTIFY_WINDOW_HANDLE,
        )?;

        let mut msg = MSG::default();
        while GetMessageW (&mut msg, None, 0, 0) .as_bool() {
            let _ = TranslateMessage(&msg);
            DispatchMessageW(&msg);
        }
        Ok (())
    } }



    unsafe extern "system" fn win_evs_proc_cb (hwnd: HWND, msg: u32, wp: WPARAM, lp: LPARAM) -> LRESULT { unsafe {
        use windows::Win32::UI::WindowsAndMessaging::*;   // for msg constants
        use crate::SysEvent_T::*;                         // for SysEv_ enums

        let inject = |e| SysEventsListener::instance().iproc.inject_sys_event(e);

        match msg {

            WM_POWERBROADCAST => {
                match wp.0 as u32 {

                    PBT_POWERSETTINGCHANGE => {
                        let s = &*(lp.0 as *const POWERBROADCAST_SETTING);
                        if s.PowerSetting == GUID_SESSION_DISPLAY_STATUS {
                            match s.Data[0] {
                                0 => inject (SysEv_DisplayOff),
                                1 => inject (SysEv_DisplayOn),
                                _ => {}
                    }  }   }

                    PBT_APMSUSPEND         => inject (SysEv_Suspend),
                    PBT_APMRESUMEAUTOMATIC => inject (SysEv_Resume),

                    _ => {}
                }
                LRESULT (0)
            }

            WM_WTSSESSION_CHANGE => {
                match wp.0 as u32 {
                    WTS_SESSION_LOCK    => inject (SysEv_Lock),
                    WTS_SESSION_UNLOCK  => inject (SysEv_Unlock),
                    WTS_SESSION_LOGON   => inject (SysEv_Logon),
                    WTS_SESSION_LOGOFF  => inject (SysEv_Logoff),
                    _ => {}
                }
                LRESULT (0)
            }

            _ => DefWindowProcW (hwnd, msg, wp, lp),
        }
    } }


}
