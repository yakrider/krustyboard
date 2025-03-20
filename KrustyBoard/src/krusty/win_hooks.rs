#![ allow (non_snake_case) ]


use std::thread::spawn;
use std::sync::RwLock;
use std::sync::atomic::{AtomicIsize, Ordering};
use once_cell::sync::OnceCell;
use windows::core::BOOL;
use windows::Win32::Foundation::{HWND};
use windows::Win32::UI::Accessibility::{HWINEVENTHOOK, SetWinEventHook};
use windows::Win32::UI::WindowsAndMessaging::{GetMessageW, MSG, EVENT_SYSTEM_FOREGROUND, EVENT_SYSTEM_MINIMIZEEND, EVENT_OBJECT_NAMECHANGE};



use crate::*;


#[derive(Debug, Default)]
pub struct FgndInfo {
    pub hwnd  : Hwnd,
    pub title : String,
    pub class : String,
    pub pid   : u32,
    pub exe   : String,
}

pub struct WinEventsListener {

    pub is_hooked : Flag,
    pub fgnd_info : RwLock <FgndInfo>,

    // we'll hold a hwnd ref for q-bar window to discount during fgnd tracking
    self_hwnd : AtomicIsize,

    // and we'll grab an InputProcessor ref during init (to pump fgnd events through)
    iproc : &'static InputProcessor,
}


impl WinEventsListener {

    pub fn instance () -> &'static WinEventsListener {
        static INSTANCE: OnceCell<WinEventsListener> = OnceCell::new();
        INSTANCE .get_or_init ( || {
            WinEventsListener {
                is_hooked : Flag::default(),
                fgnd_info : RwLock::new ( FgndInfo::default() ),
                self_hwnd : AtomicIsize::default(),
                iproc     : InputProcessor::instance(),
            }
            // in theory, we could setup-hooks here before returning, but we'll instead let user-code do that, e.g. based on configs
        } )
    }


    pub fn setup_win_event_hooks (&'static self) {
        /* Reference:
            pub unsafe fn SetWinEventHook (
                eventmin: u32, eventmax: u32, cb_dll: HINSTANCE, cb: WINEVENTPROC,
                idprocess: u32, idthread: u32, dwflags: u32
            ) -> HWINEVENTHOOK

            0x03   : EVENT_SYSTEM_FOREGROUND
            0x17   : EVENT_SYSTEM_MINIMIZEEND
            // ^^ we listen to un-minimize coz on clicks on taskbar win-os can un-minimize window w/o firing fgnd event !!

            0x800C : EVENT_OBJECT_NAMECHANGE        // e.g. when a window title changes
         */

        // before we start listening to events, lets update the fgnd_info with the current fgnd window
        self.proc_win_report__fgnd_hwnd (utils::win_get_fgnd());

        // if we already got setup once, we dont need to do it again
        if self.is_hooked.is_clear() { self.is_hooked.set(); } else { return }

        spawn ( move || unsafe {
            SetWinEventHook ( 0x0003, 0x0003, None, Some(Self::win_event_hook_cb), 0, 0, 0);
            SetWinEventHook ( 0x0017, 0x0017, None, Some(Self::win_event_hook_cb), 0, 0, 0);
            SetWinEventHook ( 0x800C, 0x800C, None, Some(Self::win_event_hook_cb), 0, 0, 0);

            // win32 only sends hook events to a thread with a 'message loop', so we'll  start a forever-loop waiting on GetMessage
            let mut msg: MSG = MSG::default();
            while BOOL(0) != GetMessageW (&mut msg, None, 0, 0) { };
        } );
    }


    pub unsafe extern "system" fn win_event_hook_cb (
        _id_hook: HWINEVENTHOOK, event: u32, hwnd: HWND,
        id_object: i32, id_child: i32, _id_thread: u32, _event_time: u32
    ) {
        if id_object == 0 && id_child == 0 {    // i.e its a parent window
            //let t = std::time::UNIX_EPOCH.elapsed().unwrap().as_millis();
            //println!("--> {:16} : hook event: 0x{:X}, hwnd:{:?}, id_object: 0x{:4X}", t, event, hwnd, id_object);
            match event {
                EVENT_SYSTEM_FOREGROUND   =>  WinEventsListener::instance().proc_win_report__fgnd_hwnd     (hwnd.into()),
                EVENT_SYSTEM_MINIMIZEEND  =>  WinEventsListener::instance().proc_win_report__fgnd_hwnd     (hwnd.into()),
                EVENT_OBJECT_NAMECHANGE   =>  WinEventsListener::instance().proc_win_report__title_changed (hwnd.into()),
                _ => { }
            }
        }
    }

    fn _stamp (&self) -> u128 {
        std::time::SystemTime::UNIX_EPOCH.elapsed().unwrap().as_millis()
    }

    pub fn record_self_hwnd (&self, hwnd:Hwnd) {
        self.self_hwnd.store (hwnd.0, Ordering::Relaxed)
    }

    fn proc_win_report__title_changed (&'static self, hwnd:Hwnd) {
        //println! ("@{:?} title-changed: {:?}", self._stamp(), hwnd);
        if self.fgnd_info.read() .is_ok_and (|fi| fi.hwnd == hwnd) {
            spawn ( move || { self.update_fgnd_info_title(hwnd) } );
        }
    }
    fn proc_win_report__fgnd_hwnd (&'static self, hwnd:Hwnd) {
        //println! ("@{:?} fgnd: {:?}", self._stamp(), hwnd);
        spawn ( move || { self.update_fgnd_info(hwnd) } );
    }

    fn update_fgnd_info_title (&'static self, hwnd:Hwnd) {
        let mut fi = self.fgnd_info.write().unwrap();
        if fi.hwnd == hwnd { fi.title = utils::get_win_title(hwnd); }
    }
    fn update_fgnd_info (&'static self, hwnd:Hwnd) {
        // we're going to filter out ourselves (if an hwnd has been stored for us)
        if hwnd == Hwnd (self.self_hwnd.load(Ordering::Relaxed)) {
            return
        }
        let fi_new = FgndInfo {
            hwnd,
            title : utils::get_win_title (hwnd),
            class : utils::get_win_class_by_hwnd (hwnd),
            pid   : utils::get_pid_by_hwnd (hwnd),
            exe   : utils::get_exe_by_pid (utils::get_pid_by_hwnd (hwnd)) .unwrap_or("".into()),
        };
        //println! ("fgnd: {:?}, exe: {:?}", hwnd, &fi_new.exe);

        // we'll then push out an event on fgnd change for whoever wants to set bindings to it
        self.iproc .inject_fgnd_event (fi_new.hwnd);

        // and update our fgnd-info cache
        *self.fgnd_info.write().unwrap() = fi_new;
    }


}
