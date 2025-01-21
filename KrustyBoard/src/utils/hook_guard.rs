use std::{env, thread};
use std::time::Duration;
use std::process::{Child, Command};
use std::sync::Mutex;
use core::ffi::c_int;
use once_cell::sync::OnceCell;
use windows::Win32::Foundation::{BOOL, HINSTANCE, HWND, LPARAM, LRESULT, WPARAM};
use windows::Win32::UI::WindowsAndMessaging::{CallNextHookEx, GetMessageW, HHOOK, MSG, SetWindowsHookExW, WH_KEYBOARD_LL};


/// The idea here is that we needed a setup to ensure that hooks from the krusty process were NOT the most recent installed hook.
/// So the goal of this module is to switch behavior based on whether this process was started with '-hook-guard'
/// If not, we dont do anything immediately, and simply allow spawning off a hook guard process later on demand
/// Else, we simply assume we are the hook-guard and install a dummy LL Keyboard hook and sit on it, never returning!

pub struct HookGuard {
    guard : Mutex <Option <Child>>,
}

impl HookGuard {

    pub fn instance () -> &'static HookGuard {
        static INSTANCE : OnceCell <HookGuard> = OnceCell::new();

        INSTANCE .get_or_init ( || unsafe {
            // if we got started with "--hook-guard", we install dummy hook and sit checking on it forever
            // else, we're not the guard, nothing to do right away .. we'll just launch some guards on demand later
            if env::args() .any (|arg| arg == "--hook-guard") {
                let _ = SetWindowsHookExW (WH_KEYBOARD_LL, Some(hook_proc), HINSTANCE(0), 0);
                let mut msg: MSG = MSG::default();
                while BOOL(0) != GetMessageW (&mut msg, HWND(0), 0, 0) { }
            }
            HookGuard { guard : Mutex::new (None) }
        } )
    }

    pub fn guard (&'static self) {
        // if we ever get called, we must not have been the guard itself, so we should allow launching one
        // if we've already launched one, we'd have a process-handle stored, we'll kill that and relaunch

        let mut guard = self.guard.lock().unwrap();
        if guard.is_some() {
            println! ("killing hook-guard with pid : {:?}", guard.as_ref().unwrap().id());
            let _ = guard.as_mut().unwrap().kill();
            *guard = None;
        }
        // we can now launch a new guard process ..
        // and just to make accidental process bombing during dev etc maangeable, we'll add a small delay
        thread::spawn ( move || {
            thread::sleep (Duration::from_millis (500));
            // lets recheck to ensure something hasnt already thrown up a guard
            if self.guard.lock().unwrap().is_some() { return }
            // and now we can launch the guard process w the ll-kbd-hook
            if let Ok (guard) = Command::new (env::current_exe().unwrap()) .arg("--hook-guard") .spawn() {
                println! ("Launched a new hook-guard with pid: {:?}", guard.id());
                *self.guard.lock().unwrap() = Some(guard);
            }
        } );
    }

}

pub unsafe extern "system"
fn hook_proc ( code: c_int, w_param: WPARAM, l_param: LPARAM ) -> LRESULT {
    CallNextHookEx (HHOOK(0), code, w_param, l_param)
}
