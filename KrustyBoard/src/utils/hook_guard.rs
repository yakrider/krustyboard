use std::{env, thread};
use std::time::Duration;
use std::process::{Child, Command};
use std::sync::Mutex;
use core::ffi::c_int;
use once_cell::sync::OnceCell;
use windows::core::BOOL;
use windows::Win32::Foundation::{HANDLE, LPARAM, LRESULT, WPARAM};
use windows::Win32::System::JobObjects::{AssignProcessToJobObject, CreateJobObjectW, JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE, JOBOBJECT_EXTENDED_LIMIT_INFORMATION, JobObjectExtendedLimitInformation, SetInformationJobObject};
use windows::Win32::System::Threading::{OpenProcess, PROCESS_SET_QUOTA, PROCESS_TERMINATE};
use windows::Win32::UI::WindowsAndMessaging::{CallNextHookEx, GetMessageW, MSG, SetWindowsHookExW, WH_KEYBOARD_LL};


/// The idea here is that we needed a setup to ensure that hooks from the krusty process were NOT the most recent installed hook.
/// So the goal of this module is to switch behavior based on whether this process was started with '-hook-guard'
/// If not, we dont do anything immediately, and simply allow spawning off a hook guard process later on demand
/// Else, we simply assume we are the hook-guard and install a dummy LL Keyboard hook and sit on it, never returning!

pub struct HookGuard {
    // we'll hold a job object to assign any hook-guard processes we create (so they get auto cleaned up on exit)
    job : Option <isize>,
    // and we'll keep a handle to any active hook-guard child process (to restart it when need be)
    guard : Mutex <Option <Child>>,
}


impl HookGuard {

    pub fn instance () -> &'static HookGuard {

        static INSTANCE : OnceCell <HookGuard> = OnceCell::new();

        INSTANCE .get_or_init ( || unsafe {
            // if we got started with "--hook-guard", we install dummy hook and sit checking on it forever
            // else, we're not the guard, nothing to do right away .. we'll just launch some guards on demand later
            if env::args() .any (|arg| arg == "--hook-guard") {
                let _ = SetWindowsHookExW (WH_KEYBOARD_LL, Some(hook_proc), None, 0);
                let mut msg: MSG = MSG::default();
                while BOOL(0) != GetMessageW (&mut msg, None, 0, 0) { }
            }

            // we'll create a job object that we'll associate hook-guards to, and set to kill the guards if the main process exits
            let job = CreateJobObjectW (None, None) .ok();
            if let Some(jh) = job.as_ref() {
                let mut info = JOBOBJECT_EXTENDED_LIMIT_INFORMATION::default();
                info.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
                let _ = SetInformationJobObject (*jh, JobObjectExtendedLimitInformation, &info as *const _ as *const _, size_of_val(&info) as u32);
            }
            let job = job .map (|h| h.0 as isize);
            HookGuard { job,  guard : Mutex::new (None) }
        } )

    }


    /// this sets up and launches a new hook-guard process (killing any old ones).
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
        thread::spawn ( move || {
            // and just to make accidental process bombing during dev etc maangeable, we'll add a small delay
            thread::sleep (Duration::from_millis (500));

            // lets recheck to ensure something hasnt already thrown up a guard
            if self.guard.lock().unwrap().is_some() { return }

            // and now we can launch the guard process w the ll-kbd-hook
            if let Ok (guard) = Command::new (env::current_exe().unwrap()) .arg("--hook-guard") .spawn() { unsafe {
                println! ("Launched a new hook-guard with pid: {:?}", guard.id());
                // we'll also add this process to our job (so it will be cleaned up if we get killed/exit)
                let gh = OpenProcess (PROCESS_TERMINATE | PROCESS_SET_QUOTA, false, guard.id());
                if let (Some(jh), Ok(gh)) = (self.job, gh) {
                    let _ = AssignProcessToJobObject (HANDLE (jh as _), gh);
                }
                *self.guard.lock().unwrap() = Some(guard);
            } }
        } );
    }

}

pub unsafe extern "system"
fn hook_proc ( code: c_int, w_param: WPARAM, l_param: LPARAM ) -> LRESULT {
    CallNextHookEx (None, code, w_param, l_param)
}
