

use std::{
    mem::MaybeUninit,
    ptr::null_mut,
    thread,
    sync::atomic::{Ordering, AtomicPtr},
    os::raw::c_int,
};

use windows::Win32::{
    Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, WPARAM},
    UI::WindowsAndMessaging::*,
};

use once_cell::sync::Lazy;


use crate::{
    *, MouseButton::*, MouseWheel::*, EventPropagationDirective::*,
    KbdEvCbComboProcDirective::*, KbdEventCallbackFnType::*, MouseEventCallbackFnType::*,
};


// this is used for identifying the fake keypresses we insert, so we don't process them in an infinite loop
// note that 0xFFC3D44F is from ahk, though ahk uses further complex variations on it to signal more things
//const FAKE_EXTRA_INFO: ULONG_PTR = 0x14C;
pub const FAKE_EXTRA_INFO: usize = 0xFFC3D44F;


pub static KEYBD_HHOOK: Lazy <AtomicPtr <HHOOK>> = Lazy::new (AtomicPtr::default);
pub static MOUSE_HHOOK: Lazy <AtomicPtr <HHOOK>> = Lazy::new (AtomicPtr::default);



# [ allow (non_camel_case_types) ]
# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone) ]
pub enum EventPropagationDirective {
    EventProp_Continue,
    EventProp_Stop,
    EventProp_Undetermined,
}





/// set lower-level hook
pub fn set_hook (
    hook_id: WINDOWS_HOOK_ID,
    hook_ptr: &AtomicPtr<HHOOK>,
    hook_proc: unsafe extern "system" fn (c_int, WPARAM, LPARAM) -> LRESULT,
) {
    hook_ptr.store (
        unsafe { & mut SetWindowsHookExW (hook_id, Some(hook_proc), HINSTANCE(0), 0) .unwrap() },
        Ordering::Relaxed,
    );
}



/// unset lower-level hook
#[allow(dead_code)]
pub fn unset_hook (hook_ptr: &AtomicPtr<HHOOK>) {
    if !hook_ptr .load (Ordering::Relaxed) .is_null() {
        unsafe { UnhookWindowsHookEx ( * hook_ptr .load (Ordering::Relaxed) ) };
        hook_ptr .store (null_mut(), Ordering::Relaxed);
    }
}





/// Starts listening for bound input events.
pub fn handle_input_events () {
    if !MouseBindings::instance().read().unwrap().is_empty() {
        set_hook (WH_MOUSE_LL, &*MOUSE_HHOOK, mouse_proc);
    };
    if !KbdBindings::instance().read().unwrap().is_empty() || CombosMap::instance().is_enabled() {
        set_hook (WH_KEYBOARD_LL, &*KEYBD_HHOOK, kbd_proc);
    };
    // win32 sends hook events to a thread with a 'message loop', but we dont create any windows,
    //  so we wont get any actual messages, so we can just leave a forever waiting GetMessage instead of setting up a msg-loop
    // .. basically while its waiting, the thread is awakened simply to call kbd hook (for an actual msg, itd awaken give the msg)
    let mut msg: MSG = unsafe { MaybeUninit::zeroed().assume_init() };
    unsafe { GetMessageW (&mut msg, HWND(0), 0, 0) };
}






/// Keyboard lower-level-hook processor
pub unsafe extern "system"
fn kbd_proc (code: c_int, w_param: WPARAM, l_param: LPARAM) -> LRESULT {

    //use crate::{EventPropagationDirective::*, KbdEvCbComboProcDirective::*, KbdEventCallbackFnType::*};

    let return_call = || { CallNextHookEx(HHOOK(0), code, w_param, l_param) };

    if code < 0 { return return_call() }      // ms-docs says we MUST do this, so ig k fine

    /*
    // .. disabling this, as it's basically never applicable for our usage, and incurs runtime cost on every event
    if COMBO_MAPS_PROCESSOR.read().unwrap().is_none() && KEYBD_CALLBACKS.read().unwrap().is_empty() {
        unset_hook(&*KEYBD_HHOOK);
        return return_call();
    }*/

    let kb_struct = *(l_param.0 as *const KBDLLHOOKSTRUCT);

    /*
    println!("code: {:X}, w_param: {:X}, vk_code: {:#06X}, scanCode: {:#06X}, flags: {:X}, time: {}, dwExtraInfo: {:X}",
             code, w_param,
             kb_struct.vkCode, kb_struct.scanCode, kb_struct.flags, kb_struct.time, kb_struct.dwExtraInfo);
    */

    // if we injected this event ourselves, we should just bail
    if kb_struct.dwExtraInfo == FAKE_EXTRA_INFO { return return_call() }

    //let (mut do_ev_prop, mut do_combo_proc) = (EventProp_Continue, ComboProc_Enable);
    let mut event_proc_d = KbdEvProcDirectives::default();

    use KbdEventType::*;
    if let Some(ev_t) = match w_param.0 as u32 {
        WM_KEYDOWN      => Some (KbdEvent_KeyDown),
        WM_SYSKEYDOWN   => Some (KbdEvent_SysKeyDown),
        WM_KEYUP        => Some (KbdEvent_KeyUp),
        WM_SYSKEYUP     => Some (KbdEvent_SysKeyUp),
        _               => None,
    } {
        let key = KbdKey::from(u64::from(kb_struct.vkCode));
        let event = KbdEvent { ev_t, key, vk_code: kb_struct.vkCode as u32, sc_code: kb_struct.scanCode as u32 };

        // first route it through any per-key registered callbacks
        if let Some(cbe) = KbdBindings::instance() .read().unwrap() .get (&KbdEventCbMapKey::from_event(&event)) .as_ref() {
            //do_ev_prop = cbe.event_prop_directive;
            //do_combo_proc = cbe.combo_proc_directive;
            event_proc_d = cbe.event_proc_d;
            match &cbe.cb {
                KbdEvCbFn_InlineCallback (cb)  => {
                    let epd = cb(event);
                    if event_proc_d.event_prop_d == EventProp_Undetermined { event_proc_d = epd }
                }
                KbdEvCbFn_SpawnedCallback (cb) => {
                    let (cb, kbe) = (cb.clone(), event.clone());    // clone as we'll need the event later again
                    thread::spawn (move || cb(kbe));
                }
        } }

        // now lets call the bulk defaults/combos processor if its available, and if combo_proc for this event not disabled from above
        if event_proc_d.combo_proc_d == ComboProc_Enable {
            let mut event_prop_d = EventProp_Continue;
            if let Some (cb) = CombosMap::instance().get_processor().as_ref() {
                event_prop_d = cb(event)
            }
            if event_prop_d == EventProp_Stop {
                event_proc_d = KbdEvProcDirectives::new (event_prop_d, ComboProc_Disable)
            }
    }  }

    if event_proc_d.event_prop_d == EventProp_Stop {
        return LRESULT(1);  // returning with non-zero code signals OS to block further processing on the input event
    }

    return return_call()
}





#[allow(non_snake_case)]
fn HIWORD(l: u32) -> u16 { ((l >> 16) & 0xffff) as u16 }


/// mouse lower-level-hook processor
pub unsafe extern "system"
fn mouse_proc (code: c_int, w_param: WPARAM, l_param: LPARAM) -> LRESULT {

    let return_call = || { CallNextHookEx(HHOOK(0), code, w_param, l_param) };

    if code < 0 { return return_call() }      // ms-docs says we MUST do this, so ig k fine

    /*
    if MOUSE_CALLBACKS.read().unwrap().is_empty() {
        unset_hook(&*MOUSE_HHOOK);
        return return_call();
    }*/

    let mh_struct = &*(l_param.0 as *const MSLLHOOKSTRUCT);

    if mh_struct.dwExtraInfo == FAKE_EXTRA_INFO {
        // if we injected it, we should just bail (and call the next guy down the line)
        return return_call()
    }

    //println!("{:#?}", mh_struct);
    use {MouseEvent::*, MouseBtnEvent_T::*};
    if let Some (event) = match w_param.0 as u32 {
        WM_LBUTTONDOWN => Some ( btn_event { src_btn: LeftButton,   ev_t: BtnDown } ),
        WM_RBUTTONDOWN => Some ( btn_event { src_btn: RightButton,  ev_t: BtnDown } ),
        WM_MBUTTONDOWN => Some ( btn_event { src_btn: MiddleButton, ev_t: BtnDown } ),
        WM_XBUTTONDOWN => {
            match HIWORD(mh_struct.mouseData) {
                XBUTTON1 => Some ( btn_event { src_btn: X1Button, ev_t: BtnDown } ),
                XBUTTON2 => Some ( btn_event { src_btn: X2Button, ev_t: BtnDown } ),
                _ => None,
        } }
        WM_LBUTTONUP => Some ( btn_event { src_btn: LeftButton,   ev_t: BtnUp } ),
        WM_RBUTTONUP => Some ( btn_event { src_btn: RightButton,  ev_t: BtnUp } ),
        WM_MBUTTONUP => Some ( btn_event { src_btn: MiddleButton, ev_t: BtnUp } ),
        WM_XBUTTONUP => {
            match HIWORD(mh_struct.mouseData) {
                XBUTTON1 => Some ( btn_event { src_btn: X1Button, ev_t: BtnUp } ),
                XBUTTON2 => Some ( btn_event { src_btn: X2Button, ev_t: BtnUp } ),
                _ => None,
        } }
        WM_MOUSEWHEEL  => Some ( wheel_event { src_wheel: DefaultWheel,    delta: HIWORD(mh_struct.mouseData) as i16 as i32 } ),
        WM_MOUSEHWHEEL => Some ( wheel_event { src_wheel: HorizontalWheel, delta: HIWORD(mh_struct.mouseData) as i16 as i32 } ),

        //WM_MOUSEMOVE => move_event { x_pos: mh_struct.pt.x, y_pos: mh_struct.pt.y },
        _ => None,
    } {
        //{ let ec = event.clone(); thread::spawn(move || println!("{:?}", ec)); }

        let mut do_ev_prop = EventProp_Continue;

        if let Some(cbe) = MouseBindings::instance() .read().unwrap() .get (&MouseEventCbMapKey::from_event(&event)) .as_ref() {
            do_ev_prop = cbe.event_prop_directive;
            match &cbe.cb {
                MouseEvCbFn_InlineCallback (cb)  => {
                    let ev_prop_drctv = cb(event);
                    if cbe.event_prop_directive == EventProp_Undetermined { do_ev_prop = ev_prop_drctv }
                }
                MouseEvCbFn_SpawnedCallback (cb) => {
                    let cb = cb.clone();
                    thread::spawn (move || cb(event));
                }
            }
        } else {
            /*thread::spawn( move || {
                println!("no match for mouse event {:?} in mouse-cb-map", event.ev_src);
                println!("current mouse-cb map is :\n {:#?} ", (MOUSE_CALLBACKS.lock().unwrap()));
            });*/
        }
        if do_ev_prop == EventProp_Stop {
            return LRESULT(1);  // returning with non-zero code signals OS to block further processing on the input event
        }
    }
    return return_call();

}



