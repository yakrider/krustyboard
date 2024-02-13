

use std::{
    mem::MaybeUninit,
    ptr::null_mut,
    thread,
    sync::{Arc, RwLock},
    sync::atomic::{Ordering, AtomicPtr, AtomicU32},
    sync::mpsc::{sync_channel, SyncSender},
    os::raw::c_int,
    ops::Deref,
};

use windows::Win32::{
    Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, WPARAM},
    UI::WindowsAndMessaging::*,
};

use once_cell::sync::{OnceCell};

use crate::{
    *, MouseButton::*, MouseWheel::*, EventPropagationDirective::*,
    KbdEvCbComboProcDirective::*, KbdEventCallbackFnType::*, MouseEventCallbackFnType::*,
};
use crate::utils::{win_set_cur_process_priority_high, win_set_thread_dpi_aware};


// this is used for identifying the fake keypresses we insert, so we don't process them in an infinite loop
// note that 0xFFC3D44F is from ahk, though ahk uses further complex variations on it to signal more things
//const FAKE_EXTRA_INFO: ULONG_PTR = 0x14C;
pub const FAKE_EXTRA_INFO: usize = 0xFFC3D44F;



# [ allow (non_camel_case_types) ]
# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone) ]
pub enum EventPropagationDirective {
    EventProp_Continue,
    EventProp_Stop,
    EventProp_Undetermined,
}



pub struct _InputProcessor {
    // we hold handles returned by OS to the lower level kbd/mouse hooks we set (needed to unhook them later)
    kbd_hook   : AtomicPtr <HHOOK>,
    mouse_hook : AtomicPtr <HHOOK>,
    // the kbd/mouse bindings hold mapping for keys/buttons events to bound actions
    pub kbd_bindings     : KbdBindings,
    pub mouse_bindings   : MouseBindings,
    // the combos processor, if present, is called after any bindings callbacks are processed
    pub combos_processor : Arc <RwLock <Option <KbdEvCbFn_ComboProc_T>>>,
    // for queued callback types, we'll hold senders of the kbd/mouse events channels
    pub kbd_af_queue     : SyncSender <( KbdEvCbFn_OffThreadCb_T, KbdEvent )>,
    pub mouse_af_queue   : SyncSender <( MouseEvCbFn_OffThreadCb_T, MouseEvent )>,
}

# [ derive (Clone) ]
pub struct InputProcessor ( Arc <_InputProcessor> );

impl Deref for InputProcessor {
    type Target = _InputProcessor;
    fn deref(&self) -> &_InputProcessor { &self.0 }
}


impl InputProcessor {

    /// Creates or returns the singleton InputProcessor.
    /// (.. and when initializing, starts the mpsc channels for kbd/mouse event actions too)
    pub fn instance() -> InputProcessor {
        static INSTANCE: OnceCell <InputProcessor> = OnceCell::new();
        INSTANCE .get_or_init ( || {
            // we'll create and spawn out the channels for kbd and mouse queued events
            // (we expect queue drained asap, for kbd 20 should be plenty, but wheel-events on spin can get quite bursty)
            let (kbd_queue_sender,   kbd_queue_receiver)   = sync_channel::<(KbdEvCbFn_OffThreadCb_T, KbdEvent)>(20);
            let (mouse_queue_sender, mouse_queue_receiver) = sync_channel::<(MouseEvCbFn_OffThreadCb_T, MouseEvent)>(100);
            // lets get them going too
            thread::spawn (move || { win_set_thread_dpi_aware(); while let Ok((af,ev)) = kbd_queue_receiver.recv()   { af(ev) } });
            thread::spawn (move || { win_set_thread_dpi_aware(); while let Ok((af,ev)) = mouse_queue_receiver.recv() { af(ev) } });
            // the processor will hold the senders to these queues for everyone to clone/use
            InputProcessor ( Arc::new ( _InputProcessor {
                kbd_hook   : AtomicPtr::default(),
                mouse_hook : AtomicPtr::default(),
                kbd_bindings     : KbdBindings::instance(),
                mouse_bindings   : MouseBindings::instance(),
                combos_processor : Arc::new ( RwLock::new ( None) ),
                kbd_af_queue     : kbd_queue_sender,
                mouse_af_queue   : mouse_queue_sender,
            } ) )
        } ) .clone()
    }


    pub fn set_combo_processor (&self, cb:KbdEvCbFn_ComboProc_T) {
        *self.combos_processor.write().unwrap() = Some(cb);
    }
    pub fn clear_combo_processor (&self) {
        *self.combos_processor.write().unwrap() = None;
    }


    fn set_hook (
        hook_id: WINDOWS_HOOK_ID,
        hook_ptr: &AtomicPtr<HHOOK>,
        hook_proc: unsafe extern "system" fn (c_int, WPARAM, LPARAM) -> LRESULT,
    ) {
        let mut hook_handle = unsafe { SetWindowsHookExW (hook_id, Some(hook_proc), HINSTANCE(0), 0) };
        hook_handle .iter_mut() .for_each (|hh| hook_ptr.store (hh, Ordering::Relaxed));
    }
    fn set_kbd_hook   (&self) { InputProcessor::set_hook (WH_KEYBOARD_LL, &self.kbd_hook,   kbd_proc); }
    fn set_mouse_hook (&self) { InputProcessor::set_hook (WH_MOUSE_LL,    &self.mouse_hook, mouse_proc); }


    fn unset_hook (hook_ptr: &AtomicPtr<HHOOK>) {
        if !hook_ptr .load (Ordering::Relaxed) .is_null() {
            let _ = unsafe { UnhookWindowsHookEx ( * hook_ptr .load (Ordering::Relaxed) ) };
            hook_ptr .store (null_mut(), Ordering::Relaxed);
        }
    }
    #[allow(dead_code)]
    fn unset_kbd_hook   (&self) { InputProcessor::unset_hook (&self.kbd_hook) }

    #[allow(dead_code)]
    fn unset_mouse_hook (&self) { InputProcessor::unset_hook (&self.mouse_hook) }


    /// Starts listening for bound input events.
    pub fn begin_input_processing (&self) {
        if !self.kbd_bindings.read().unwrap().is_empty() || self.combos_processor.read().unwrap().is_some() {
            self.set_kbd_hook();
        };
        if !self.mouse_bindings.read().unwrap().is_empty() {
            self.set_mouse_hook();
        };
        // before starting to listen to events, lets set this thread dpi-aware (for rare cases we do direct processing upon callback)
        win_set_thread_dpi_aware();

        // also, we might as well set the whole process higher priority, as we dont want lag in basic input processing
        let _ = win_set_cur_process_priority_high();

        // win32 sends hook events to a thread with a 'message loop', but we dont create any windows,
        //  so we wont get any actual messages, so we can just leave a forever waiting GetMessage instead of setting up a msg-loop
        // .. basically while its waiting, the thread is awakened simply to call kbd hook (for an actual msg, itd awaken give the msg)
        let mut msg: MSG = unsafe { MaybeUninit::zeroed().assume_init() };
        unsafe { GetMessageW (&mut msg, HWND(0), 0, 0) };
    }


}





/// debug printout
#[allow(dead_code)]
fn print_kbd_event (wp:&WPARAM, kbs:&KBDLLHOOKSTRUCT) {
    println!("w_param: {:X}, vk_code: {:#06X}, scanCode: {:#06X}, flags: {:#018b}, time: {}, dwExtraInfo: {:X}",
             wp.0, kbs.vkCode, kbs.scanCode, kbs.flags.0, kbs.time, kbs.dwExtraInfo);
}

/// Keyboard lower-level-hook processor
pub unsafe extern "system"
fn kbd_proc (code: c_int, w_param: WPARAM, l_param: LPARAM) -> LRESULT {

    let return_call = || { CallNextHookEx(HHOOK(0), code, w_param, l_param) };

    if code < 0 { return return_call() }      // ms-docs says we MUST do this, so ig k fine

    let iproc = InputProcessor::instance();
    /* //.. disabling this, as it's basically never applicable for our usage, and incurs runtime cost on every event
    if iproc.kbd_bindings.read().unwrap().is_empty() && !CombosMap::instance().is_enabled() {
        iproc.unset_kbd_hook();
        return return_call();
    }*/

    let kb_struct = *(l_param.0 as *const KBDLLHOOKSTRUCT);

    //print_kbd_event (&w_param, &kb_struct);

    // if we injected this event ourselves, we should just bail
    if kb_struct.dwExtraInfo == FAKE_EXTRA_INFO { return return_call() }

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
        let stamp = kb_struct.time;
        let injected = kb_struct.flags & LLKHF_INJECTED == LLKHF_INJECTED;
        let event = KbdEvent { ev_t, key, vk_code: kb_struct.vkCode as u32, sc_code: kb_struct.scanCode as u32, stamp, injected };

        // first route it through any per-key registered callbacks
        if let Some(cbe) = iproc.kbd_bindings .read().unwrap() .get (&KbdEventCbMapKey::from_event(&event)) .as_ref() {
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
                KbdEvCbFn_QueuedCallback (cb) => {
                    let _ = iproc.kbd_af_queue.send((cb.clone(), event.clone()));
                }
        } }

        // now lets call the bulk defaults/combos processor if its available, and if combo_proc for this event not disabled from above
        if event_proc_d.combo_proc_d == ComboProc_Enable {
            let mut event_prop_d = EventProp_Continue;
            if let Some (cb) = iproc.combos_processor.read().unwrap().as_ref() {
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


static LAST_STAMP: AtomicU32 = AtomicU32::new(0);
#[allow(dead_code)]
fn print_mouse_ev (ev:MouseEvent) {
    let last_s = LAST_STAMP.swap(ev.get_stamp(), Ordering::Relaxed);
    let gap_dur = ev.get_stamp() - last_s;
    thread::spawn(move || println!("{:?}, {:?}", ev, gap_dur));
}

/// mouse lower-level-hook processor
pub unsafe extern "system"
fn mouse_proc (code: c_int, w_param: WPARAM, l_param: LPARAM) -> LRESULT {

    let return_call = || { CallNextHookEx(HHOOK(0), code, w_param, l_param) };

    if code < 0 { return return_call() }      // ms-docs says we MUST do this, so ig k fine

    let iproc = InputProcessor::instance();
    /*
    if iproc.mouse_bindings.read().unwrap().is_empty() {
        iproc.unset_mouse_hook();
        return return_call();
    }*/

    let mh_struct = &*(l_param.0 as *const MSLLHOOKSTRUCT);

    if mh_struct.dwExtraInfo == FAKE_EXTRA_INFO {
        // if we injected it ourselves, we should just bail (and call the next guy down the line)
        return return_call()
    }
    let stamp = mh_struct.time;
    let injected = mh_struct.flags & LLMHF_INJECTED == LLMHF_INJECTED;

    //println!("{:#?}", mh_struct);
    use {MouseEvent::*, MouseBtnEvent_T::*};
    if let Some (event) = match w_param.0 as u32 {
        WM_LBUTTONDOWN => Some ( btn_event { src_btn: LeftButton,   ev_t: BtnDown, stamp, injected } ),
        WM_RBUTTONDOWN => Some ( btn_event { src_btn: RightButton,  ev_t: BtnDown, stamp, injected } ),
        WM_MBUTTONDOWN => Some ( btn_event { src_btn: MiddleButton, ev_t: BtnDown, stamp, injected } ),
        WM_XBUTTONDOWN => {
            match HIWORD(mh_struct.mouseData) {
                XBUTTON1 => Some ( btn_event { src_btn: X1Button, ev_t: BtnDown, stamp, injected } ),
                XBUTTON2 => Some ( btn_event { src_btn: X2Button, ev_t: BtnDown, stamp, injected } ),
                _ => None,
        } }
        WM_LBUTTONUP => Some ( btn_event { src_btn: LeftButton,   ev_t: BtnUp, stamp, injected } ),
        WM_RBUTTONUP => Some ( btn_event { src_btn: RightButton,  ev_t: BtnUp, stamp, injected } ),
        WM_MBUTTONUP => Some ( btn_event { src_btn: MiddleButton, ev_t: BtnUp, stamp, injected } ),
        WM_XBUTTONUP => {
            match HIWORD(mh_struct.mouseData) {
                XBUTTON1 => Some ( btn_event { src_btn: X1Button, ev_t: BtnUp, stamp, injected } ),
                XBUTTON2 => Some ( btn_event { src_btn: X2Button, ev_t: BtnUp, stamp, injected } ),
                _ => None,
        } }
        WM_MOUSEWHEEL  => Some ( wheel_event { src_wheel: DefaultWheel,    delta: HIWORD(mh_struct.mouseData) as i16 as i32, stamp, injected } ),
        WM_MOUSEHWHEEL => Some ( wheel_event { src_wheel: HorizontalWheel, delta: HIWORD(mh_struct.mouseData) as i16 as i32, stamp, injected } ),

        WM_MOUSEMOVE => Some ( move_event { x_pos: mh_struct.pt.x, y_pos: mh_struct.pt.y, stamp, injected } ),
        _ => None,
    } {
        //print_mouse_ev(event);

        let mut do_ev_prop = EventProp_Continue;

        if let Some(cbe) = iproc.mouse_bindings .read().unwrap() .get (&MouseEventCbMapKey::from_event(&event)) .as_ref() {
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
                MouseEvCbFn_QueuedCallback (cb) => {
                    let _ = iproc.mouse_af_queue.send((cb.clone(), event));
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



