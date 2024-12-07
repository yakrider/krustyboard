#![ allow (non_camel_case_types) ]

use std::{panic, thread};
use std::os::raw::c_int;
use std::sync::atomic::{Ordering, AtomicU32, AtomicIsize, AtomicU64};
use std::sync::mpsc::{sync_channel, SyncSender};

use once_cell::sync::{OnceCell};

use windows::Win32::Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, WPARAM, BOOL, GetLastError};
use windows::Win32::System::Threading::GetCurrentThreadId;
use windows::Win32::UI::WindowsAndMessaging::*;

use crate::{ *, EvCbFn_T::* };


// this is used for identifying the fake keypresses we insert, so we don't process them in an infinite loop
// note that 0xFFC3D44F is from ahk, though ahk uses further complex variations on it to signal more things
//const FAKE_EXTRA_INFO: ULONG_PTR = 0x14C;
pub const KRUSTY_INJECTED_IDENTIFIER_EXTRA_INFO  : usize = 0xFFC3D44F;
pub const SWITCHE_INJECTED_IDENTIFIER_EXTRA_INFO : usize = 0x5317C7EE;      // switche's own injected identifier

pub const MSG_LOOP_KILL_MSG: u32 = WM_USER + 1;


# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone) ]
/// The directive on whether to continue OS event propagation upon an event-callback or combo-processing
pub enum EvProp_D {
    EvProp_Continue,
    EvProp_Stop,
    EvProp_Undet,
}
/// The directive included in a bindings callback entry that indicates whether to skip or proceed
/// with combo processing after it is done
# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone) ]
pub enum ComboProc_D {
    ComboProc_Enable,
    ComboProc_Disable,
    ComboProc_Undet,
}
/// Directives on whether to continue with combo processing or OS event propagation upon a kbd-callback
# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone) ]
pub struct EvProc_Ds {
    pub ev_prop_d    : EvProp_D,
    pub combo_proc_d : ComboProc_D,
}

impl EvProc_Ds {
    /// pure syntatic sugar to crate kbd-event-processin-directive slightly less verbosely
    pub fn new (ev_prop_d: EvProp_D, combo_proc_d: ComboProc_D) -> EvProc_Ds {
        EvProc_Ds { ev_prop_d, combo_proc_d, }
    }
}




pub struct InputProcessor {
    /// handle returned by OS to the lower level kbd hook that we set (needed to unhook later)
    kbd_hook : AtomicIsize,
    /// handle returned by OS to the lower level mouse hook that we set (needed to unhook later)
    mouse_hook : AtomicIsize,
    /// handle to the input-processing thread (so we can send msg to stop it when desired)
    iproc_thread : AtomicU32,

    /// the input bindings hold mapping for kbdkeys/mouse events to bound actions
    pub input_bindings : Bindings,
    /// for queued callback types, send all input events (kbd/mouse) to same processing queue (w event args pre-packaged in it)
    pub input_af_queue : SyncSender <EvCbFn_QueuedProc_T>,

    /// cached last-kbd-event (digest) .. used for setting is_repeat flag for kbd events
    last_kbd_event : AtomicU64,
}


impl InputProcessor {

    /// Creates or returns the singleton InputProcessor.
    /// (.. and when initializing, starts the mpsc channel for kbd/mouse event actions too)
    pub fn instance () -> &'static InputProcessor {

        static INSTANCE: OnceCell <InputProcessor> = OnceCell::new();

        INSTANCE .get_or_init ( || {
            // we'll create and spawn out channel for kbd and mouse queued events, and get it started
            // (we expect queue drained asap, but we'll keep excess slots as wheel-events on spin can get quite bursty)
            // the processor will hold the sender to this queue for everyone to clone/use
            let (input_queue_sender, input_queue_receiver) = sync_channel::<EvCbFn_QueuedProc_T> (500);

            thread::spawn (move || {
                utils::win_set_thread_dpi_aware();
                // if this thread panics, we might potentially leave the pc unusable .. so we'd rather have the process quit
                if panic::catch_unwind ( || {
                    while let Ok(af) = input_queue_receiver.recv() { af() }
                } ) .is_err() {
                    println! ("PANIC in Input Processor AF Queue thread .. we gonna quit!");
                    std::process::exit(1);
                }
            });

            InputProcessor {
                kbd_hook       : AtomicIsize::default(),
                mouse_hook     : AtomicIsize::default(),
                iproc_thread   : AtomicU32::default(),
                input_bindings : Bindings::new(),
                input_af_queue : input_queue_sender,
                last_kbd_event : AtomicU64::default(),
            }
        } )

    }


    /// caches kbd-ev in compact form, and returns whether the new and old values match (for key-repeat flagging)
    pub fn cache_kbd_event (&self, vk_code:u32, ev_t: KbdEvent_T ) -> bool {
        let digest = ((ev_t as u64) << 32) | (vk_code as u64);
        digest == self.last_kbd_event .swap (digest, Ordering::Relaxed)
    }



    fn set_hook (
        hook_id: WINDOWS_HOOK_ID,
        hhook: &AtomicIsize,
        hook_proc: unsafe extern "system" fn (c_int, WPARAM, LPARAM) -> LRESULT,
    ) { unsafe {
        if let Ok(hh) = SetWindowsHookExW (hook_id, Some(hook_proc), HINSTANCE(0), 0) {
            println! ("hooking attempt .. succeeded!");
            hhook.store (hh.0, Ordering::SeqCst);
        } else {
            eprintln!("hooking attempt .. FAILED .. error code : {:?} !!", GetLastError());
        }
    } }
    fn set_kbd_hook   (&self) { InputProcessor::set_hook (WH_KEYBOARD_LL, &self.kbd_hook,   kbd_proc); }
    fn set_mouse_hook (&self) { InputProcessor::set_hook (WH_MOUSE_LL,    &self.mouse_hook, mouse_proc); }


    fn unset_hook (hhook: &AtomicIsize) -> bool {
        if HHOOK (hhook.load (Ordering::SeqCst)) != HHOOK::default() {
            if true == unsafe { UnhookWindowsHookEx ( HHOOK (hhook.load(Ordering::SeqCst)) ) } {
                hhook.store (HHOOK::default().0, Ordering::SeqCst);
                println!("unhooking attempt .. succeeded!");
                return true
            }
            eprintln!("unhooking attempt .. failed .. error code : {:?} !!", unsafe { GetLastError() });
        } else {
            println!("unhooking attempt .. no prior hook found !!");
        }
        false
    }
    fn unset_kbd_hook   (&self) -> bool { InputProcessor::unset_hook (&self.kbd_hook) }
    fn unset_mouse_hook (&self) -> bool { InputProcessor::unset_hook (&self.mouse_hook) }


    pub fn re_set_hooks (&'static self) {
        if self.are_hooks_set() {
            self.stop_input_processing();
        }
        self.begin_input_processing();
    }

    pub fn are_hooks_set (&'static self) -> bool {
        HHOOK (self.kbd_hook.load(Ordering::Relaxed)) != HHOOK::default()
            || HHOOK (self.mouse_hook.load(Ordering::Relaxed)) != HHOOK::default()
    }

    pub fn stop_input_processing (&'static self) { unsafe {
        // we'll unhook any prior hooks and signal prior input-processing thread to terminate
        self.unset_kbd_hook();
        self.unset_mouse_hook();
        PostThreadMessageW (self.iproc_thread.load(Ordering::Relaxed), MSG_LOOP_KILL_MSG, WPARAM::default(), LPARAM::default());
    } }


    /// Starts listening for bound input events.
    pub fn begin_input_processing (&'static self) {

        thread::spawn ( || unsafe {

            self.set_kbd_hook();
            self.set_mouse_hook();

            // we'll store the thread-id so we can send a message to kill the thread if need be later
            self.iproc_thread .store ( GetCurrentThreadId(), Ordering::Relaxed );

            // before starting to listen to events, lets set this thread dpi-aware (for rare cases we do direct processing upon callback)
            utils::win_set_thread_dpi_aware();

            // also, we might as well set the whole process higher priority, as we dont want lag in basic input processing
            let _ = utils::win_set_cur_process_priority_high();
            // todo : ^^ check if can get away w simply increasing our thread priority
            // (^^ although, note that the hook callback is called in the context of the thread that set the hook)

            // win32 sends hook events to a thread with a 'message loop', but we dont create any windows,
            //  so we wont get any actual messages, so we can just leave a forever waiting GetMessage instead of setting up a msg-loop
            // .. basically while its waiting, the thread is awakened simply to call kbd hook (for an actual msg, itd awaken give the msg)
            let mut msg: MSG = MSG::default();
            while BOOL(0) != GetMessageW (&mut msg, HWND(0), 0, 0) {
                if msg.message == MSG_LOOP_KILL_MSG {
                    println! ("received kill-msg in input-processing thread .. terminating thread ..");
                    break
                }
            }

        } );

    }



    /// This can be used to directly send internal-events to the input processor .. <br>
    /// which will lookup bindings for the event, and if has queued cb-types, those will get sent to af-queue for in-order processing
    pub fn inject_internal_event (ev_t:InternalEvent_T) {
        let event = Event {
            stamp: 0,
            injected: true,
            extra_info: KRUSTY_INJECTED_IDENTIFIER_EXTRA_INFO,
            dat: ( EventDat::internal_event { ev_t } ),
        };
        let _ = InputProcessor::instance().proc_input_event (event);
    }



    /// Common input events processor .. <br>
    /// Both kbd and mouse events from hooks get packaged into an InputEvent and sent here for processing
    pub fn proc_input_event (&self, event:Event) -> EvProp_D {

        //println!("{:?}",event)

        use { EvProp_D::*, ComboProc_D::* };
        let mut ev_proc_ds = EvProc_Ds::new (EvProp_Continue, ComboProc_Enable);

        let bmk = BindingsMapKey::from_event(&event);
        let mut had_binding = false;

        // first route it through any per-key registered callbacks
        //if let Some(cbe) = self.input_bindings .borrow() .get (&bmk) {
        if let Some(cbe) = unsafe { & *self.input_bindings.as_ptr() } .get (&bmk) {
            // ^^ the borrow is fine too, but since we dont write at runtime, just direct usage should be fine (and faster)
            // .. further this direct deref means we can get by w/o having to clone the cbs to pass into thread/queue below
            // .. which again, we're ok with, given we expect no runtime updates to the bindings (so no worries about the cb ref's lifetime)
            had_binding = true;
            ev_proc_ds = cbe.ev_proc_ds;
            match &cbe.cb {
                EvCbFn_Inline(cb)  => {
                    let epds = cb(event);
                    if ev_proc_ds.ev_prop_d == EvProp_Undet { ev_proc_ds = epds; }
                }
                EvCbFn_Spawned(cb) => {
                    //let cb = cb.clone();
                    thread::spawn (move || cb(event));
                }
                EvCbFn_Queued(cb) => {
                    //let cb = cb.clone();
                    let _ = self.input_af_queue.send ( Box::new ( move || cb(event) ) );
                }
            }
        }

        // if combo_proc for this event is already disabled, we can return early .. (else we'll go through combo processing)
        if ev_proc_ds.combo_proc_d != ComboProc_Enable {
            return ev_proc_ds.ev_prop_d
        }

        let cm = CombosMap::instance();
        if let EventDat::key_event {key, ..} = event.dat {
            // we'll let injected events pass through (both kdn/kup) .. note that modifier keys already dealt w injected events above
            if event.injected { return EvProp_Continue }
            // if its not in the combo-proc handled-keys whitelist, we should just let it pass through
            // (whitelist coz unknown apps (incl switche) send unknown keys for valid reasons, and they should passthrough)
            if !cm.check_if_handled_key (&key) { return EvProp_Continue }
        } else {
            // for non-key events (mouse btn/wheel/move), we want to allow combo proc only for those that have binding entries registered ..
            // .. and therefore have combo-proc directives specified in the binding .. this is 'friendlier' as w/o explicitly
            // .. configuring the bindings, btns etc wont auto get combo-searched in a potentially unpopulated table)
            // note that we're not rejecting injected events here, as looks like x1/x2 mbtns come as injected, (at least in MX mouse)
            if !had_binding { return EvProp_Continue }
        }

        // if we got this far, we can queue this up for combos processing ..
        // .. we queue all combo actions (instead of spawning out) so they dont get out of sequence
        // note that we're using the same input-af-queue ..
        // .. and its non-ideal as some other event might have snuck in between event and its combo proc
        // .. but a separate queue woudlnt fix it either .. and eitherway shoudlnt be a problem if queue clearance is fast enough
        let _ = self.input_af_queue .send (Box::new (move || cm.combo_maps_handle_input (bmk, &event)));

        // combo-proc-handled keys should be completely blocked past combo-proc (both keydn and keyup etc)
        // (not least because the actual combo proc is queued for later .. so either we bail early, or we combo-proc and stop cur event)
        EvProp_Stop

    }


}





/// debug printout
fn _print_kbd_event (wp:&WPARAM, kbs:&KBDLLHOOKSTRUCT) {
    println!("w_param: {:X}, vk_code: {:?}, scanCode: {:#06X}, flags: {:#018b}, time: {}, dwExtraInfo: {:X}",
             wp.0, KbdKey::from(kbs.vkCode as u64), kbs.scanCode, kbs.flags.0, kbs.time, kbs.dwExtraInfo);
}


/// Keyboard lower-level-hook processor
pub unsafe extern "system"
fn kbd_proc (code: c_int, w_param: WPARAM, l_param: LPARAM) -> LRESULT {

    let return_call = || { CallNextHookEx(HHOOK(0), code, w_param, l_param) };

    if code < 0 { return return_call() }      // ms-docs says we MUST do this, so ig k fine

    let iproc = InputProcessor::instance();
    /* //.. disabling this, as it's basically never applicable for our usage, and incurs runtime cost on every event
    if iproc.input_bindings.read().unwrap().is_empty() && !CombosMap::instance().is_enabled() {
        iproc.unset_kbd_hook();
        return return_call();
    }*/

    let kb_struct = *(l_param.0 as *const KBDLLHOOKSTRUCT);

    // if we injected this event ourselves, we should just bail
    if kb_struct.dwExtraInfo == KRUSTY_INJECTED_IDENTIFIER_EXTRA_INFO { return return_call() }

    //_print_kbd_event (&w_param, &kb_struct);

    use KbdEvent_T::*;
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
        let extra_info = kb_struct.dwExtraInfo;

        let is_repeat = iproc.cache_kbd_event (kb_struct.vkCode, ev_t);
        // ^^ note that currently we're allowing injected events to affect key-repeat flag

        let dat = EventDat::key_event { key, ev_t, is_repeat, vk_code: kb_struct.vkCode, sc_code: kb_struct.scanCode };

        let event = Event { stamp, injected, extra_info, dat };

        //println! ("{:?}", event);

        if iproc.proc_input_event (event) == EvProp_D::EvProp_Stop {
            return LRESULT(1);
            // ^^ returning with non-zero code signals OS to block further processing on the input event
        }
    }

    return_call()
}





#[allow(non_snake_case)]
fn hi_word (l: u32) -> u16 { ((l >> 16) & 0xffff) as u16 }


static LAST_STAMP: AtomicU32 = AtomicU32::new(0);
#[allow(dead_code)]
fn print_mouse_ev (ev: Event) {
    let last_s = LAST_STAMP.swap(ev.stamp, Ordering::Relaxed);
    let gap_dur = ev.stamp - last_s;
    thread::spawn(move || println!("{:?}, {:?}", ev, gap_dur));
}


/// mouse lower-level-hook processor
pub unsafe extern "system"
fn mouse_proc (code: c_int, w_param: WPARAM, l_param: LPARAM) -> LRESULT {

    let return_call = || { CallNextHookEx(HHOOK(0), code, w_param, l_param) };

    if code < 0 { return return_call() }      // ms-docs says we MUST do this, so ig k fine

    let iproc = InputProcessor::instance();
    /*
    if iproc.input_bindings.read().unwrap().is_empty() {
        iproc.unset_mouse_hook();
        return return_call();
    }*/

    let mh_struct = &*(l_param.0 as *const MSLLHOOKSTRUCT);

    if mh_struct.dwExtraInfo == KRUSTY_INJECTED_IDENTIFIER_EXTRA_INFO {
        // if we injected it ourselves, we should just bail (and call the next guy down the line)
        return return_call()
    }

    let stamp = mh_struct.time;
    let injected = mh_struct.flags & LLMHF_INJECTED == LLMHF_INJECTED;
    let extra_info = mh_struct.dwExtraInfo;

    //println!("{:#?}", mh_struct);

    use { MouseButton::*, MouseWheel::*, EventDat::*, MouseBtnEv_T::* };
    if let Some (dat) = match w_param.0 as u32 {
        WM_LBUTTONDOWN => Some ( btn_event { btn: LeftButton,   ev_t: BtnDown } ),
        WM_RBUTTONDOWN => Some ( btn_event { btn: RightButton,  ev_t: BtnDown } ),
        WM_MBUTTONDOWN => Some ( btn_event { btn: MiddleButton, ev_t: BtnDown } ),
        WM_XBUTTONDOWN => {
            match hi_word(mh_struct.mouseData) {
                XBUTTON1 => Some ( btn_event { btn: X1Button, ev_t: BtnDown } ),
                XBUTTON2 => Some ( btn_event { btn: X2Button, ev_t: BtnDown } ),
                _ => None,
        } }
        WM_LBUTTONUP => Some ( btn_event { btn: LeftButton,   ev_t: BtnUp } ),
        WM_RBUTTONUP => Some ( btn_event { btn: RightButton,  ev_t: BtnUp } ),
        WM_MBUTTONUP => Some ( btn_event { btn: MiddleButton, ev_t: BtnUp } ),
        WM_XBUTTONUP => {
            match hi_word(mh_struct.mouseData) {
                XBUTTON1 => Some ( btn_event { btn: X1Button, ev_t: BtnUp } ),
                XBUTTON2 => Some ( btn_event { btn: X2Button, ev_t: BtnUp } ),
                _ => None,
        } }
        WM_MOUSEWHEEL  => Some ( wheel_event { wheel: DefaultWheel,    delta: hi_word(mh_struct.mouseData) as i16 as i32 } ),
        WM_MOUSEHWHEEL => Some ( wheel_event { wheel: HorizontalWheel, delta: hi_word(mh_struct.mouseData) as i16 as i32 } ),

        WM_MOUSEMOVE => Some ( pointer_event { x_pos: mh_struct.pt.x, y_pos: mh_struct.pt.y } ),
        _ => None,
    } {
        let event = Event { stamp, injected, extra_info, dat };
        //print_mouse_ev(event);

        if iproc.proc_input_event (event) == EvProp_D::EvProp_Stop {
            return LRESULT(1);
            // ^^ returning with non-zero code signals OS to block further processing on the input event
        }
    }

    return_call()
}


