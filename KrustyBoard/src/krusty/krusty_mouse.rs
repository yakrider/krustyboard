#![ allow (non_snake_case) ]

use std::{
    thread, time,
    ops::Deref,
    sync::{Arc, RwLock},
};

use windows::Win32::Foundation::{HWND, POINT, RECT};
use windows::Win32::UI::WindowsAndMessaging::{
    GA_ROOT, GetAncestor, SetForegroundWindow, WHEEL_DELTA, WindowFromPoint
};

use crate::{
    *, key_utils::*, utils::*, EventPropagationDirective::*,
    MouseEventCallbackFnType::*, MouseEventCbMapKeyAction::*,
};
use crate::KbdKey::{ExtLeft, ExtRight};
use rand::Rng;


# [ derive (Debug) ]
pub struct _MouseBtnState {
    pub btn : MouseButton,
    pub down : Flag,
    pub active : Flag,
    pub consumed : Flag,
    //pub kdn_stamp : EventStamp,
    //pub kup_stamp : EventStamp,
}

# [ derive (Debug, Clone) ]
pub struct MouseBtnState ( Arc <_MouseBtnState> );

impl Deref for MouseBtnState {
    type Target = _MouseBtnState;
    fn deref(&self) -> &_MouseBtnState { &self.0 }
}

impl MouseBtnState {
    pub fn new (btn:MouseButton) -> MouseBtnState {
        MouseBtnState ( Arc::new ( _MouseBtnState {
            btn,
            down     : Flag::default(),
            active   : Flag::default(),
            consumed : Flag::default(),
            //kdn_stamp: EventStamp::new(),
            //kup_stamp: EventStamp::new(),
        } ) )
    }

    // note: we intended to impl shared debounce logic for btns here, but after queued setup we see very few actual debounce issues
    // - plus, an actual decent debounce impl was getting complex, and would involve introducing new delays before sending out btn-events
    // .. so for now, we'll only leave these stubs here are reminder ..
    // - further, on expt it looks like the resolution on the event-stamp is NOT reliable for debounce .. as often the stamps seem to get
    // .. pushed out and bunched when PC busy (as if they were pulled from an events queue higher up in the stack later and only then get stamped)
    pub fn debounced_kdn (&self, af:&AF, _:MouseEvent) { af() }
    pub fn debounced_kup (&self, af:&AF, _:MouseEvent) { af() }

}


# [ derive (Debug) ]
pub struct _MouseWheelState {
    pub wheel : MouseWheel,
    pub last_stamp : TimeStamp,
    // we'll also hold a flag to invalidate an ongoing inertial spin by e.g. mid-spin mod press (or actual spin stop (spacing > 120ms))
    pub spin_invalidated : Flag,
}

# [ derive (Debug, Clone) ]
pub struct MouseWheelState ( Arc <_MouseWheelState> );

impl Deref for MouseWheelState {
    type Target = _MouseWheelState;
    fn deref(&self) -> &_MouseWheelState { &self.0 }
}

impl MouseWheelState {
    pub fn new (wheel:MouseWheel) -> MouseWheelState {
        MouseWheelState ( Arc::new ( _MouseWheelState {
            wheel,
            last_stamp       : TimeStamp::new(),
            spin_invalidated : Flag::default(),
        } ) )
    }
}

# [ derive (Debug, Default, Copy, Clone) ]
pub struct PreDragDat { hwnd:HWND, rect:RECT, frame:RECT, pointer:POINT,  }

# [ derive (Debug) ]
pub struct _Mouse {
    _private  : (),
    pub lbtn  : MouseBtnState,
    pub rbtn  : MouseBtnState,
    pub mbtn  : MouseBtnState,

    pub x1btn : MouseBtnState,
    pub x2btn : MouseBtnState,

    pub vwheel : MouseWheelState,
    //pub hwheel : MouseWheelState,

    pub pre_drag_dat : Arc < RwLock <PreDragDat>>,
}

# [ derive (Debug, Clone) ]
pub struct Mouse ( Arc <_Mouse> );

impl Deref for Mouse {
    type Target = _Mouse;
    fn deref (&self) -> &_Mouse { &self.0 }
}

impl Mouse {

    pub fn new() -> Mouse {
        use crate::{MouseButton::*, MouseWheel::*};
        Mouse ( Arc::new ( _Mouse {
            _private: (),
            lbtn   : MouseBtnState::new(LeftButton),
            rbtn   : MouseBtnState::new(RightButton),
            mbtn   : MouseBtnState::new(MiddleButton),
            x1btn  : MouseBtnState::new(X1Button),
            x2btn  : MouseBtnState::new(X2Button),
            vwheel : MouseWheelState::new(DefaultWheel),
            //hwheel : MouseWheelState::new(HorizontalWheel),
            //pointer: MousePointerState::default(),
            pre_drag_dat : Arc::new (RwLock::new (PreDragDat::default()))
        } ) )
    }

    pub fn setup_mouse (&self, k:&Krusty) {
        // handling for mouse left btn, mostly to allow caps-as-ctrl behavior during drag drops and clicks
        setup_mouse_left_btn_handling (k);

        // also for mouse right btn, mostly to allow switche scrolling w right-btn-wheel combo
        setup_mouse_right_btn_handling (k);

        // also setup both Xbutton srcs to act as middle btns (used for link clicks, closing tabs etc)
        setup_middle_btn_eqv_handling (&self.x1btn, k);
        setup_middle_btn_eqv_handling (&self.x2btn, k);

        // setup handling for mouse wheel .. complex overloading over alt-tab, switche, volume, brightness etc !!
        setup_mouse_wheel_handling (k);

        setup_mouse_move_handling (k);

    }

    pub fn capture_pre_drag_dat (&self) { unsafe {
        win_set_thread_dpi_aware();
        let (x,y) = MousePointer::pos();
        let hwnd = WindowFromPoint (POINT {x, y});
        let hwnd = GetAncestor (hwnd, GA_ROOT);
        let rect = win_get_window_rect (hwnd);
        let frame = win_get_window_frame (hwnd);
        *self.pre_drag_dat.write().unwrap() = PreDragDat { hwnd, rect, frame, pointer: POINT {x, y} };
    } }

    // mod-keys notify here in case we need to do some cleanup/flagging etc
    pub fn proc_notice__modkey_down (&self, mk:Option<&SyncdModKey>, ks:&KrustyState) {
        // (note that caps notice comes with mk param set to None)
        self.vwheel.spin_invalidated.set();
        if ks.mouse.lbtn.down.is_set() && ( mk.is_none() ||  mk.filter(|mk| mk.key == Key::LWin).is_some() ) {
            // we'll want to capture/refresh pre-drag-dat on caps/win presses w lbtn down as they both modify drag/resize origin behavior
            self.capture_pre_drag_dat();
        }
    }
    pub fn proc_notice__modkey_up (&self, mk:Option<&SyncdModKey>, ks:&KrustyState) {
        self.vwheel.spin_invalidated.set();
        if mk.is_none() && ks.mod_keys.lwin.down.is_set() && ks.mouse.lbtn.down.is_set() {
            // if we're exiting drag-resize into drag-move, so we should refresh our pre-drag dat reference
            self.capture_pre_drag_dat();
        }
    }

}






/// sets up mouse left btn for msotly pass-through eqv w tracking, but allowing caps-as-ctrl behavior for the mouse
pub fn setup_mouse_left_btn_handling (k:&Krusty) {
    use crate::{MouseButton::*, MouseBtnEvent_T::*};
    let (ks, mbtn) = (k.ks.clone(), k.ks.mouse.lbtn.clone());
    let af:AF = Arc::new ( move || handle_mouse_left_btn_down(&ks) );
    let dbaf = Arc::new ( move |ev:MouseEvent| mbtn.debounced_kdn(&af, ev) );
    k.iproc.mouse_bindings .bind_btn_event ( LeftButton, BtnEventCb(BtnDown), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_QueuedCallback(dbaf),
    } );
    let (ks, mbtn) = (k.ks.clone(), k.ks.mouse.lbtn.clone());
    let af:AF = Arc::new ( move || handle_mouse_left_btn_up(&ks) );
    let dbaf = Arc::new ( move |ev:MouseEvent| mbtn.debounced_kup(&af,ev) );
    k.iproc.mouse_bindings .bind_btn_event ( LeftButton, BtnEventCb(BtnUp), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_QueuedCallback(dbaf),
    } );
    // ^^ note: to ensure that the order of btn dn/up is preserved, we want to make btn-up have blocking queued-callback too
    //  .. even when its not actually doing much (and otherwise couldve been allowed to pass through)
}

fn handle_mouse_left_btn_down (ks:&KrustyState) {
    ks.mouse.lbtn.down.set();
    ks.mouse.capture_pre_drag_dat();
    ks.mod_keys.proc_notice__mouse_btn_down (ks.mouse.lbtn.btn);
    if ks.mod_keys.lwin.down.check() {
        // window move/resize modes
        unsafe { SetForegroundWindow (ks.mouse.pre_drag_dat.read().unwrap().hwnd) };
        if ks.mod_keys.caps.down.check() {
            // window drag-to-resize mode
        } else {
            // this is window drag-to-move mode
            //set_cursor(IDC_SIZEALL); // nope, only seems to work for apps own created windows!
        }
    } else if ks.mod_keys.caps.down.check() {
        ks.in_managed_ctrl_down_state.set();
        ks.mod_keys.lctrl.ensure_active();   // this allows caps-as-ctrl for drag drop etc
        ks.mouse.lbtn.active.set();
        MouseButton::LeftButton.press();
    } else {
        ks.mouse.lbtn.active.set();
        MouseButton::LeftButton.press();
    }
}

fn handle_mouse_left_btn_up (ks:&KrustyState) {
    ks.mouse.lbtn.down.clear(); ks.mouse.lbtn.consumed.clear();
    ks.mod_keys.proc_notice__mouse_btn_up (ks.mouse.lbtn.btn);

    if ks.mouse.lbtn.active.is_clear(){
        //
    } else {
        ks.mouse.lbtn.active.clear();
        MouseButton::LeftButton.release();
    }
    // release ctrl (when need to) only after click btn up is sent
    if ks.in_managed_ctrl_down_state.is_set() {
        ks.in_managed_ctrl_down_state.clear();
        ks.mod_keys.lctrl.ensure_inactive();
    }
}





/// sets up mouse right btn for pass-through eqv w/ tracking, but allows right-held-scroll for switche tab-switching
// tracking right-btn-down-state is required for switche scroll (via F16/F17/Ctrl-F18)
// note that unlike other mouse btn/wheels, the right btn is set to passthrough!
pub fn setup_mouse_right_btn_handling (k:&Krusty) {
    use crate::{MouseButton::*, MouseBtnEvent_T::*};
    let ks = k.ks.clone();
    k.iproc.mouse_bindings .bind_btn_event ( RightButton, BtnEventCb(BtnDown), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_QueuedCallback ( Arc::new ( move |_| handle_mouse_right_btn_down(&ks) ) )
    } );
    let ks = k.ks.clone();
    k.iproc.mouse_bindings .bind_btn_event ( RightButton, BtnEventCb(BtnUp), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_QueuedCallback ( Arc::new ( move |_| handle_mouse_right_btn_up(&ks) ) )
    } );
}

fn handle_mouse_right_btn_down (ks:&KrustyState) {
    ks.mouse.rbtn.down.set();
    ks.mouse.capture_pre_drag_dat();
    ks.mod_keys.proc_notice__mouse_btn_down (ks.mouse.rbtn.btn);
    if ks.mod_keys.lwin.down.check() {
        //ks.mouse.rbtn.consumed.set();
    } else {
        ks.mouse.rbtn.active.set();
        MouseButton::RightButton.press();
    }
}

fn handle_mouse_right_btn_up (ks:&KrustyState) {
    ks.mouse.rbtn.down.clear(); ks.mouse.rbtn.consumed.clear();
    ks.mod_keys.proc_notice__mouse_btn_up (ks.mouse.rbtn.btn);

    if ks.in_right_btn_scroll_state.check() {
        ks.in_right_btn_scroll_state.clear();
        handle_right_btn_scroll_end(ks);        // will send a rbtn release itself!
    } else  if ks.mouse.rbtn.active.is_clear() {
        //set_cursor(IDC_ARROW);       // does not work on non-owned windows
    } else {
        //if ks.mouse.rbtn.consumed.is_set() { /* masking doesnt really work for rbtn */ }
        ks.mouse.rbtn.active.clear();
        MouseButton::RightButton.release();
    }
}

fn handle_right_btn_scroll_end (ks:&KrustyState) {
    // Ctrl-F18 is the exit hotkey for switche if we were doing right-btn-scroll
    ks.mod_keys.lctrl.active_on_key(Key::F18)();

    // now we have to release the rbtn, but to avoid triggering the context menu, we'll release it at corner of screen
    //MouseButton::RightButton.release_at (0xFFFF, 0xFFFF);
    // ^^ ugh this doesnt seem to actually do that .. so we'll manually move there, release, then restore

    // and looks like for such a manual-move strategy to work, there HAS to be a delay before we move the pointer back
    // (also, this works for almost all applications EXCEPT for windows-explorer .. MS ofc has to be special .. meh)
    let (x,y) = MousePointer::pos();
    MousePointer::move_abs (0xFFFF, 0xFFFF);
    MouseButton::RightButton.release();
    // some delay to actually have the release processed while pointer is still away
    thread::spawn ( move || {
        thread::sleep(time::Duration::from_millis(5));
        // note that reducing this delay or even removing it will mostly work (as the event handling can happen before spawned thread comes up)..
        // .. however, for times when there's load etc and the event handling is also pushed out, we'll want at least some delay
        MousePointer::move_abs(x,y);
    } );

}


/// x1/x2 btns can be set up to behave like they were middle btn
pub fn setup_middle_btn_eqv_handling (mbs:&MouseBtnState, k:&Krusty) {
    use crate::{MouseButton::*, MouseBtnEvent_T::*};
    // note that we cant make X1/X2 btns act truly like mbtn as they dont seem to send dn/up events on press/rel ..
    // .. instead they send nothing on btn-dn and send dn/up at btn-rel .. (or nothing if held too long!)
    let ks = k.ks.clone();
    let (back,fwd) = (k.ag(ExtLeft).m(ModKey::alt).gen_af(), k.ag(ExtRight).m(ModKey::alt).gen_af());
    k.iproc.mouse_bindings .bind_btn_event ( mbs.btn, BtnEventCb(BtnDown), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_QueuedCallback ( Arc::new ( move |_| {
            if ks.mod_keys.lshift.down.check() { back() }
            else if ks.mod_keys.lctrl.down.check() { fwd() }
            else { MiddleButton.press(); }
        } ) ),
    } );

    k.iproc.mouse_bindings .bind_btn_event ( mbs.btn, BtnEventCb(BtnUp), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_QueuedCallback ( Arc::new ( move |_| { MiddleButton.release(); } ) )
    } );

}





/// sets up mouse wheel with various overloaded modes incl brightness, volume, tab-switching, caps-as-ctrl etc
pub fn setup_mouse_wheel_handling (k:&Krusty) {
    use crate::{MouseEvent::*, MouseWheel::*};
    let ks = k.ks.clone();
    k.iproc.mouse_bindings .bind_wheel_event ( DefaultWheel, MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_QueuedCallback ( Arc::new ( move |ev| {
            if let wheel_event {delta, ..} = ev { handle_wheel_guarded (delta, &ks) }
        } ) ),
    } );
}

fn handle_wheel_guarded (delta:i32, ksr:&KrustyState) {
    // this is mostly to make the super-fast inertial smooth-scroll wheel on my (MX3) mouse a lil more usable by spacing things out
    // also, the invalidation setup prevents things like caps down when wheel is still unintentionally inertially spinning to trigger zooms etc
    let last_stamp = ksr.mouse.vwheel.last_stamp.get();
    ksr.mouse.vwheel.last_stamp.capture();
    //let gap = ksr.last_wheel_stamp.read().unwrap().duration_since(last_stamp);
    //println!("{:#?}", dur.as_millis());
    const GUARD_DUR_MS: u128 = 120;  // from dur printouts above, looked like max inertial gap is 120 (min 7ms, usually <100)
    if !ksr.mouse.vwheel.spin_invalidated.check() {
        handle_wheel_action(delta, ksr);
    } else if GUARD_DUR_MS < ksr.mouse.vwheel.last_stamp.get().duration_since(last_stamp).as_millis() {
        ksr.mouse.vwheel.spin_invalidated.clear();
        handle_wheel_action(delta, ksr);
    } else {
        // if its invalidated AND wheel-spin spacing is below guard-dur, we suppress the wheel event
    }
}

fn handle_wheel_action (delta:i32, ksr:&KrustyState) {
    let incr = delta / WHEEL_DELTA as i32;
    if ksr.mouse.rbtn.down.check() {
        // right-mouse-btn-wheel support for switche task switching
        ksr.in_right_btn_scroll_state.set();
        let key = if incr.is_positive() { Key::F17 } else { Key::F16 };
        //ksr.mod_keys.lalt.active_action(base_action(key))();       // not usable due to masking keys (so changed in hotkey switche)
        //ksr.mod_keys.lctrl.active_action(base_action(key))();      // not ideal as even non-masked wrapping causes slower/choppy switche scroll
        press_release(key);                                 // we'd rather use direct press hotkeys for best perf
    } else  if ksr.mod_keys.lalt.down.check() {
        // wheel support for scrolling in windows native alt-tab task-switching screen
        // .. we could consider spawning this part out, but meh we're in side-thread queue, its prob ok
        ksr.mod_keys.lalt.consumed.set();
        if utils::get_fgnd_win_class() == "MultitaskingViewFrame" { // alt-tab states
            ksr.mod_keys.lalt.ensure_active();    // we're already down but just in case its down/inactive
            handle_alt_tab_wheel(incr)
        } else {
            // alt-wheel for (fine delta) control, qks1-alt-wheel for larger adjustments
            let mult = if ksr.mode_states.qks1.down.check() {5} else {1};
            incr_brightness (incr * mult)
            // ^^ potentially could spawn this out too, but in this queue thread, meh it'll be fine
        }
    } else if ksr.mod_keys.lwin.down.check() {
        // win-wheel for (fine delta) control, qks1-win-wheel for larger adjustments
        ksr.mod_keys.lwin.consumed.set();
        let mult = if ksr.mode_states.qks1.down.check() {2} else {1};
        incr_volume (incr * mult)
    } else if ksr.mod_keys.caps.down.check() || ksr.mod_keys.some_ctrl_down() {
        if ksr.mod_keys.caps.down.check() {
            ksr.in_managed_ctrl_down_state.set();
            ksr.mod_keys.lctrl.ensure_active();
        }
        if ksr.in_ctrl_tab_scroll_state.is_set() {
            handle_wheel_to_arrows_action(incr);
        } else {
            MouseWheel::DefaultWheel.scroll(delta);     //  // caps-wheel as ctrl-wheel (zoom etc)
        }
        // could set up delayed trigger here to clear managed-ctrl-down-state, but meh, just let it be till caps-up
    } else if ksr.mod_keys.some_shift_down() {
        //handle_horiz_scroll_wheel(incr);
        // ^^ todo:: .. (and for now, just let default pass through)
        MouseWheel::DefaultWheel.scroll(delta);
    } else {
        MouseWheel::DefaultWheel.scroll(delta);
    }
}

fn handle_wheel_to_arrows_action (incr:i32) {
    let arrow = if incr > 0 { Key::ExtUp } else { Key::ExtDown };
    (0 .. incr.abs()) .for_each (|_| press_release(arrow) );
}

pub fn incr_volume (incr:i32) {
    let key = if incr > 0 { Key::VolumeUp } else { Key::VolumeDown };
    (0 .. incr.abs()) .for_each(|_| key.press());
}

pub fn incr_brightness (incr:i32) {
    static INCR_STEP:i32 = 1;
    // note again that we're always spawned out from hook thread, so slower tasks are also ok here
    //utils::brightness_ps_wmi::incr_brightness(INCR_STEP*incr);
    let _ = utils::incr_brightness(INCR_STEP*incr);
}

fn handle_alt_tab_wheel (incr:i32) {
    // todo potentially impl additional separate timer-spacing here, to slow this down even more than regular wheel spacing
    // note that for these we DONT want to release Alt key .. presumably, expecting it to be physically released later
    if incr.is_positive() { shift_press_release(Key::Tab) }
    else { press_release(Key::Tab) }
}

#[allow(dead_code)]
fn handle_horiz_scroll_wheel (_incr:i32) {
    // todo we could in theory impl this overriding the native horiz scroll behavior
}



pub fn setup_mouse_move_handling (k:&Krusty) {
    use crate::{MouseEvent::*};
    let ks = k.ks.clone();
    k.iproc.mouse_bindings .bind_pointer_event ( MouseEventCallbackEntry {
        event_prop_directive: EventProp_Continue,
        cb: MouseEvCbFn_QueuedCallback ( Arc::new ( move |ev| {
            if ks.mod_keys.lwin.down.check() && ks.mouse.lbtn.down.check() { //&& ks.mouse.pre_drag_dat.read().unwrap().is_some() {
                ks.mod_keys.lwin.consumed.set();
                if let move_event { x_pos, y_pos, .. } = ev {
                    handle_pointer_move (x_pos, y_pos, &ks)
            } }
        } ) ),
    } );
}

# [ derive (Debug, Default, Clone) ]
pub struct WinRect { x:i32, y:i32, width:i32, height:i32 }

fn handle_pointer_move (x:i32, y:i32, ks:&KrustyState) {
    // NOTE that mouse move is inline pass-through before these handler calls get queued
    // .. so there could be some lag, but should be quick enough that we can work with ks states as we currently see it

    //win_set_thread_dpi_aware();    // no need, we do that right while initing the events-queue thread itself

    // for now, we'll send it to drag directly, later if we do more we can add state checks
    if ks.mouse.lbtn.down.check() && ks.mouse.lbtn.consumed.is_clear(){
        if ks.mod_keys.caps.down.check() {
            handle_pointer_window_resize_spaced (x, y, &ks)
        } else {
            handle_pointer_window_drag_spaced (x, y, &ks)
        }
    }
}


fn handle_pointer_window_drag (x:i32, y:i32, ks:&KrustyState) {
    // we'll have saved mouse loc on win-lclick, so we can reference that on where/how to move the window
    let pdd = ks.mouse.pre_drag_dat.read().unwrap().clone();
    let dest =  WinRect {
        x      : pdd.rect.left + (x - pdd.pointer.x),
        y      : pdd.rect.top  + (y - pdd.pointer.y),
        width  : pdd.rect.right  - pdd.rect.left,
        height : pdd.rect.bottom - pdd.rect.top,
    };
    let dr = snap_to_edge_rect_delta (&dest, &calc_window_padding(&pdd.rect, &pdd.frame));
    // we want to keep width/height same so we'll snap to left (and top) in preference to right (and bottom) if both of are available
    let dlr = if dr.left != 0 {dr.left} else {dr.right };
    let dtb = if dr.top  != 0 {dr.top } else {dr.bottom};
    win_move_to ( pdd.hwnd, dest.x + dlr, dest.y + dtb, dest.width, dest.height, true );
}
fn handle_pointer_window_drag_spaced (x:i32, y:i32, ks:&KrustyState) {
    if rand::thread_rng().gen_range(0..10) < 8 {  handle_pointer_window_drag (x,y,ks) }
}


fn handle_pointer_window_resize (x:i32, y:i32, ks:&KrustyState) {
    let point = POINT {x,y};
    let pdd = ks.mouse.pre_drag_dat.read().unwrap().clone();
    let dp = POINT { x: point.x - pdd.pointer.x, y: point.y - pdd.pointer.y };
    let dest = WinRect {
        x      : pdd.rect.left,
        y      : pdd.rect.top,
        width  : pdd.rect.right  - pdd.rect.left + dp.x,
        height : pdd.rect.bottom - pdd.rect.top  + dp.y,
    };
    let dr = snap_to_edge_rect_delta (&dest, &calc_window_padding(&pdd.rect, &pdd.frame));
    win_move_to ( pdd.hwnd, dest.x, dest.y, dest.width + dr.right, dest.height + dr.bottom, true );
}
fn handle_pointer_window_resize_spaced (x:i32, y:i32, ks:&KrustyState) {
    if rand::thread_rng().gen_range(0..10) < 2 {  handle_pointer_window_resize (x,y,ks) }
}


pub fn handle_pointer_action_cancel (ks:&KrustyState) {
    let pdd = ks.mouse.pre_drag_dat.read().unwrap().clone();
    win_move_to (pdd.hwnd, pdd.rect.left, pdd.rect.top, pdd.rect.right - pdd.rect.left, pdd.rect.bottom - pdd.rect.top, true);
}



fn snap_to_edge_rect_delta (wr:&WinRect, pad:&RECT) -> RECT {
    static SNAP_THRESH : i32 = 30;     // threshold to activate snap to edges  .. todo: prob should make this configurable .. prob dpi aware too
    // we'll calc delta to snap for each window edge (similar to how the pad rect is specified)
    let wa = win_get_work_area();
    let mut dr = RECT::default();
    if let v = wr.x + pad.left - wa.left                    { if v.abs() < SNAP_THRESH { dr.left   = -v } }
    if let v = wr.y + pad.top  - wa.top                     { if v.abs() < SNAP_THRESH { dr.top    = -v } }
    if let v = wa.right  - (wr.x + wr.width  - pad.right )  { if v.abs() < SNAP_THRESH { dr.right  =  v } }
    if let v = wa.bottom - (wr.y + wr.height - pad.bottom)  { if v.abs() < SNAP_THRESH { dr.bottom =  v } }
    //println!("{:?}",(wa, wr, &dr));
    dr
}

fn calc_window_padding (rect:&RECT, frame:&RECT) -> RECT {
    RECT {  // rect includes padding of the 'drop shadow' around the frame
        left   : ( rect.left   -  frame.left   ).abs(),
        top    : ( rect.top    -  frame.top    ).abs(),
        right  : ( rect.right  -  frame.right  ).abs(),
        bottom : ( rect.bottom -  frame.bottom ).abs(),
    }
}

// todo consider whether reasonable to try and add snapping to adjacent window borders too!
