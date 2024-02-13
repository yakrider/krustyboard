#![ allow (non_snake_case) ]

use std::{
    thread, time,
    ops::Deref,
    sync::{Arc, RwLock},
};

use crate::{
    *, key_utils::*, utils::*, EventPropagationDirective::*,
    MouseEventCallbackFnType::*, MouseEventCbMapKeyAction::*,
};


# [ derive (Debug) ]
pub struct _MouseBtnState {
    pub btn      : MouseButton,
    pub down     : Flag,
    pub active   : Flag,
    pub consumed : Flag,
    pub stamp    : EventStamp,
    pub dbl_tap  : Flag,
}

# [ derive (Debug, Clone) ]
pub struct MouseBtnState ( Arc <_MouseBtnState> );

impl Deref for MouseBtnState {
    type Target = _MouseBtnState;
    fn deref(&self) -> &_MouseBtnState { &self.0 }
}

// since debounced action-functions need to pass the events through, cant use Fn() AF, so we'll define a DBAF
/// Debounced-Arc/Action-Function Fn(MouseEvent) representation that can be passed around to debounce wrapper
pub type DBAF  = Arc <dyn Fn(MouseEvent) + Send + Sync + 'static> ;

impl MouseBtnState {
    pub fn new (btn:MouseButton) -> MouseBtnState {
        MouseBtnState ( Arc::new ( _MouseBtnState {
            btn,
            down     : Flag::default(),
            active   : Flag::default(),
            consumed : Flag::default(),
            stamp    : EventStamp::default(),
            dbl_tap  : Flag::default(),
        } ) )
    }

    // note: we intended to impl shared debounce logic for btns here, but after queued setup we see very few actual debounce issues
    // - plus, an actual decent debounce impl was getting complex, and would involve introducing new delays before sending out btn-events
    // .. so for now, we'll only leave these stubs here are reminder ..
    // - further, on expt it looks like the resolution on the event-stamp is NOT reliable for debounce .. as often the stamps seem to get
    // .. pushed out and bunched when PC busy (as if they were pulled from an events queue higher up in the stack later and only then get stamped)
    pub fn debounced_kdn (&self, af:&DBAF, ev:MouseEvent) { af(ev) }
    pub fn debounced_kup (&self, af:&DBAF, ev:MouseEvent) { af(ev) }

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

    pub pre_drag_dat : Arc <RwLock <PreDragDat>>,
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
        setup_middle_btn_eqv_handling (&self.mbtn,  k);
        setup_middle_btn_eqv_handling (&self.x1btn, k);
        setup_middle_btn_eqv_handling (&self.x2btn, k);

        // setup handling for mouse wheel .. complex overloading over alt-tab, switche, volume, brightness etc !!
        setup_mouse_wheel_handling (k);

        setup_mouse_move_handling (k);

    }

    pub fn capture_pre_drag_dat (&self, ks:&KrustyState) {
        let ks = ks.clone();
        //thread::spawn ( move || {     // .. nuh uh
        // ^^ spawning this not only is not necessary as metrics show its only couple ms max ..
        // .. but also often right after calling this we're doing other related work that expects this to be filled out!
        *ks.mouse.pre_drag_dat.write().unwrap() = capture_pre_drag_dat(&ks);
    }

    // mod-keys notify here in case we need to do some cleanup/flagging etc
    pub fn proc_notice__modkey_down (&self, mk:ModKey, ks:&KrustyState) {
        use ModKey::*;
        self.vwheel.spin_invalidated.set();
        if ks.mouse.lbtn.down.is_set() && ( mk == caps ||  mk == lwin) {
            // we'll want to capture/refresh pre-drag-dat on caps/win presses w lbtn down as they both modify drag/resize origin behavior
            self.capture_pre_drag_dat(ks);
        }
    }
    pub fn proc_notice__modkey_up (&self, mk:ModKey, ks:&KrustyState) {
        use ModKey::*;
        self.vwheel.spin_invalidated.set();
        if mk == caps  && ks.mod_keys.lwin.down.is_set() && ks.mouse.lbtn.down.is_set() {
            // if we're exiting drag-resize into drag-move, so we should refresh our pre-drag dat reference
            self.capture_pre_drag_dat(ks);
        }
        else if mk == lalt && ks.is_replacing_alt_tab.is_set() && ks.in_right_btn_scroll_state.is_set() {
            // todo prob want to add check for switche in fgnd as we'd like clicking outside to take us out of the state
            // some special handling for alt-tab replacment
            ks.in_right_btn_scroll_state.clear();
            let switche_af = ks.mod_keys.lalt.inactive_action ( ks.mod_keys.lctrl.active_on_key(Key::F18));
            //switche_af.as_ref()();
            //thread::sleep (time::Duration::from_millis(50));
            switche_af.as_ref()();
        }

    }

}






/// sets up mouse left btn for msotly pass-through eqv w tracking, but allowing caps-as-ctrl behavior for the mouse
pub fn setup_mouse_left_btn_handling (k:&Krusty) {
    use crate::{MouseButton::*, MouseBtnEvent_T::*};

    let (ks, mbtn) = (k.ks.clone(), k.ks.mouse.lbtn.clone());
    let af:DBAF = Arc::new ( move |ev:MouseEvent| handle_mouse_left_btn_down(&ks,ev) );
    let dbaf = Arc::new ( move |ev:MouseEvent| mbtn.debounced_kdn(&af,ev) );
    k.iproc.mouse_bindings .bind_btn_event ( LeftButton, BtnEventCb(BtnDown), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_QueuedCallback(dbaf),
    } );

    let (ks, mbtn) = (k.ks.clone(), k.ks.mouse.lbtn.clone());
    let af:DBAF = Arc::new ( move |ev:MouseEvent| handle_mouse_left_btn_up(&ks,ev) );
    let dbaf = Arc::new ( move |ev:MouseEvent| mbtn.debounced_kup(&af,ev) );
    k.iproc.mouse_bindings .bind_btn_event ( LeftButton, BtnEventCb(BtnUp), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_QueuedCallback(dbaf),
    } );
    // ^^ note: to ensure that the order of btn dn/up is preserved, we want to make btn-up have blocking queued-callback too
    //  .. even when its not actually doing much (and otherwise couldve been allowed to pass through)
}

fn handle_mouse_left_btn_down (ks:&KrustyState, ev:MouseEvent) {
    use ModeState_T::*;
    ks.mouse.lbtn.down.set();
    update_stamp_mouse_dbl_click (ev.get_stamp(), &ks.mouse.lbtn.stamp, &ks.mouse.lbtn.dbl_tap);
    //ks.mouse.capture_pre_drag_dat();   // to avoid unnecessary processing, we'll only do this when win down
    ks.mod_keys.proc_notice__mouse_btn_down (ks.mouse.lbtn.btn);
    if ks.mod_keys.lwin.down.is_set() {
        // for all three modes (drag, resize, grp-drag), we need to capture dat,
        ks.mouse.capture_pre_drag_dat(ks);
        win_set_fgnd (ks.mouse.pre_drag_dat.read().unwrap().hwnd);
        // and for grp we have additional grp-add overload on the click
        if ks.mod_keys.caps.down.is_set() {
            // win-groups mode .. caps-lwin-qks<?> + lbtn click on window is add that window to the corresponding group
            if ks.mode_states.qks1.down.is_set() {
                ks.win_groups.add_to_group ( qks1.try_into().unwrap(), win_get_hwnd_from_pointer() )
            } else if ks.mode_states.qks2.down.is_set() {
                ks.win_groups.add_to_group ( qks2.try_into().unwrap(), win_get_hwnd_from_pointer() )
            } else if ks.mode_states.qks3.down.is_set() {
                ks.win_groups.add_to_group ( qks3.try_into().unwrap(), win_get_hwnd_from_pointer() )
            }
        } else if ks.mouse.lbtn.dbl_tap.is_set() {
            win_toggle_maximize (ks.mouse.pre_drag_dat.read().unwrap().hwnd);
        }
    } else if ks.mod_keys.caps.down.is_set() {
        ks.in_managed_ctrl_down_state.set();
        ks.mod_keys.lctrl.ensure_active();   // this allows caps-as-ctrl for drag drop etc
        ks.mouse.lbtn.active.set();
        MouseButton::LeftButton.press();
    } else {
        ks.mouse.lbtn.active.set();
        MouseButton::LeftButton.press();
    }
}

fn handle_mouse_left_btn_up (ks:&KrustyState, _ev:MouseEvent) {
    ks.mouse.lbtn.down.clear(); ks.mouse.lbtn.consumed.clear(); ks.mouse.lbtn.dbl_tap.clear();
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
        cb : MouseEvCbFn_QueuedCallback ( Arc::new ( move |ev:MouseEvent| handle_mouse_right_btn_down(&ks,ev) ) )
    } );

    let ks = k.ks.clone();
    k.iproc.mouse_bindings .bind_btn_event ( RightButton, BtnEventCb(BtnUp), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_QueuedCallback ( Arc::new ( move |ev:MouseEvent| handle_mouse_right_btn_up(&ks,ev) ) )
    } );
}

fn handle_mouse_right_btn_down (ks:&KrustyState, ev:MouseEvent) {
    use ModeState_T::*;
    ks.mouse.rbtn.down.set();
    update_stamp_mouse_dbl_click (ev.get_stamp(), &ks.mouse.rbtn.stamp, &ks.mouse.rbtn.dbl_tap);
    //ks.mouse.capture_pre_drag_dat(); // we dont enable drag/resize on right btn anymore
    ks.mod_keys.proc_notice__mouse_btn_down (ks.mouse.rbtn.btn);
    if ks.mod_keys.lwin.down.is_set() {
        if ks.mod_keys.caps.down.is_set() {
            // win-groups mode .. caps-lwin-qks<?> + lbtn click on window is add that window to the corresponding group
            if ks.mode_states.qks1.down.is_set() { ks.win_groups.remove_from_group (qks1.try_into().unwrap(), win_get_hwnd_from_pointer()) }
            if ks.mode_states.qks2.down.is_set() { ks.win_groups.remove_from_group (qks2.try_into().unwrap(), win_get_hwnd_from_pointer()) }
            if ks.mode_states.qks3.down.is_set() { ks.win_groups.remove_from_group (qks3.try_into().unwrap(), win_get_hwnd_from_pointer()) }
        }
        return
    }
    ks.mouse.rbtn.active.set();
    MouseButton::RightButton.press();
}

fn handle_mouse_right_btn_up (ks:&KrustyState, _ev:MouseEvent) {
    ks.mouse.rbtn.down.clear(); ks.mouse.rbtn.dbl_tap.clear();
    ks.mod_keys.proc_notice__mouse_btn_up (ks.mouse.rbtn.btn);

    if ks.in_right_btn_scroll_state.is_set() {
        ks.in_right_btn_scroll_state.clear();
        // Ctrl-F18 is the exit hotkey for switche if we were doing right-btn-scroll
        ks.mod_keys.lctrl.active_on_key(Key::F18)();
        mouse_rbtn_release_masked();
    } else  if ks.mouse.rbtn.active.is_clear() {
        // if it is inactive we didnt send a report out, so nothing to release
    } else  if ks.mouse.rbtn.consumed.is_set() {
        ks.mouse.rbtn.consumed.clear();
        mouse_rbtn_release_masked();
    } else {
        ks.mouse.rbtn.active.clear();
        MouseButton::RightButton.release();
    }
}

fn mouse_rbtn_release_masked () {
    // now we have to release the rbtn, but to avoid triggering the context menu, we'll release it at corner of screen
    //MouseButton::RightButton.release_at (0xFFFF, 0xFFFF);
    // ^^ ugh this doesnt seem to actually do that .. so we'll manually move there, release, then restore

    // and looks like for such a manual-move strategy to work, there HAS to be a delay before we move the pointer back
    // (also, this works for almost all applications EXCEPT for windows-explorer .. MS ofc has to be special .. meh)
    let point = MousePointer::pos();
    thread::spawn ( move || {
        // we'll add a delay before release, to avoid focus stealing from switche intended window etc
        MousePointer::move_abs (0xFFFF, 0xFFFF);
        thread::sleep(time::Duration::from_millis(20));
        MouseButton::RightButton.release();
        // we'll add some delay to actually have the release processed while pointer is still away
        thread::sleep(time::Duration::from_millis(40));
        // note that reducing this delay or even removing it will mostly work (as the event handling can happen before spawned thread comes up)..
        // .. however, for times when there's load etc and the event handling is also pushed out, we'll want at least some delay
        win_set_thread_dpi_aware();
        MousePointer::move_abs (point.x, point.y);
    } );
}


/// x1/x2 btns can be set up to behave like they were middle btn
pub fn setup_middle_btn_eqv_handling (mbs:&MouseBtnState, k:&Krusty) {
    use crate::MouseBtnEvent_T::*;
    // note that we cant make X1/X2 btns act truly like mbtn as they dont seem to send dn/up events on press/rel ..
    // .. instead they send nothing on btn-dn and send dn/up at btn-rel .. (or nothing if held too long!)
    let ks = k.ks.clone(); let mbsc = mbs.clone();
    k.iproc.mouse_bindings .bind_btn_event ( mbs.btn, BtnEventCb(BtnDown), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_QueuedCallback ( Arc::new ( move |ev:MouseEvent| { handle_mouse_middle_btn_down(&ks,ev,&mbsc) } ) ),
    } );
    let ks = k.ks.clone(); let mbsc = mbs.clone();
    k.iproc.mouse_bindings .bind_btn_event ( mbs.btn, BtnEventCb(BtnUp), MouseEventCallbackEntry {
        event_prop_directive: EventProp_Stop,
        cb : MouseEvCbFn_QueuedCallback ( Arc::new ( move |ev:MouseEvent| { handle_mouse_middle_btn_up(&ks,ev,&mbsc) } ) )
    } );

}

fn handle_mouse_middle_btn_down (ks:&KrustyState, ev:MouseEvent, mbs:&MouseBtnState) {
    mbs.down.set();
    update_stamp_mouse_dbl_click (ev.get_stamp(), &mbs.stamp, &mbs.dbl_tap);
    if ks.mod_keys.lwin.down.is_set() {
        ks.mod_keys.lwin.consumed.set();
        let pointed_hwnd = win_get_hwnd_from_pointer();
        if ( // for the pointed window, w caps/ctrl/shift and fgnd, we'll try close-tab via ctrl-w
            ks.mod_keys.caps.down.is_set() || ks.mod_keys.lctrl.down.is_set() || ks.mod_keys.lshift.down.is_set()
        ) && pointed_hwnd == win_get_fgnd() {
            ks.ag (Key::W) .m(ModKey::ctrl) .gen_af()();
        } else { // and for no-mods win-mbtn we'll close the mouse-pointed window
            win_close (pointed_hwnd);
        }
    } else if ks.mod_keys.lshift.down.is_set() {
        // lshift-mbtn to go backwards in IDE cursor location
        ks.ag (Key::ExtLeft ) .m(ModKey::alt) .gen_af()();
    } else if ks.mod_keys.caps.down.is_set() || ks.mod_keys.lctrl.down.is_set() {
        // lctrl-mbtn to go forwards in IDE cursor location
        ks.ag (Key::ExtRight) .m(ModKey::alt) .gen_af()();
    } else {
        ks.mouse.mbtn.active.set();
        MouseButton::MiddleButton.press();
    }
}
fn handle_mouse_middle_btn_up (ks:&KrustyState, _ev:MouseEvent, mbs:&MouseBtnState) {
    mbs.down.clear(); mbs.consumed.clear(); mbs.dbl_tap.clear();
    if ks.mouse.mbtn.active.is_set() {
        ks.mouse.mbtn.active.clear();
        MouseButton::MiddleButton.release();
    }
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
    if !ksr.mouse.vwheel.spin_invalidated.is_set() {
        handle_wheel_action(delta, ksr);
    } else if GUARD_DUR_MS < ksr.mouse.vwheel.last_stamp.get().duration_since(last_stamp).as_millis() {
        ksr.mouse.vwheel.spin_invalidated.clear();
        handle_wheel_action(delta, ksr);
    } else {
        // if its invalidated AND wheel-spin spacing is below guard-dur, we suppress the wheel event
    }
}

fn handle_wheel_action (delta:i32, ksr:&KrustyState) {
    use windows::Win32::UI::WindowsAndMessaging::WHEEL_DELTA;
    let incr = delta / WHEEL_DELTA as i32;
    if ksr.mouse.rbtn.down.is_set() {
        // right-mouse-btn-wheel support for switche task switching
        ksr.in_right_btn_scroll_state.set();
        ksr.mouse.rbtn.consumed.set();
        let key = if incr.is_positive() { Key::F17 } else { Key::F16 };
        //ksr.mod_keys.lalt.active_action(base_action(key))();       // not usable due to masking keys (so changed in hotkey switche)
        //ksr.mod_keys.lctrl.active_action(base_action(key))();      // not ideal as even non-masked wrapping causes slower/choppy switche scroll
        press_release(key);                                 // we'd rather use direct press hotkeys for best perf
    } else  if ksr.mod_keys.lalt.down.is_set() {
        // wheel support for scrolling in windows native alt-tab task-switching screen
        // .. we could consider spawning this part out, but meh we're in side-thread queue, its prob ok
        ksr.mod_keys.lalt.consumed.set();
        if ksr.is_replacing_alt_tab.is_set() && ksr.in_right_btn_scroll_state.is_set() {
            ksr.mod_keys.lalt.ensure_inactive();    // we cant have alt-tab go out as thatd trigger the actual alt-tab
            handle_alt_tab_wheel(incr);
        } else {
            let fgnd_class = get_fgnd_win_class();
            if fgnd_class == "XamlExplorerHostIslandWindow" || fgnd_class == "MultitaskingViewFrame" {  //dbg!(task_switching_state);
                ksr.mod_keys.lalt.ensure_active();    // we're already down but just in case its down/inactive
                handle_alt_tab_wheel(incr)
            } else {
                // alt-wheel for (large delta) control, qks1-alt-wheel for finer adjustments
                let mult = if ksr.mode_states.qks1.down.is_set() {1} else {4};
                incr_brightness (incr * mult)
                // ^^ potentially could spawn this out too, but in this queue thread, meh it'll be fine
            }
        }
    } else if ksr.mod_keys.lwin.down.is_set() {
        // win-wheel for (large delta) control, qks1-win-wheel for finer adjustments
        ksr.mod_keys.lwin.consumed.set();
        let mult = if ksr.mode_states.qks1.down.is_set() {1} else {1}; // meh just do incrs in 1 .. its already too fast
        incr_volume (incr * mult)
    } else if ksr.mod_keys.caps.down.is_set() || ksr.mod_keys.some_ctrl_down() {
        if ksr.mod_keys.caps.down.is_set() {
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
        // ^^ todo:: .. (for now, just let default pass through for shift scrolls)
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
    //bringup_click_monitor_cdc();
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
            if ks.mod_keys.lwin.down.is_set() && ks.mouse.lbtn.down.is_set() {
                ks.mod_keys.lwin.consumed.set();
                if let move_event { x_pos, y_pos, .. } = ev {
                    handle_pointer_move (x_pos, y_pos, &ks)
            } }
        } ) ),
    } );
}

fn handle_pointer_move (x:i32, y:i32, ks:&KrustyState) {
    // NOTE that mouse move is inline pass-through before these handler calls get queued
    // .. so there could be some lag, but should be quick enough that we can work with ks states as we currently see it
    // NOTE also that we already set the thread dpi-aware when initing the events-queue thread itself
    if ks.mouse.lbtn.down.is_set() && ks.mouse.lbtn.consumed.is_clear(){
        if ks.mod_keys.caps.down.is_set() && ks.mode_states.qks1.down.is_clear() {
            handle_pointer_window_resize_spaced (x, y, &ks)
        } else {
            handle_pointer_window_drag_spaced (x, y, &ks)
        }
    }
}
