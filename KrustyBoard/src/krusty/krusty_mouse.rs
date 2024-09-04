#![ allow (non_snake_case) ]

use std::{
    thread, time,
    sync::{Arc, RwLock},
    sync::atomic::{AtomicI32, Ordering},
};
use derive_deref::Deref;

use crate::{
    *, utils::*,
    EvProp_D::*, ComboProc_D::*,
    EvCbFn_T::*, EvCbMapKey_Action::*,
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

# [ derive (Debug, Clone, Deref) ]
pub struct MouseBtnState ( Arc <_MouseBtnState> );


// since debounced action-functions need to pass the events through, cant use Fn() AF, so we'll define a DBAF
/// Debounced-Arc/Action-Function Fn(InputEvent) representation that can be passed around to debounce wrapper
//pub type DBAF  = Arc <dyn Fn(InputEvent) + Send + Sync + 'static> ;

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
    //pub fn debounced_kdn (&self, af:&DBAF, ev:InputEvent) { af(ev) }
    //pub fn debounced_kup (&self, af:&DBAF, ev:InputEvent) { af(ev) }

}


# [ derive (Debug) ]
pub struct _MouseWheelState {
    pub wheel : MouseWheel,
    pub last_stamp : TimeStamp,
    pub last_delta : AtomicI32,
    // we'll also hold a flag to invalidate an ongoing inertial spin by e.g. mid-spin mod press (or actual spin stop (spacing > 120ms))
    pub spin_invalidated : Flag,
}

# [ derive (Debug, Clone, Deref) ]
pub struct MouseWheelState ( Arc <_MouseWheelState> );


impl MouseWheelState {
    pub fn new (wheel:MouseWheel) -> MouseWheelState {
        MouseWheelState ( Arc::new ( _MouseWheelState {
            wheel,
            last_stamp       : TimeStamp::new(),
            last_delta       : Default::default(),
            spin_invalidated : Flag::default(),
        } ) )
    }
}

# [ derive (Debug) ]
pub struct Mouse {
    _private  : (),
    pub lbtn  : MouseBtnState,
    pub rbtn  : MouseBtnState,
    pub mbtn  : MouseBtnState,

    pub x1btn : MouseBtnState,
    pub x2btn : MouseBtnState,

    pub vwheel : MouseWheelState,
    //pub hwheel : MouseWheelState,

    pub pre_drag_dat : RwLock <PreDragDat>,
}



impl Mouse {

    pub fn new() -> Mouse {
        use crate::{MouseButton::*, MouseWheel::*};
        Mouse {
            _private: (),
            lbtn   : MouseBtnState::new(LeftButton),
            rbtn   : MouseBtnState::new(RightButton),
            mbtn   : MouseBtnState::new(MiddleButton),
            x1btn  : MouseBtnState::new(X1Button),
            x2btn  : MouseBtnState::new(X2Button),
            vwheel : MouseWheelState::new(DefaultWheel),
            //hwheel : MouseWheelState::new(HorizontalWheel),
            //pointer: MousePointerState::default(),
            pre_drag_dat : RwLock::new (PreDragDat::default())
        }
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

    pub fn get_btn_state (&self, btn:MouseButton) -> Option<MouseBtnState> {
        use crate::MouseButton::*;
        match btn {
            LeftButton   => Some(self.lbtn.clone()),
            RightButton  => Some(self.rbtn.clone()),
            MiddleButton => Some(self.mbtn.clone()),
            X1Button     => Some(self.x1btn.clone()),
            X2Button     => Some(self.x2btn.clone()),
            _ => None
        }
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
    }

}






/// sets up mouse left btn for msotly pass-through eqv w tracking, but allowing caps-as-ctrl behavior for the mouse
pub fn setup_mouse_left_btn_handling (k:&Krusty) {
    use crate::{MouseButton::*, MouseBtnEvent_T::*};

    let ks = k.ks.clone();
    k.iproc.input_bindings .bind_btn_event (LeftButton, BtnEventCb(BtnDown), EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Undet, ComboProc_Undet),
        cb : EvCbFn_Inline( Arc::new ( move |ev| {
            ks.mouse.lbtn.down.set();
            update_stamp_mouse_dbl_click (ev.stamp, &ks.mouse.lbtn.stamp, &ks.mouse.lbtn.dbl_tap);
            //ks.mouse.capture_pre_drag_dat();   // to avoid unnecessary processing, we'll only do this when win down
            ks.mod_keys.proc_notice__mouse_btn_down (ks.mouse.lbtn.btn);
            // the rest of the behavior we'll let be defined via combo mapping
            EvProc_Ds::new (EvProp_Stop, ComboProc_Enable)
        } ) ),
    } );

    let ks = k.ks.clone();
    k.iproc.input_bindings .bind_btn_event (LeftButton, BtnEventCb(BtnUp), EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Stop, ComboProc_Disable),
        cb : EvCbFn_Queued( Arc::new ( move |ev| { handle_mouse_left_btn_up (&ks, ev) } ) ),
    } );
    // ^^ note: to ensure that the order of btn dn/up is preserved, we want to make btn-up have blocking queued-callback too
    //  .. even when its not actually doing much (and otherwise couldve been allowed to pass through)
}


fn handle_mouse_left_btn_up (ks:&KrustyState, _:Event) {
    ks.mouse.lbtn.down.clear(); ks.mouse.lbtn.consumed.clear(); ks.mouse.lbtn.dbl_tap.clear();
    ks.mod_keys.proc_notice__mouse_btn_up (ks.mouse.lbtn.btn);
    if ks.mouse.lbtn.active.is_set(){
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
// note that unlike other mouse btn/wheels, the right btn UP is set to passthrough!
pub fn setup_mouse_right_btn_handling (k:&Krusty) {
    use crate::{MouseButton::*, MouseBtnEvent_T::*};

    let ks = k.ks.clone();
    k.iproc.input_bindings .bind_btn_event (RightButton, BtnEventCb(BtnDown), EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Undet, ComboProc_Undet),
        cb : EvCbFn_Inline( Arc::new ( move |ev| {
            ks.mouse.rbtn.down.set();
            update_stamp_mouse_dbl_click (ev.stamp, &ks.mouse.rbtn.stamp, &ks.mouse.rbtn.dbl_tap);
            //ks.mouse.capture_pre_drag_dat(); // we dont enable drag/resize on right btn anymore
            ks.mod_keys.proc_notice__mouse_btn_down (ks.mouse.rbtn.btn);
            // the rest of the behavior we'll let be defined via combo mapping
            EvProc_Ds::new (EvProp_Stop, ComboProc_Enable)
        } ) )
    } );

    /* NOTE: typically, we'd have this be queued like most others, and if we need to let the event through, we'd reinject in the stream,
        .. but rbtn up has the peculiar situation where to support switche, when swi injects an rbtn-up to start rbtn-scroll mode,
        .. we cant just reinject the rbtn-up, as switche would then reprocess it in a feedback loop since it doesnt have is own extra-info..
        .. So instead, we handle it inline so we can let it through in that special case
       Note ofc, that inline and queued will both remain in seq order ..
        .. the queued just lets us do longer proc w/o spawning and still keep event-stream fast
        .. even doing mostly inline w spawning for heavy tasks would introduce out-of-seq possibility .. so queueing is a bit better ..
        .. though ofc for real heavy tasks, and where sequencing is less essential, we should spawn from queue proc thread too
     */
    let ks = k.ks.clone();
    k.iproc.input_bindings .bind_btn_event (RightButton, BtnEventCb(BtnUp), EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Undet, ComboProc_Disable),
        cb : EvCbFn_Inline( Arc::new ( move |ev| handle_mouse_right_btn_up (&ks, ev) ) )
    } );
}

// todo : consider if theres any reasonable way to move some of this inline btn-up handling out to combos and/or app code

fn handle_mouse_right_btn_up (ks:&KrustyState, ev: Event) -> EvProc_Ds {
    // ^^ also see comment in setup-handling section re details on why this callback is inline for switche support
    if ev.extra_info == SWITCHE_INJECTED_IDENTIFIER_EXTRA_INFO {
        ks.mouse.rbtn.active.clear(); ks.mouse.rbtn.down.clear();
        // ^^ note that we'll even clear down state now, even though its not phys rbtn-up, as switche might block the phys rbtn-up
        // (mostly in case swi is before krusty in hook chain .. else we'd hear the phys rbtn-up before swi anyway)
        return EvProc_Ds::new (EvProp_Continue, ComboProc_Disable);
    }
    // the rest we'll treat as physical events (even if its from other unrecognized injections)
    ks.mouse.rbtn.down.clear(); ks.mouse.rbtn.dbl_tap.clear();
    ks.mod_keys.proc_notice__mouse_btn_up (ks.mouse.rbtn.btn);

    if ks.in_right_btn_scroll_state.is_set() {
        ks.in_right_btn_scroll_state.clear();
        ks.mouse.rbtn.consumed.clear(); ks.mouse.rbtn.active.clear();
        // ^^ on switche rbtn scroll, switche sends early rbtn-rel, so when we get this real one, we might be rbtn-inactive
        // .. so catching this separately here, lets us pass this through for switche (for cases its hook is behind us)
        // (and lower down, we can separately check just for cases w rbtn inactive coz we/krusty suppressed it, and for those, suppress release)
        return EvProc_Ds::new (EvProp_Continue, ComboProc_Disable);
    }
    if ks.mouse.rbtn.active.is_set() && ks.mouse.rbtn.consumed.is_set() {
        ks.mouse.rbtn.consumed.clear(); ks.mouse.rbtn.active.clear();
        mouse_rbtn_release_masked();
        return EvProc_Ds::new (EvProp_Stop, ComboProc_Disable);
        // ^^ this was mostly for switche while we drove it from here .. swi now has native impl and does its own masking ..
        // .. but we might as well leave the masking mechanism here in case we ever have other uses for rbtn and need to avoid context menu popups
    }
    if ks.mouse.rbtn.active.is_clear() {
        // if we arent active, we likely never sent out a press, so no need to send a release either
        // (note that we checked switche rbtn-scroll case above, so this specifically only blocks rel whose press we also likely blocked)
        return EvProc_Ds::new (EvProp_Stop, ComboProc_Disable);
    }
    ks.mouse.rbtn.consumed.clear(); ks.mouse.rbtn.active.clear();
    return EvProc_Ds::new (EvProp_Continue, ComboProc_Disable);
    // ^^ everything else we can just let them through .. it should be mostly harmless (other than for swi that we deal w above)
    // note: this includes cases where we thought it was inactive already .. as we dont want to risk affecting anyone behind us in hook chain
}

fn mouse_rbtn_release_masked () {
    // now we have to release the rbtn, but to avoid triggering the context menu, we'll release it at corner of screen
    //MouseButton::RightButton.release_at (0xFFFF, 0xFFFF);
    // ^^ ugh this doesnt seem to actually do that .. so we'll manually move there, release, then restore

    // and looks like for such a manual-move strategy to work, there HAS to be a delay before we move the pointer back
    // (also, this works for almost all applications EXCEPT for windows-explorer .. MS ofc has to be special .. meh)
    let point = MousePointer::pos();
    thread::spawn ( move || {
        win_set_thread_dpi_aware();
        // we'll add a delay before release, to avoid focus stealing from switche intended window etc
        MousePointer::move_abs (0xFFFF, 0xFFFF);
        thread::sleep(time::Duration::from_millis(20));
        MouseButton::RightButton.release();
        // we'll add some delay to actually have the release processed while pointer is still away
        thread::sleep(time::Duration::from_millis(40));
        // note that reducing this delay or even removing it will mostly work (as the event handling can happen before spawned thread comes up)..
        // .. however, for times when there's load etc and the event handling is also pushed out, we'll want at least some delay
        MousePointer::move_abs (point.x, point.y);
    } );
}





/// x1/x2 btns can be set up to behave like they were middle btn
pub fn setup_middle_btn_eqv_handling (mbs:&MouseBtnState, k:&Krusty) {
    use crate::MouseBtnEvent_T::*;
    // note that we cant make X1/X2 btns act truly like mbtn as they dont seem to send dn/up events on press/rel ..
    // .. instead they send nothing on btn-dn and send dn/up at btn-rel .. (or nothing if held too long!)
    let mbsc = mbs.clone();
    k.iproc.input_bindings .bind_btn_event (mbs.btn, BtnEventCb(BtnDown), EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Undet, ComboProc_Undet),
        cb : EvCbFn_Inline( Arc::new ( move |ev| {
            mbsc.down.set();
            update_stamp_mouse_dbl_click (ev.stamp, &mbsc.stamp, &mbsc.dbl_tap);
            // the rest of the behavior we'll let be defined via combo mapping
            EvProc_Ds::new (EvProp_Stop, ComboProc_Enable)
        } ) ),
    } );
    let mbsc = mbs.clone();
    k.iproc.input_bindings .bind_btn_event (mbs.btn, BtnEventCb(BtnUp), EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Undet, ComboProc_Undet),
        cb : EvCbFn_Inline( Arc::new ( move |_| {
            mbsc.down.clear(); mbsc.consumed.clear(); mbsc.dbl_tap.clear();
            // the rest of the behavior we'll let be defined via combo mapping
            EvProc_Ds::new (EvProp_Stop, ComboProc_Enable)
        } ) )
    } );
}





/// sets up mouse wheel with various overloaded modes incl brightness, volume, tab-switching, caps-as-ctrl etc
pub fn setup_mouse_wheel_handling (k:&Krusty) {
    let ks = k.ks.clone();
    k.iproc.input_bindings .bind_wheel_event (MouseWheel::DefaultWheel, EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Undet, ComboProc_Undet),
        cb : EvCbFn_Inline( Arc::new ( move |ev| {
            if let EventDat::wheel_event {delta, ..} = ev.dat {
                ks.mouse.vwheel.last_delta.store (delta, Ordering::Relaxed);
            }
            let combo_proc_d = if check_wheel_spaced(&ks) { ComboProc_Enable } else { ComboProc_Disable };
            // the rest of the behavior we'll let be defined via combo mapping
            EvProc_Ds::new (EvProp_Stop, combo_proc_d)
        } ) ),
    } );
}

fn check_wheel_spaced (ksr:&KrustyState) -> bool {
    // the invalidation setup below prevents things like caps down when wheel is still unintentionally inertially spinning to trigger zooms etc
    // however, we NO-LONGER space out the super-fast inertial smooth-scroll wheel (e.g. on my MX3 mouse) for improved usability
    // so here, we suppress wheel event if wheel-spin spacing is below guard-dur AND it has already been invalidated
    let last_stamp = ksr.mouse.vwheel.last_stamp.get();
    ksr.mouse.vwheel.last_stamp.capture();
    //let gap = ksr.last_wheel_stamp.read().unwrap().duration_since(last_stamp);
    //println!("{:#?}", dur.as_millis());
    const GUARD_DUR_MS: u128 = 120;  // from dur printouts above, looked like max inertial gap is 120 (min 7ms, usually <100)
    if !ksr.mouse.vwheel.spin_invalidated.is_set() {
        return true
    }
    if GUARD_DUR_MS < ksr.mouse.vwheel.last_stamp.get().duration_since(last_stamp).as_millis() {
        ksr.mouse.vwheel.spin_invalidated.clear();
        return true
    }
    false
}




#[allow(dead_code)]
fn handle_horiz_scroll_wheel (_incr:i32) {
    // todo we could in theory impl this overriding the native horiz scroll behavior
}





pub fn setup_mouse_move_handling (k:&Krusty) {
    use crate::EventDat::*;
    let ks = k.ks.clone();
    k.iproc.input_bindings .bind_pointer_event ( EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Continue, ComboProc_Disable),
        cb: EvCbFn_Queued( Arc::new ( move |ev| {
            if ks.mod_keys.lwin.down.is_set() && ks.mouse.lbtn.down.is_set() {
                ks.mod_keys.lwin.consumed.set();
                if let move_event { x_pos, y_pos, .. } = ev.dat {
                    handle_pointer_move (x_pos, y_pos, &ks)
            } }
        } ) ),
    } );
}

fn handle_pointer_move (x:i32, y:i32, ks:&KrustyState) {
    // NOTE that mouse move is inline pass-through before these handler calls get queued
    // .. so there could be some lag, but should be quick enough that we can work with ks states as we currently see it
    // NOTE also that we already set the thread dpi-aware when initing the events-queue thread itself
    if ks.mouse.lbtn.down.is_set() && ks.mouse.lbtn.consumed.is_clear() {
        if ks.mod_keys.caps.down.is_set() && ks.mode_states.qks1.down.is_clear() {
            handle_pointer_window_resize_spaced (x, y, &ks)
        } else {
            handle_pointer_window_drag_spaced (x, y, &ks)
        }
    }
}
