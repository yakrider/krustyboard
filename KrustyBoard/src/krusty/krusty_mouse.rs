#![ allow (non_snake_case) ]

use std::{
    thread, time,
    sync::Arc,
    sync::atomic::{AtomicI32, Ordering},
};
use once_cell::sync::OnceCell;

use crate::{
    *, utils::*,
    EvProp_D::*, ComboProc_D::*, EvCbFn_T::*
};



pub const DEFAULT_MOUSE_WHEEL_DELTA: i32 = 120;




#[derive (Debug)]
pub struct MouseBtnState {
    pub btn      : MouseButton,   // btn enum
    pub down     : Flag,          // physically down
    pub active   : Flag,          // externally active (press has been sent out)
    pub pending  : Flag,          // press-rel is set to be sent out upon release
    pub consumed : Flag,          // if consumed, the release should be masked
    pub dbl_tap  : Flag,          // dbl-tap
    pub down_xy  : PointAtomic,   // the xy point where this last press happened
}

// since debounced action-functions need to pass the events through, cant use Fn() AF, so we'll define a DBAF
/// Debounced-Arc/Action-Function Fn(InputEvent) representation that can be passed around to debounce wrapper
//pub type DBAF  = Arc <dyn Fn(InputEvent) + Send + Sync + 'static> ;


impl MouseBtnState {
    pub fn new (btn:MouseButton) -> MouseBtnState {
        MouseBtnState {
            btn,
            down     : Flag::default(),
            active   : Flag::default(),
            pending  : Flag::default(),
            consumed : Flag::default(),
            dbl_tap  : Flag::default(),
            down_xy  : PointAtomic::default(),
        }
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
pub struct MouseWheelState {
    pub wheel : MouseWheel,
    pub last_stamp : EventStamp,
    pub last_delta : AtomicI32,
    // we'll also hold a flag to invalidate an ongoing inertial spin by e.g. mid-spin mod press (or actual spin stop (spacing > 120ms))
    pub spin_invalidated : Flag,
}

impl MouseWheelState {
    pub fn new (wheel:MouseWheel) -> MouseWheelState {
        MouseWheelState {
            wheel,
            last_stamp       : EventStamp::new(),
            last_delta       : AtomicI32::from(DEFAULT_MOUSE_WHEEL_DELTA),
            spin_invalidated : Flag::default(),
        }
    }
}



#[derive (Debug)]
pub struct Mouse {
    _private  : (),
    // btns
    pub lbtn  : &'static MouseBtnState,
    pub rbtn  : &'static MouseBtnState,
    pub mbtn  : &'static MouseBtnState,
    pub x1btn : &'static MouseBtnState,
    pub x2btn : &'static MouseBtnState,
    // wheels
    pub vwheel : &'static MouseWheelState,
    pub hwheel : &'static MouseWheelState,
    // pointer
    //pub pointer : MousePointer,
}



impl Mouse {

    pub fn instance () -> &'static Mouse {
        use crate::{MouseButton::*, MouseWheel::*};

        static VERT_WHEEL  : OnceCell<MouseWheelState> = OnceCell::new();
        static HORIZ_WHEEL : OnceCell<MouseWheelState> = OnceCell::new();

        static LEFT_BTN   : OnceCell<MouseBtnState> = OnceCell::new();
        static RIGHT_BTN  : OnceCell<MouseBtnState> = OnceCell::new();
        static MIDDLE_BTN : OnceCell<MouseBtnState> = OnceCell::new();
        static X1_BTN     : OnceCell<MouseBtnState> = OnceCell::new();
        static X2_BTN     : OnceCell<MouseBtnState> = OnceCell::new();

        static INSTANCE : OnceCell<Mouse> = OnceCell::new();

        INSTANCE .get_or_init ( || {
            Mouse {
                _private: (),

                lbtn   : LEFT_BTN   .get_or_init (|| MouseBtnState::new(LeftButton  )),
                rbtn   : RIGHT_BTN  .get_or_init (|| MouseBtnState::new(RightButton )),
                mbtn   : MIDDLE_BTN .get_or_init (|| MouseBtnState::new(MiddleButton)),
                x1btn  : X1_BTN     .get_or_init (|| MouseBtnState::new(X1Button    )),
                x2btn  : X2_BTN     .get_or_init (|| MouseBtnState::new(X2Button    )),

                vwheel : VERT_WHEEL .get_or_init (|| MouseWheelState::new(DefaultWheel)),
                hwheel : HORIZ_WHEEL.get_or_init (|| MouseWheelState::new(HorizontalWheel)),
            }
        } )
    }

    pub fn clear_flags (&self) {
        for mbtn in [ &self.lbtn, &self.rbtn, &self.mbtn, &self.x1btn, &self.x2btn ] {
            mbtn.down.clear(); mbtn.dbl_tap.clear(); mbtn.active.clear(); mbtn.consumed.clear(); mbtn.pending.clear();
        }
        self.vwheel.spin_invalidated.clear();
        self.hwheel.spin_invalidated.clear();
    }

    pub fn setup_mouse (&self, k:KR) {

        // for most mouse btn actions, we can setup standard skeleton bindings, and let actual 'business-logic' be setup via combo bindings

        setup_standard_mbtn_press_handling   (k.ks.mouse.lbtn, k);
        setup_standard_mbtn_release_handling (k.ks.mouse.lbtn, k);

        setup_standard_mbtn_press_handling   (k.ks.mouse.mbtn, k);
        setup_standard_mbtn_release_handling (k.ks.mouse.mbtn, k);

        setup_standard_mbtn_press_handling   (k.ks.mouse.x1btn, k);
        setup_standard_mbtn_release_handling (k.ks.mouse.x1btn, k);

        setup_standard_mbtn_press_handling   (k.ks.mouse.x2btn, k);
        setup_standard_mbtn_release_handling (k.ks.mouse.x2btn, k);

        setup_standard_mbtn_press_handling   (k.ks.mouse.rbtn, k);
        //setup_standard_mbtn_release_handling (k.ks.mouse.rbtn, k);
        setup_mouse_right_btn_release_handling (k);
        // ^^ for the mouse right-btn, we have to make small special case for switche-injected events, so we do it separately
        // ^^ but we're now driving from kr to avoid ctx menu popups .. so in theory we wouldnt have to
        // .. but we're still keeping this in case sw is ran w mouse hook enabled (and so injects rbtn-rel)


        // for wheels, we set up uniform binding for all wheels/directions, and let combo mapping add specific behavior
        use MouseWheelEv_T::*;
        setup_mouse_wheel_handling (k, k.ks.mouse.vwheel, WheelForwards );
        setup_mouse_wheel_handling (k, k.ks.mouse.vwheel, WheelBackwards);

        setup_mouse_wheel_handling (k, k.ks.mouse.hwheel, WheelForwards );
        setup_mouse_wheel_handling (k, k.ks.mouse.hwheel, WheelBackwards);


        setup_mouse_move_handling (k);

    }

    pub fn get_btn_state (&self, btn:MouseButton) -> Option<&'static MouseBtnState> {
        use crate::MouseButton::*;
        match btn {
            LeftButton   => Some (self.lbtn),
            RightButton  => Some (self.rbtn),
            MiddleButton => Some (self.mbtn),
            X1Button     => Some (self.x1btn),
            X2Button     => Some (self.x2btn),
            _ => None
        }
    }
    pub fn get_wheel_state (&self, wheel:MouseWheel) -> Option<&'static MouseWheelState> {
        match wheel {
            MouseWheel::DefaultWheel    => Some (self.vwheel),
            MouseWheel::HorizontalWheel => Some (self.hwheel),
            _ => None
        }
    }

    // mod-keys notify here in case we need to do some cleanup/flagging etc
    pub fn proc_notice__modkey_down (&self, mk:ModKey, ks:KSR) {
        use ModKey::*;
        self.vwheel.spin_invalidated.set();
        if ks.mouse.lbtn.down.is_set() && ( mk == caps ||  mk == lwin) {
            // we'll want to capture/refresh win-snap-dat on caps/win presses w lbtn down as they both modify drag/resize origin behavior
            // .. for lwin, we always want to capture the hwnd at lbtn clicked point ..
            // .. but with caps ..ideally, we should allow adding caps to win-drag to modify the drag from whereever the pointer is at ..
            // however, given how wildly the pointer can lead the window, we still want the hwnd to be clamped to the lbtn-pressed hwnd!
            // .. so we'll use pointer from cur mouse, but keep the hwnd as was in the last win-snap-dat currently being used!
            let action = Box::new ( move || {
                let (cur_xy, lbtn_xy) = (MousePointer::pos(), ks.mouse.lbtn.down_xy.load());
                let wsd_hwnd = ks.win_snap_dat.read() .map (|wsd| wsd.hwnd) .unwrap_or (win_get_hwnd_from_point(lbtn_xy));
                let (xy, hwnd) = if mk == lwin {
                    //(lbtn_xy, win_get_hwnd_from_point(lbtn_xy))
                    (lbtn_xy, wsd_hwnd)
                } else { // i.e. caps
                    //let hwnd = ks.win_snap_dat.read() .map (|wsd| wsd.hwnd) .unwrap_or (win_get_hwnd_from_point(lbtn_xy));
                    (cur_xy, wsd_hwnd)
                };
                ks.capture_win_snap_dat (xy, hwnd, None);
            } );
            let _ = InputProcessor::instance().input_af_queue .send (action);
        }
        else if ks.mouse.rbtn.down.is_set() && mk == caps {
            // since we use rbtn-scrolls for switching etc, we want to disable rbtn-pending on any caps-activity while rbtn down
            // (mostly to suppress stray ctx menu when we might half-heartedly start rbtn-scroll but dont actually scroll lol)
            ks.mouse.rbtn.pending.clear();
        }
    }
    pub fn proc_notice__modkey_up (&self, mk:ModKey, ks:KSR) {
        use ModKey::*;
        self.vwheel.spin_invalidated.set();
        if mk == caps {
            if ks.mod_keys.lwin.down.is_set() && ks.mouse.lbtn.down.is_set() {
                // if we're exiting drag-resize into drag-move, so we should refresh our win-snap dat reference
                let (cur_xy, lbtn_xy) = (MousePointer::pos(), ks.mouse.lbtn.down_xy.load());
                let wsd_hwnd = ks.win_snap_dat.read() .map (|wsd| wsd.hwnd) .unwrap_or (win_get_hwnd_from_point(lbtn_xy));
                let action = Box::new (move || ks.capture_win_snap_dat (cur_xy, wsd_hwnd, None) );
                let _ = InputProcessor::instance().input_af_queue .send (action);
            }
            else if ks.mouse.rbtn.down.is_set() {
                // we'll clear pending rbtns like upon press above
                ks.mouse.rbtn.pending.clear();
            }
        } // nothing for non-caps modkeys
    }

}





/// setup standard mouse btn PRESS handling expecting the actual 'business-logic' to be setup via combo mappings
pub fn setup_standard_mbtn_press_handling (mbs: &'static MouseBtnState, k:KR) {
    use crate::MouseBtnEv_T::*;
    k.iproc.input_bindings .bind_btn_event (mbs.btn, BtnDown, EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Undet, ComboProc_Undet),
        cb : EvCbFn_Inline ( Arc::new ( move |ev| {
            mbs.down.set(); mbs.consumed.clear();
            if let EventDat::btn_event { xy, .. } = ev.dat {  mbs.down_xy.store (xy) }
            update_dbl_tap (&ev, &mbs.dbl_tap);
            // the rest of the behavior we'll let be defined via combo mapping
            EvProc_Ds::new (EvProp_Stop, ComboProc_Enable)
        } ) ),
    } );
}

/// setup standard mouse btn RELEASE handling expecting the actual 'business-logic' to be setup via combo mappings
pub fn setup_standard_mbtn_release_handling (mbs: &'static MouseBtnState, k:KR) {
    use crate::MouseBtnEv_T::*;
    k.iproc.input_bindings .bind_btn_event (mbs.btn, BtnUp, EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Undet, ComboProc_Undet),
        cb : EvCbFn_Inline ( Arc::new ( move |_| {
            mbs.down.clear(); mbs.dbl_tap.clear();
            // the rest of the behavior we'll let be defined via combo mapping
            EvProc_Ds::new (EvProp_Stop, ComboProc_Enable)
        } ) ),
    } );
}



/// sets up mouse right btn RELEASE with special case for to handle switche injected rbtn-ups during rbtn-held-scroll
pub fn setup_mouse_right_btn_release_handling (k:KR) {
    /* NOTE: this has a bit of special consideration, as mostly we'd just use queued callbacks or light inline wrapper then combo-proc ..
        .. but rbtn up has the peculiar situation where to support switche, when swi injects an rbtn-up to start rbtn-scroll mode,
        .. we cant just reinject the rbtn-up in combo handling, as switche would reprocess it in a feedback loop (for lack of have switche extra-info)
        .. So instead, we check for that in the inline binding handler itself so we can let it through in that special case
    */
    use crate::{MouseButton::*, MouseBtnEv_T::*};
    let mbtn = k.ks.mouse.rbtn;
    k.iproc.input_bindings .bind_btn_event (RightButton, BtnUp, EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Undet, ComboProc_Undet),
        //cb : EvCbFn_Inline ( Arc::new ( move |ev| handle_mouse_right_btn_up (&ks, ev) ) )
        cb : EvCbFn_Inline ( Arc::new ( move |ev| {
            mbtn.down.clear(); mbtn.dbl_tap.clear();
            if ev.extra_info == SWITCHE_INJECTED_IDENTIFIER_EXTRA_INFO {
                mbtn.active.clear();
                k.ks.in_right_btn_scroll_state.set();
                // ^^ note that we let even down state be cleared above, even though its not phys rbtn-up, as switche might block the phys rbtn-up
                // (mostly in case swi is before krusty in hook chain .. else we'd hear the phys rbtn-up before swi anyway)
                EvProc_Ds::new (EvProp_Continue, ComboProc_Disable)
            } else {
                // the rest we'll treat as physical events (even if its from other unrecognized injections)
                // .. and let combo mapping handle everything else
                EvProc_Ds::new (EvProp_Stop, ComboProc_Enable)
            }
        } ) ),
    } );
}


pub fn mouse_rbtn_release_masked () {
    mouse_action_masked (Arc::new ( || MouseButton::RightButton.release() ));
}
pub fn mouse_action_masked (af:AF) {
    // for cases we have to release the rbtn, but try to avoid triggering the context menu, we'll release it at corner of screen
    // .. it will still produce a context menu, but at least it is deterministic (cf clicking on random fgnd app)
    // .. also, in theory, could consider grabbing/restoring fgnd focus too .. but meh this is already almost too much

    //MouseButton::RightButton.release_at (0xFFFF, 0xFFFF);
    // ^^ ugh this doesnt seem to actually do that .. so we'll manually move there, release, then restore

    // and looks like for such a manual-move strategy to work, there HAS to be a delay before we move the pointer back
    let point = MousePointer::pos();
    thread::spawn ( move || {
        win_set_thread_dpi_aware();
        // move the pointer away from fgnd app
        MousePointer::move_abs (0xFFFF, 0xFFFF);
        // we'll add delay to let the pointer move to be processed
        thread::sleep(time::Duration::from_millis(20));
        af();
        // and more delay for any af() sent events to be processed while pointer is still away
        thread::sleep(time::Duration::from_millis(40));
        // restore pointer location
        MousePointer::move_abs (point.x, point.y);
    } );
}




/// sets up mouse wheel (vert-wheel or horiz-wheel as specified in params)
pub fn setup_mouse_wheel_handling (k:KR, whl: &'static MouseWheelState, ev_t:MouseWheelEv_T) {
    // we'll define a common binding AF for wheel types and direction, and let combo mapping add specific behavior
    k.iproc.input_bindings .bind_wheel_event (whl.wheel, ev_t, EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Undet, ComboProc_Undet),
        cb : EvCbFn_Inline ( Arc::new ( move |ev| {
            if let EventDat::wheel_event {delta, ..} = ev.dat {
                whl.last_delta.store (delta, Ordering::Relaxed);
            }
            let combo_proc_d = if check_wheel_spaced(whl, &ev) { ComboProc_Enable } else { ComboProc_Disable };
            // the rest of the behavior we'll let be defined via combo mapping
            EvProc_Ds::new (EvProp_Stop, combo_proc_d)
        } ) ),
    } );
}

fn check_wheel_spaced (whl:&MouseWheelState, ev:&Event) -> bool {
    // the invalidation setup below prevents things like caps down when wheel is still unintentionally inertially spinning to trigger zooms etc
    // however, we NO-LONGER space out the super-fast inertial smooth-scroll wheel (e.g. on my MX3 mouse) for improved usability
    // so here, we suppress wheel event if wheel-spin spacing is below guard-dur AND it has already been invalidated

    let last_stamp = whl.last_stamp.swap(ev.stamp);

    //println!("{:#?}", ev.stamp - last_stamp);
    // from ^^ these, looked like max inertial gap is 120 (min 7ms, usually <100)

    if !whl.spin_invalidated.is_set() {
        return true
    }
    const GUARD_DUR_MS: u32 = 120;
    if GUARD_DUR_MS < ev.stamp - last_stamp {
        whl.spin_invalidated.clear();
        return true
    }
    false
}




pub fn setup_mouse_move_handling (k:KR) {
    use crate::EventDat::*;
    k.iproc.input_bindings .bind_pointer_event ( EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Continue, ComboProc_Disable),
        cb: EvCbFn_Queued ( Arc::new ( move |ev| {
            if k.ks.mouse.lbtn.down.is_set() {
                if k.ks.mod_keys.lwin.down.is_set() || k.qbar.is_drag_active() {
                    k.ks.mod_keys.lwin.consumed.set();
                    if let pointer_event { xy } = ev.dat {
                        handle_lwin_mouse_drag (xy.x, xy.y, k.ks)
            } } }
        } ) ),
    } );
}

fn handle_lwin_mouse_drag (x:i32, y:i32, ks:KSR) {
    // NOTE that mouse move is inline pass-through before these handler calls get queued
    // .. so there could be some lag, but should be quick enough that we can work with ks states as we currently see it
    // NOTE also that we already set the thread dpi-aware when initing the events-queue thread itself
    if ks.mouse.lbtn.down.is_set() && ks.mouse.lbtn.consumed.is_clear() {
        if ks.mod_keys.caps.down.is_set() && ks.mode_states.some_qks_mode_active.is_clear() {
            handle_pointer_window_resize_spaced (x, y, ks)
        } else {
            handle_pointer_window_drag_spaced (x, y, ks)
        }
    }
}
