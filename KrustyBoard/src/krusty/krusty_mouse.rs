#![ allow (non_snake_case) ]

use std::{
    thread, time,
    sync::Arc,
    sync::atomic::{AtomicI32, Ordering},
};
use derive_deref::Deref;

use crate::{
    *, utils::*,
    EvProp_D::*, ComboProc_D::*, EvCbFn_T::*
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
    pub hwheel : MouseWheelState,

    //pub pointer : MousePointer,
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
            hwheel : MouseWheelState::new(HorizontalWheel),
            //pointer: MousePointerState::default(),
        }
    }

    pub fn setup_mouse (&self, k:&Krusty) {

        // for most mouse btn actions, we can setup standard skeleton bindings, and let actual 'business-logic' be setup via combo bindings

        setup_standard_mbtn_press_handling   (k.ks.mouse.lbtn.clone(), k);
        setup_standard_mbtn_release_handling (k.ks.mouse.lbtn.clone(), k);

        setup_standard_mbtn_press_handling   (k.ks.mouse.mbtn.clone(), k);
        setup_standard_mbtn_release_handling (k.ks.mouse.mbtn.clone(), k);

        setup_standard_mbtn_press_handling   (k.ks.mouse.x1btn.clone(), k);
        setup_standard_mbtn_release_handling (k.ks.mouse.x1btn.clone(), k);

        setup_standard_mbtn_press_handling   (k.ks.mouse.x2btn.clone(), k);
        setup_standard_mbtn_release_handling (k.ks.mouse.x2btn.clone(), k);

        setup_standard_mbtn_press_handling   (k.ks.mouse.rbtn.clone(), k);
        //setup_standard_mbtn_release_handling (k.ks.mouse.rbtn.clone(), k);
        setup_mouse_right_btn_release_handling (k);
        // ^^ for the mouse right-btn, we have to make small special case for switche-injected events, so we do it separately


        // for wheels, we set up uniform binding for all wheels/directions, and let combo mapping add specific behavior
        use MouseWheelEv_T::*;
        setup_mouse_wheel_handling (k, k.ks.mouse.vwheel.clone(), WheelForwards );
        setup_mouse_wheel_handling (k, k.ks.mouse.vwheel.clone(), WheelBackwards);

        setup_mouse_wheel_handling (k, k.ks.mouse.hwheel.clone(), WheelForwards );
        setup_mouse_wheel_handling (k, k.ks.mouse.hwheel.clone(), WheelBackwards);


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
    pub fn get_wheel_state (&self, wheel:MouseWheel) -> Option<MouseWheelState> {
        match wheel {
            MouseWheel::DefaultWheel    => Some (self.vwheel.clone()),
            MouseWheel::HorizontalWheel => Some (self.hwheel.clone()),
            _ => None
        }
    }

    // mod-keys notify here in case we need to do some cleanup/flagging etc
    pub fn proc_notice__modkey_down (&self, mk:ModKey, ks:&KrustyState) {
        use ModKey::*;
        self.vwheel.spin_invalidated.set();
        if ks.mouse.lbtn.down.is_set() && ( mk == caps ||  mk == lwin) {
            // we'll want to capture/refresh pre-drag-dat on caps/win presses w lbtn down as they both modify drag/resize origin behavior
            ks.capture_pointer_win_snap_dat();
        }
    }
    pub fn proc_notice__modkey_up (&self, mk:ModKey, ks:&KrustyState) {
        use ModKey::*;
        self.vwheel.spin_invalidated.set();
        if mk == caps  && ks.mod_keys.lwin.down.is_set() && ks.mouse.lbtn.down.is_set() {
            // if we're exiting drag-resize into drag-move, so we should refresh our pre-drag dat reference
            ks.capture_pointer_win_snap_dat();
        }
    }

}





/// setup standard mouse btn PRESS handling expecting the actual 'business-logic' to be setup via combo mappings
pub fn setup_standard_mbtn_press_handling (mbs:MouseBtnState, k:&Krusty) {
    use crate::MouseBtnEv_T::*;
    k.iproc.input_bindings .bind_btn_event (mbs.btn, BtnDown, EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Undet, ComboProc_Undet),
        cb : EvCbFn_Inline ( Arc::new ( move |ev| {
            mbs.down.set();
            update_stamp_mouse_dbl_click (ev.stamp, &mbs.stamp, &mbs.dbl_tap);
            // the rest of the behavior we'll let be defined via combo mapping
            EvProc_Ds::new (EvProp_Stop, ComboProc_Enable)
        } ) ),
    } );
}

/// setup standard mouse btn RELEASE handling expecting the actual 'business-logic' to be setup via combo mappings
pub fn setup_standard_mbtn_release_handling (mbs:MouseBtnState, k:&Krusty) {
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
pub fn setup_mouse_right_btn_release_handling (k:&Krusty) {
    /* NOTE: this has a bit of special consideration, as mostly we'd just use queued callbacks or light inline wrapper then combo-proc ..
        .. but rbtn up has the peculiar situation where to support switche, when swi injects an rbtn-up to start rbtn-scroll mode,
        .. we cant just reinject the rbtn-up in combo handling, as switche would reprocess it in a feedback loop (for lack of have switche extra-info)
        .. So instead, we check for that in the inline binding handler itself so we can let it through in that special case
    */
    use crate::{MouseButton::*, MouseBtnEv_T::*};
    let ks = k.ks.clone();
    k.iproc.input_bindings .bind_btn_event (RightButton, BtnUp, EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Undet, ComboProc_Undet),
        //cb : EvCbFn_Inline ( Arc::new ( move |ev| handle_mouse_right_btn_up (&ks, ev) ) )
        cb : EvCbFn_Inline ( Arc::new ( move |ev| {
            ks.mouse.rbtn.down.clear(); ks.mouse.rbtn.dbl_tap.clear();
            if ev.extra_info == SWITCHE_INJECTED_IDENTIFIER_EXTRA_INFO {
                ks.in_right_btn_scroll_state.set(); ks.mouse.rbtn.active.clear();
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
    // for cases we have to release the rbtn, but try to avoid triggering the context menu, we'll release it at corner of screen
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




/// sets up mouse wheel (vert-wheel or horiz-wheel as specified in params)
pub fn setup_mouse_wheel_handling (k:&Krusty, whl:MouseWheelState, ev_t:MouseWheelEv_T) {
    // we'll define a common binding AF for wheel types and direction, and let combo mapping add specific behavior
    k.iproc.input_bindings .bind_wheel_event (whl.wheel, ev_t, EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Undet, ComboProc_Undet),
        cb : EvCbFn_Inline ( Arc::new ( move |ev| {
            if let EventDat::wheel_event {delta, ..} = ev.dat {
                whl.last_delta.store (delta, Ordering::Relaxed);
            }
            let combo_proc_d = if check_wheel_spaced(&whl) { ComboProc_Enable } else { ComboProc_Disable };
            // the rest of the behavior we'll let be defined via combo mapping
            EvProc_Ds::new (EvProp_Stop, combo_proc_d)
        } ) ),
    } );
}

fn check_wheel_spaced (whl:&MouseWheelState) -> bool {
    // the invalidation setup below prevents things like caps down when wheel is still unintentionally inertially spinning to trigger zooms etc
    // however, we NO-LONGER space out the super-fast inertial smooth-scroll wheel (e.g. on my MX3 mouse) for improved usability
    // so here, we suppress wheel event if wheel-spin spacing is below guard-dur AND it has already been invalidated
    let last_stamp = whl.last_stamp.get();
    whl.last_stamp.capture();
    //let gap = whl.last_stamp.read().unwrap().duration_since(last_stamp);
    //println!("{:#?}", dur.as_millis());
    const GUARD_DUR_MS: u128 = 120;  // from dur printouts above, looked like max inertial gap is 120 (min 7ms, usually <100)
    if !whl.spin_invalidated.is_set() {
        return true
    }
    if GUARD_DUR_MS < whl.last_stamp.get().duration_since(last_stamp).as_millis() {
        whl.spin_invalidated.clear();
        return true
    }
    false
}




pub fn setup_mouse_move_handling (k:&Krusty) {
    use crate::EventDat::*;
    let ks = k.ks.clone();
    k.iproc.input_bindings .bind_pointer_event ( EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_Continue, ComboProc_Disable),
        cb: EvCbFn_Queued ( Arc::new ( move |ev| {
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
