#![ allow (dead_code) ]

use std::thread;
use std::time::{Instant, Duration};
use std::sync::{Arc, RwLock};
use std::sync::atomic::{AtomicBool, Ordering};
use derive_deref::Deref;

use once_cell::sync::OnceCell;
use windows::core::PCWSTR;
use windows::Win32::Foundation::HINSTANCE;
use windows::Win32::UI::WindowsAndMessaging::{
    CopyIcon, HCURSOR, HICON, IDC_ARROW, IDC_IBEAM, IDC_SIZEALL, IDC_SIZENWSE, IDC_WAIT,
    LoadCursorW, SetSystemCursor, SYSTEM_CURSOR_ID
};

use crate::*;


// todo : just a reminder that we added some hacky meddling into keycodes and sending key events to get L/R scancodes out on alt/ctrl/shift


// we'll define some easier type aliases (CallBack, ActionFn etc) to pass around triggered actions and so on
/// Arc/Action-Function Fn() representation that can be passed around between threads
pub type AF  = Arc <dyn Fn() + Send + Sync + 'static> ;

pub type Key = KbdKey ;




# [ derive (Debug, Default, Clone) ]
/// representation for all our flags for states mod-states, modifier-keys, mouse-btn-state etc
pub struct Flag (Arc<AtomicBool>);
// ^^ simple sugar that helps reduce clutter in code

impl Flag {
    pub fn new (state:bool) -> Flag { Flag ( Arc::new ( AtomicBool::new(state) ) ) }

    pub fn set    (&self) { self.0 .store (true,  Ordering::SeqCst) }
    pub fn clear  (&self) { self.0 .store (false, Ordering::SeqCst) }
    pub fn toggle (&self) { self.0 .store ( !self.0.load (Ordering::SeqCst), Ordering::SeqCst) }
    pub fn store  (&self, state:bool) { self.0 .store (state, Ordering::SeqCst) }

    pub fn is_set   (&self) -> bool { true  == self.0 .load (Ordering::SeqCst) }
    pub fn is_clear (&self) -> bool { false == self.0 .load (Ordering::SeqCst) }
}




# [ derive (Debug, Clone) ]
pub struct TimeStamp (Arc<RwLock<Instant>>);

impl TimeStamp {
    pub fn new() -> TimeStamp {
        TimeStamp ( Arc::new ( RwLock::new ( Instant::now() ) ) )
    }
    pub fn default() -> TimeStamp {
        TimeStamp::new()
    }
    pub fn capture (&self) -> Instant {
        let stamp = Instant::now();
        *self.0.write().unwrap() = stamp;
        stamp
    }
    pub fn get (&self) -> Instant {
        *self.0.read().unwrap()
    }
}



# [ derive (Debug, Clone) ]
pub struct EventStamp (Arc<RwLock<u32>>);

impl EventStamp {
    pub fn new() -> EventStamp {
        EventStamp ( Arc::new ( RwLock::new(0) ) )
    }
    pub fn default() -> EventStamp { EventStamp::new() }
    pub fn set (&self, stamp:u32) { *self.0.write().unwrap() = stamp }
    pub fn get (&self) -> u32 { *self.0.read().unwrap() }
}





pub const KEY_DOUBLE_TAP_MS  : u32 = 400;
pub const MBTN_DOUBLE_TAP_MS : u32 = 500;

pub fn update_stamp_key_dbl_tap (ev_t:u32, stamp:&EventStamp, dbl_flag:&Flag) -> bool {
    let is_double_tap = update_stamp_dbl_tap (ev_t, stamp, dbl_flag, KEY_DOUBLE_TAP_MS);
    if is_double_tap {
        jiggle_cursor();
        //let curs = Cursors::instance();
        //curs.hc_app_starting.toggle_cursor (OCR_NORMAL, &curs.hc_arrow);
        //curs.hc_app_starting.toggle_cursor (OCR_IBEAM,  &curs.hc_ibeam)
    }
    is_double_tap
}
pub fn update_stamp_mouse_dbl_click (ev_t:u32, stamp:&EventStamp, dbl_flag:&Flag) -> bool {
    update_stamp_dbl_tap (ev_t, stamp, dbl_flag, MBTN_DOUBLE_TAP_MS)
}
fn update_stamp_dbl_tap (ev_t:u32, stamp:&EventStamp, dbl_flag:&Flag, thresh_ms:u32) -> bool {
    let dt = ev_t - stamp.get();
    stamp.set(ev_t);
    let is_double_tap = dt < thresh_ms && dt > 50;  // we'll put a small mandatory gap for debounce
    dbl_flag .store (is_double_tap);
    is_double_tap
}
fn jiggle_cursor() {
    thread::spawn (|| {
        MousePointer::move_rel(5,5);
        thread::sleep(Duration::from_millis(100));
        MousePointer::move_rel(-5,-5);
    } );
}




# [ derive (Debug, Clone) ]
struct Cursor (Option<HICON>);
impl Cursor {
    unsafe fn load_copy_cursor (id:PCWSTR) -> Option<HICON> { LoadCursorW (HINSTANCE(0), id).ok() .map (|hc| CopyIcon(hc).ok()) .flatten() }
    fn new (id:PCWSTR) -> Cursor { unsafe { Cursor ( Cursor::load_copy_cursor(id) ) } }
    fn get (&self) -> Option<HICON> { self.0.clone() }

    pub fn swap_cursor ( &self, id:SYSTEM_CURSOR_ID ) { unsafe {
        self.0 .iter() .map (|&hc| CopyIcon(hc).ok()) .flatten() .for_each (|hc| {SetSystemCursor (HCURSOR(hc.0), id);});
    } }
    pub fn toggle_cursor ( &self,  id:SYSTEM_CURSOR_ID, cur_restore:&Cursor ) { unsafe {
        let cur = self.get(); let res = cur_restore.get();
        thread::spawn ( move || {
            cur .map (|hc| CopyIcon(hc).ok()) .flatten() .map (|hc| SetSystemCursor (HCURSOR(hc.0), id));
            thread::sleep(Duration::from_millis(300));
            res .map (|hc| CopyIcon(hc).ok()) .flatten() .map (|hc| SetSystemCursor (HCURSOR(hc.0), id));
        } );
    } }
}



# [ derive (Debug, Clone) ]
pub struct _Cursors {
    hc_arrow        : Cursor,
    hc_ibeam        : Cursor,
    hc_size_all     : Cursor,
    hc_size_nwse    : Cursor,
    hc_app_starting : Cursor,
}
# [ derive (Debug, Clone, Deref) ]
pub struct Cursors ( Arc <_Cursors> );


impl Cursors {
    pub fn instance() -> Cursors {
        static INSTANCE : OnceCell<Cursors> = OnceCell::new();
        INSTANCE .get_or_init ( ||
            Cursors ( Arc::new ( _Cursors {
                hc_arrow        : Cursor::new (IDC_ARROW),
                hc_ibeam        : Cursor::new (IDC_IBEAM),
                hc_size_all     : Cursor::new (IDC_SIZEALL),
                hc_size_nwse    : Cursor::new (IDC_SIZENWSE),
                hc_app_starting : Cursor::new (IDC_WAIT),
            } ) )
        ) .clone()
    }
}









# [ derive (Debug) ]    // note that we def dont want this clonable (we'd rather clone its Arc than all underlying!)
/// KrustyState holds all our direct state flags, or encapsulating state objects like mode-states or modifier-keys collections
pub struct _KrustyState {
    // having this disallows direct instantiation
    _private: (),

    // used for toggling key processing .. should only listen to turn-back-on combo
    pub in_disabled_state: Flag,

    // mod-keys .. manages the modifier-keys, their flags, and their action-wrapping
    pub mod_keys: ModKeys,

    // mode states .. manages the flagged caps-mode states, their trigger keys etc
    pub mode_states: ModeStates,

    // mouse .. manages the mouse btns, wheels, wheel-spin invalidations etc
    pub mouse: Mouse,

    // win-groups .. maanges the three supported window-grouping functionalty
    pub win_groups: WinGroups,

    // for caps-ctrl eqv for caps-tab, caps-wheel, ctrl-move etc, we'll send ctrl press/rel at the right times, and will need to track that
    pub in_managed_ctrl_down_state: Flag,
    // and since wheel support during ctrl-tab is missing in many applications incl IDEs, we'll impl that ourselves
    pub in_ctrl_tab_scroll_state: Flag,

    // and for right-mouse-btn-wheel switche support, we'll track that state too (and send switche specific keys)
    // note that although we have that native in swi now, since we want to overload alt-wheel for brightness etc, we still want to track it
    pub in_right_btn_scroll_state: Flag,

}


# [ derive (Debug, Clone, Deref) ]
/// Arc wrapped KrustyState for cheap cloning/sharing
pub struct KrustyState ( Arc <_KrustyState> );
// ^^ we'll use this wrapped type so cloning and passing around is cheap





/// Representation for all full state and data fro our Krusty-Board application
pub struct Krusty {
    // this is mostly just a utility wrapper sugar to pass things around
    _private : (),   // prevents direct instantiation of this struct
    // ks is Arc<KrustyState>, holds all state flags
    pub ks : KrustyState,
    // we'll have a combos map to register all combos (key + modifiers + modes) to their mapped actions
    pub cm : CombosMap,
    // we have the InputProcessor itself, which will hold the kbd/mouse bindings, combo-processing-af, the side-thread-queues
    pub iproc : InputProcessor,
    // we'll also (optionally) listen to window-events like fgnd-win or fgnd-win-title change (to have fngd-win details pre-fetched)
    pub wel : WinEventsListener,
}



/// impl for Krusty-State
impl KrustyState {

    pub fn instance () -> KrustyState {
        static INSTANCE: OnceCell<KrustyState> = OnceCell::new();
        INSTANCE .get_or_init ( ||
            KrustyState ( Arc::new ( _KrustyState {
                _private : (),
                in_disabled_state : Flag::default(),

                mod_keys    : ModKeys::new(),
                mode_states : ModeStates::new(),
                mouse       : Mouse::new(),
                win_groups  : WinGroups::new(),

                in_managed_ctrl_down_state : Flag::default(),
                in_ctrl_tab_scroll_state   : Flag::default(),
                in_right_btn_scroll_state  : Flag::default(),
            } ) )
        ) .clone()
    }

    pub fn unstick_all (&self) {
        println! ("WARNING: Attempting to UNSTICK_ALL !!");
        self.mode_states.clear_flags();
        self.mod_keys.unstick_all();

        use MouseButton::*;
        LeftButton.release(); RightButton.release(); MiddleButton.release();
        X1Button.release(); X2Button.release();

        [  &self.mouse.lbtn.down, &self.mouse.rbtn.down, &self.mouse.mbtn.down,
           &self.in_managed_ctrl_down_state, &self.in_ctrl_tab_scroll_state, &self.in_right_btn_scroll_state,
        ] .into_iter() .for_each (|flag| flag.clear());

        // lets send a delayed Esc for any context menus etc that show up
        thread::spawn (|| { thread::sleep (Duration::from_millis(300)); key_utils::press_release(Key::Escape) } );
    }

    /// Utlity function to create a new Combo-generator (for combo-specification) <br>
    /// By default, it sets the modifier-keys to have mask-release (consumed), and mod-keys to have repeats suppressed (consumed)
    pub fn cg (&self, key:Key) -> ComboGen { ComboGen::new (key, &self) }

    /// Utility function to create a new Combo-Action generator (key-action output type) <br>
    /// By default, it WILL wrap the AF with modifier key guard actions, can be set to not do so w .mkg_nw()
    pub fn ag (&self, key:Key) -> ActionGen_wKey { ActionGen_wKey::new (key, &self) }

    /// Utlity function to create a new Combo-Action-generator (non-key action-function output type). <br>
    /// By default, it WILL NOT wrap the AF with modifier key guard actions, can be set to do so w .mkg_w()
    pub fn ag_af (&self, af:AF) -> ActionGen_wAF { ActionGen_wAF::new (af, &self) }

}





/// impl for Krusty data and state struct
impl Krusty {

    /// create a new Krusty object (holds the KrustyState, the combos-map, and a registry of keys to do default-bindings on)
    pub fn new() -> Krusty {
        Krusty {
            _private : (),
            ks    : KrustyState::instance(),
            cm    : CombosMap::instance(),
            iproc : InputProcessor::instance(),
            wel   : WinEventsListener::instance(),
        }
    }


    #[allow(dead_code)]
    /// utility fn to globally disable krusty functionality
    pub fn setup_global_disable (&self) {
        // todo: might not be straight-forward if we want to handle this at hook receipt
        // .. will need a global check there, as well as selective listening for the re-enable combo when disabled
    }

}





/// simple key-action utilities
pub mod key_utils {

    use std::sync::Arc;
    use std::thread;
    use std::time::Duration;
    use crate::*;

    pub fn press                (key:Key) { key.press(); }
    pub fn release              (key:Key) { key.release(); }
    pub fn press_release        (key:Key) { key.press(); key.release(); }
    pub fn double_press_release (key:Key) { press_release(key); press_release(key); }

    pub fn wrapped_press_release (wrap_key:Key, key_action:fn(Key), key:Key) {
        wrap_key.press(); key_action(key); wrap_key.release();
    }
    pub fn wrapped_action (wrap_key:Key, af:AF) -> AF {
        Arc::new ( move || { wrap_key.press(); af(); wrap_key.release(); } )
    }

    // note that these are ONLY to be used when the mod key states DONT need to be tracked (e.g. in composition fallback actions)
    pub fn ctrl_press_release  (key:Key) { wrapped_press_release (Key::Ctrl,  press_release, key) }
    pub fn shift_press_release (key:Key) { wrapped_press_release (Key::Shift, press_release, key) }
    pub fn win_press_release   (key:Key) { wrapped_press_release (Key::LWin,  press_release, key) }


    // we'll define some arc-wrapper util fns, but really, its just as easy to just use arcs directly

    /// wraps a given unitary function closure with NO input args into an Arc Fn
    //pub fn action (f:fn()) -> AF { Arc::new (move || f()) }
    pub fn action<F> (f:F) -> AF
        where F: Fn() + Send + Sync + 'static
    { Arc::new (move || f()) }

    /// wraps a given unitary function closure with ONE input arg into an Arc Fn
    //pub fn action_p1<T> (f:fn(T), t:T) -> AF where T: Copy + Send + Sync + 'static { Arc::new (move || f(t)) }
    pub fn action_p1<F,T> (f:F, t:T) -> AF
        where F: Fn(T) +  Send + Sync + 'static,
              T: Copy + Send + Sync + 'static
    { Arc::new ( move || f(t) ) }


    pub fn no_action           () -> AF { Arc::new ( || {} ) }
    pub fn base_action  (key:Key) -> AF { action_p1 (press_release,        key) }
    pub fn fast_action  (key:Key) -> AF { action_p1 (double_press_release, key) }
    pub fn ctrl_action  (key:Key) -> AF { action_p1 (ctrl_press_release,   key) }
    pub fn shift_action (key:Key) -> AF { action_p1 (shift_press_release,  key) }
    pub fn win_action   (key:Key) -> AF { action_p1 (win_press_release,    key) }


    /// wraps a given AF into an action that is spawned in its own thread
    pub fn spawned_action (af:AF) -> AF {
        Arc::new ( move || {
            let af = af.clone();
            thread::spawn ( move || af() );
    } ) }

    /// wraps a given AF into an action that is spawned in its own thread and executed with the specified milliseconds delay
    pub fn delayed_action (tms:u64, af:AF) -> AF {
        Arc::new ( move || {
            let af = af.clone();
            thread::spawn ( move || { thread::sleep(Duration::from_millis(tms)); af() } );
    } ) }
}


