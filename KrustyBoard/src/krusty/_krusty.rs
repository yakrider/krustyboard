#![ allow (dead_code, non_snake_case) ]

use std::thread;
use std::time::{Instant, Duration};
use std::sync::{Arc, RwLock};
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};

use once_cell::sync::OnceCell;

use crate::*;
use crate::utils::Cursors;


// todo : just a reminder that we added some hacky meddling into keycodes and sending key events to get L/R scancodes out on alt/ctrl/shift



pub type Key = KbdKey ;




# [ derive (Debug, Default) ]
/// representation for all our atomic flags for states mod-states, modifier-keys, mouse-btn-state etc <br>
/// (Note that this uses Acquire/Release memory ordering semantics, and shouldnt be used as lock/mutex etc)
pub struct Flag (AtomicBool);
// ^^ simple sugar that helps reduce clutter in code

impl Flag {
    /* Note regarding Atomic Memory Ordering usage here ..
       - The Flag struct is intended for use as simple flags, not as synchronization primitives (i.e locks)
       - On x86, there is strong memory model and Acq/Rel is free .. so no benefit to using Relaxed
       - SeqCst however requires a memory fence that could be potentially be costly (flush writes before atomic op etc)
       - For the very rare cases that would require total global ordering with SeqCst, we should just use lib facilities instead!!
    */
    pub fn new (state:bool) -> Flag { Flag (AtomicBool::new(state)) }

    /// toggling returns prior state .. better to use this than to check and set
    pub fn toggle (&self) -> bool { self.0 .fetch_xor (true, Ordering::AcqRel) }

    /// swap stores new state and returns prior state .. better to use this than to update and check/load separately
    pub fn swap   (&self, state:bool) -> bool { self.0 .swap (state, Ordering::AcqRel) }

    pub fn set   (&self) { self.0 .store (true,  Ordering::Release) }
    pub fn clear (&self) { self.0 .store (false, Ordering::Release) }

    pub fn store  (&self, state:bool) { self.0.store (state, Ordering::Release) }

    pub fn is_set   (&self) -> bool {  self.0 .load (Ordering::Acquire) }
    pub fn is_clear (&self) -> bool { !self.0 .load (Ordering::Acquire) }



}




# [ derive (Debug) ]
pub struct TimeStamp (RwLock<Instant>);

// Note that we have moved everything to using the EventStamp instead so this is just vestigial now

impl TimeStamp {
    pub fn new() -> TimeStamp {
        TimeStamp (RwLock::new (Instant::now()))
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



# [ derive (Debug, Default) ]
pub struct EventStamp (AtomicU32);

impl EventStamp {
    pub fn new() -> EventStamp {
        EventStamp (AtomicU32::default())
    }

    pub fn get  (&self) -> u32 { self.0.load (Ordering::Relaxed) }
    pub fn set  (&self, stamp:u32) { self.0.store (stamp, Ordering::Relaxed) }
    pub fn swap (&self, stamp:u32) -> u32 { self.0.swap (stamp, Ordering::Relaxed) }
}



/// utility sugar to extract the dbl_tap from the right EventDat variant to update flag
pub fn update_dbl_tap (ev:&Event, dbl_flag:&Flag) -> bool {
    let is_dbl_tap = match ev.dat {
        EventDat::btn_event { is_dbl_tap, .. } => is_dbl_tap,
        EventDat::key_event { is_dbl_tap, .. } => is_dbl_tap,
        _ => false
    };
    dbl_flag.store (is_dbl_tap);
    is_dbl_tap
}


/// jiggle the cursor for a short duration as visual reminder of some state change etc
fn jiggle_cursor (n:isize) {
    thread::spawn ( move || {
        for _ in 0 .. n {
            MousePointer::move_rel(5,5);
            thread::sleep (Duration::from_millis(50));
            MousePointer::move_rel(-5,-5);
        }
    } );
}
/// flash the cursor with a transient color as visual indicator of some state change etc
fn flash_cursor (_n:isize) {
    // re _n .. currently we'll ignore the flash count .. oh well
    let ks = KrustyState::instance();
    let cursors = Cursors::instance();
    // the apply below is spawned, so we wont spawn here
    // .. and it will flash before it switches too
    if !ks.sticky_first_stroke.is_empty() {
        cursors.apply_sfsc()
    } else if !ks.latching_first_stroke.is_empty() {
        cursors.apply_lfsc()
    } else {
        cursors.apply_norm()
    }
}
/// apply transient visual indicator to the cursor (flash cursor if enabled, else jiggle it)
pub fn blip_cursor (n:isize) {
    let cursors = Cursors::instance();
    if cursors.is_enabled() {
        flash_cursor(n)
    } else {
        jiggle_cursor(n)
    }
}




# [ derive (Debug) ]    // note that we def dont want this clonable (we'd rather clone its Arc than all underlying!)
/// KrustyState holds all our direct state flags, or encapsulating state objects like mode-states or modifier-keys collections
pub struct KrustyState {
    // having this disallows direct instantiation
    _private: (),

    /// mod_keys obj manage the modifier-keys, their flags, and their action-wrapping
    pub mod_keys: &'static ModKeys,

    /// mode_states obj manage the flagged caps-mode states, their trigger keys etc
    pub mode_states: &'static ModeStates,

    /// mouse obj manages the mouse btns, wheels, wheel-spin invalidations etc
    pub mouse: &'static Mouse,

    /// win-groups obj maanges the qks[1-4] associated window-grouping functionalty
    pub win_groups: &'static WinGroups,

    /// flag marking right-mouse-btn-wheel scroll switche support <br>
    /// note that although we have that native in swi now, since we want to overload alt-wheel for brightness etc, we still want to track it
    pub in_right_btn_scroll_state: Flag,

    /// win_snap_dat snapshot holds data to support moving/dragging/resizing windows and window-groups
    pub win_snap_dat : RwLock <WinSnapDat>,

    /// the active first-stroke for modkey-sticky two-stroke-combos .. will clear when all modkeys are released
    pub sticky_first_stroke : ComboHashAtomic,

    /// the active first-stroke for latching two-stroke-combos .. will clear only when clear-latches is triggered
    pub latching_first_stroke : ComboHashAtomic,

}


// since we'll be passing 'static refs everywhere, we'll just alias it for ease
pub type KSR = &'static KrustyState;




/// Representation for all full state and data fro our Krusty-Board application
pub struct Krusty {
    // this is mostly just a utility wrapper sugar to pass things around

    // we'll have a _private guard to allow direct instantiation from outside
    _private : (),

    // KrustyState holds all state flags
    pub ks : &'static KrustyState,

    // we'll have a combos map to register all combos (key + modifiers + modes) to their mapped actions
    pub cm : &'static CombosMap,

    // we have the InputProcessor itself, which will hold the kbd/mouse bindings, combo-processing-af, the side-thread-queues
    pub iproc : &'static InputProcessor,

    // we'll also (optionally) listen to window-events like fgnd-win or fgnd-win-title change (to have fngd-win details pre-fetched)
    pub wel : &'static WinEventsListener,

}



/// impl for Krusty-State
impl KrustyState {

    /// Get a 'static ref of the sole instance of the global krusty-state
    pub fn instance () -> &'static KrustyState {
        static INSTANCE: OnceCell<KrustyState> = OnceCell::new();

        INSTANCE .get_or_init ( ||
            KrustyState {
                _private : (),

                mod_keys    : ModKeys::instance(),
                mode_states : ModeStates::instance(),
                mouse       : Mouse::instance(),
                win_groups  : WinGroups::instance(),

                in_right_btn_scroll_state  : Flag::default(),

                win_snap_dat : RwLock::new (WinSnapDat::default()),

                sticky_first_stroke   : ComboHashAtomic::default(),
                latching_first_stroke : ComboHashAtomic::default(),
            }
        )
    }


    pub fn proc_notice__modkey_down (&'static self, mk:ModKey) {
        if mk == ModKey::caps {
            self.mod_keys.proc_notice__caps_down(self)
        }
        self.mouse.proc_notice__modkey_down (mk,self);
    }
    pub fn proc_notice__modkey_up (&'static self, mk:ModKey) {
        if mk == ModKey::caps {
            self.mod_keys.proc_notice__caps_up(self)
        }
        self.mouse.proc_notice__modkey_up (mk, self);

        // only after the regular updates etc are finished, do we want to check for any fsc actions
        // (this lines up w how combos proc is done after bindings are executed, and ensures flags are updated)
        if !self.mod_keys.some_mk_down() { self.clear_cur_sticky_fsc() }
    }


    /// this is typically called upon caps-release, but users could manually call this for complex scenarios <br>
    /// (note that user-registered fsc-clearing AFs are bound separately to clearing event for this fsc)
    pub fn clear_cur_sticky_fsc (&'static self) {
        let sfsc = self.sticky_first_stroke.get();
        if !sfsc.is_empty() {
            //println! ("clearing sticky fsc: {:?}", sfsc);
            self.sticky_first_stroke.clear();

            if self.latching_first_stroke.is_empty() {
                Cursors::instance().apply_norm()
            } else { Cursors::instance().apply_lfsc() }

            // we'll inject an event to trigger user-registered clearing actions .. (which goes to queue)
            Self::inject_event_sticky_fsc_cleared(sfsc);
        }
    }
    /// this is called upon combo-af of explicit latch-clearing combo, but users could manually call this for complex scenarios <br>
    /// (note that user-registered fsc-clearing AFs are bound separately to clearing event for this fsc)
    pub fn clear_cur_latching_fsc (&'static self) {
        //println! ("clearing latching fsc: {:?}", last_lfsc);
        let last_lfsc = self.latching_first_stroke.get();
        if last_lfsc.is_empty() {
            blip_cursor(2);
        } else {
            self.latching_first_stroke.clear();

            if self.sticky_first_stroke.is_empty() {
                Cursors::instance().apply_norm()
            } else {
                Cursors::instance().apply_sfsc_w_norm_flash()
            }

            Self::inject_event_latching_fsc_cleared (last_lfsc);
        }
    }
    /// this action is typically auto registered for combos that trigger sticky fscs
    pub fn activate_sticky_fsc (&'static self, fsc:ComboHash) {
        // sticky-fscs are typically required to have some mod-key in them and are active until all modkeys are released
        // .. however, we no longer gate them by checking mk-down before activation to accommodate complex user-drive use-cases
        let last_sfsc = self.sticky_first_stroke.get();
        if fsc != last_sfsc {
            //println! ("activating sticky fsc: {:?}", fsc);
            self.sticky_first_stroke.store(fsc);
            Cursors::instance().apply_sfsc();
            if !last_sfsc.is_empty() {
                // we'll inject (into the queue) the last fsc cleared event so any user registered action can run
                Self::inject_event_sticky_fsc_cleared (last_sfsc)
            }
        }
    }
    /// this action is typically auto registered for combos that trigger latching fscs
    pub fn activate_latching_fsc (&'static self, fsc:ComboHash) {
        //println! ("activating latching fsc: {:?}", fsc);
        let last_lfsc = self.latching_first_stroke.get();
        if !last_lfsc.is_empty() {
            Self::inject_event_latching_fsc_cleared (last_lfsc)
        }
        self.latching_first_stroke.store(fsc);

        // we'll provide visual feedback via cursor colors/flashing
        if self.sticky_first_stroke.is_empty() {
            Cursors::instance().apply_lfsc();
        } else {
            Cursors::instance().apply_sfsc_w_lfsc_flash();
        }
    }


    pub fn inject_event_sticky_fsc_cleared (fsc:ComboHash) {
        //println! ("injecting sticky fsc cleared action for : {:?}", fsc);
        InputProcessor::instance() .inject_internal_event ( InternalEvent_T::Fsc_Sticky_Cleared { fsc } )
        // ^^ this will immediately call input-processor with this event .. which will lookup bindings for it ..
        // .. and if it has queued cb type bindings (as intended), those cbs will get sent to af-queue for in-order processing
    }
    pub fn inject_event_latching_fsc_cleared (fsc:ComboHash) {
        //println! ("injecting latching fsc cleared action for : {:?}", fsc);
        InputProcessor::instance() .inject_internal_event ( InternalEvent_T::Fsc_Latching_Cleared { fsc } )
    }



    pub fn capture_fgnd_win_snap_dat (&'static self) {
        //thread::spawn ( move || {     // .. nuh uh
        // ^^ spawning this not only is not necessary as metrics show its only couple ms max ..
        // .. but also often right after calling this we're doing other related work that expects this to be filled out!
        *self.win_snap_dat.write().unwrap() = capture_win_snap_dat (self, utils::win_get_fgnd(), None);
    }
    pub fn capture_pointer_win_snap_dat (&'static self, wgo:Option<WinGroups_E>) {
        // again, we'll not spawn this here, but those who can tolerate being spawned can call this on a spawned thread etc
        *self.win_snap_dat.write().unwrap() = capture_win_snap_dat (self, utils::win_get_hwnd_from_pointer(), wgo);
    }



    /// Goes through all keys and mouse-btns doing press/rel, and clears out all internal states
    pub fn unstick_all (&'static self) {
        println! ("WARNING: Attempting to UNSTICK_ALL !!");

        self.mode_states.clear_flags();
        self.mod_keys.unstick_all();
        self.mouse.clear_flags();

        self.in_right_btn_scroll_state.clear();

        self.sticky_first_stroke.clear();
        self.latching_first_stroke.clear();
        Cursors::instance().apply_norm();

        CapsModKey::clear_caps_lock_state();

        update_tray__krusty_suspend_state (false);    // is_suspended = false

        let mouse_masked_af = Arc::new ( || {
            use MouseButton::*;

            RightButton.press(); LeftButton.press();
            // ^^ w/o these presses, the esc wont get rid of context menu! .. presumably about where focus is
            RightButton.release(); LeftButton.release(); MiddleButton.release();
            X1Button.release(); X2Button.release();

            // send the Esc to get rid of win context menu
            thread::sleep (Duration::from_millis(50));
            Key::Escape.press_release();

            // now finally, reset hooks
            InputProcessor::instance().re_set_hooks();

            // and setup visual cue
            blip_cursor(3);
        } );
        mouse_action_masked (mouse_masked_af);
        // ^^ this moves pointer to 0xFF,0FF before attempting clicks (is spawned out, ~50ms)
        // .. it wont prevent a context menu from appearing, but at least in typical setups it will be in windows notif area
        // .. which is better than in the random fgnd app .. and afterwards, we can clear the win-notif area context menu w an Esc
        // .. and fgnd focus will be lost, but trying to capture/restore that would be even less worth the trouble

    }

    pub fn suspend_krusty (&'static self) {
        let iproc = InputProcessor::instance();
        if iproc.are_hooks_set() {
            iproc.stop_input_processing();
        }
        update_tray__krusty_suspend_state(true);
        Cursors::instance().apply_sys();
    }
    pub fn un_suspend_krusty (&'static self) {
        //InputProcessor::instance().begin_input_processing();
        //update_tray__krusty_suspend_state(false);
        self.unstick_all();
        // ^^ will also update tray-menu suspended state
    }
    pub fn check_krusty_suspended (&'static self) -> bool {
        !InputProcessor::instance().are_hooks_set()
    }

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

}





/// simple key-action utilities
pub mod key_utils {

    use std::sync::Arc;
    use std::thread;
    use std::time::Duration;
    use crate::*;


    pub fn wrapped_press_release (wrap_key:Key, key_action:fn(Key), key:Key) {
        wrap_key.press(); key_action(key); wrap_key.release();
    }
    pub fn wrapped_action (wrap_key:Key, af:AF) -> AF {
        Arc::new ( move || { wrap_key.press(); af(); wrap_key.release(); } )
    }

    // note that these are ONLY to be used when the mod key states DONT need to be tracked (e.g. in composition fallback actions)
    pub fn ctrl_press_release  (key:Key) { wrapped_press_release (Key::Ctrl,  Key::press_release, key) }
    pub fn shift_press_release (key:Key) { wrapped_press_release (Key::Shift, Key::press_release, key) }
    pub fn win_press_release   (key:Key) { wrapped_press_release (Key::LWin,  Key::press_release, key) }

    pub fn double_press_release (key:Key) { key.press_release(); key.press_release(); }


    // we'll define some arc-wrapper util fns, but really, its just as easy to just use arcs directly

    /// wraps a given unitary function closure with NO input args into an Arc Fn
    //pub fn action (f:fn()) -> AF { Arc::new (move || f()) }
    pub fn action<F> (f:F) -> AF
        where F: Fn() + Send + Sync + 'static
    { Arc::new(f) }

    /// wraps a given unitary function closure with ONE input arg into an Arc Fn
    //pub fn action_p1<T> (f:fn(T), t:T) -> AF where T: Copy + Send + Sync + 'static { Arc::new (move || f(t)) }
    pub fn action_p1<F,T> (f:F, t:T) -> AF
        where F: Fn(T) +  Send + Sync + 'static,
              T: Copy + Send + Sync + 'static
    { Arc::new ( move || f(t) ) }


    pub fn no_action      (       ) -> AF { Arc::new ( || {} ) }
    pub fn press_action   (key:Key) -> AF { action_p1 (Key::press,           key) }
    pub fn release_action (key:Key) -> AF { action_p1 (Key::release,         key) }
    pub fn base_action    (key:Key) -> AF { action_p1 (Key::press_release,   key) }

    pub fn fast_action    (key:Key) -> AF { action_p1 (double_press_release, key) }
    pub fn ctrl_action    (key:Key) -> AF { action_p1 (ctrl_press_release,   key) }
    pub fn shift_action   (key:Key) -> AF { action_p1 (shift_press_release,  key) }
    pub fn win_action     (key:Key) -> AF { action_p1 (win_press_release,    key) }


    /// wraps a given AF into an action that is spawned in its own thread
    pub fn spawned_action<F> (f:F) -> AF
        where F: Fn() + Send + Sync + 'static
    {
        let af = Arc::new(f);
        Arc::new ( move || {
            let af = af.clone();
            thread::spawn ( move || af() );
    } ) }

    /// wraps a given AF into an action that is spawned in its own thread and executed with the specified milliseconds delay
    pub fn delayed_action<F> (tms:u64, f:F) -> AF
        where F: Fn() + Send + Sync + 'static
    {
        let af = Arc::new(f);
        Arc::new ( move || {
            let af = af.clone();
            thread::spawn ( move || { thread::sleep(Duration::from_millis(tms)); af() } );
    } ) }
}


