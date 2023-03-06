mod combo_maps;
mod mod_keys;

use std::{time, time::Instant};
use std::thread::{sleep, spawn};
use std::collections::{HashSet};
use std::ops::Deref;
use std::sync::{Arc, RwLock};
use std::sync::atomic::{AtomicBool, Ordering};

//use derivative::Derivative;

//use closure::closure;
// ^^ gives a nice macro that eliminates clutter from having to clone everywhere
// .. we'd use it a lot more, except IDE syntax highlighting doesnt work inside the macro .. which is waay too big a loss for the benefit!

use crate::{KbdEvent, KbdKey, MouseButton, MouseWheel, utils};

use crate::krusty::{mod_keys::*, combo_maps::*};


// todo : just a reminder that we added some hacky meddling into keycodes and sending key events to get L/R scancodes out on alt/ctrl/shift


// we'll define some easier type aliases (CallBack, ActionFn etc) to pass around triggered actions and so on
/// Arc/Action-Function Fn() representation that can be passed around between threads
type AF  = Arc <dyn Fn() + Send + Sync + 'static> ;

type Key = KbdKey ;





# [ derive (Debug, Default, Clone) ]
/// representation for all our flags for states mod-states, modifier-keys, mouse-btn-state etc
pub struct Flag (Arc<AtomicBool>);
// ^^ simple sugar that helps reduce clutter in code

impl Flag {
    pub fn check (&self) -> bool { self.0 .load (Ordering::SeqCst) }
    pub fn set   (&self) { self.0 .store (true,  Ordering::SeqCst) }
    pub fn clear (&self) { self.0 .store (false, Ordering::SeqCst) }
}





# [ derive (Debug) ]    // note that we def dont want this clonable (we'd rather clone its Arc than all underlying!)
/// KrustyState holds all our direct state flags, or encapsulating state objects like mode-states or modifier-keys collections
pub struct KrustyState {
    // having this disallows direct instantiation
    _private : (),

    // used for toggling key processing .. should only listen to turn-back-on combo
    pub in_disabled_state: Flag,

    // mod-keys .. details in its declaration
    pub mod_keys : MKS,

    // mode states .. details in its declaration
    pub mode_states : MSS,

    // mouse btn states .. need to impl caps-as-ctrl, or right-mouse-scroll behavior etc
    pub mouse_left_btn_down:  Flag,
    pub mouse_right_btn_down: Flag,

    // for caps-ctrl eqv for caps-tab or caps-wheel, we'll send ctrl press/release at the right times, and will need to track that
    pub in_managed_ctrl_down_state: Flag,
    // and for right-mouse-btn-wheel switche support, we'll track that state too (and send switche specific keys)
    pub in_right_btn_scroll_state: Flag,

    pub last_wheel_stamp: RwLock<Instant>,
    pub is_wheel_spin_invalidated: Flag, // invalidated by e.g. mid-spin mod press, or spin stop (spacing > 120ms)
}


# [ derive (Debug, Clone) ]
/// Arc wrapped KrustyState for cheap cloning/sharing
pub struct KrS (Arc <KrustyState>);
// ^^ we'll use this wrapped type so cloning and passing around is cheap

impl Deref for KrS {
    type Target = KrustyState;
    fn deref(&self) -> &KrustyState { &self.0 }
}





/// Representation for all full state and data fro our Krusty-Board application
pub struct Krusty {
    // this is mostly just a utility wrapper sugar to pass things around
    _private : (),   // prevents direct instantiation of this struct
    // KrS is Arc<KrustyState>, holds all state flags
    pub ks: KrS,
    // we'll have a combos map to register all combos (key + modifiers + modes) to their mapped actions
    pub cm: CombosMap,
    // instead of polluting combos_map, we'll hold a registry for keys to gen default bindings for
    pub default_bind_keys : Arc <RwLock <HashSet <Key>>>,
}





/// impl for Krusty-State
impl KrS {

    pub fn new () -> KrS {
        KrS ( Arc::new ( KrustyState {
            _private : (),
            in_disabled_state : Flag::default(),

            mod_keys    : MKS::new(),
            mode_states : MSS::new(),

            mouse_left_btn_down  : Flag::default(),
            mouse_right_btn_down : Flag::default(),

            in_managed_ctrl_down_state : Flag::default(),
            in_right_btn_scroll_state  : Flag::default(),

            last_wheel_stamp : RwLock::new(Instant::now()),
            is_wheel_spin_invalidated : Flag::default(),
        } ) )
    }

    fn unstick_all (&self) {
        self.mode_states.clear_flags();
        self.mod_keys.unstick_all();

        use MouseButton::*;
        LeftButton.release(); RightButton.release(); MiddleButton.release();
        X1Button.release(); X2Button.release();

        [   &self.mouse_left_btn_down, &self.mouse_right_btn_down,
            &self.in_right_btn_scroll_state, &self.in_managed_ctrl_down_state
        ] .into_iter() .for_each (|flag| flag.clear());
    }

}





/// impl for Krusty data and state struct
impl Krusty {

    // NOTE: we've decided to mostly impl things grouped by related functionality in disparate modules rather than under impl here
    // .. mostly because doing that allows easier piecemeal imports, as well as better pub/private management

    pub fn new() -> Krusty {

        let ks = KrS::new();

        // and we'll pair up the left/right values where applicable
        ks.mod_keys.lctrl  .link_pair (&ks.mod_keys.rctrl);  ks.mod_keys.rctrl  .link_pair (&ks.mod_keys.lctrl);
        ks.mod_keys.lshift .link_pair (&ks.mod_keys.rshift); ks.mod_keys.rshift .link_pair (&ks.mod_keys.lshift);

        // now we can construct the Krusty struct we want
        Krusty {
            _private : (),
            ks,
            cm : CombosMap::new(),
            default_bind_keys : Arc::new(RwLock::new (HashSet::new())),
        }
    }

    pub fn cg    (&self, key:Key) -> ComboGen_wKey { ComboGen_wKey::new (key, &self.ks) }
    pub fn cg_af (&self, af:AF)   -> ComboGen_wAF  { ComboGen_wAF::new  (af,  &self.ks) }

    #[allow(dead_code)]
    pub fn setup_global_disable (&self) {
        // todo: wont be straight-forward if we want to handle this at hook receipt
        // .. will need a global check there, as well as allowing selectively for the re-enable combo!
    }

}





/// simple key-action utilities
pub mod key_utils {

    use crate::krusty::*;

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

    // we'll define some arc-wrapper util fns, but really, its just as easy to just use arcs directly
    /// wraps a given unitary function with NO input args into an Arc Fn
    pub fn action (f:fn()) -> AF { Arc::new (move || f()) }

    /// wraps a given unitary function with ONE input arg into an Arc Fn
    pub fn action_p1<T> (f:fn(T), t:T) -> AF where T: Copy + Send + Sync + 'static { Arc::new (move || f(t)) }

    pub fn no_action           () -> AF { Arc::new ( || {} ) }
    pub fn base_action  (key:Key) -> AF { action_p1 (press_release,        key) }
    pub fn fast_action  (key:Key) -> AF { action_p1 (double_press_release, key) }
    pub fn ctrl_action  (key:Key) -> AF { action_p1 (ctrl_press_release,   key) }

}





/// setups for binding mouse btns and handling their events
pub mod mouse_btn_setups {

    use crate::krusty::{*, key_utils::*};
    use crate::{MouseEventCallbackEntry, EventPropagationDirective::*, MouseEventCallbackFnType::*, MouseEventCbMapKeyAction::*};

    // tracking left btn down state is required for ctrl-wheel (zoom) etc
    fn handle_mouse_left_btn_down (ks:&KrS) {
        ks.mouse_left_btn_down.set();
        if ks.mod_keys.caps.down.check() {
            ks.in_managed_ctrl_down_state.set();
            ks.mod_keys.lctrl.ensure_active();   // this allows caps-as-ctrl for drag drop etc
        }
        MouseButton::LeftButton.press()
    }
    fn handle_mouse_left_btn_up (ks:&KrS) {
        ks.mouse_left_btn_down.clear();
        MouseButton::LeftButton.release();
    }
    pub fn setup_mouse_left_btn_handling (k:&Krusty) {
        let ks = k.ks.clone();
        MouseButton::LeftButton .bind ( BtnDownCb, MouseEventCallbackEntry {
            event_prop_directive: EventProp_Stop,
            cb : MouseEvCbFn_SpawnedCallback { cb : Arc::new ( move |_| handle_mouse_left_btn_down(&ks) ) }
        } );
        let ks = k.ks.clone();
        MouseButton::LeftButton .bind ( BtnUpCb, MouseEventCallbackEntry {
            event_prop_directive: EventProp_Stop,
            cb : MouseEvCbFn_SpawnedCallback { cb : Arc::new ( move |_| handle_mouse_left_btn_up(&ks) ) }
        } );
        // ^^ in theory could do non-blocking bind for release, but that can occasionally expose us to timing issues as press is on block-bind
        //      and that will be slower to send the actual press from the spawned thread (so making release slower too helps)
    }



    // tracking right-btn-down-state is required for switche scroll (via F16/F17/Ctrl-F18)
    // note that unlike other mouse btn/wheels, the right btn is set to passthrough!
    fn handle_mouse_right_btn_down (ks:&KrS) {
        ks.mouse_right_btn_down.set();
    }
    fn handle_mouse_right_btn_up (ks:&KrS) {
        ks.mouse_right_btn_down.clear();
        if ks.in_right_btn_scroll_state.check() {
            ks.in_right_btn_scroll_state.clear();
            //k.ks.lalt.active_action(base_action(Key::F18))();
            //k.ks.lctrl.active_action(base_action(Key::F18))();
            //base_action(Key::F18)();
            ctrl_press_release(Key::F18);
        }
    }
    pub fn setup_mouse_right_btn_handling (k:&Krusty) {
        let ks = k.ks.clone();
        MouseButton::RightButton .bind ( BtnDownCb, MouseEventCallbackEntry {
            event_prop_directive: EventProp_Continue,
            cb : MouseEvCbFn_SpawnedCallback { cb : Arc::new ( move |_| handle_mouse_right_btn_down(&ks) ) }
        } );
        let ks = k.ks.clone();
        MouseButton::RightButton .bind ( BtnUpCb, MouseEventCallbackEntry {
            event_prop_directive: EventProp_Continue,
            cb : MouseEvCbFn_SpawnedCallback { cb : Arc::new ( move |_| handle_mouse_right_btn_up(&ks) ) }
        } );
    }


    // for x1/x2 btns, we want them to behave like they were middle btn too
    // note: it turns out just doing press/release on initial press works snappier/more-reliable than default btn-holds
    pub fn setup_mouse_x_btn_1_handling () {
        MouseButton::X1Button .bind ( BtnDownCb, MouseEventCallbackEntry {
            event_prop_directive: EventProp_Stop,
            cb : MouseEvCbFn_SpawnedCallback { cb : Arc::new ( move |_| {
                MouseButton::MiddleButton.press(); MouseButton::MiddleButton.release();
            } ) },
        } );
        MouseButton::X1Button .bind ( BtnUpCb, MouseEventCallbackEntry {
            event_prop_directive: EventProp_Stop,
            cb : MouseEvCbFn_InlineCallback { cb : Arc::new ( move |_| EventProp_Stop) }
        } );
    }

    pub fn setup_mouse_x_btn_2_handling () {
        MouseButton::X2Button .bind ( BtnDownCb, MouseEventCallbackEntry {
            event_prop_directive: EventProp_Stop,
            cb : MouseEvCbFn_SpawnedCallback { cb : Arc::new ( move |_| {
                MouseButton::MiddleButton.press(); MouseButton::MiddleButton.release();
            } ) },
        } );
        MouseButton::X2Button .bind ( BtnUpCb, MouseEventCallbackEntry {
            event_prop_directive: EventProp_Stop,
            cb : MouseEvCbFn_InlineCallback { cb : Arc::new ( move |_| EventProp_Stop) }
        } );
    }

}





/// setups for mouse wheel bindings and handling their events
pub mod mouse_wheel_setups {

    use crate::krusty::{*, key_utils::*};
    use crate::{MouseEventCallbackEntry, EventPropagationDirective::*, MouseEventCallbackFnType::*, MouseEventCbMapKeyAction::*};
    use windows::Win32::UI::WindowsAndMessaging::WHEEL_DELTA;
    use crate::utils::window_utils::get_fgnd_win_class;

    pub fn handle_wheel_guarded (delta:i32, ksr:&KrS) {
        // this is mostly to make the super-fast inertial smooth-scroll wheel on my (MX3) mouse a lil more usable by spacing things out
        // also, the invalidation setup prevents things like caps down when wheel is still unintentionally inertially spinning to trigger zooms etc
        let last_stamp = *ksr.last_wheel_stamp.read().unwrap();
        *ksr.last_wheel_stamp.write().unwrap() = Instant::now();
        //let gap = ksr.last_wheel_stamp.read().unwrap().duration_since(last_stamp);
        //println!("{:#?}", dur.as_millis());
        const GUARD_DUR_MS: u128 = 120;  // from dur printouts above, looked like max inertial gap is 120 (min 7ms, usually <100)
        if !ksr.is_wheel_spin_invalidated.check() {
            handle_wheel_action(delta, ksr);
        } else if GUARD_DUR_MS < ksr.last_wheel_stamp.read().unwrap().duration_since(last_stamp).as_millis() {
            ksr.is_wheel_spin_invalidated.clear();
            handle_wheel_action(delta, ksr);
        } else {
            // if its invalidated AND wheel-spin spacing is below guard-dur, we suppress the wheel event
        }
    }

    pub fn handle_wheel_action (delta:i32, ksr:&KrS) {
        let incr = delta / WHEEL_DELTA as i32;
        if ksr.mouse_right_btn_down.check() {
            // right-mouse-btn-wheel support for switche task switching
            ksr.in_right_btn_scroll_state.set();
            let key = if incr.is_positive() { Key::F17 } else { Key::F16 };
            //ksr.mod_keys.lalt.active_action(base_action(key))();       // not usable due to masking keys (so changed in hotkey switche)
            //ksr.mod_keys.lctrl.active_action(base_action(key))();      // not ideal as even non-masked wrapping causes slower/choppy switche scroll
            press_release(key);                                 // we'd rather use direct press hotkeys for best perf
        } else  if ksr.mod_keys.lalt.down.check() {
            // wheel support for scrolling in windows native alt-tab task-switching screen
            // this requires a system call to check alt-tab window, so push it out to thread?
            // .. naah, we're always spawned out from hook thread (for blocking binds), so theres no point
            ksr.mod_keys.lalt.consumed.set();
            if get_fgnd_win_class() == "MultitaskingViewFrame" { // alt-tab states
                ksr.mod_keys.lalt.ensure_active();    // we're already down but just in case its down/inactive
                handle_alt_tab_wheel(incr)
            } else {
                // alt-wheel for (fine delta) control, caps-alt-wheel for larger adjustments
                let mult = if ksr.mod_keys.caps.down.check() || ksr.mod_keys.some_shift_down() || ksr.mod_keys.lwin.down.check() {5} else {1};
                incr_brightness (incr * mult)
            }
        } else if ksr.mod_keys.lwin.down.check() {
            // win-wheel for (fine delta) control, caps-win-wheel for larger adjustments
            ksr.mod_keys.lwin.consumed.set();
            let mult = if ksr.mod_keys.caps.down.check() || ksr.mod_keys.some_shift_down() {2} else {1};
            incr_volume (incr * mult)
        } else if ksr.mod_keys.caps.down.check() {
            ksr.in_managed_ctrl_down_state.set();
            //Key::LCtrl.press();
            ksr.mod_keys.lctrl.ensure_active();
            MouseWheel::DefaultWheel.scroll(delta); // caps-wheel as ctrl-wheel (zoom etc)
        } else if ksr.mod_keys.some_shift_down() {
            //handle_horiz_scroll_wheel(incr);
            // ^^ todo:: .. (and for now, just let default pass through)
            MouseWheel::DefaultWheel.scroll(delta);
        } else {
            MouseWheel::DefaultWheel.scroll(delta);
        }
    }

    pub fn incr_volume(incr:i32) {
        let key = if incr > 0 { Key::VolumeUp } else { Key::VolumeDown };
        (0 .. incr.abs()) .for_each(|_| key.press());
    }

    pub fn incr_brightness (incr:i32) {
        static INCR_STEP:i32 = 1;
        // note again that we're always spawned out from hook thread, so slower tasks are also ok here
        //utils::brightness_ps_wmi::incr_brightness(INCR_STEP*incr);
        let _ = utils::brightness_utils::incr_brightness(INCR_STEP*incr);
    }

    pub fn handle_alt_tab_wheel (incr:i32) {
        // todo potentially impl additional separate timer-spacing here, to slow this down even more than regular wheel spacing
        // note that for these we DONT want to release Alt key .. presumably, expecting it to be physically released later
        if incr.is_positive() { shift_press_release(Key::Tab) }
        else { press_release(Key::Tab) }
    }

    pub fn handle_horiz_scroll_wheel (_incr:i32) {
    }

    pub fn setup_mouse_wheel_handling (k:&Krusty) {
        let ks = k.ks.clone();
        MouseWheel::DefaultWheel .bind ( WheelForwardCb, MouseEventCallbackEntry {
            event_prop_directive: EventProp_Stop,
            cb : MouseEvCbFn_SpawnedCallback { cb : Arc::new ( move |ev| {
                ev .wheel_ev_data .iter() .for_each (|d| handle_wheel_guarded(d.delta, &ks) );
            } ) },
        } );
        let ks = k.ks.clone();
        MouseWheel::DefaultWheel .bind ( WheelBackwardCb, MouseEventCallbackEntry {
            event_prop_directive: EventProp_Stop,
            cb : MouseEvCbFn_SpawnedCallback { cb : Arc::new ( move |ev| {
                ev .wheel_ev_data .iter() .for_each (|d| handle_wheel_guarded(d.delta, &ks) );
            } ) },
        } );
    }

}





/// handling for any 'special' keys that need to be bound/handled directly (like for mouse btns) rather than via combo-maps
pub mod special_keys_setups {

    use crate::krusty::*;

    pub fn setup_direct_binding_keys (_k:&Krusty) {

        // currently, we dont need to do it for anything .. (was more essential when mod-tracking / combo-maps mechanisms were more limited)

        // HOWEVER, there might be cases where it might be simpler to just do direct bindings than to populate all the combo maps etc etc
        // ALSO, for things that require actual action in key-up callbacks, we'd do it direclty here because key-dn actions arent registered
        // > specifically in maps and so combo-maps composed callbacks can only either set key-up to do nothing, or at most clear mode flags

        // IMPORTANT : note that whatever we put here, we will NOT want to include them in ANY action maps (or overwrite w these at the end) !!

    }

}





/// setup for the entire krusty-board application, incl setting up key/btn bindings and combos
pub fn setup_krusty_board () {

    use crate::krusty::{*, key_utils::*, special_keys_setups::*, mouse_btn_setups::*, mouse_wheel_setups::*};

    use crate::utils::{window_utils::*, process_utils::*};

    use crate::krusty::{KbdKey::*, mod_keys::ModKey::*, combo_maps::ModeState_T::*};
    // ^^ a little extreme, but we'll see if its tolerable


    let k = Krusty::new();

    // setup all the mod-keys .. (can override this with own setup if desired)
    k.ks.mod_keys.setup_tracking(&k.ks);


    // handling for mouse left btn, mostly to allow caps-as-ctrl behavior during drag drops and clicks
    setup_mouse_left_btn_handling (&k);
    // also for mouse right btn, mostly to allow switche scrolling w right-btn-wheel combo
    setup_mouse_right_btn_handling (&k);
    // also setup both Xbutton srcs to act as middle btns (used for link clicks, closing tabs etc)
    setup_mouse_x_btn_1_handling ();
    setup_mouse_x_btn_2_handling ();

    // setup handling for mouse wheel .. complex overloading over alt-tab, switche, volume, brightness etc !!
    setup_mouse_wheel_handling (&k);




    // we'll setup most keys via key-combo action maps that we'll compose into relevant callbacks after all mapping is registered
    // HOWEVER, there are some keys (incl those that look for shift/ctrl) that will be set directly at the end after all the action-map setups

    // in addition, we'll want to bind MOST keys so default actions for things like ralt or caps combos are generated for them even if we
    // >  dont have any special combos to setup for them .. to keep combos tables light, we'll register everything there first

    // NOTE that if we wanted special-keys-setups (which we dont currently for any key), we should do that after all binding is done so as to
    // >  overwrite what default bindings would otherwise be generated .. (we currently invoke that at the end, even though its not yet needed)

    // NOTE that fallback defaults are : ralt-as-shift, caps-as-ctrl, caps-ralt/shift/alt/win as ctrl-shift/shift/alt/win

    let char_keys: Vec<Key> = "qwertasdfgzxcvb`123456yuiop[]\\hjkl;\'nm,./7890-=" .chars() .map (|c| Key::from_char(c)) .flatten() .collect();
    let fnum_keys: Vec<Key> = (u64::from(F1) .. u64::from(F24)) .map (|v| Key::from(v)) .collect();
    let nav_keys   = vec![Left, Right, Up, Down, PageUp, PageDown, Home, End];
    let spcl_keys  = vec![Backspace, Delete, Space, Tab, Enter, Escape, Insert, Apps];
    //let media_keys = vec![BrowserBack, BrowserForward, BrowserRefresh, VolumeMute, VolumeDown, VolumeUp,
    //                      MediaNextTrack, MediaPrevTrack, MediaStop, MediaPlayPause];

    vec![char_keys, fnum_keys, nav_keys, spcl_keys] .concat() .into_iter() .for_each ( |key| {
        k .default_bind_keys .write().unwrap() .insert (key);
    } );
    // ^^ we can ofc put combos for these later in code .. all these do is register for default binding if no combo gets mapped!



    // we'll start with some caps-atypical caps-as-shift mappings, basically nums or kbd-right symbols not otherwise involved in l2
    "1234567890-=[]\\;\'/." .chars() .for_each ( |c| {
        Key::from_char(c) .into_iter() .for_each ( |key|
            k.cm .add_combo (&k.ks, &k.cg(key).m(caps), &k.cg(key).m(lshift))
        )
    } );



    // lets also disable the win-number combos as they annoyingly activate/minimize items from taskbar etc
    "1234567890" .chars() .map (|c| Key::from_char(c)) .flatten() .for_each ( |key| {
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(lwin),                  no_action() );
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(lwin).m(caps),          no_action() );
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(lwin).m(lalt),          no_action() );
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(lwin).m(ralt),          no_action() );
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(lwin).m(caps).m(lalt),  no_action() );
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(lwin).m(caps).m(ralt),  no_action() );
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(lwin).m(caps).m(shift), no_action() );
    } ); // some of ^^ these will get overwritten by specific win-combos added later .. which is fine
    // note that at least for now, we're choosing to ignore caps-shift, caps-ctrl etc combos, though ofc could impl if need arises

    // we'll disable win-d too, as I never use that show/hide desktop and it's disruptive
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(D).m(lwin), no_action());



    // we'll set up a combo (caps-alt-Insert) to unstick everything in case we get into weird states due to other hooks stealing/suppressing key events etc
    // note since the whole point is to invoke it at stuck times, we'd want to overload that also for alt/ctr/shift etc and their combinations!
    // .. otoh, they can all be unstuck by just press/releasing them, incl mouse btns, so for now we'll ignore that, can update as necessary
    // further, combos incl all mode states and some other flags .. for a combo to trigger those would have to match too ..
    // soo .. we'll let this simple combo be, but it will mostly only be useful to clear mouse-btn states .. not even eqv of press-rel mod-keys
    let ks = k.ks.clone(); let clear = Arc::new (move || ks.unstick_all());
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Insert).m(caps).m(lalt), clear.clone());



    fn register_mode_key (k: &Krusty, key: Key, ms: &MS) {
        // first we'll do the registration, then we can try and set any auxillary combos here too
        ms .register_key (key);

        //add_bare_af_combo (k, k.cg(key).m(caps).s(ms), no_action());
        // ^^ not adequate as we'll have multi-mode and multi-modkey combos .. better to just disable it in runtime fallbacks

        // however, we can add in some replacement actions here .. basically ralt w/ those can give the expected w/caps actions
        k.cm .add_combo (&k.ks, &k.cg(key).s(ms.ms_t).m(ralt),           &k.cg(key).m(lctrl));
        k.cm .add_combo (&k.ks, &k.cg(key).s(ms.ms_t).m(ralt).m(lalt),   &k.cg(key).m(lctrl).m(lalt));
        k.cm .add_combo (&k.ks, &k.cg(key).s(ms.ms_t).m(ralt).m(lshift), &k.cg(key).m(lctrl).m(lshift));
        k.cm .add_combo (&k.ks, &k.cg(key).s(ms.ms_t).m(ralt).m(lwin),   &k.cg(key).m(lctrl).m(lwin));
    }

    // setup keys for layer-2 caret nav sel/del/fast modes
    // note: registering as mode key will set w/caps actions to nothing, other combos can be set as usual elsewhere
    // .. it will also add default replacement for caps-plus-combos to be caps-ralt-plus combos, but ofc can override those too
    register_mode_key ( &k, E, &k.ks.mode_states.sel  );
    register_mode_key ( &k, D, &k.ks.mode_states.del  );
    register_mode_key ( &k, F, &k.ks.mode_states.word );
    register_mode_key ( &k, R, &k.ks.mode_states.fast );

    // setup the key for l4 shortcuts mode, but the mechanism is same as for the caret modes
    register_mode_key ( &k, Q,        &k.ks.mode_states.qks  );
    register_mode_key ( &k, Numrow_1, &k.ks.mode_states.qks1 );
    register_mode_key ( &k, Numrow_2, &k.ks.mode_states.qks2 );
    register_mode_key ( &k, Numrow_3, &k.ks.mode_states.qks3 );



    // f in caret mode, so we'll remap some of the other combos to replace ctr-f etc
    k.cm .add_combo (&k.ks, &k.cg(F).m(lalt),         &k.cg(F).m(ctrl));     // alt-f to ctrl-f
    k.cm .add_combo (&k.ks, &k.cg(F).s(word).m(ralt), &k.cg(F).m(lalt));     // caps-ralt-f to alt-f (instead of default ctrl-f)
    k.cm .add_combo (&k.ks, &k.cg(F).s(word).m(lalt), &k.cg(F).m(lalt));     // caps-lalt-f to alt-f too, though it goes against typical mode-key usage
    k.cm .add_combo (&k.ks, &k.cg(F).s(word).s(qks1), &k.cg(F).m(lalt));     // caps-1-f also to alt-f (at least its one handed)

    // e in caret mode, so we'll put our left-handed-enter on alt-e instead .. (note that there are also caps-space-* combos for *-enter)
    k.cm .add_combo (&k.ks, &k.cg(E).m(lalt),         &k.cg(Enter));             // alt-e -> enter






    // setup backquote .. make normal case be Delete, caps or alt do back-tick, and shift do its tilde
    k.cm .add_combo (&k.ks, &k.cg(Backquote),          &k.cg(ExtDelete));
    k.cm .add_combo (&k.ks, &k.cg(Backquote).m(shift), &k.cg(Backquote).m(shift));
    k.cm .add_combo (&k.ks, &k.cg(Backquote).m(caps),  &k.cg(Backquote));
    k.cm .add_combo (&k.ks, &k.cg(Backquote).m(lalt),  &k.cg(Backquote));
    //k.cm .add_combo (&k.sk, &k.cg(Backquote).m(ralt),  &k.cg(Backquote).m(shift));
    // ^^ not strictly necessary as cb composition now defaults to this, but also useful to see here for reference


    // setup tab .. caps-as-ctrl for caps-tab switching, incl for ctrl-shift-tab .. also ralt-tab for shift-tab
    let ks = k.ks.clone();
    let cb : AF = Arc::new (move || {
        if ks.mod_keys.caps.down.check() {
            ks.in_managed_ctrl_down_state.set();
            ks.mod_keys.lctrl.ensure_active();  // this enables caps-as-ctrl for caps-tab switching
            // ^^ we're not gonna release ctrl immediately, but keep track and release when caps is released
            press_release(Tab)
        }
    } );
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Tab).m(caps), cb.clone());
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Tab).m(caps).m(shift), wrapped_action(LShift, cb.clone()));
    // ^^ this enables the natural ctrl-shift-tab to do backwards tablist nav
    // .. note that we do 'bare' here because caps-tab is in managed ctrl state, and we dont wana get wrapped w ctrl inactive guards here

    // we'll also need to put these in combos wit the managed-ctrl-state already active (as its among combo bits now)
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Tab).m(caps).s(mngd_ctrl_dn), cb.clone());
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Tab).m(caps).m(shift).s(mngd_ctrl_dn), wrapped_action(LShift, cb.clone()));

    // lets add explicit support for arrow keys during ctrl tab (default doesnt check for active/inactive guards)
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Left ).m(caps).s(mngd_ctrl_dn), base_action(Left ));
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Right).m(caps).s(mngd_ctrl_dn), base_action(Right));
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Up   ).m(caps).s(mngd_ctrl_dn), base_action(Up   ));
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Down ).m(caps).s(mngd_ctrl_dn), base_action(Down ));

    // and finally for ralt-as-shift support for caps-tabbing too
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Tab).m(caps).m(ralt),                 wrapped_action(LShift, cb.clone()));
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Tab).m(caps).m(ralt).s(mngd_ctrl_dn), wrapped_action(LShift, cb.clone()));




    // setup space key .. ralt-space as enter, caps-space as ctrl-space, caps-lalt-space as alt-enter for intellij
    k.cm .add_combo (&k.ks, &k.cg(Space).m(ralt),         &k.cg(Enter));             // ralt-space -> enter
    k.cm .add_combo (&k.ks, &k.cg(Space).m(caps).m(lalt), &k.cg(Enter).m(lalt));     // caps-lalt-space -> alt-enter
    k.cm .add_combo (&k.ks, &k.cg(Space).m(caps).m(ralt), &k.cg(Escape));            // caps-ralt-space -> Escape
    //k.cm .add_combo (&k.ks, &k.cg(Space).m(caps),         &k.cg(Space).m(ctrl));   // caps-space -> ctrl-enter
    // ^^ not strictly necessary as cb composition now defaults to this, but also useful to see here for reference



    // win-m by default minimized all windows .. we just want to disable it
    k.cm .add_cnsm_bare_af_combo (&k.ks, k.cg(M).m(lwin), no_action());

    // win-i should start irfanview
    k.cm .add_cnsm_bare_af_combo (&k.ks, k.cg(I).m(lwin), Arc::new (|| start_irfanview()));

    // win-n should start chrome-incognito
    k.cm .add_cnsm_bare_af_combo (&k.ks, k.cg(N).m(lwin), Arc::new (|| start_chrome_incognito()));

    // we'll setup win-C (and caps-alt-C) to quickly bring up chrome Tabs-Outliner via switche Alt-F20 hotkey
    k.cm .add_combo (&k.ks, k.cg(C).m(lwin),         &k.cg(F20).m(lctrl));      // win-c -> ctrl-F20 .. switche tabs-outliner
    k.cm .add_combo (&k.ks, k.cg(C).m(caps).m(lalt), &k.cg(F20).m(lctrl));      // caps-lalt-c -> ctrl-F20 .. one-handed


    // in cur laptop, Fn-F6/F7 do brightness, but at +10 incrs .. set them to do small incrs with alt combos
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(F6).m(lalt), Arc::new (|| incr_brightness(-1)));
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(F7).m(lalt), Arc::new (|| incr_brightness(1)));

    // actually, since we use win-1/2/3 as vol mute/down/up, might as well also set alt-1/2/3 for brightness zero/down/up
    // (note that numrow 1/2/3 with caps are qks* keys, so they cant be used with any caps combos as those would be silent!)
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(Numrow_1).m(lalt),           Arc::new (|| incr_brightness(-100)));
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(Numrow_2).m(lalt),           Arc::new (|| incr_brightness(-1)));
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(Numrow_3).m(lalt),           Arc::new (|| incr_brightness(1)));
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(Numrow_2).m(lalt).m(lshift), Arc::new (|| incr_brightness(-5)));
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(Numrow_3).m(lalt).m(lshift), Arc::new (|| incr_brightness(5)));

    // win-2 is vol down, win-3 is vol up, win-1 can do mute
    k.cm .add_af_combo (&k.ks, &k.cg(Numrow_1).m(lwin),           &k.cg_af(base_action(VolumeMute)));
    k.cm .add_af_combo (&k.ks, &k.cg(Numrow_2).m(lwin),           &k.cg_af(base_action(VolumeDown)));
    k.cm .add_af_combo (&k.ks, &k.cg(Numrow_3).m(lwin),           &k.cg_af(base_action(VolumeUp)));
    k.cm .add_af_combo (&k.ks, &k.cg(Numrow_2).m(lwin).m(lshift), &k.cg_af(fast_action(VolumeDown)));  // double-action
    k.cm .add_af_combo (&k.ks, &k.cg(Numrow_3).m(lwin).m(lshift), &k.cg_af(fast_action(VolumeUp)));    // double-action

    // win-f1 play/pause, caps-f1 toggle mute, base-case: switche-invoke alt-F1: switche silent-switch, ralt for actual F1
    k.cm .add_combo (&k.ks, &k.cg(F1),          &k.cg(F16));             // switche next
    //k.cm .add_combo (&k.ks, &k.cg(F1).m(shift), &k.cg(F17));           // switche prev
    k.cm .add_combo (&k.ks, &k.cg(F1).m(shift), &k.cg(F16).m(shift));    // switche prev (passthrough shift-F16 instead of F17 is more efficient)
    k.cm .add_combo (&k.ks, &k.cg(F1).m(lalt),  &k.cg(F19).m(ctrl));     // switche no-popup next switch
    k.cm .add_combo (&k.ks, &k.cg(F1).m(ralt),  &k.cg(F1));
    k.cm .add_combo (&k.ks, &k.cg(F1).m(caps),  &k.cg(VolumeMute));
    k.cm .add_combo (&k.ks, &k.cg(F1).m(lwin),  &k.cg(MediaPlayPause));
    // and keeping w the theme, set caps-win-F1 (key with vol-mute printed on it) to toggle microphone mute
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(F1).m(caps).m(lwin), Arc::new (|| {mic_mute_toggle(); open_mic_cpl();}));
    // we'll set Alt-F2 to bring chrome tabs-outliner (via switche) to keep w the theme of Alt-F<n> keys for task switching
    k.cm .add_combo (&k.ks, &k.cg(F2).m(lalt),  &k.cg(F20).m(ctrl));     // switche no-popup tabs-outliner switch


    // want win-f2 for next with some initial skip .. we'll use caps-win-f2 for prev, so we'll set it up for both
    // note that our mechanism for wrapping mod-key-state restoring guards operates via AFs, hence setting those up (instead of fns)

    // skips work by alt-ctrl-volUp (needs to guard win-inactive since its on win-combo)
    fn media_skips_action (n_skips:u32, ks:&KrS) -> AF {
        ks.mod_keys.lwin.inactive_action ( ks.mod_keys.lalt.active_action ( ks.mod_keys.lctrl.active_action ( Arc::new ( move || {
            (0 .. n_skips) .into_iter() .for_each (|_| { press_release(VolumeUp) });
    } ) ) ) ) }
    // ^^ gives an AF with specified number of skips

    // it uses alt-active media-skips, so we'll need alt_inactive-action, plus guard on win-combo it is on
    let ks = k.ks.clone();
    let media_next_action = k.ks.mod_keys.lwin.inactive_action ( k.ks.mod_keys.lalt.inactive_action ( Arc::new ( move || {
        if !ks.mod_keys.caps.down.check() { press_release(MediaNextTrack) }
        else { press_release(MediaPrevTrack) }
        let ks = ks.clone();  // clone again to allow moving into spawned thread closure
        //spawn ( move || { sleep(time::Duration::from_millis(2000));  media_skips_action(3,&ks)(); } );
        // .. note again that we're always spawned out from hook thread, so slower tasks are also ok here
        sleep(time::Duration::from_millis(2000));
        media_skips_action(3,&ks)();
    } ) ) );

    // win-f2 for next with some initial skip
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(F2).m(lwin),         media_next_action.clone());
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(F2).m(lwin).m(caps), media_next_action);

    // win-f3 for skip forward a bit
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(F3).m(lwin), media_skips_action(1, &k.ks));



    // escape is just escape, but we just want it to do press-release immediately (so switche is faster)
    k.cm .add_combo (&k.ks, &k.cg(Escape), &k.cg(Escape));

    // use the apps key to send shift-escape .. or caps-escape too
    k.cm .add_combo (&k.ks, &k.cg(Apps),           &k.cg(Escape).m(shift));
    k.cm .add_combo (&k.ks, &k.cg(Escape).m(caps), &k.cg(Escape).m(shift));




    // caps-lalt-F for full screen ..(but from bulk mapping above, caps-f should still give ctrl-f)
    //k.cm .add_combo (&k.ks, &k.cg(F).m(caps).m(lalt), &k.cg(F11));
    // ^^ no longer available as we made F a caret mode key, and set caps-alt-F to alt-F instead

    // 'w' should have caps-ctrl mapping, but when w/ alt, send alt-f4 (to close all-tabs, windows etc)
    //k.cm .add_combo (&k.ks, &k.cg(W).m(caps).m(shift), &k.cg(F4).m(alt));
    // ^^ note: initially we wanted this with caps-shift-w, but turns out (at least on my kbd, turns out caps+shift+[F1, 2, w, s, x]
    // >  dont produce any key event at the hook at all .. nothing .. its like the keyboard driver not sending those out
    // funnily enough, there's a bunch of complaints about specifically those keys for dell/hp laptops .. looks like hardware
    // >  appears to be a common kbd pcb layout issue .. heres from 2007: (https://www.joachim-breitner.de/blog/250-Shift-Caps-2)
    // sooo .. to makeup, we'll do alt-caps-w do the alt-f4 business instead
    k.cm .add_combo (&k.ks, &k.cg(W).m(caps).m(lalt), &k.cg(F4).m(lalt));




    // filling out l2 actions (incl w caps-alt combos)
    /* l2-setup config summary:
     - only j/k for left/right get f-for-word-nav mode speedup (native word nav by sending ctrl)
     - those and i/comma for up/down get r-for-double-speed nav mode (2x nav) .. i/comma get that for f too
     - h/l/u/m for home/end/pgup/pgdown get no speedup modes
     - e/d do sel/del modes, and those can be freely combined with the f/r word/fast nav modes
     - in del mode, left/home/up/pgup get ExtBackspace, right/end/down/pgdn get ExtDelete
     - in del mode, left/right do direct bksp/del, but others get select then bksp/del
     */

    type AFG = fn(Key) -> AF ;

    fn setup_l2_key (k:&Krusty, key:Key, l2k:Key, dk:Key, wafg:AFG, fafg:AFG, del_via_sel:bool) {
        // register nav actions for normal-nav, word-nav, and fast-nav modes
        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps),         base_action(l2k));
        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps).s(word), wafg(l2k));
        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps).s(fast), fafg(l2k));

        // selection actions are via wrapping those with shift press-release
        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps).s(sel),         wrapped_action (LShift, base_action(l2k)));
        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps).s(sel).s(word), wrapped_action (LShift, wafg(l2k)));
        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps).s(sel).s(fast), wrapped_action (LShift, fafg(l2k)));

        fn del_sel_afg (del_key:Key, nav_af:AF) -> AF {
            Arc::new ( move || {
                LShift.press(); nav_af(); LShift.release(); // dont need guards for shift here.. this is deep into multi key L2
                press_release(del_key);
        } ) }

        let (dna, dwa, dfa) = if del_via_sel {
            // if deleting via sel, we wrap the del-sel action around the normal nav actions
            ( del_sel_afg(dk,base_action(l2k)), del_sel_afg(dk,wafg(l2k)), del_sel_afg(dk,fafg(l2k)) )
        } else { // and for direct deletes, we perform the nav-eqv action but with the specified delete-key
            ( base_action(dk), ctrl_action(dk), fast_action(dk) )
        };

        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps).s(del),         dna);
        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps).s(del).s(word), dwa);
        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps).s(del).s(fast), dfa);

        // might as well setup l3 right here too .. more details in the setup fn
        setup_l3_key (k, key, l2k);
    }


    // idk what layer this even is, but since we're so used to l2 ej/k nav keys, we'll set them up for various mod-combo eqvs too
    // .. these are useful for various second order nav .. e.g between IDE tabs etc .. no fancy speedups etc here
    // (note ofc that we've filled out the whole set of these for completeness even though realistically we wont use all/most of them)
    fn setup_l3_key (k:&Krusty, key:Key, l3k:Key) {
        // first the single mod-key combo eqvs .. (and since caps-ctrl is hard to press, we'll make qks1 and lalt do ctrl for these too)
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).m(lalt  ),  k.cg(l3k).m(lalt  ));
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).m(lshift),  k.cg(l3k).m(lshift));
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).m(lctrl ),  k.cg(l3k).m(lctrl ));
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).m(ralt  ),  k.cg(l3k).m(lctrl ));
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).s(qks1  ),  k.cg(l3k).m(lctrl ));
        // then double mod-key combos
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).m(lalt ).m(lshift),  k.cg(l3k).m(lalt ).m(lshift));
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).m(lctrl).m(lshift),  k.cg(l3k).m(lctrl).m(lshift));
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).m(lctrl).m(lalt  ),  k.cg(l3k).m(lctrl).m(lalt  ));
        // double combos with ralt filling in for a mod-key
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).m(ralt).m(lshift),  k.cg(l3k).m(lctrl).m(lshift));
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).m(ralt).m(lctrl ),  k.cg(l3k).m(lctrl).m(lshift));
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).m(ralt).m(lalt  ),  k.cg(l3k).m(lctrl).m(lalt  ));
        // double combos with qks1 filling in for a mod-key
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).s(qks1).m(lshift),  k.cg(l3k).m(lctrl).m(lshift));
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).s(qks1).m(lctrl ),  k.cg(l3k).m(lctrl).m(lshift));
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).s(qks1).m(lalt  ),  k.cg(l3k).m(lctrl).m(lalt  ));
        // finally, meh lets do all three mod-keys together too
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).m(lalt).m(lctrl).m(lshift),   k.cg(l3k).m(lctrl).m(lalt).m(lshift));
        // and triple combos w ralt filling in
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).m(ralt).m(lalt ).m(lshift),   k.cg(l3k).m(lctrl).m(lalt).m(lshift));
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).m(ralt).m(lalt ).m(lctrl ),   k.cg(l3k).m(lctrl).m(lalt).m(lshift));
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).m(ralt).m(lctrl).m(lshift),   k.cg(l3k).m(lctrl).m(lalt).m(lshift));
        // and triple combos w qks1 filling in
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).s(qks1).m(lalt ).m(lshift),   k.cg(l3k).m(lctrl).m(lalt).m(lshift));
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).s(qks1).m(lalt ).m(lctrl ),   k.cg(l3k).m(lctrl).m(lalt).m(lshift));
        k.cm .add_combo (&k.ks, k.cg(key).m(caps).s(qks1).m(lctrl).m(lshift),   k.cg(l3k).m(lctrl).m(lalt).m(lshift));

        // (note .. cap-win combos for these nav-keys are already used for window move/stretch etc)
    }


    // filling out l2/l3 actions
    setup_l2_key ( &k,  J,     ExtLeft,   Backspace,   ctrl_action,   fast_action,   false );
    setup_l2_key ( &k,  K,     ExtRight,  ExtDelete,   ctrl_action,   fast_action,   false );
    setup_l2_key ( &k,  I,     ExtUp,     Backspace,   fast_action,   fast_action,   true  );
    setup_l2_key ( &k,  Comma, ExtDown,   ExtDelete,   fast_action,   fast_action,   true  );
    setup_l2_key ( &k,  H,     ExtHome,   Backspace,   base_action,   base_action,   true  );
    setup_l2_key ( &k,  L,     ExtEnd,    ExtDelete,   base_action,   base_action,   true  );
    setup_l2_key ( &k,  U,     ExtPgUp,   Backspace,   base_action,   base_action,   true  );
    setup_l2_key ( &k,  M,     ExtPgDn,   ExtDelete,   base_action,   base_action,   true  );






    // then the caps-win combo (l4?) actions :

    // caps-win-U should vert-max (via shift-win-up) if not already, or else restore window from vert-max
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(U).m(caps).m(lwin), Arc::new (|| win_fgnd_toggle_vertmax()));
    // caps-win-m should maximize (via win-m) if not, else restore from max
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(M).m(caps).m(lwin), Arc::new (|| win_fgnd_toggle_max()));
    // caps-win-n should minimize (via win-arrrowDown)
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(N).m(caps).m(lwin), Arc::new (|| win_fgnd_min()));

    fn setup_win_move_key (k:&Krusty, key:Key, wmfn:fn(i32, i32), dx:i32, dy:i32, m:i32) {
        // we'll setup caps-win combos for regular move/stretch etc, and caps-ctrl or caps-qks1 combos for fineer control
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(caps).m(lwin).m(lctrl), Arc::new (move || wmfn (dx, dy) ));
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(caps).m(lwin).s(qks1),  Arc::new (move || wmfn (dx, dy) ));
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(caps).m(lwin),          Arc::new (move || wmfn (dx*m, dy*m) ));
    }
    // caps-win-[j,k,i,comma] should  move window [left, right, top, bottom] respectively
    setup_win_move_key ( &k, J,     win_fgnd_move,  -1,   0, 20 );
    setup_win_move_key ( &k, K,     win_fgnd_move,   1,   0, 20 );
    setup_win_move_key ( &k, I,     win_fgnd_move,   0,  -1, 20 );
    setup_win_move_key ( &k, Comma, win_fgnd_move,   0,   1, 20 );

    // caps-win-[h,semicolon,period,o] should stretch window [narrower, wider, shorter, taller] respectively
    setup_win_move_key ( &k, H,         win_fgnd_stretch,  -1,   0, 20 );
    setup_win_move_key ( &k, O,         win_fgnd_stretch,   0,  -1, 20 );
    setup_win_move_key ( &k, Period,    win_fgnd_stretch,   0,   1, 20 );
    //setup_win_move_key ( &k, L,       win_fgnd_stretch,   1,   0, 20 );     // win-L is reserved by windows for lock
    setup_win_move_key ( &k, Semicolon, win_fgnd_stretch,   1,   0, 20 );



    // some additional caps-win combos
    // caps-win-c being used to launch winmerge diff from last two clipboard entries
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(C).m(caps).m(lwin), Arc::new (|| start_winmerge_clipboard()));
    // gaah we'll just throw in iDEA diff for drag-drop diffing (just coz winmerge doesnt do dark mode)
    //k.cm .add_af_combo (&k.ks, &k.cg(C).m(lwin), &k.cg_af (Arc::new (|| start_idea_diff() )));
    // ^^ cant do from here, turns out idea diff from cmd line can ONLY be opened with two files pointed, unlike empty from Idea shortcut!





    // then we can add in any l4 quick-keys shortcuts combos we want
    // note: there are 3 quick-keys modes (qks, qks2, qks3) on keys (q, 2, 3) respectively! .. all are pretty ergonomic!
    // note also that during combo gen, the 'caps' mod-key is auto added to caps-based modes, incl these quick-keys

    // we'll add some nav overloading for IDES on qks2 for starters!!
    // and this one to travel along bookmarks in IDE
    k.cm .add_combo (&k.ks, &k.cg(I    ).s(qks2), &k.cg(ExtUp  ).m(lalt).m(lctrl).m(lshift));
    k.cm .add_combo (&k.ks, &k.cg(Comma).s(qks2), &k.cg(ExtDown).m(lalt).m(lctrl).m(lshift));
    // and to toggle a bookmark at the current caret location
    k.cm .add_combo (&k.ks, &k.cg(U).s(qks2), &k.cg(F11).m(lctrl).m(lshift));
    // and to bring up the bookmarks viewer
    k.cm .add_combo (&k.ks, &k.cg(K).s(qks2), &k.cg(F11).m(lshift));
    // this toggles IDE col edit mode via Alt-Shift-C (but can be done w/o moving hands much)
    k.cm .add_combo (&k.ks, &k.cg(C).s(qks2), &k.cg(C).m(lalt).m(lshift));



    // we could also set up specific r-alt combos (for F<?> keys etc other than the default ralt-as-shift)
    // hmm.. cant think of any so far?


    // could also setup caps-ralt combos (for non l2/caret keys), which can be separate from caps-lalt combos!
    // hah .. nothing here yet either huh .. well these are two hand combos, so not preferable anyway


    // if really want/need to, could do completely independent lalt_ralt_<key> combos too (with minimal extra coding)
    // not yet implemented as dont see any need or much utlity for doing so given there are other simpler unused combos available


    // also fyi re free combos: caps-win-<non-l3>, win-<num>, caps-alt-<num>, caps-ralt<non-l2>
    // .. even for caps-lalt-<?> defaulting to ctr-alt-<?> most are still free (other than l2, caret, e, f, w, space, f2)
    // .. and almost all F<num> combos with caps, caps-win, caps-lalt, ralt, caps-ralt, even w just lalt


    // plus, for additional l3+ setup, (e.g. moving windows across monitors), could impl 'mode' in l3 (w/ caps-win) like for l2 (w/ caps-alt)
    // .. or add additional mode keys in l2 (caps-alt) .. although not a lot of free keys there .. maybe q .. could reuse mod keys w/ caps-win though



    // test/debug section
    /*
    k.cm .add_combo (&k.ks, &k.cg(F12).m(win).m(ctrl), &k.cg(F19).m(shift)); // try rctrl-win

    k.cm .add_combo (&k.ks, &k.cg(F10).m(win).m(shift), &k.cg(F19)); // .. try rshift ..

    k.cm .add_combo (&k.ks, &k.cg(Backslash).m(ctrl), &k.cg(F19));  // try rctrl
    k.cm .add_combo (&k.ks, &k.cg(Backslash).m(caps), &k.cg(F19));

    //k.cm .add_combo (&k.ks, &k.cg(Numrow_0).m(ctrl), &k.cg(F19).m(lctrl));
    //k.cm .add_combo (&k.ks, &k.cg(Numrow_9).m(ctrl), &k.cg(F19).m(ctrl));
    k.cm .add_combo (&k.ks, &k.cg(Numrow_8).m(ctrl), &k.cg(F19).m(shift));  // try rctrl
    k.cm .add_combo (&k.ks, &k.cg(Numrow_0).m(ctrl), &k.cg(F19).m(ctrl));
    //k.cm .add_combo (&k.ks, &k.cg(Numrow_7).m(win),  &k.cg(F19).m(shift));
    k.cm .add_combo (&k.ks, &k.cg(Numrow_6).m(ctrl), &k.cg(F19).m(lctrl));   // try rctrl
    //k.cm .add_combo (&k.ks, &k.cg(Numrow_5).m(ctrl).m(win), &k.cg(F19).m(lctrl));
    //k.cm .add_combo (&k.ks, &k.cg(Numrow_4).m(ctrl).m(win), &k.cg(F19).m(lshift));

    k.cm .add_combo (&k.ks, &k.cg(Numrow_9).m(lctrl), &k.cg(F19).m(lshift));
    k.cm .add_combo (&k.ks, &k.cg(Numrow_7).m(lalt ), &k.cg(F19).m(lshift));
    k.cm .add_combo (&k.ks, &k.cg(Numrow_5).m(lwin ), &k.cg(F19).m(lshift));
    */
    //k.cm .add_combo (&k.ks, &k.cg(F12).m(alt), &k.cg(F19).m(lshift));
    //k.cm .add_combo (&k.ks, &k.cg(Numrow_7).m(lalt ), &k.cg(F19).m(lshift));




    // finally we can start binding key maps .. first the specialized handling for mode-trigger keys
    k.ks.mode_states.bind_mode_keys_actions();
    // then setup the combo-processor itself .. (note that modifier key handlers were already set up earlier)
    *crate::COMBO_MAPS_PROCESSOR.write().unwrap() = Some (k.cm.gen_combo_maps_processor(&k));



    // and we'll put any direct special key setups after all this
    // > which is for safety in case anything above accidentally included those, although ofc we dont want to rely on that!
    setup_direct_binding_keys (&k);



    // note: the handle_input_events to start the whole shebang should be being called somewhere in main after this setup
    //handle_input_events();

}

