mod combo_maps;
mod mod_keys;

use std::{time, time::Instant, cell::RefCell};
use std::thread::{sleep, spawn};
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::sync::{Arc, RwLock};

use derivative::Derivative;

//use closure::closure;
// ^^ gives a nice macro that eliminates clutter from having to clone everywhere
// .. we'd use it a lot more, except IDE syntax highlighting doesnt work inside the macro .. which is waay too big a loss for the benefit!

use crate::{KbdEvent, KbdEvntCbMapKeyType::*, KbdKey, MouseButton, MouseWheel, utils};

use crate::krusty::{mod_keys::*};


// todo : just a reminder that we added some hacky meddling into keycodes and sending key events to get L/R scancodes out on alt/ctrl/shift


// we'll define some easier type aliases (CallBack, ActionFn etc) to pass around triggered actions and so on
type CB  = Box <dyn Fn(KbdEvent) + Send + Sync + 'static> ;
type AF  = Arc <dyn Fn() + Send + Sync + 'static> ;

type Key = KbdKey ;


//type KrS = Arc <KrustyState>;
//type SMK = Arc <SyncdModKey>;
//type TMK = Arc <TrackedModKey>;
// ^^ made these into new-type (some in their own module/file), with deref impld, and then impld all functionality on those
// .. on gains on L2K, but on SMK, lets us impl functionality on the Arc wrapped SMK so we can clone and use it w/o doing smk.0.<bla-bla>



# [ derive (Debug, Default, Clone) ]
pub struct Flag (Arc<RwLock<bool>>);
// ^^ simple sugar that helps reduce a lot of clutter in code

impl Flag {
    pub fn check (&self) -> bool { *self.0.read().unwrap() }
    pub fn set   (&self) { *self.0.write().unwrap() = true }
    pub fn clear (&self) { *self.0.write().unwrap() = false }

    pub fn set_if_clear (&self) { if !self.check() { self.set() } }
    pub fn clear_if_set (&self) { if self.check() { self.clear() } }
}



# [ derive (Derivative) ]
# [ derivative (Debug, Default) ]    // note that we def dont want this clonable (we'd rather clone its Arc than all underlying!)
pub struct KrustyState {
    // used for toggling key processing .. should only listen to turn-back-on combo
    in_disabled_state: Flag,

    // caps will be tracked for internal reference, and we'll assume we'll ALWAYS operate with caps-lock off
    caps_down: Flag,

    // we'll track all mod keys except r-alt as syncd-modifier-keys, where we track the phys and logical states, as well as whether to mask their release
    // this allows us to add any composition of these in combos with any other key incl other mod keys while keeping internal/external models accurate
    # [ derivative ( Default ( value = "SMK::new(Key::LAlt)" ) ) ]
    lalt : SMK,
    # [ derivative ( Default ( value = "SMK::new(Key::LWin)" ) ) ]
    lwin : SMK,
    # [ derivative ( Default ( value = "SMK::new(Key::LCtrl)" ) ) ]
    lctrl : SMK,
    # [ derivative ( Default ( value = "SMK::new(Key::RCtrl)" ) ) ]
    rctrl : SMK,
    # [ derivative ( Default ( value = "SMK::new(Key::LShift)" ) ) ]
    lshift : SMK,
    # [ derivative ( Default ( value = "SMK::new(Key::RShift)" ) ) ]
    rshift : SMK,

    // the only other mod key r-alt we'll do a simpler tracked-modifier-key (which we dont try to keep syncd w external state)
    // and for r-alt, at binding time, it is completely blocked (there used to be ctrl/shift here before that were pass through tracked)
    # [ derivative ( Default ( value = "TMK::new(Key::RAlt)" ) ) ]
    ralt : TMK,

    // note that we're not tracking rwin that some machines (not mine) have .. could easily add that later if need be


    // these track the word/fast nav and sel/del caret modes for l2 action
    l2_sel_mode_key_down:  Flag,
    l2_del_mode_key_down:  Flag,
    l2_word_mode_key_down: Flag,
    l2_fast_mode_key_down: Flag,

    // the quick keys (aka shortcuts) mode designates a shortcuts combo (ctrl-Q) in combo with which any other key combo can be mapped
    qks_mode_key_down:  Flag,
    qks2_mode_key_down: Flag,
    qks3_mode_key_down: Flag,

    mouse_left_btn_down:  Flag,
    mouse_right_btn_down: Flag,

    // for caps-ctrl eqv for caps-tab or caps-wheel, we'll send ctrl press/release at the right times, and will need to track that
    in_managed_ctrl_down_state: Flag,
    // and for right-mouse-btn-wheel switche support, we'll track that state too (and send switche specific keys)
    in_right_btn_scroll_state: Flag,

    # [ derivative ( Default (value = "RwLock::new(Instant::now())") ) ]
    last_wheel_stamp: RwLock<Instant>,
    is_wheel_spin_invalidated: Flag, // invalidated by e.g. mid-spin mod press, or spin stop (spacing > 120ms)
}


# [ derive (Debug, Clone) ]
pub struct KrS (Arc <KrustyState>);
// ^^ we'll use this wrapped type so cloning and passing around is cheap

impl Deref for KrS {
    type Target = KrustyState;
    fn deref(&self) -> &KrustyState { &self.0 }
}

impl KrS {
    pub fn new () -> KrS {
        KrS ( Arc::new ( KrustyState::default() ) )
    }
    fn clear_mode_flags (&self) {
        self.l2_sel_mode_key_down.clear();
        self.l2_del_mode_key_down.clear();
        self.l2_word_mode_key_down.clear();
        self.l2_fast_mode_key_down.clear();
        self.qks_mode_key_down.clear();
        self.qks2_mode_key_down.clear();
        self.qks3_mode_key_down.clear();
    }
    fn some_shift_down (&self) -> bool { self.lshift.down.check() || self.rshift.down.check() }
    fn some_ctrl_down  (&self) -> bool { self.lctrl.down.check()  || self.rctrl.down.check() }
    fn some_alt_down   (&self) -> bool { self.lalt.down.check() } // ralt is disabled for all purposes
    fn some_win_down   (&self) -> bool { self.lwin.down.check() } // we've not impld rwin so far

    fn some_qks_mode_key_down(&self) -> bool {
        // NOTE that these dont actually check that caps is down, and its the ONLY mod down, which is also req for qks mode
        // these basically only check that some qky mode key is down (which would hve to be w caps an nothing else to trigger mode)
        self.qks_mode_key_down.check() || self.qks2_mode_key_down.check() || self.qks3_mode_key_down.check()
    }
    fn only_caps_mod_key_down (&self) -> bool {
        self.caps_down.check() && !self.some_win_down() && !self.some_ctrl_down()
            && !self.some_shift_down() && !self.lalt.down.check() && !self.ralt.down.check()
    }
}





pub struct Krusty {
    // this is mostly just a utility wrapper sugar to pass things around
    _private : (),   // prevents direct instantiation of this struct
    // KrS is Arc<KrustyState>, holds all state flags
    pub ks: KrS,
    // we'll also keep a mapping of mode keys and their associated flags for l2 impl
    pub mode_keys_map : RefCell <HashMap <Key, Flag>>,
    // we'll have a combos map to register all combos (key + modifiers + modes) to their mapped actions
    pub combos_map : RefCell <HashMap <combo_maps::Combo, AF>>,
    // instead of polluting combos_map, we'll hold a registry for keys to gen default bindings for
    pub default_bind_keys : RefCell <HashSet <Key>>,
}



impl Krusty {

    // NOTE: we've decided to mostly impl things grouped by related functionality in disparate modules rather than under impl here
    // .. mostly because doing that allows easier piecemeal imports, as well as better pub/private management

    pub fn new() -> Krusty {

        let ks = KrS::new();

        // we'll fill these with backlinks to parent (mostly to avoid always having to pass ks into their impld fns)
        ks.lalt .link_parent (&ks);
        ks.lwin .link_parent (&ks);

        // and we'll pair up the left/right values where applicable
        ks.lctrl  .link_pair (&ks.rctrl);  ks.rctrl  .link_pair (&ks.lctrl);
        ks.lshift .link_pair (&ks.rshift); ks.rshift .link_pair (&ks.lshift);

        // now we can construct the Krusty struct we want
        Krusty {
            _private : (),
            ks,
            mode_keys_map : RefCell::new (HashMap::new()),
            combos_map : RefCell::new (HashMap::new()),
            default_bind_keys : RefCell::new (HashSet::new()),
        }
    }

    pub fn cg    (&self, key:Key) -> combo_maps::CG_K  { combo_maps::CG_K::new  (key, &self.ks) }
    pub fn cg_af (&self, af:AF)   -> combo_maps::CG_AF { combo_maps::CG_AF::new (af,  &self.ks) }

    fn setup_global_disable (&self) {
        // todo: wont be straight-forward if we want to handle this at hook receipt
        // .. will need a global check there, as well as allowing selectively for the re-enable combo!
    }

}




pub mod caps_setup {

    // setting up caps as the global Layer-2 modifier key
    // .. other mod-keys are via SMK (Syncd-Tracked modifier key: alt/win)  or TMK (Tracked modifier keys: ralt/lctrl/rctrl/lshift/rshift)

    use crate::krusty::*;

    pub fn setup_caps_tracking (k:&Krusty) {
        // note that for caps, we completely block it from ever being sent up, and just manage internally
        if Key::CapsLock.is_toggled() {
            Key::CapsLock.press();  // toggle off first if necessary (to clear key light)
        }

        let ks = k.ks.clone();
        Key::CapsLock.block_bind(KeyDownCallback, move |_| {
            if !ks.caps_down.check() {
                // capslock can come as repeats like other mod keys .. this was a fresh one
                ks.caps_down.set();
                ks.is_wheel_spin_invalidated.set_if_clear();
                // lets notify the synced tracked mod keys, so they can invalidate/release themselves
                [ &ks.lalt, &ks.lwin, &ks.lctrl, &ks.rctrl, &ks.lshift, &ks.rshift
                ] .iter() .for_each ( |smk| smk.process_caps_down() );
            }
            if ks.mouse_left_btn_down.check() && !ks.in_managed_ctrl_down_state.check() {
                ks.in_managed_ctrl_down_state.set();
                ks.lctrl.ensure_active();
            }
        });

        let ks = k.ks.clone();
        Key::CapsLock.block_bind(KeyUpCallback, move |_| {
           ks.caps_down.clear_if_set();
            if ks.in_managed_ctrl_down_state.check() {
                ks.in_managed_ctrl_down_state.clear();
                ks.lctrl.ensure_inactive();
            }
            // the following isnt strictly necessary, but useful in case some keyup falls through
            ks.clear_mode_flags();
            // lets also notify the alt/win tracked mod keys so they can re-enable themselves if applicable
            [ &ks.lalt, &ks.lwin, &ks.lctrl, &ks.rctrl, &ks.lshift, &ks.rshift
            ] .iter() .for_each ( |smk| smk.process_caps_release() );
        });

    }



}



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



pub mod mouse_btn_setups { // setups for mouse btn handling

    use crate::krusty::{*, key_utils::*};

    pub fn setup_mouse_left_btn_handling (k:&Krusty) {
        let ks = k.ks.clone();
        MouseButton::LeftButton.block_bind(true, move |_| {
            ks.mouse_left_btn_down.set_if_clear();
            if ks.caps_down.check() {
                ks.in_managed_ctrl_down_state.set_if_clear();
                ks.lctrl.ensure_active();   // this allows caps-as-ctrl for drag drop etc
            }
            MouseButton::LeftButton.press()
        });
        let ks = k.ks.clone();
        MouseButton::LeftButton.block_bind(false, move |_| {
            ks.mouse_left_btn_down.clear_if_set();
            sleep(time::Duration::from_millis(10));
            MouseButton::LeftButton.release();
        });
        // ^^ in theory could do non-blocking bind for release, but that can occasionally expose us to timing issues as press is on block
        //      and that will be slower to send the actual press from the spawned thread (so making release slower too helps)
    }

    pub fn setup_mouse_right_btn_handling (k:&Krusty) {
        // tracking btn down is for ctrl-wheel (zoom) etc, and right-btn-down-state is for switche scroll (via F21/F22/F23)
        let ks = k.ks.clone();
        MouseButton::RightButton.non_blocking_bind(true, move |_| {
            ks.mouse_right_btn_down.set_if_clear();
        });
        let ks = k.ks.clone();
        let switche_action = base_action(Key::F23);
        MouseButton::RightButton.non_blocking_bind(false, move |_| {
            ks.mouse_right_btn_down.clear_if_set();
            if ks.in_right_btn_scroll_state.check() {
                ks.in_right_btn_scroll_state.clear();
                switche_action();
            }
        });
    }

    pub fn setup_mouse_x_btn_1_handling () {
        // turns out just doing press/release on initial press works snappier/more-reliable than trying to be true to btn-holds
        MouseButton::X1Button.block_bind(true, |_| {
            MouseButton::MiddleButton.press(); MouseButton::MiddleButton.release();
        });
        MouseButton::X1Button.block_bind(false, |_| { });
    }

    pub fn setup_mouse_x_btn_2_handling () {
        MouseButton::X2Button.block_bind(true, |_| {
            MouseButton::MiddleButton.press(); MouseButton::MiddleButton.release();
        });
        MouseButton::X2Button.block_bind(false, |_| { });
    }

}



pub mod mouse_wheel_setups { // setups for mouse wheel handling

    use crate::krusty::{*, key_utils::*};
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
            ksr.in_right_btn_scroll_state.set_if_clear();
            let key = if incr.is_positive() { Key::F22 } else { Key::F21 };
            //ksr.lalt.mod_press_release_guarded(key);
            press_release(key);
        } else  if ksr.lalt.down.check() {
            // wheel support for scrolling in windows native alt-tab task-switching screen
            // this requires a system call to check alt-tab window, so push it out to thread
            ksr.lalt.consumed.set_if_clear();
            let lalt = ksr.lalt.clone();
            spawn ( move || {
                if get_fgnd_win_class() == "MultitaskingViewFrame" { // alt-tab states
                    lalt.ensure_active(); // we're already down but just in case its down/inactive
                    handle_alt_tab_wheel(incr)
                } else { incr_volume(incr); } // simple alt wheel for volume
            } );
        } else if ksr.lwin.down.check() {
            ksr.lwin.consumed.set_if_clear();
            incr_brightness(incr); // win-wheel for brightness
        } else if ksr.caps_down.check() {
            ksr.in_managed_ctrl_down_state.set_if_clear();
            //Key::LCtrl.press();
            ksr.lctrl.ensure_active();
            MouseWheel::DefaultWheel.scroll(delta); // caps-wheel as ctrl-wheel (zoom etc)
        } else if ksr.some_shift_down() {
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
        //utils::brightness_ps_wmi::incr_brightness(INCR_STEP*incr);
        //utils::brightness_utils::incr_brightness(INCR_STEP*incr);
        spawn (move || utils::brightness_utils::incr_brightness(INCR_STEP*incr));
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
        MouseWheel::DefaultWheel.block_bind(true, move |ev| {
            ev .wheel_ev_data .iter() .for_each (|d| handle_wheel_guarded(d.delta, &ks) );
        });
        let ks = k.ks.clone();
        MouseWheel::DefaultWheel.block_bind(false, move |ev| {
            ev .wheel_ev_data .iter() .for_each (|d| handle_wheel_guarded(d.delta, &ks) );
        })
    }

}



pub mod special_keys_setups { // special keys handling


    use crate::krusty::*;

    pub fn setup_direct_binding_keys (_k:&Krusty) {

        // currently, we dont need to do it for anything .. (was more essential when mod-tracking / combo-maps mechanisms were more limited)

        // HOWEVER, there might be cases where it might be simpler to just do direct bindings than to populate all the combo maps etc etc
        // ALSO, for things that require actual action in key-up callbacks, we'd do it direclty here because key-dn actions arent registered
        // > specifically in maps and so combo-maps composed callbacks can only either set key-up to do nothing, or at most clear mode flags

        // IMPORTANT : note that whatever we put here, we will NOT want to include them in ANY action maps (or overwrite w these at the end) !!

    }

}





pub fn setup_krusty_board () {

    use crate::krusty::{*, key_utils::*, caps_setup::*, special_keys_setups::*, combo_maps::*,
                        mouse_btn_setups::*, mouse_wheel_setups::*};

    use crate::utils::{window_utils::*, process_utils::*};

    use crate::krusty::{KbdKey::*, combo_maps::{ModKey::*, ModeState::*}};
    // ^^ a little extreme, but we'll see if its tolerable


    let k = Krusty::new();

    // setup capslock, we'll completely disable it other than for krusty use
    setup_caps_tracking (&k);

    // setup tracking for right-alt .. its completely blocked but tracked, we use it as shifts and combos
    k.ks.ralt .setup_tracking (true);      // for ralt can setup w doBlock=true

    // and shift .. this is simple non-blocking tracking, only tracked to enable a few combos with caps
    k.ks.lshift.setup_tracking (&k.ks);     // these others have doBlock = false
    k.ks.rshift.setup_tracking (&k.ks);

    // and even ctrl .. again simple non-blocking, just tracking flags to allow for natural ctrl-alt-? when alt-? is remapped
    k.ks.lctrl.setup_tracking (&k.ks);
    k.ks.rctrl.setup_tracking (&k.ks);

    // lalt and lwin are set as syncd-tracked-modifier-key with special (but identical) impl .. more details under SyncdModKey impl
    k.ks.lalt.setup_tracking (&k.ks);
    k.ks.lwin.setup_tracking (&k.ks);


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
        k .default_bind_keys .borrow_mut() .insert (key);
    } );
    // ^^ we can ofc put combos for these later in code .. all these do is register for default binding if no combo gets mapped!



    // we'll start with some caps-atypical caps-as-shift mappings, basically nums or kbd-right symbols not otherwise involved in l2
    "1234567890-=[]\\;\'/." .chars() .for_each ( |c| {
        Key::from_char(c) .into_iter() .for_each ( |key| add_combo (&k, &k.cg(key).m(caps), &k.cg(key).m(lshift)) )
    } );



    // lets also disable the win-number combos as they annoyingly activate/minimize items from taskbar
    "1234567890" .chars() .map (|c| Key::from_char(c)) .flatten() .for_each ( |key| {
        add_cnsm_bare_af_combo (&k, &k.cg(key).m(lwin),         no_action());
        add_cnsm_bare_af_combo (&k, &k.cg(key).m(caps).m(lwin), no_action());
    } ); // some of ^^ these will get overwritten by specific win-combos added later .. which is fine

    // note that at least for now, we're choosing to ignore caps-shift, caps-ctrl etc combos, though ofc could impl if need arises






    // setup keys for layer-2 caret nav sel/del/fast modes
    // note: registering as mode key will set caps-only actions to nothing, other combos can be set as usual elsewhere
    register_mode_key (&k, E, &k.ks.l2_sel_mode_key_down);
    register_mode_key (&k, D, &k.ks.l2_del_mode_key_down);
    register_mode_key (&k, F, &k.ks.l2_word_mode_key_down);
    register_mode_key (&k, R, &k.ks.l2_fast_mode_key_down);

    // setup the key for l4 shortcuts mode, but the mechanism is same as for the caret modes
    register_mode_key (&k, Q,        &k.ks.qks_mode_key_down);
    register_mode_key (&k, Numrow_2, &k.ks.qks2_mode_key_down);
    register_mode_key (&k, Numrow_3, &k.ks.qks3_mode_key_down);



    // f in caret mode, so we'll remap some of the other combos to replace ctr-f etc
    add_combo (&k, &k.cg(F).m(lalt),         &k.cg(F).m(ctrl));         // alt-f to ctrl-f
    add_combo (&k, &k.cg(F).m(caps).m(lalt), &k.cg(F).m(lalt));         // caps-alt-f to alt-f

    // e in caret mode, so we'll put our left-handed-enter on alt-e instead .. caps-alt-e can continue to do alt-enter
    add_combo (&k, &k.cg(E).m(lalt),         &k.cg(Enter));             // alt-e -> enter
    add_combo (&k, &k.cg(E).m(caps).m(lalt), &k.cg(Enter).m(lalt));     // caps-alt-e -> alt-enter






    // setup backquote .. make normal case be Delete, caps do back-tick, and shift do its tilde, alt will do quick switch via Alt-F20
    // note that since base is remapped, it will change ctrl/shift combos w/ alt/win etc too .. e.g. ctrl-alt-tick gives ctrl-alt-delete !
    add_combo (&k, &k.cg(Backquote),          &k.cg(ExtDelete));
    add_combo (&k, &k.cg(Backquote).m(shift), &k.cg(Backquote).m(shift));
    add_combo (&k, &k.cg(Backquote).m(caps),  &k.cg(Backquote));
    add_combo (&k, &k.cg(Backquote).m(lalt),  &k.cg(F20));
    //add_combo (*k, &k.cg(Backquote).m(ralt),  &k.cg(Backquote).m(shift));
    // ^^ not strictly necessary as cb composition now defaults to this, but also useful to see here for reference


    // setup tab .. caps-as-ctrl for caps-tab switching, incl for ctrl-shift-tab .. also ralt-tab for shift-tab
    let ks = k.ks.clone();
    let cb : AF = Arc::new (move || {
        if ks.caps_down.check() {
            ks.in_managed_ctrl_down_state.set_if_clear();
            ks.lctrl.ensure_active();  // this enables caps-as-ctrl for caps-tab switching
            // ^^ we're not gonna release ctrl immediately, but keep track and release when caps is released
            press_release(Tab)
        }
    } );
    add_bare_af_combo (&k, &k.cg(Tab).m(caps), cb.clone());
    add_bare_af_combo (&k, &k.cg(Tab).m(caps).m(shift), wrapped_action(LShift, cb.clone()));
    // ^^ this enables the natural ctrl-shift-tab to do backwards tablist nav
    // .. note that we do 'bare' here because caps-tab is in managed ctrl state, and we dont wana get wrapped w ctrl inactive guards here

    // we'll also need to put these in combos wit the managed-ctrl-state already active (as its among combo bits now)
    add_bare_af_combo (&k, &k.cg(Tab).m(caps).s(mngd_ctrl_dn), cb.clone());
    add_bare_af_combo (&k, &k.cg(Tab).m(caps).m(shift).s(mngd_ctrl_dn), wrapped_action(LShift, cb.clone()));

    // lets add exlicit support for arrow keys during ctrl tab (default doesnt check for active/inactive guards)
    add_bare_af_combo (&k, &k.cg(Left ).m(caps).s(mngd_ctrl_dn), base_action(Left ));
    add_bare_af_combo (&k, &k.cg(Right).m(caps).s(mngd_ctrl_dn), base_action(Right));
    add_bare_af_combo (&k, &k.cg(Up   ).m(caps).s(mngd_ctrl_dn), base_action(Up   ));
    add_bare_af_combo (&k, &k.cg(Down ).m(caps).s(mngd_ctrl_dn), base_action(Down ));

    // and finally for ralt-as-shift support for caps-tabbing too
    add_bare_af_combo (&k, &k.cg(Tab).m(caps).m(ralt),                 wrapped_action(LShift, cb.clone()));
    add_bare_af_combo (&k, &k.cg(Tab).m(caps).m(ralt).s(mngd_ctrl_dn), wrapped_action(LShift, cb.clone()));




    // setup space key .. ralt-space as enter, caps-space as ctrl-space, caps-lalt-space as alt-enter for intellij
    add_combo (&k, &k.cg(Space).m(ralt),         &k.cg(Enter));
    add_combo (&k, &k.cg(Space).m(caps).m(lalt), &k.cg(Enter).m(lalt));
    //add_combo (&k, &k.cg(Space).m(caps),         &k.cg(Space).m(ctrl));
    // ^^ not strictly necessary as cb composition now defaults to this, but also useful to see here for reference



    // win-m by default minimized all windows .. we just want to disable it
    add_cnsm_bare_af_combo (&k, k.cg(M).m(lwin), no_action());

    // win-i should start irfanview
    add_cnsm_bare_af_combo (&k, k.cg(I).m(lwin), Arc::new (|| start_irfanview()));

    // win-n should start chrome-incognito
    add_cnsm_bare_af_combo (&k, k.cg(N).m(lwin), Arc::new (|| start_chrome_incognito()));

    // we'll setup win-C (and caps-alt-C) to quickly bring up chrome Tabs-Outliner via switche Alt-F24 hotkey
    add_combo (&k, k.cg(C).m(lwin),         &k.cg(F24));      // win-c -> alt-F24
    add_combo (&k, k.cg(C).m(caps).m(lalt), &k.cg(F24));      // caps-lalt-c -> alt-F24 .. one-handed


    // in cur laptop, Fn-F6/F7 do brightness, but at +10 incrs .. set them to do small incrs with win combos
    add_cnsm_bare_af_combo (&k, &k.cg(F6).m(lwin), Arc::new (|| incr_brightness(-1)));
    add_cnsm_bare_af_combo (&k, &k.cg(F7).m(lwin), Arc::new (|| incr_brightness(1)));

    // might as well do that for alt as well, since we use alt for most other such shortcuts
    add_cnsm_bare_af_combo (&k, &k.cg(F6).m(lalt), Arc::new (|| incr_brightness(-1)));
    add_cnsm_bare_af_combo (&k, &k.cg(F7).m(lalt), Arc::new (|| incr_brightness(1)));

    // actually, since we use alt-2/3 as vol down/up, might as well also set win-2/3 for brightness down/up
    add_cnsm_bare_af_combo (&k, &k.cg(Numrow_2).m(lwin), Arc::new (|| incr_brightness(-1)));
    add_cnsm_bare_af_combo (&k, &k.cg(Numrow_3).m(lwin), Arc::new (|| incr_brightness(1)));

    // alt-2 is vol down, alt-3 is vol up
    add_combo (&k, &k.cg(Numrow_2).m(lalt),  &k.cg(VolumeDown));
    add_combo (&k, &k.cg(Numrow_3).m(lalt),  &k.cg(VolumeUp));

    // alt-f1 play/pause, caps-f1 toggle mute, base-case alt-F21 for switche caller, ralt for actual F1
    add_combo (&k, &k.cg(F1),         &k.cg(F21));
    add_combo (&k, &k.cg(F1).m(ralt), &k.cg(F1));
    add_combo (&k, &k.cg(F1).m(caps), &k.cg(VolumeMute));
    add_combo (&k, &k.cg(F1).m(lalt), &k.cg(MediaPlayPause));


    // want al-f2 for next with some initial skip .. we'll use caps-alt-f2 for prev, so we'll set it up for both
    // note that our mechanism for wrapping alt-state restoring guards operates via AFs, hence setting those up (instead of fns)

    // skips work by alt-ctrl-volUp, so gotta wrap with alt_active_action
    fn media_skips_action (n_skips:u32, ks:&KrS) -> AF {
        ks.lalt.active_action ( Arc::new ( move || {
            (0 .. n_skips) .into_iter() .for_each (|_| { ctrl_press_release(VolumeUp) });
    } ) ) }
    // ^^ gives an AF with specified number of skips

    // media next key shouldnt have alt on it, so should use alt_inactive-action .. (esp given we're on alt combo)
    let ks = k.ks.clone();
    let media_next_action =  k.ks.lalt.inactive_action ( Arc::new ( move || {
        if !ks.caps_down.check() { press_release(MediaNextTrack) }
        else { press_release(MediaPrevTrack) }
        let ks = ks.clone();  // clone again to allow moving into spawned thread closure
        spawn ( move || { sleep(time::Duration::from_millis(2000));  media_skips_action(3,&ks)(); } );
    } ) );

    // al-f2 for next with some initial skip
    add_cnsm_bare_af_combo (&k, &k.cg(F2).m(lalt),         media_next_action.clone());
    add_cnsm_bare_af_combo (&k, &k.cg(F2).m(caps).m(lalt), media_next_action);

    // alt-f3 for skip forward a bit
    add_cnsm_bare_af_combo (&k, &k.cg(F3).m(lalt), media_skips_action(1, &k.ks));



    // escape is just escape, but we just want it to do press-release immediately (so switche is faster)
    add_combo (&k, &k.cg(Escape), &k.cg(Escape));

    // use the apps key to send shift-escape .. or caps-escape too
    add_combo (&k, &k.cg(Apps),           &k.cg(Escape).m(shift));
    add_combo (&k, &k.cg(Escape).m(caps), &k.cg(Escape).m(shift));

    // similar to backquote, lets also add alt-1 for switche next-win (via its Alt-F20 hotkey)
    add_combo (&k, &k.cg(Numrow_1).m(lalt), &k.cg(F20));



    // caps-lalt-F for full screen ..(but from bulk mapping above, caps-f should still give ctrl-f)
    //add_combo (&k, &k.cg(F).m(caps).m(lalt), &k.cg(F11));
    // ^^ no longer available as we made F a caret mode key, and set caps-alt-F to alt-F instead

    // 'w' should have caps-ctrl mapping, but when w/ alt, send alt-f4 (to close all-tabs, windows etc)
    //add_combo (&k, &k.cg(W).m(caps).m(shift), &k.cg(F4).m(alt));
    // ^^ note: initially we wanted this with caps-shift-w, but turns out (at least on my kbd, turns out caps+shift+[F1, 2, w, s, x]
    // >  dont produce any key event at the hook at all .. nothing .. its like the keyboard driver not sending those out
    // funnily enough, there's a bunch of complaints about specifically those keys for dell/hp laptops .. looks like hardware
    // >  appears to be a common kbd pcb layout issue .. heres from 2007: (https://www.joachim-breitner.de/blog/250-Shift-Caps-2)
    // sooo .. to makeup, we'll do alt-caps-w do the alt-f4 business instead
    add_combo (&k, &k.cg(W).m(caps).m(lalt), &k.cg(F4).m(lalt));




    // filling out l2 actions (incl w caps-alt combos)
    /* l2-setup config summary:
     - only j/k for left/right get f-for-word-nav mode speedup (native word nav by sending ctrl)
     - those and i/comma for up/down get r-for-double-speed nav mode (2x nav) .. i/comma get that for f too
     - h/l/u/m for home/end/pgup/pgdown get no speedup modes
     - e/d do sel/del modes, and those can be freely combined with the f/r word/fast nav modes
     - in del mode, left/home/up/pgup get ExtBackspace, right/end/down/pgdn get ExtDelete
     - in del mode, left/right do direct bksp/del, but others get select then bksp/del
     */

    pub type AFG = fn(Key) -> AF ;

    pub fn setup_l2_key (k:&Krusty, key:Key, l2k:Key, dk:Key, wafg:AFG, fafg:AFG, del_via_sel:bool) {
        // register nav actions for normal-nav, word-nav, and fast-nav modes
        add_bare_af_combo (k, k.cg(key).m(caps),         base_action(l2k));
        add_bare_af_combo (k, k.cg(key).m(caps).s(word), wafg(l2k));
        add_bare_af_combo (k, k.cg(key).m(caps).s(fast), fafg(l2k));

        // selection actions are via wrapping those with shift press-release
        add_bare_af_combo (k, k.cg(key).m(caps).s(sel),         wrapped_action (LShift, base_action(l2k)));
        add_bare_af_combo (k, k.cg(key).m(caps).s(sel).s(word), wrapped_action (LShift, wafg(l2k)));
        add_bare_af_combo (k, k.cg(key).m(caps).s(sel).s(fast), wrapped_action (LShift, fafg(l2k)));

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

        add_bare_af_combo (k, k.cg(key).m(caps).s(del),         dna);
        add_bare_af_combo (k, k.cg(key).m(caps).s(del).s(word), dwa);
        add_bare_af_combo (k, k.cg(key).m(caps).s(del).s(fast), dfa);
    }

    // filling out l2 actions
    setup_l2_key ( &k,  J,     ExtLeft,   Backspace,   ctrl_action,   fast_action,   false );
    setup_l2_key ( &k,  K,     ExtRight,  ExtDelete,   ctrl_action,   fast_action,   false );
    setup_l2_key ( &k,  I,     ExtUp,     Backspace,   fast_action,   fast_action,   true  );
    setup_l2_key ( &k,  Comma, ExtDown,   ExtDelete,   fast_action,   fast_action,   true  );
    setup_l2_key ( &k,  H,     ExtHome,   Backspace,   base_action,   base_action,   true  );
    setup_l2_key ( &k,  L,     ExtEnd,    ExtDelete,   base_action,   base_action,   true  );
    setup_l2_key ( &k,  U,     ExtPgUp,   Backspace,   base_action,   base_action,   true  );
    setup_l2_key ( &k,  M,     ExtPgDn,   ExtDelete,   base_action,   base_action,   true  );






    // idk what layer this even is, but since we're so used to j/k nav keys, we'll set them up for ctrl-alt nav s/ caps-shift
    // .. these are useful for various second order nav in IDEs etc .. e.g between tabs etc .. no fancy speedups etc here
    add_combo (&k, &k.cg(J    ).m(caps).m(shift),   &k.cg(ExtLeft ).m(lctrl).m(lalt));
    add_combo (&k, &k.cg(K    ).m(caps).m(shift),   &k.cg(ExtRight).m(lctrl).m(lalt));
    add_combo (&k, &k.cg(I    ).m(caps).m(shift),   &k.cg(ExtUp   ).m(lctrl).m(lalt));
    add_combo (&k, &k.cg(Comma).m(caps).m(shift),   &k.cg(ExtDown ).m(lctrl).m(lalt));
    add_combo (&k, &k.cg(H    ).m(caps).m(shift),   &k.cg(ExtHome ).m(lctrl).m(lalt));
    add_combo (&k, &k.cg(L    ).m(caps).m(shift),   &k.cg(ExtEnd  ).m(lctrl).m(lalt));
    add_combo (&k, &k.cg(U    ).m(caps).m(shift),   &k.cg(ExtPgUp ).m(lctrl).m(lalt));
    add_combo (&k, &k.cg(M    ).m(caps).m(shift),   &k.cg(ExtPgDn ).m(lctrl).m(lalt));







    // then the caps-win combo l3 actions :

    // caps-win-U should vert-max (via shift-win-up) if not already, or else restore window from vert-max
    add_cnsm_bare_af_combo (&k, &k.cg(U).m(caps).m(lwin), Arc::new (|| win_fgnd_toggle_vertmax()));
    // caps-win-m should maximize (via win-m) if not, else restore from max
    add_cnsm_bare_af_combo (&k, &k.cg(M).m(caps).m(lwin), Arc::new (|| win_fgnd_toggle_max()));
    // caps-win-n should minimize (via win-arrrowDown)
    add_cnsm_bare_af_combo (&k, &k.cg(N).m(caps).m(lwin), Arc::new (|| win_fgnd_min()));

    // caps-win-[j,k,i,,] should  move window [left, right, top, bottom] respectively
    add_cnsm_bare_af_combo (&k, &k.cg(J    ).m(caps).m(lwin), Arc::new (|| win_fgnd_move(-80, 0) ));
    add_cnsm_bare_af_combo (&k, &k.cg(K    ).m(caps).m(lwin), Arc::new (|| win_fgnd_move(80, 0) ));
    add_cnsm_bare_af_combo (&k, &k.cg(I    ).m(caps).m(lwin), Arc::new (|| win_fgnd_move(0, -50) ));
    add_cnsm_bare_af_combo (&k, &k.cg(Comma).m(caps).m(lwin), Arc::new (|| win_fgnd_move(0, 50) ));

    // caps-win-[h,;,.,o] should stretch window [narrower, wider, shorter, taller] respectively
    // .. note that caps-win-L gets unavoidably captured by windows to lock laptop, hence the semicolon
    add_cnsm_bare_af_combo (&k, &k.cg(H     ).m(caps).m(lwin), Arc::new (|| win_fgnd_stretch(-30, 0) ));
    add_cnsm_bare_af_combo (&k, &k.cg(O     ).m(caps).m(lwin), Arc::new (|| win_fgnd_stretch(0, -30) ));
    add_cnsm_bare_af_combo (&k, &k.cg(Period).m(caps).m(lwin), Arc::new (|| win_fgnd_stretch(0, 30) ));
    //add_af_combo (&k, &k.cg(L).m(caps).m(lwin),  &k.cg_af (Arc::new (|| win_fgnd_stretch(30, 0) )));
    // ^^ any win-L combo is hardcoded at OS level to lock machine, cant override that, so we'll make semicolon do that instead
    add_cnsm_bare_af_combo (&k, &k.cg(Semicolon).m(caps).m(lwin), Arc::new (|| win_fgnd_stretch(30, 0) ));


    // some additional caps-win combos
    // caps-win-c being used to launch winmerge diff from last two clipboard entries
    add_cnsm_bare_af_combo (&k, &k.cg(C).m(caps).m(lwin), Arc::new (|| start_winmerge_clipboard()));
    // gaah we'll just throw in iDEA diff for drag-drop diffing (just coz winmerge doesnt do dark mode)
    //add_af_combo (&k, &k.cg(C).m(lwin), &k.cg_af (Arc::new (|| start_idea_diff() )));
    // ^^ cant do from here, turns out idea diff from cmd line can ONLY be opened with two files pointed, unlike empty from Idea shortcut!





    // then we can add in any l4 quick-keys shortcuts combos we want
    // note: there are 3 quick-keys modes (qks, qks2, qks3) on keys (q, 2, 3) respectively! .. all are pretty ergonomic!

    // first we'll just add some qev for what we set as win combos .. should be tmp just to try things out
    //add_af_combo (&k, &k.cg(I).m(caps).s(qkeys), &k.cg_af (action (start_irfanview)));
    //add_af_combo (&k, &k.cg(N).m(caps).s(qkeys), &k.cg_af (action (start_chrome_incognito)));

    // we'll add some nav overloading for IDES on qks2 for starters!!
    // and this one to travel along bookmarks in IDE
    add_combo (&k, &k.cg(I    ).m(caps).s(qks2), &k.cg(ExtUp  ).m(lalt).m(lctrl).m(lshift));
    add_combo (&k, &k.cg(Comma).m(caps).s(qks2), &k.cg(ExtDown).m(lalt).m(lctrl).m(lshift));
    // and to toggle a bookmark at the current caret location
    add_combo (&k, &k.cg(U).m(caps).s(qks2), &k.cg(F11).m(lctrl).m(lshift));
    // and to bring up the bookmarks viewer
    add_combo (&k, &k.cg(K).m(caps).s(qks2), &k.cg(F11).m(lshift));



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
    add_combo (&k, &k.cg(F12).m(win).m(ctrl), &k.cg(F19).m(shift)); // try rctrl-win

    add_combo (&k, &k.cg(F10).m(win).m(shift), &k.cg(F19)); // .. try rshift ..

    add_combo (&k, &k.cg(Backslash).m(ctrl), &k.cg(F19));  // try rctrl
    add_combo (&k, &k.cg(Backslash).m(caps), &k.cg(F19));

    //add_combo (&k, &k.cg(Numrow_0).m(ctrl), &k.cg(F19).m(lctrl));
    //add_combo (&k, &k.cg(Numrow_9).m(ctrl), &k.cg(F19).m(ctrl));
    add_combo (&k, &k.cg(Numrow_8).m(ctrl), &k.cg(F19).m(shift));  // try rctrl
    add_combo (&k, &k.cg(Numrow_0).m(ctrl), &k.cg(F19).m(ctrl));
    //add_combo (&k, &k.cg(Numrow_7).m(win),  &k.cg(F19).m(shift));
    add_combo (&k, &k.cg(Numrow_6).m(ctrl), &k.cg(F19).m(lctrl));   // try rctrl
    //add_combo (&k, &k.cg(Numrow_5).m(ctrl).m(win), &k.cg(F19).m(lctrl));
    //add_combo (&k, &k.cg(Numrow_4).m(ctrl).m(win), &k.cg(F19).m(lshift));

    add_combo (&k, &k.cg(Numrow_9).m(lctrl), &k.cg(F19).m(lshift));
    add_combo (&k, &k.cg(Numrow_7).m(lalt ), &k.cg(F19).m(lshift));
    add_combo (&k, &k.cg(Numrow_5).m(lwin ), &k.cg(F19).m(lshift));
    */
    //add_combo (&k, &k.cg(F12).m(alt), &k.cg(F19).m(lshift));
    //add_combo (&k, &k.cg(Numrow_7).m(lalt ), &k.cg(F19).m(lshift));




    // finally bind everything from action-maps !!
    bind_all_from_action_maps (&k);



    // and we'll put any direct special key setups after all this
    // > which is for safety in case anything above accidentally included those, although ofc we dont want to rely on that!
    setup_direct_binding_keys (&k);


    // note: the handle_input_events to start the whole shebang should be being called somewhere in main after this setup

}

