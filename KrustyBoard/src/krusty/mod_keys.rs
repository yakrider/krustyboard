
use std::{time};
use std::thread::{sleep, spawn};
use std::ops::Deref;
use std::sync::{Arc, RwLock};

use once_cell::sync::OnceCell;
//use strum::IntoEnumIterator;
use strum_macros::EnumIter;


use crate::{ KbdEvCbFn_InlineCb_T, KbdEventCallbackEntry, KbdEventCallbackFnType::*,
             KbdEventCbMapKeyType::*, EventPropagationDirective::*, KbdEvCbComboProcDirective::* };

use crate::krusty::{key_utils, Key, Flag, KrS, AF};
use crate::krusty::combo_maps::{COMBO_STATE__MOD_KEY_BITS_N, ComboStatesBits_ModKeys};
use crate::krusty::mod_keys::ModKey::*;





# [ allow (non_camel_case_types) ]
# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter) ]
/// all the supported modifier-keys (incl some w incomplete impl like rwin)
pub enum ModKey {
    caps, lalt, ralt, lwin, rwin, lctrl, rctrl, lshift, rshift, alt, win, ctrl, shift
    // ^^ the last four (Alt/Win/Ctrl/Shfit) are intended to imply either of the L/R versions
    // .. and those are only for use during combo specification for l/r/lr expansion .. they dont exist in combo-bitmaps
}
pub type MK = ModKey;





# [ derive (Debug) ]
/// mostly a wrapper that holds the caps-lock key and its impl as the base for most l2/l3 functionality
pub struct CapsModKey {
    // we've defined this just to be consistent with other mod-keys, but its really just a wrapper for caps key!
    _private : (),
    pub key  : Key,
    pub down : Flag,
}

# [ derive (Debug, Clone) ]
/// arc-wrapped CapsModKey for cheap passing between threads etc
pub struct CMK (Arc<CapsModKey>);

impl Deref for CMK {
    type Target = CapsModKey;
    fn deref(&self) -> &CapsModKey { &self.0 }
}
// NOTE rest of the impl for this further down





# [ derive (Debug) ]
/// TrackedModKeys only get key-down-state tracked, but no syncing against active-state externally (currently only for ralt)
pub struct TrackedModKey {
    // we'll use this for modifier keys that we want to simply track down state, w/o attempting to track or sync their external active states
    // again, motivation for making this separate is to share fumctionality implementations
    _private : (),
    pub key  : Key,
    pub down : Flag,
}

# [ derive (Debug, Clone) ]
/// Arc wraps TrackedModKey functionality for easier cloning/sharing
pub struct TMK (Arc<TrackedModKey>);

impl Deref for TMK {
    type Target = TrackedModKey;
    fn deref(&self) -> &TrackedModKey { &self.0 }
}
// NOTE rest of the impl for this further down





// note that we dont want this cloneable to avoid cloning all underlying, we'll work with it Arc-wrapped instead
# [ derive (Debug) ]
/// Synced-Modifier-Key tracks modifier key down-states as well as externally active state (used for lalt/lwin/lctrl/rctrl/lshift/rshift)
pub struct SyncdModKey {
    // this struct holds flags required for a outside-state syncd tracking of a modifier key (we only do this for alt and win)
    // the alt and win keys are now tracked identically and so doing this helps us avoid duplication of that bunch of related code
    // these flags track whether the key is down, if it has been sent out (active), and if we should release it masked (consumed)
    _private     : (),
    pub key      : Key,
    pub down     : Flag,
    pub active   : Flag,
    pub consumed : Flag,

    pub pair     : Arc<RwLock<Option<SMK>>>, // backlink to populate to the left/right counterpart if desired
    // ^^ not the most ideal to have these cyclic backlinks, but we'll populate it at krusty creation time, so is reliable anyway
}

# [ derive (Debug, Clone) ]
/// Arc wrapped Synced-Modifer-Key for cheap cloning/sharing
pub struct SMK (Arc<SyncdModKey>);
// ^^ wrapping like this lets us impl functionality direclty on this cheaply clonable new-type to pass around
// .. the alternate of making SyncdModKey clonable would be more costly as it would clone each of the underlying flags

// and we'll impl deref on it so we dont have to keep using smk.0.<bla-bla> etc .. and its kosher as its underlying is Arc anyway
impl Deref for SMK {
    type Target = SyncdModKey;
    fn deref(&self) -> &SyncdModKey { &self.0 }
}
// NOTE:  the impl for the actual SMK functionality is further down





# [ derive (Debug) ]
/// holds representation for all modifier keys together, interlinked functionality etc impld here
pub struct ModifierKeys {

    // caps will be tracked for internal reference, and we'll assume we'll ALWAYS operate with caps-lock off
    // we'll also track all mod keys as syncd-modifier-keys, where we track the phys and logical states, as well as whether to mask their release
    // this allows us to add any composition of these in combos with any other key incl other mod keys while keeping internal/external models accurate

    // except r-alt which we track, but dont try to keep synced w external state .. (as we completely disable it every going out!)
    // (previously, there used to be ctrl/shift here that were pass through tracked like ralt, but they got upgraded to full SMK treatment!)

    // note also that we're not tracking rwin that some machines (not mine) have .. could easily add that later if need be

    _private   : (),
    // capslock tracking
    pub caps   : CMK,
    // ralt is only tracked, but not synced
    pub ralt   : TMK,
    // the rest are synced mode keys (rwin not added so far as its not in my machine)
    pub lalt   : SMK,
    pub lwin   : SMK,
    pub lctrl  : SMK,
    pub rctrl  : SMK,
    pub lshift : SMK,
    pub rshift : SMK,

}

# [ derive (Debug, Clone) ]
/// Arc wraps our collection of ModiferKeys for cheap cloning/sharing
pub struct MKS (Arc<ModifierKeys>);

impl Deref for MKS {
    type Target = ModifierKeys;
    fn deref(&self) -> &ModifierKeys { &self.0 }
}





/// holds representation for all modifier keys together, interlinked functionality etc impld here
impl MKS {

    pub fn new() -> MKS {
        MKS ( Arc::new ( ModifierKeys {
            _private : (),
            // caps is a special singleton for itself
            caps   : CMK::instance(),
            // ralt is the only TMK left by now, for which we track but dont keep it sync
            ralt   : TMK::new(Key::RAlt),
            // the rest are SMKs (synced mod keys) .. (rwin excluded for now since I dont have it in this machine to test with)
            lalt   : SMK::new(Key::LAlt),
            lwin   : SMK::new(Key::LWin),
            lctrl  : SMK::new(Key::LCtrl),
            rctrl  : SMK::new(Key::RCtrl),
            lshift : SMK::new(Key::LShift),
            rshift : SMK::new(Key::RShift)
        } ) )
    }

    /// NOTE: this static will be our source of ordering for the mod-keys in the combo-mod-keys-state bitmap!!
    pub fn static_combo_bits_mod_keys() -> [ModKey; COMBO_STATE__MOD_KEY_BITS_N] {
        // Note that there are bits in combo-bitmap only for physical keys
        // (i.e. it excludes the virtual l/r agnostic enum-vals used during combo construction)
        static COMBO_STATE_BITS_MOD_KEYS: [ModKey; COMBO_STATE__MOD_KEY_BITS_N] = {
            [caps, lalt, ralt, lwin, rwin, lctrl, rctrl, lshift, rshift]
        };
        COMBO_STATE_BITS_MOD_KEYS
    }

    /// NOTE: the ordering in this static tuples array will be used to expand the l/r agnostic combo keys into their specialized versions
    // note that this order is relied on elsewhere, plus we want the fn composition to have ctrl innermost (if we use ctrl masking, which we dont anymore)
    // .. and the successive wrapping means the first one on this list is innermost .. and so its state will be updated with others masking
    // also, we'd rather have win at the end here (and wrap outermost), because that has a spawn and delay in reactivation .. still ok but still
    pub fn static_lr_mods_triplets () -> [(MK,MK,MK);4] {
        static LR_MODS_TRIPLETS : [(MK,MK,MK);4] = [
            (ctrl,  lctrl,  rctrl),
            (shift, lshift, rshift),
            (alt,   lalt,   ralt),
            (win,   lwin,   rwin),
        ];
        LR_MODS_TRIPLETS
    }

    /// this will give the SMKs in order defined at the static LR_MODS_TRIPLETS so they can be matched up at runtime
    pub fn lrmk_smks (&self) -> [&SMK;4] {
        [&self.lctrl, &self.lshift, &self.lalt, &self.lwin]
    }
    pub fn mod_smk_pairs (&self) -> [(MK, &SMK);6] { [ // note that ralt is TMK not SMK (and doesnt to activate/inactivate etc)
        (lwin,   &self.lwin  ), (lalt,   &self.lalt  ),
        (lctrl,  &self.lctrl ), (rctrl,  &self.rctrl ),
        (lshift, &self.lshift), (rshift, &self.rshift),
    ] }
    pub fn mk_flag_pairs (&self) -> [(MK,Option<&Flag>);9] { [
        (caps,   Some(&self.caps.down)),
        (lalt,   Some(&self.lalt.down)),
        (ralt,   Some(&self.ralt.down)),
        (lwin,   Some(&self.lwin.down)),
        (rwin,   None),
        (lctrl,  Some(&self.lctrl.down)),
        (rctrl,  Some(&self.rctrl.down)),
        (lshift, Some(&self.lshift.down)),
        (rshift, Some(&self.rshift.down)),
        // ^^ note again, that for the combo bitmap construction, the l/r agnostic keys should have been expanded out and eliminated
    ] }

    pub fn get_cur_mod_keys_states_bitmap(&self) -> ComboStatesBits_ModKeys {
        self.mk_flag_pairs() .map (|(_,fgo)| fgo.filter(|fg| fg.check()).is_some())
    }
    pub fn make_combo_mod_keys_states_bitmap(mod_keys:&[ModKey]) -> ComboStatesBits_ModKeys {
        MKS::static_combo_bits_mod_keys() .map (|mk| mod_keys.contains(&mk))
    }

    pub fn some_shift_down (&self) -> bool { self.lshift.down.check() || self.rshift.down.check() }
    pub fn some_ctrl_down  (&self) -> bool { self.lctrl.down.check()  || self.rctrl.down.check() }
    pub fn some_alt_down   (&self) -> bool { self.lalt.down.check() } // ralt is disabled for all purposes
    pub fn some_win_down   (&self) -> bool { self.lwin.down.check() } // we've not impld rwin so far

    pub fn unstick_all (&self) {
        [   &self.lalt, &self.lwin, &self.lctrl,
            &self.rctrl, &self.lshift, &self.rshift
        ] .into_iter() .for_each (|smk| {
            smk.key.release(); smk.down.clear(); smk.active.clear();
        });
        self.ralt.down.clear(); self.ralt.key.release(); // this is the only TMK, others above were SMK

        if self.caps.key.is_toggled() { key_utils::press_release(self.caps.key) }
        self.caps.down.clear();
    }

    pub fn setup_tracking (&self, ksr:&KrS) {
        // setup capslock, we'll completely disable it other than for krusty use
        self.caps.setup_tracking (ksr);

        // setup tracking for right-alt .. its completely blocked but tracked, we use it as shifts and combos
        // (ctrl/shift etc used to be like this, but they have been promoted to full synced-mod-key tracking since)
        self.ralt .setup_tracking (true);      // for ralt can setup w doBlock=true

        // lalt and lwin are set as syncd-tracked-modifier-key with special (but identical) impl .. more details under SyncdModKey impl
        // (we're not doing rwin simply coz its not in my machine .. and ofc ralt is used in fully disabled mode above)
        self.lalt.setup_tracking (ksr);
        self.lwin.setup_tracking (ksr);

        // and we also updated both L/R of both shift and ctrl to do identical full-synced-tracked management like for lalt/lwin
        self.lctrl.setup_tracking (ksr);
        self.rctrl.setup_tracking (ksr);
        self.lshift.setup_tracking (ksr);
        self.rshift.setup_tracking (ksr);
    }

}





/// impl for the caps-lock key specific functionality
impl CMK {

    // ^^ CMK : Caps-Modifier-Key type .. basically tracks caps state and sets up caps as the global Layer-2/3/qks etc modifier key
    // (other mod-keys are via SMK (Syncd-Tracked modifier key: alt/win)  or TMK (Tracked modifier keys: ralt/lctrl/rctrl/lshift/rshift))

    pub fn instance () -> CMK {
        // note that since ofc there's only one caps key, we'll set this up as singleton (unlike for the TMKs and SMKs below)
        static INSTANCE: OnceCell<CMK> = OnceCell::new();
        INSTANCE .get_or_init (||
            CMK ( Arc::new ( CapsModKey {
                _private : (),
                key      : Key::CapsLock,
                down     : Flag::default(),
            } ) )
        ).clone()
    }

    fn handle_key_down (&self, ks:&KrS) {
        // note that for caps, we completely block it from ever being sent up, and just manage internally
        if !ks.mod_keys.caps.down.check() {
            // capslock can come as repeats like other mod keys .. this was a fresh one
            self.down.set();
            ks.is_wheel_spin_invalidated.set();
            // lets notify the synced tracked mod keys, so they can invalidate/release themselves
            ks.mod_keys.mod_smk_pairs() .iter() .for_each (|(_,smk)| smk.process_caps_down());
        }
        if ks.mouse_left_btn_down.check() && !ks.in_managed_ctrl_down_state.check() {
            ks.in_managed_ctrl_down_state.set();
            ks.mod_keys.lctrl.ensure_active();
        }
    }

    fn handle_key_up (&self, ks:&KrS) {
        ks.mod_keys.caps.down.clear();
        ks.is_wheel_spin_invalidated.set();
        if ks.in_managed_ctrl_down_state.check() {
            ks.in_managed_ctrl_down_state.clear();
            if !ks.mod_keys.lctrl.down.check() { ks.mod_keys.lctrl.ensure_inactive() }
        }
        // the following isnt strictly necessary, but useful in case some keyup falls through
        ks.mode_states.clear_flags();
        // lets also notify the alt/win tracked mod keys so they can re-enable themselves if applicable
        ks.mod_keys.mod_smk_pairs() .iter() .for_each (|(_,smk)| smk.process_caps_release());
    }

    pub fn setup_tracking (&self, ksr:&KrS) {
        // note that for caps, we completely block it from ever being sent up, and just manage internally
        if Key::CapsLock.is_toggled() { // toggle off first if necessary (to clear key light)
            key_utils::press_release (Key::CapsLock);
        }
        let ks = ksr.clone();
        Key::CapsLock .bind ( KeyDownCallback, KbdEventCallbackEntry {
            event_prop_directive: EventProp_Stop,
            combo_proc_directive: ComboProc_Disable,
            cb : KbdEvCbFn_InlineCallback {
                cb : Arc::new ( move |_| { ks.mod_keys.caps.handle_key_down(&ks); EventProp_Stop } )
            },
        } );
        let ks = ksr.clone();
        Key::CapsLock .bind ( KeyUpCallback, KbdEventCallbackEntry {
            event_prop_directive: EventProp_Stop,
            combo_proc_directive: ComboProc_Disable,
            cb : KbdEvCbFn_InlineCallback {
                cb: Arc::new ( move |_| { ks.mod_keys.caps.handle_key_up(&ks); EventProp_Stop } )
            },
        } );
    }

}





/// impl for Tracked-Modifier-Key functionality (currently only for R-Alt)
impl TMK {

    // ^^ TMK : Tracked-Modifier-Key type .. unlike the SMK, this only tracks the physical (is down) state of the modifier key
    // we should currently be doing this for left/right ctrl/shift, as well as right-alt

    pub fn new (key:Key) -> TMK {
        TMK ( Arc::new ( TrackedModKey {
            _private : (),
            key : key,
            down : Flag::default(),
        } ) )
    }

    pub fn setup_tracking (&self, do_block:bool) {
        // the setup for these is mostly just tracking their down state ..
        // however, we will also disable repeats, mostly for no functional reason than to ease looking at keystreams
        let tmk = self.clone();
        let (ev_prop_drctv, cb) = if do_block {
            let cb : KbdEvCbFn_InlineCb_T = Arc::new ( move |_| { tmk.down.set(); EventProp_Stop } );
            (EventProp_Stop, cb)
        } else {
            let cb : KbdEvCbFn_InlineCb_T = Arc::new (move |_| {
                if tmk.down.check() { EventProp_Stop }
                else { tmk.down.set(); EventProp_Continue }
            });
            (EventProp_Continue, cb)
        };
        self.key .bind ( KeyDownCallback, KbdEventCallbackEntry {
            event_prop_directive: ev_prop_drctv,
            combo_proc_directive: ComboProc_Disable,
            cb : KbdEvCbFn_InlineCallback {cb},
        } );

        // for key-ups, we simply update flag and let them go through
        let ev_prop_drctv = if do_block { EventProp_Stop } else { EventProp_Continue };
        let (tmk, epd) = (self.clone(), ev_prop_drctv.clone());
        let cb = Arc::new ( move |_| { tmk.down.clear(); epd } );
        self.key .bind ( KeyUpCallback, KbdEventCallbackEntry {
            event_prop_directive: ev_prop_drctv,
            combo_proc_directive: ComboProc_Disable,
            cb : KbdEvCbFn_InlineCallback {cb},
        } );
    }

}





/// impl for Synced-Modifier-Key functionality .. (e.g. for lalt/lwin/lctrl/rctrl/lshift/rshift)
impl SMK {

    // ^^ SMK : Synced-Modifier-Key type .. tracks internal (is down), external (is active), and to-mask (is consumed) states
    // .. we should currently be doing this for left-alt and left-win identically (.. expanded out to ctrl/shift too)

    pub fn new (key:Key) -> SMK {
        SMK ( Arc::new ( SyncdModKey {
            _private : (),
            key      : key,
            down     : Flag::default(),
            active   : Flag::default(),
            consumed : Flag::default(),
            pair     : Arc::new(RwLock::new(None)),
        } ) )
    }


    pub fn link_pair (&self, smk:&SMK) {
        let _ = self.pair.write().unwrap() .insert (smk.clone());
    }


    // NOTE: given that kbds seem to have idiosyncrasies with what scancode vs vk codes they sent, we end up getting out of sync w
    // what keys with let through and what we simulate .. e.g in my machine lshift comes in vk while rshift comes sc and so sending our
    // vk shift doesnt clear it out .. so we've decided to just block everything and send our uniform up/down reports instead!

    // beyond that, we'll block repeats to keep code logic (and keystream inspections) manageable
    // we'll also track and update both physical and externally expressed states and try and keep our model always in sync w the outside
    // we'll also track if we've used up the press so we can mask its release later (for things like win/alt that trigger menus on release)

    // so goal here is, any presses with caps active, we suppress mod-key going outside
    // and since we can have caps come in after the mod-key is already down, we'll have to capture disparity states ..
    //  .. as well as restoring them when either caps/alt gets released etc
    // (plus, if we're down and caps goes down, we'll get notification below so we're enforcing the disabled state from both sides)

    pub fn handle_key_down (&self, ks:&KrS) {
        if ks.mod_keys.caps.down.check() {
            // caps is down, record alt being down if not already, but either way block it (so no change to alt-active state)
            self.down.set();
            self.consumed.clear();
        } else {
            if self.down.check() {
                // caps isnt down, but alt was, so its repeat .. we'll block it even if out-of-sync or its coming after combo caps release
            } else {
                // caps isnt down, and alt wasnt down, so record states and let it through
                self.down.set();
                self.active.set();
                self.consumed.clear();
                ks.is_wheel_spin_invalidated.set();
                let key = self.key;
                spawn (move || key.press());
        } }
    }

    fn handle_key_up (&self, ks:&KrS) {
        // if caps is pressed, or alt is already inactive (via masked-rel, press-rel etc), we block it
        // else if win was consumed, we release with mask, else we can actually pass it through unblocked
        // (note.. no more passing through of mod-keys, we'll instead send replacement ones if we need to (due to R/L sc-codes mismatch etc))
        self.down.clear();
        ks.is_wheel_spin_invalidated.set();
        if !self.active.check() || ks.mod_keys.caps.down.check() {
            // if inactive or caps-down we just suppress this keyup
        } else {
            if self.is_keyup_unified() && self.paired_down() {
                // for shift (w/ keyup state unified), ONLY send up a keyup if the other key isnt down .. so do nothing, not even clear active
            } else {
                let smk = self.clone();
                spawn ( move || {
                    smk.release_w_masking();  // this checks/updates flags too
                    if smk.is_keyup_unified() { // and for up-unified, try and clear the other too
                        smk.pair.read().unwrap() .iter() .for_each (|p| {
                            if !p.down.check() && p.active.check() { p.release_w_masking(); }
                    } ) }
                } );
        }  }
    }

    pub fn setup_tracking (&self, ksr:&KrS) {
        let (ks, smk) = (ksr.clone(), self.clone());
        self.key .bind ( KeyDownCallback, KbdEventCallbackEntry {
            event_prop_directive: EventProp_Stop,
            combo_proc_directive: ComboProc_Disable,
            cb : KbdEvCbFn_InlineCallback {
                cb: Arc::new ( move |_| { smk.handle_key_down(&ks); EventProp_Stop } )
            },
        } );
        let (ks, smk) = (ksr.clone(), self.clone());
        self.key .bind ( KeyUpCallback, KbdEventCallbackEntry {
            event_prop_directive: EventProp_Stop,
            combo_proc_directive: ComboProc_Disable,
            cb : KbdEvCbFn_InlineCallback {
                cb: Arc::new ( move |_| { smk.handle_key_up(&ks); EventProp_Stop } )
            },
        } );
    }



    pub fn process_caps_down (&self) {
        // we will immediately invalidate and clear any down mod-key found upon caps activation!
        // note that internally tracked physical is_down will continue to be down
        // note also that each of paired mod-keys will get their own notification too
        if self.down.check() && self.active.check() {
            self.consumed.set();
            let smk = self.clone();
            spawn ( move || smk.release_w_masking() ); // this will update flags too
        }
    }
    pub fn process_caps_release (&self) {
        // since we deactivate mod-keys on caps press, check to see if we want to reactivate them
        // note: we'll setup a delay for activation to allow for some sloppy combo releases etc
        // note also, that if inspecting in browser-key-events, this might appear unexpected coz browser does its own 'unifying'
        // .. so to check the logic here must use lower level key inspections like via ahk key history!!
        // plus if doing caps release while both shift down, on my machine even the raw events are wonky (no caps evnt until one releases!!)
        if self.down.check() {
            let smk = self.clone();
            spawn ( move || {
                sleep(time::Duration::from_millis(200));
                if smk.down.check() && !smk.active.check() {
                    smk.key.press(); smk.active.set(); smk.consumed.set();
            } } );
        }
    }


    // unassigned vks: 0x88-0x8F, 0x97-0x9F, 0xD8-0xDA, 0xE8 ..  undefined: 0x07, 0x0E-0x0F, 0x3A-0x40
    //fn mask (&self) -> Key { Key::Other(0xFF) }
    fn mask (&self) -> Key { Key::OtherKey(0x9A) }


    fn is_rel_masking   (&self) -> bool { self.key == Key::LWin || self.key == Key::RWin || self.key == Key::LAlt } // excluding RAlt
    fn is_rel_delaying  (&self) -> bool { self.key == Key::LWin || self.key == Key::RWin }
    fn is_keyup_unified (&self) -> bool { self.key == Key::LShift || self.key == Key::RShift }

    fn paired_down     (&self) -> bool { self.pair.read() .unwrap() .iter() .any (|p| p.down.check()) }
    fn paired_active   (&self) -> bool { self.pair.read() .unwrap() .iter() .any (|p| p.active.check()) }
    fn pair_any_active (&self) -> bool { self.active.check() || self.paired_active() }

    fn release_w_masking(&self) {
        // masking w an unassigned key helps avoid/reduce focus loss to menu etc for alt/win
        self.active.clear();
        if !self.is_rel_masking() || !self.consumed.check() { self.key.release(); }
        else { self.mask().press(); self.key.release(); self.mask().release(); }
    }

    fn reactivate        (&self) { self.active.set(); self.key.press(); }
    fn paired_reactivate (&self) { self.pair.read().unwrap() .iter() .for_each (|p| p.reactivate()) }

    pub fn ensure_inactive (&self) {
        // utility to ensure modkey is inactive regardless if held down
        // shouldnt really be necessary since there are action wrappers available to set/restore mod-key for any need at any mod-key state
        self.consumed.set();
        if self.active.check() { self.release_w_masking(); } // rel call will clear active flag too
    }
    pub fn ensure_active (&self) {
        // utility to get the mod out reliably whether its currently pressed or not, while keeping state tracking updated
        // this should really ONLY be necessary where we want the mod to be left hanging on until later .. e.g. to simulate alt-tab
        self.consumed.set();
        if !self.active.check() { self.active.set(); self.key.press(); }
    }


    // since the physical and effective state of these keys can differ and we manage internally, all press/releases should also be
    // >  guarded to ensure that our internal model is always in-sync with the outside state .. hence the util fns below
    // we'll also mark the down lalt/lwin as consumed and mask its release with ctrl so it doesnt activate menus/start-btn etc

    #[allow(dead_code)]
    fn do_w_mod (&self, key:Key, key_action:fn(Key)) {
        self.consumed.set();
        if self.active.check() { key_action(key) }
        else { self.key.press(); self.active.set(); key_action(key); self.release_w_masking(); }
    }
    #[allow(dead_code)]
    pub fn mod_press_release_guarded (&self, key:Key) { self.do_w_mod (key, key_utils::press_release) }
    // ^^ these direct guarding fns are left here if needed for complex actions later, but currently all is handled via action guards below


    /// all mod-actions mark the mod-down consumed too, but if its a no-key action (like brightness etc), wrap with this to mark consumed
    /// .. the consumed flag marks it to have its later release be masked with control to avoid activating win-menu etc
    pub fn keydn_consuming_action (&self, af:AF) -> AF {
        let smk = self.clone();
        Arc::new ( move || { smk.consumed.set(); af(); } )
    }


    /// use this to wrap actions when we want the mod-key to be ACTIVE in the combo .. can use for both self-mod-key combos or unrelated combos
    /// .. e.g. if setting up alt-X to send alt-win-y, we'd set lalt-mapping on Key::X as k.alt.active_action(k.win.active_on_key(Key::Y))
    pub fn active_action (&self, af:AF) -> AF {
        let smk = self.clone();
        Arc::new ( move || {
            smk.consumed.set();
            if smk.pair_any_active() { af() }
            else { smk.key.press(); af(); smk.release_w_masking(); }
        })
    }
    #[allow(dead_code)]
    pub fn active_on_key (&self, key:Key) -> AF { self.active_action (key_utils::base_action(key)) }
    // ^^ some sugar to make common things simpler
    


    /// use this to wrap actions ONLY when setting combos with this mod key itself AND we want the mod-key to be INACTIVE in the combo
    /// .. e.g. if setting up alt-X to send win-y, we'd set lalt-mapping on Key::X as k.alt.inactive_action(k.win.inactive_on_key(Key::Y))
    pub fn inactive_action (&self, af:AF) -> AF { // note that given our setup, this only gets called for left-side of LR mod keys
        // in theory, we should be able to just do a masked release here, and that work for alt .. win however is finicky
        // apparently win start menu triggers unless there's some timing gap between the masked release and another press
        // .. and from quick expts apparently even 80ms is sometimes too little .. not sure if also machine dependent
        let smk = self.clone();
        Arc::new ( move || {
            if !smk.pair_any_active() { af() }
            else {
                smk.consumed.set();
                if smk.active.check() { smk.release_w_masking() }
                if smk.paired_active() { smk.pair.read().unwrap() .iter() .for_each (|p| p.release_w_masking()) }
                af();
                if !smk.is_rel_delaying() { // post release reactivation delays (for win)
                    // basically any/both thats down is activated, but if none are down, still reactivate self (which is the left one)
                    if smk.paired_down() { smk.paired_reactivate() } else { smk.reactivate() }
                    if smk.down.check() && !smk.active.check() { smk.reactivate() } // if still not active from above when down, do it
                } else {
                    let smk = smk.clone(); // for the delay closure
                    spawn ( move || {
                        sleep(time::Duration::from_millis(100));
                        // since we're delayed, we'll check if the modkeys are still down before reactivating
                        if smk.down.check() { smk.reactivate() }
                        if smk.paired_down() { smk.paired_reactivate() }
                    } );
        } } } )
    }
    #[allow(dead_code)]
    pub fn inactive_on_key (&self, key:Key) -> AF { self.inactive_action (key_utils::base_action(key)) }
    // ^^ some sugar to make common things simpler .. could add for ctrl etc too if there was use


}

