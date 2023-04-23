#![ allow (non_snake_case) ]


use std::mem::size_of;
use std::time;
use std::thread;
use std::ops::Deref;
use std::sync::{Arc, RwLock};

use once_cell::sync::OnceCell;
use strum_macros::EnumIter;


use crate::{*, ModKey::*};





/// All the supported modifier-keys (incl some w incomplete impl like rwin)
# [ allow (non_camel_case_types) ]
# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter) ]
pub enum ModKey {
    no_mk, caps, lalt, ralt, lwin, rwin, lctrl, rctrl, lshift, rshift, alt, win, ctrl, shift
    // ^^ the last four (Alt/Win/Ctrl/Shfit) are intended to imply either of the L/R versions
    // .. and those are only for use during combo specification for l/r/lr expansion .. they dont exist in combo-bitmaps
    // note that no_mk can be useful to fill in fn defs set to take somethhing .. its ignored at its not in bitmaps
}





/// Wrapper that holds the caps-lock key and its impl as the base for most l2/l3 functionality
# [ derive (Debug) ]
pub struct _CapsModKey {
    // we've defined this just to be consistent with other mod-keys, but its really just a wrapper for caps key!
    _private : (),
    pub key  : Key,
    pub down : Flag,
}

/// Arc-wrapped CapsModKey for cheap passing between threads etc
# [ derive (Debug, Clone) ]
pub struct CapsModKey ( Arc <_CapsModKey> );

impl Deref for CapsModKey {
    type Target = _CapsModKey;
    fn deref(&self) -> &Self::Target { &self.0 }
}
// NOTE rest of the impl for this further down





/// TrackedModKeys only get key-down-state tracked, but no syncing against active-state externally (currently only for ralt)
# [ derive (Debug) ]
pub struct _TrackedModKey {
    // we'll use this for modifier keys that we want to simply track down state, w/o attempting to track or sync their external active states
    // again, motivation for making this separate is to share fumctionality implementations
    _private : (),
    pub key  : Key,
    pub down : Flag,
}

/// Arc wraps TrackedModKey functionality for easier cloning/sharing
# [ derive (Debug, Clone) ]
pub struct TrackedModKey ( Arc <_TrackedModKey> );

impl Deref for TrackedModKey {
    type Target = _TrackedModKey;
    fn deref(&self) -> &_TrackedModKey { &self.0 }
}
// NOTE rest of the impl for this further down





/// Synced-Modifier-Key tracks modifier key down-states as well as externally active state (used for lalt/lwin/lctrl/rctrl/lshift/rshift)
// note that we dont want this cloneable to avoid cloning all underlying, we'll work with it Arc-wrapped instead
# [ derive (Debug) ]
pub struct _SyncdModKey {
    // this struct holds flags required for a outside-state syncd tracking of a modifier key (we only do this for alt and win)
    // the alt and win keys are now tracked identically and so doing this helps us avoid duplication of that bunch of related code
    // these flags track whether the key is down, if it has been sent out (active), and if we should release it masked (consumed)
    _private     : (),
    pub key      : Key,
    pub down     : Flag,
    pub active   : Flag,
    pub consumed : Flag,

    pub pair     : Arc <RwLock <Option <SyncdModKey>>>, // backlink to populate to the left/right counterpart if desired
    // ^^ not the most ideal to have these cyclic backlinks, but we'll populate it at krusty creation time, so is reliable anyway
}

/// Arc wrapped Synced-Modifer-Key for cheap cloning/sharing
# [ derive (Debug, Clone) ]
pub struct SyncdModKey ( Arc <_SyncdModKey> );
// ^^ wrapping like this lets us impl functionality direclty on this cheaply clonable new-type to pass around
// .. the alternate of making SyncdModKey clonable would be more costly as it would clone each of the underlying flags

// and we'll impl deref on it so we dont have to keep using smk.0.<bla-bla> etc .. and its kosher as its underlying is Arc anyway
impl Deref for SyncdModKey {
    type Target = _SyncdModKey;
    fn deref(&self) -> &_SyncdModKey { &self.0 }
}
// NOTE:  the impl for the actual SMK functionality is further down





/// Holds representation for all modifier keys together, interlinked functionality etc impld here
# [ derive (Debug) ]
pub struct _ModKeys {

    // caps will be tracked for internal reference, and we'll assume we'll ALWAYS operate with caps-lock off
    // we'll also track all mod keys as syncd-modifier-keys, where we track the phys and logical states, as well as whether to mask their release
    // this allows us to add any composition of these in combos with any other key incl other mod keys while keeping internal/external models accurate

    // except r-alt which we track, but dont try to keep synced w external state .. (as we completely disable it every going out!)
    // (previously, there used to be ctrl/shift here that were pass through tracked like ralt, but they got upgraded to full SMK treatment!)

    // note also that we're not tracking rwin that some machines (not mine) have .. could easily add that later if need be

    _private   : (),
    // capslock tracking
    pub caps   : CapsModKey,
    // ralt is only tracked, but not synced
    pub ralt   : TrackedModKey,
    // the rest are synced mode keys (rwin not added so far as its not in my machine)
    pub lalt   : SyncdModKey,
    pub lwin   : SyncdModKey,
    pub lctrl  : SyncdModKey,
    pub rctrl  : SyncdModKey,
    pub lshift : SyncdModKey,
    pub rshift : SyncdModKey,

}

/// Arc wraps our collection of ModiferKeys for cheap cloning/sharing
# [ derive (Debug, Clone) ]
pub struct ModKeys ( Arc <_ModKeys> );

impl Deref for ModKeys {
    type Target = _ModKeys;
    fn deref(&self) -> &_ModKeys { &self.0 }
}





/// Holds representation for all modifier keys together, interlinked functionality etc impld here
impl ModKeys {

    pub fn new() -> ModKeys {
        let mks = _ModKeys {
            _private : (),
            // caps is a special singleton for itself
            caps   : CapsModKey::instance(),
            // ralt is the only TMK left by now, for which we track but dont keep it sync
            ralt   : TrackedModKey::new(Key::RAlt),
            // the rest are SMKs (synced mod keys) .. (rwin excluded for now since I dont have it in this machine to test with)
            lalt   : SyncdModKey::new(Key::LAlt),
            lwin   : SyncdModKey::new(Key::LWin),
            lctrl  : SyncdModKey::new(Key::LCtrl),
            rctrl  : SyncdModKey::new(Key::RCtrl),
            lshift : SyncdModKey::new(Key::LShift),
            rshift : SyncdModKey::new(Key::RShift)
        };

        // and we'll pair up the left/right mod-keys where applicable
        mks.lctrl  .link_pair (&mks.rctrl);
        mks.rctrl  .link_pair (&mks.lctrl);
        mks.lshift .link_pair (&mks.rshift);
        mks.rshift .link_pair (&mks.lshift);
        // note ofc that ralt is set to silent so it wont link w lalt, and we've not impld rwin yet as its not on my keyboards

        ModKeys ( Arc::new ( mks ) )
    }

    /// NOTE: this static will be our source of ordering for the mod-keys in the combo-mod-keys-state bitmap!!
    pub fn static_combo_bits_mod_keys() -> [ModKey; size_of::<ComboStatesBits_ModKeys>()] {
        // Note that there are bits in combo-bitmap only for physical keys
        // (i.e. it excludes the virtual l/r agnostic enum-vals used during combo construction)
        static COMBO_STATE_BITS_MOD_KEYS: [ModKey; size_of::<ComboStatesBits_ModKeys>()] = {
            [caps, lalt, ralt, lwin, rwin, lctrl, rctrl, lshift, rshift]
        };
        COMBO_STATE_BITS_MOD_KEYS
    }

    /// NOTE: the ordering in this static tuples array will be used to expand the l/r agnostic combo keys into their specialized versions
    // note that this order is relied on elsewhere, plus we want the fn composition to have ctrl innermost (if we use ctrl masking, which we dont anymore)
    // .. and the successive wrapping means the first one on this list is innermost .. and so its state will be updated with others masking
    // also, we'd rather have win at the end here (and wrap outermost), because that has a spawn and delay in reactivation .. still ok but still
    pub fn static_lr_mods_triplets () -> [(ModKey,ModKey,ModKey);4] {
        static LR_MODS_TRIPLETS : [(ModKey,ModKey,ModKey);4] = [
            (ctrl,  lctrl,  rctrl),
            (shift, lshift, rshift),
            (alt,   lalt,   ralt),
            (win,   lwin,   rwin),
        ];
        LR_MODS_TRIPLETS
    }

    /// this will give the SMKs in order defined at the static LR_MODS_TRIPLETS so they can be matched up at runtime
    pub fn lrmk_smks (&self) -> [&SyncdModKey;4] {
        [&self.lctrl, &self.lshift, &self.lalt, &self.lwin]
    }
    pub fn mod_smk_pairs (&self) -> [(ModKey, &SyncdModKey);6] { [ // note that ralt is TMK not SMK (and doesnt to activate/inactivate etc)
        (lwin,   &self.lwin  ), (lalt,   &self.lalt  ),
        (lctrl,  &self.lctrl ), (rctrl,  &self.rctrl ),
        (lshift, &self.lshift), (rshift, &self.rshift),
    ] }
    pub fn mk_flag_pairs (&self) -> [(ModKey, Option<&Flag>); size_of::<ComboStatesBits_ModKeys>()] { [
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
        ModKeys::static_combo_bits_mod_keys() .map (|mk| mod_keys.contains(&mk))
    }

    // mouse btns notify here in case we need to do cleanup/markings for win move/resize setups
    //pub fn process_mbtn_down (&self, mbtn:MouseButton) { self.lwin.consumed.set(); }
    //pub fn process_mbtn_up   (&self, mbtn:MouseButton) { }

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

    pub fn proc_notice__caps_down (&self) {
        self.mod_smk_pairs() .iter() .for_each (|(_,smk)| smk.proc_notice__caps_down());
    }
    pub fn proc_notice__caps_up (&self) {
        self.mod_smk_pairs() .iter() .for_each (|(_,smk)| smk.proc_notice__caps_up());
    }

    pub fn proc_notice__mouse_btn_down (&self, _mbtn:MouseButton) {
        self.lwin.consumed.set();  // doesnt matter, we can set it for any mouse btn
    }
    pub fn proc_notice__mouse_btn_up (&self, _mbtn:MouseButton) {
        // nothing to do for btn-up
    }



    pub fn setup_tracking (&self, k:&Krusty) {
        // setup capslock, we'll completely disable it other than for krusty use
        self.caps.setup_tracking (k);

        // setup tracking for right-alt .. its completely blocked but tracked, we use it as shifts and combos
        // (ctrl/shift etc used to be like this, but they have been promoted to full synced-mod-key tracking since)
        self.ralt .setup_tracking (k, true);      // for ralt can setup w doBlock=true

        // lalt and lwin are set as syncd-tracked-modifier-key with special (but identical) impl .. more details under SyncdModKey impl
        // (we're not doing rwin simply coz its not in my machine .. and ofc ralt is used in fully disabled mode above)
        self.lalt.setup_tracking (k);
        self.lwin.setup_tracking (k);

        // and we also updated both L/R of both shift and ctrl to do identical full-synced-tracked management like for lalt/lwin
        self.lctrl.setup_tracking (k);
        self.rctrl.setup_tracking (k);
        self.lshift.setup_tracking (k);
        self.rshift.setup_tracking (k);
    }

}





/// Impl for the caps-lock key specific functionality
impl CapsModKey {

    // ^^ CMK : Caps-Modifier-Key type .. basically tracks caps state and sets up caps as the global Layer-2/3/qks etc modifier key
    // (other mod-keys are via SMK (Syncd-Tracked modifier key: alt/win)  or TMK (Tracked modifier keys: ralt/lctrl/rctrl/lshift/rshift))

    pub fn instance () -> CapsModKey {
        // note that since ofc there's only one caps key, we'll set this up as singleton (unlike for the TMKs and SMKs below)
        static INSTANCE: OnceCell<CapsModKey> = OnceCell::new();
        INSTANCE .get_or_init (||
            CapsModKey ( Arc::new ( _CapsModKey {
                _private : (),
                key      : Key::CapsLock,
                down     : Flag::default(),
            } ) )
        ) .clone()
    }

    fn handle_key_down (&self, ks:&KrustyState) {
        // note that for caps, we completely block it from ever being sent up, and just manage internally
        if !ks.mod_keys.caps.down.check() {
            // capslock can come as repeats like other mod keys .. this was a fresh one
            self.down.set();
            // lets notify the synced tracked mod keys, so they can invalidate/release themselves
            ks.mod_keys.proc_notice__caps_down();
            ks.mouse.proc_notice__modkey_down (None, &ks);
        }
        if ks.mouse.lbtn.down.check() && !ks.mod_keys.lwin.down.check() && !ks.in_managed_ctrl_down_state.is_set() {
            // caps w mouse lbtn down, should be managed ctrl down (for ctrl-click, drag-drop etc)
            ks.in_managed_ctrl_down_state.set();
            ks.mod_keys.lctrl.ensure_active();
        }
    }

    fn handle_key_up (&self, ks:&KrustyState) {
        ks.mod_keys.caps.down.clear();
        ks.mouse.proc_notice__modkey_up(None, &ks);
        if ks.in_managed_ctrl_down_state.is_set() {
            ks.in_managed_ctrl_down_state.clear();
            if !ks.mod_keys.some_ctrl_down() { ks.mod_keys.lctrl.ensure_inactive() }
        }
        if ks.in_ctrl_tab_scroll_state.is_set() {
            // we do this separately from managed-ctrl-down, as this should work even just w ctrl and no caps
            if !ks.mod_keys.some_ctrl_down() { ks.in_ctrl_tab_scroll_state.clear() }
        }
        // lets also notify the alt/win tracked mod keys so they can re-enable themselves if applicable
        ks.mod_keys.proc_notice__caps_up();
    }


    pub fn setup_tracking (&self, k:&Krusty) {
        // note that for caps, we completely block it from ever being sent up, and just manage internally
        use crate::{EventPropagationDirective::*, KbdEventCbMapKeyType::*, KbdEvCbComboProcDirective::*, KbdEventCallbackFnType::*};

        if Key::CapsLock.is_toggled() { // toggle off first if necessary (to clear key light)
            key_utils::press_release (Key::CapsLock);
        }

        let ks = k.ks.clone();
        let event_proc_d = KbdEvProcDirectives::new (EventProp_Stop, ComboProc_Disable);
        let cb = KbdEvCbFn_InlineCallback ( Arc::new ( move |_| { ks.mod_keys.caps.handle_key_down(&ks); event_proc_d } ) );
        k.iproc.kbd_bindings .bind_kbd_event ( self.key, KeyEventCb_KeyDown, KbdEventCallbackEntry { event_proc_d, cb } );

        let ks = k.ks.clone();
        let cb = KbdEvCbFn_InlineCallback ( Arc::new ( move |_| { ks.mod_keys.caps.handle_key_up(&ks); event_proc_d } ) );
        k.iproc.kbd_bindings .bind_kbd_event ( self.key, KeyEventCb_KeyUp, KbdEventCallbackEntry { event_proc_d, cb } );
    }

}





/// Impl for Tracked-Modifier-Key functionality (currently only for R-Alt)
impl TrackedModKey {

    // ^^ TMK : Tracked-Modifier-Key type .. unlike the SMK, this only tracks the physical (is down) state of the modifier key
    // we should currently be doing this for left/right ctrl/shift, as well as right-alt

    pub fn new (tm_key: Key) -> TrackedModKey {
        TrackedModKey ( Arc::new ( _TrackedModKey {
            _private : (),
            key  : tm_key,
            down : Flag::default(),
        } ) )
    }

    pub fn setup_tracking (&self, k:&Krusty, do_block:bool) {
        // the setup for these is mostly just tracking their down state ..
        // however, we will also disable repeats, mostly for no functional reason than to ease looking at keystreams
        use crate::{EventPropagationDirective::*, KbdEventCbMapKeyType::*, KbdEvCbComboProcDirective::*, KbdEventCallbackFnType::*};

        // we'll support TMK key-down binding in blocking and pass-through configurations
        let tmk = self.clone();
        let epds_blocked     = KbdEvProcDirectives::new (EventProp_Stop,         ComboProc_Disable);
        let epds_passthrough = KbdEvProcDirectives::new (EventProp_Continue,     ComboProc_Disable);
        let epds_checked     = KbdEvProcDirectives::new (EventProp_Undetermined, ComboProc_Disable);

        let (event_proc_d, cb) = if do_block {
            let cb = KbdEvCbFn_InlineCallback ( Arc::new ( move |e| {
                if !e.injected { tmk.down.set() }
                epds_blocked
            } ) );
            (epds_blocked, cb)
        } else {
            let cb = KbdEvCbFn_InlineCallback ( Arc::new (move |e| {
                if tmk.down.check() { epds_blocked }
                else {
                    if !e.injected { tmk.down.set() }
                    epds_passthrough
                }
            } ) );
            (epds_checked, cb)
        };
        k.iproc.kbd_bindings .bind_kbd_event ( self.key, KeyEventCb_KeyDown, KbdEventCallbackEntry {event_proc_d, cb} );

        // for key-ups, we simply update flag (and let them go through if not blocked)
        let tmk = self.clone();
        let event_proc_d = if do_block { epds_blocked } else { epds_passthrough };
        let cb = KbdEvCbFn_InlineCallback ( Arc::new ( move |e| {
            if !e.injected { tmk.down.clear() }
            event_proc_d
        } ) );
        k.iproc.kbd_bindings .bind_kbd_event ( self.key, KeyEventCb_KeyUp, KbdEventCallbackEntry { event_proc_d, cb } );
    }

}





/// Impl for Synced-Modifier-Key functionality .. (e.g. for lalt/lwin/lctrl/rctrl/lshift/rshift)
impl SyncdModKey {

    // ^^ SMK : Synced-Modifier-Key type .. tracks internal (is down), external (is active), and to-mask (is consumed) states
    // .. we should currently be doing this for left-alt and left-win identically (.. expanded out to ctrl/shift too)

    pub fn new (sm_key: Key) -> SyncdModKey {
        SyncdModKey ( Arc::new ( _SyncdModKey {
            _private : (),
            key      : sm_key,
            down     : Flag::default(),
            active   : Flag::default(),
            consumed : Flag::default(),
            pair     : Arc::new(RwLock::new(None)),
        } ) )
    }


    pub fn link_pair (&self, smk:&SyncdModKey) {
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

    pub fn handle_key_down (&self, ks:&KrustyState, e:KbdEvent) {
        if ks.mod_keys.caps.down.check() {
            // caps is down, record alt being down if not already, but either way block it (so no change to alt-active state)
            self.consumed.clear();
            if !self.down.check() { ks.mouse.proc_notice__modkey_down (Some(&self), &ks) }
            if !e.injected { self.down.set() }
        } else {
            if self.down.check() {
                // caps isnt down, but we were, so its repeat .. we'll block it even if out-of-sync or its coming after combo caps release
            } else {
                // caps isnt down, and alt wasnt down, so record states and let it through
                if !e.injected { self.down.set() }
                self.active.set();
                self.consumed.clear();
                ks.mouse.proc_notice__modkey_down (Some(&self), &ks);
                // notice that we're only doing this if not a repeat
                let key = self.key;
                thread::spawn (move || key.press());
        } }
        if ks.mouse.lbtn.down.check() || ks.mouse.rbtn.down.check() { self.consumed.set() }
    }

    fn handle_key_up (&self, ks:&KrustyState, e:KbdEvent) {
        // if caps is pressed, or alt is already inactive (via masked-rel, press-rel etc), we block it
        // else if win was consumed, we release with mask, else we can actually pass it through unblocked
        // (note.. no more passing through of mod-keys, we'll instead send replacement ones if we need to (due to R/L sc-codes mismatch etc))
        if !e.injected { self.down.clear() }
        ks.mouse.proc_notice__modkey_up (Some(&self), &ks);
        if self.key == Key::LCtrl || self.key == Key::RCtrl { ks.in_ctrl_tab_scroll_state.clear() }
        if !self.active.check() || ks.mod_keys.caps.down.check() {
            // if inactive or caps-down we just suppress this keyup
        } else {
            if self.is_keyup_unified() && self.paired_down() {
                // for shift (w/ keyup state unified), ONLY send up a keyup if the other key isnt down .. so do nothing, not even clear active
            } else {
                let smk = self.clone();
                thread::spawn ( move || {
                    smk.release_w_masking();  // this checks/updates flags too
                    if smk.is_keyup_unified() { // and for up-unified, try and clear the other too
                        smk.pair.read().unwrap() .iter() .for_each (|p| {
                            if !p.down.check() && p.active.check() { p.release_w_masking(); }
                    } ) }
                } );
        }  }
    }

    pub fn setup_tracking (&self, k:&Krusty) {
        use crate::{EventPropagationDirective::*, KbdEventCbMapKeyType::*, KbdEvCbComboProcDirective::*, KbdEventCallbackFnType::*};

        // smks always block their key-events, and if necessary generate new ones (with extended scan-codes as appropriate)
        let event_proc_d = KbdEvProcDirectives::new (EventProp_Stop, ComboProc_Disable);

        let (ks, smk) = (k.ks.clone(), self.clone());
        let cb = KbdEvCbFn_InlineCallback ( Arc::new ( move |e| { smk.handle_key_down(&ks, e); event_proc_d } ) );
        k.iproc.kbd_bindings .bind_kbd_event ( self.key, KeyEventCb_KeyDown, KbdEventCallbackEntry { event_proc_d, cb } );

        let (ks, smk) = (k.ks.clone(), self.clone());
        let cb = KbdEvCbFn_InlineCallback ( Arc::new ( move |e| { smk.handle_key_up(&ks, e); event_proc_d } ) );
        k.iproc.kbd_bindings .bind_kbd_event ( self.key, KeyEventCb_KeyUp, KbdEventCallbackEntry { event_proc_d, cb } );
    }



    pub fn proc_notice__caps_down(&self) {
        // we will immediately invalidate and clear any down mod-key found upon caps activation!
        // note that internally tracked physical is_down will continue to be down
        // note also that each of paired mod-keys will get their own notification too
        if self.down.check() && self.active.check() {
            self.consumed.set();
            let smk = self.clone();
            thread::spawn ( move || smk.release_w_masking() ); // this will update flags too
        }
    }
    pub fn proc_notice__caps_up (&self) {
        // since we deactivate mod-keys on caps press, check to see if we want to reactivate them
        // note: we'll setup a delay for activation to allow for some sloppy combo releases etc
        // note also, that if inspecting in browser-key-events, this might appear unexpected coz browser does its own 'unifying'
        // .. so to check the logic here must use lower level key inspections like via ahk key history!!
        // plus if doing caps release while both shift down, on my machine even the raw events are wonky (no caps evnt until one releases!!)
        if self.down.check() {
            let smk = self.clone();
            thread::spawn ( move || {
                thread::sleep(time::Duration::from_millis(200));
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


    /// All mod-actions mark the mod-down consumed too, but if its a no-key action (like brightness etc), wrap with this to mark it consumed.
    /// The consumed flag marks it to have its later release be masked with control to avoid activating win-menu etc
    pub fn keydn_consuming_action (&self, af:AF) -> AF {
        let smk = self.clone();
        Arc::new ( move || { smk.consumed.set(); af(); } )
    }


    /// Use this to wrap activation action blindly (whether its already active or not) and without masking on release.
    /// ... Should be useful only in cases we explicitly dont expect any contention and want to avoid masking
    pub fn bare_action (&self, af:AF) -> AF {
        let k = self.key;
        Arc::new ( move || {
            k.press(); af(); k.release()
        })
    }

    /// Use this to wrap actions when we want the mod-key to be ACTIVE in the combo .. can use for both self-mod-key combos or unrelated combos.
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
    


    /// Use this to wrap actions ONLY when setting combos with this mod key itself AND we want the mod-key to be INACTIVE in the combo.
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
                    thread::spawn ( move || {
                        thread::sleep(time::Duration::from_millis(100));
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

