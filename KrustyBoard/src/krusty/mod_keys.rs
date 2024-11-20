#![ allow (non_snake_case, non_camel_case_types) ]


use std::time;
use std::thread;
use std::sync::Arc;
use std::fmt::Debug;

use derive_deref::Deref;
use once_cell::sync::OnceCell;
use strum_macros::EnumIter;
use atomic_refcell::AtomicRefCell;


use crate::{*, ModKey::*, ComboProc_D::*, EvProp_D::*};





/// All the supported modifier-keys (incl some w incomplete impl like rwin)
# [ allow (non_camel_case_types) ]
# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter) ]
pub enum ModKey {
    no_mk,
    caps,      lalt,      ralt,      lwin,      rwin,      lctrl,      rctrl,      lshift,      rshift,
    caps_dbl,  lalt_dbl,  ralt_dbl,  lwin_dbl,  rwin_dbl,  lctrl_dbl,  rctrl_dbl,  lshift_dbl,  rshift_dbl,
    alt,  win,  ctrl,  shift,
    // ^^ the last four (alt/win/ctrl/shift) are intended to imply either of the L/R versions
    // .. and those are only for use during combo specification for l/r/lr expansion .. they dont exist in combo-bitmaps
    // note that no_mk can be useful to fill in fn defs set to take somethhing .. its ignored as its not in bitmaps
}
impl ModKey {
    pub fn key (&self) -> Key {
        // the modeky enum is mostly all mappable to Keys other than no_mk ..
        // .. to make usage simpler, we'll just map that to 0xFF and send the unwrapped result
        (*self).try_into() .unwrap_or (Key::OtherKey(0xFF))
    }
}

impl TryFrom <ModKey> for KbdKey {
    type Error = ();
    fn try_from (mk: ModKey) -> Result<Self, Self::Error> {
        use KbdKey::*;
        match mk {
            caps => Ok(CapsLock),
            alt  => Ok(LAlt),  win  => Ok(LWin),  ctrl  => Ok(LCtrl),  shift  => Ok(LShift),
            lalt => Ok(LAlt),  lwin => Ok(LWin),  lctrl => Ok(LCtrl),  lshift => Ok(LShift),
            ralt => Ok(RAlt),  rwin => Ok(RWin),  rctrl => Ok(RCtrl),  rshift => Ok(RShift),
            caps_dbl => Ok(CapsLock),
            lalt_dbl => Ok(LAlt),  lwin_dbl => Ok(LWin),  lctrl_dbl => Ok(LCtrl),  lshift_dbl => Ok(LShift),
            ralt_dbl => Ok(RAlt),  rwin_dbl => Ok(RWin),  rctrl_dbl => Ok(RCtrl),  rshift_dbl => Ok(RShift),
            _ => Err(())
    } }
}





/// CapsModKey holds the caps-lock key and its impl as the base for most l2/l3 functionality
# [ derive (Debug) ]
pub struct _CapsModKey {
    _private : (),
    pub down    : Flag,         // physically down
    pub dbl_tap : Flag,         // marker that two presses came within dbl-tab window
    pub stamp   : EventStamp,   // stamp when it was last pressed, used for dbl-tap marking
}

# [ derive (Debug, Clone, Deref) ]
pub struct CapsModKey ( Arc <_CapsModKey> );





/// for composition of modkey behavior variations, we define a KeyHandling trait ..
/// then keep a box-dyn of the right variant in the modkey struct ..
/// it needs to be send/sync/'static for cross-thread usage, but shouldnt need to be cloned itself, hence just Box instead of Arc
pub type KH = Box <dyn KeyHandling + Send + Sync + 'static>;


/// We'll used this as the common struct for all types of modkeys whether simple blocked TMKs or the complex SMKs
/// (As with couple extra bytes of storage, we get to keep code close enough to switch easily between TMK/SMK e.g. for lwin)
# [ derive (Debug) ]
pub struct _UnifModKey {
    _private     : (),

    pub mk       : ModKey,     // the mod-key enum associated with this mod-key struct
    pub mk_dbl   : ModKey,     // the doubled mod-key enum associated with this mod-key struct
    pub handling : KH,         // box dyn w handling behavior for this key (passthrough/blocked/doubled/managed)

    pub down        : Flag,    // physically down
    pub active      : Flag,    // logically down to outside world
    pub mngd_active : Flag,    // forced/latched down via code (e.g. for ctrl-wheel etc)
    pub consumed    : Flag,    // used to suppress key-repeat and/or mask/suppress release events
    pub dbl_tap     : Flag,    // marker that two presses came within dbl-tab window

    pub stamp : EventStamp,    // stamp when it was last pressed, used for dbl-tap marking

    // we'll also hold a pairing to the left/right counterpart if desired ..
    // .. this is intended to be populated once, right after post creation .. hence the AtomicRefCell
    // .. and we'll keep this private and have accesses through .paired() just to handle the dereferencing
    pair : AtomicRefCell <Option <UnifModKey>>,
}

impl _UnifModKey {
    pub fn new ( mk:ModKey, mk_dbl:ModKey, handling:KH ) -> _UnifModKey { _UnifModKey {
        _private : (),
        mk,
        mk_dbl,
        handling,
        down        : Flag::default(),
        active      : Flag::default(),
        mngd_active : Flag::default(),
        consumed    : Flag::default(),
        dbl_tap     : Flag::default(),
        stamp       : EventStamp::default(),
        pair        : AtomicRefCell::default(),
    } }
}

/// Unified-Modifier-Key is now used for all modkeys regardless of ModKey_Mgmt behavior variation (other than for the capslock key)
// (for reference, we used to have a SyncedModKey for the fully managed type, and a TrackedModKey for all the others)
# [ derive (Debug, Clone, Deref) ]
pub struct UnifModKey ( Arc <_UnifModKey> );



/// ModKey_Mgmt type determines how the particular modkey is internally managed
/// - passthrough .. simply monitors the state and lets the key events pass through (e.g. none currently)
/// - blocked     .. the key events are blocked at this level and never make it out externally (e.g. ralt )
/// - doubled     .. when double-tapped, they behave like single tapped, whereas single tapped are monitored but blocked (e.g. win)
/// - managed     .. full management .. track both physical and logical states
# [ allow (non_camel_case_types) ]
# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone) ]
pub enum ModKey_Mgmt {
    MK_Mgmt_Passthrough,
    MK_Mgmt_Blocked,
    MK_Mgmt_Doubled,
    MK_Mgmt_Managed,
}

// note that we wont need any data in the variant objects, they are simply used as types to associate the behavior

# [ derive (Debug) ]
pub struct ModKey_Passthrough;

# [ derive (Debug) ]
pub struct ModKey_Blocked;

# [ derive (Debug) ]
pub struct ModKey_Doubled;

# [ derive (Debug) ]
pub struct ModKey_Managed;


/// KeyHandling behavior variants
/// .. note that these only handle the external logical state management .. some common physical etc mgmt is in the UnifModKey itself
pub trait KeyHandling : Debug {

    fn handling_type (&self) -> ModKey_Mgmt ;

    fn handle_key_down (&self, bmk:&UnifModKey, ks:&KrustyState) -> EvProc_Ds;
    fn handle_key_up   (&self, bmk:&UnifModKey, ks:&KrustyState) -> EvProc_Ds;

    // for caps up/down the default impl should do nothing, but ModKey_Managed etc can define their own processing
    fn proc_notice__caps_down (&self, _:&UnifModKey, _:&KrustyState) { }
    fn proc_notice__caps_up   (&self, _:&UnifModKey, _:&KrustyState) { }

    // general behavioral queries can be satisfied here w/o the specific impls having to worry about them
    fn is_managed (&self) -> bool { self.handling_type() == ModKey_Mgmt::MK_Mgmt_Managed }
    fn is_doubled (&self) -> bool { self.handling_type() == ModKey_Mgmt::MK_Mgmt_Doubled }

}





/// Holds representation for all modifier keys together, interlinked functionality etc impld here
# [ derive (Debug) ]
pub struct ModKeys {
    // caps will be tracked for internal reference, and we'll assume we'll ALWAYS operate with caps-lock off
    // we'll also track all mod keys as syncd-modifier-keys, where we track the phys and logical states, as well as whether to mask their release
    // this allows us to add any composition of these in combos with any other key incl other mod keys while keeping internal/external models accurate

    // except r-alt which we track, but dont try to keep synced w external state .. (as we completely disable it every going out!)
    // (previously, there used to be ctrl/shift here that were pass through tracked like ralt, but they got upgraded to full SMK treatment!)

    _private   : (),
    // capslock tracking
    pub caps   : CapsModKey,
    // ralt is only tracked, but not synced (since lalt is treated as shift instead)
    pub ralt   : UnifModKey,
    //  and now that native win funcationality is moved to dbl-win, lwin/rwin are simple tracked keys (no sync w logical state)
    pub lwin   : UnifModKey,
    pub rwin   : UnifModKey,
    // the rest are synced mode keys (we keep track of both pressed state and external logical state, w/ or w/o pairing)
    pub lalt   : UnifModKey,
    pub lctrl  : UnifModKey,
    pub rctrl  : UnifModKey,
    pub lshift : UnifModKey,
    pub rshift : UnifModKey,
}





/// private utility fn to offload computation to the input-processor af-queue
fn afq_send (action : EvCbFn_QueuedProc_T) {
    if let Err(_err) = InputProcessor::instance().input_af_queue.send(action) {
        // meh, if we cant send it, the queue itself is fubared, oh well
    }
}





/// Holds representation for all modifier keys together, interlinked functionality etc impld here
impl ModKeys {

    pub fn new() -> Self {
        let mod_keys = ModKeys {
            _private : (),
            // caps is a special singleton for itself
            caps   : CapsModKey::instance(),
            // ralt is fully blocked .. we'll typically use it as shift instead
            ralt   : UnifModKey::new ( ralt,   ralt_dbl,   Box::new(ModKey_Blocked) ),
            // lwin/rwin are doubled, so their functionality is only activated on dbl-press
            lwin   : UnifModKey::new ( lwin,   lwin_dbl,   Box::new(ModKey_Doubled) ),
            rwin   : UnifModKey::new ( rwin,   rwin_dbl,   Box::new(ModKey_Doubled) ),
            // the other modifier-keys are fully managed
            lalt   : UnifModKey::new ( lalt,   lalt_dbl,   Box::new(ModKey_Managed) ),
            lctrl  : UnifModKey::new ( lctrl,  lctrl_dbl,  Box::new(ModKey_Managed) ),
            lshift : UnifModKey::new ( lshift, lshift_dbl, Box::new(ModKey_Managed) ),
            rctrl  : UnifModKey::new ( rctrl,  rctrl_dbl,  Box::new(ModKey_Managed) ),
            rshift : UnifModKey::new ( rshift, rshift_dbl, Box::new(ModKey_Managed) ),
        };
        // now lets set up the appropriate pairs before we return
        // (note that we wont pair lalt and ralt, as we're setting there behavior completely differently)
        mod_keys .lwin   .set_pair ( mod_keys .rwin   .clone() );
        mod_keys .rwin   .set_pair ( mod_keys .lwin   .clone() );
        mod_keys .lctrl  .set_pair ( mod_keys .rctrl  .clone() );
        mod_keys .rctrl  .set_pair ( mod_keys .lctrl  .clone() );
        mod_keys .lshift .set_pair ( mod_keys .rshift .clone() );
        mod_keys .rshift .set_pair ( mod_keys .lshift .clone() );
        mod_keys
    }


    /// NOTE: the ordering in this 'static' tuples array will be used to expand the l/r agnostic combo keys into their specialized versions
    // note that this order is relied on elsewhere, plus we want the fn composition to have ctrl innermost (if we use ctrl masking, which we dont anymore)
    // .. and the successive wrapping means the first one on this list is innermost .. and so its state will be updated with others masking
    // also, we'd rather have win at the end here (and wrap outermost), because that has a spawn and delay in reactivation .. still ok but still
    // note also that lalt/ralt not being paired etc isnt relevant here (defining 'alt' combo to trigger from either lalt/ralt is still valid)
    pub fn static_lr_mods_triplets () -> [(ModKey,ModKey,ModKey);4] {
        static LR_MODS_TRIPLETS : [(ModKey,ModKey,ModKey);4] = [
            (alt,   lalt,   ralt),
            (ctrl,  lctrl,  rctrl),
            (shift, lshift, rshift),
            (win,   lwin,   rwin),
        ];
        LR_MODS_TRIPLETS
    }

    /// NOTE: this ordering will be our source of ordering for the mod-keys in the combo-mod-keys-state bitmap!!
    pub fn ordered_unif_modkeys (&self) -> [&UnifModKey; 8] { [
        &self.lalt, &self.lctrl, &self.lshift, &self.lwin,
        &self.ralt, &self.rctrl, &self.rshift, &self.rwin
    ] }


    pub fn some_alt_down   (&self) -> bool { self.lalt.down.is_set()   || self.ralt.down.is_set()   }
    pub fn some_ctrl_down  (&self) -> bool { self.lctrl.down.is_set()  || self.rctrl.down.is_set()  }
    pub fn some_shift_down (&self) -> bool { self.lshift.down.is_set() || self.rshift.down.is_set() }
    pub fn some_win_down   (&self) -> bool { self.lwin.down.is_set()   || self.rwin.down.is_set()   }

    pub fn some_alt_dbl   (&self) -> bool { self.lalt.dbl_tap.is_set()   || self.ralt.dbl_tap.is_set()   }
    pub fn some_ctrl_dbl  (&self) -> bool { self.lctrl.dbl_tap.is_set()  || self.rctrl.dbl_tap.is_set()  }
    pub fn some_shift_dbl (&self) -> bool { self.lshift.dbl_tap.is_set() || self.rshift.dbl_tap.is_set() }
    pub fn some_win_dbl   (&self) -> bool { self.lwin.dbl_tap.is_set()   || self.rwin.dbl_tap.is_set()   }

    pub fn some_mk_down (&self) -> bool {
        self.caps.down.is_set() || self.ordered_unif_modkeys() .iter() .any (|umk| umk.down.is_set())
        // ^^ dont need to check _dbl, as if some mk_dbl is active, the mk will also be active
    }
    pub fn some_mk_dbl_down (&self) -> bool {
        self.caps.dbl_tap.is_set() || self.ordered_unif_modkeys() .iter() .any (|umk| umk.dbl_tap.is_set())
    }

    pub fn unstick_all (&self) {
        // all modkey states .. we'll do two loops to interleave them so they dont activate e.g. start-menu
        self.ordered_unif_modkeys() .iter() .for_each (|umk| umk.mk.key().press() );
        // but since ctrl-alt-shift-win press-rel triggers ms-365, we'll insert a dummy key as well
        Key::OtherKey(0x9A).release();
        self.ordered_unif_modkeys() .iter() .for_each (|umk| {
            umk.mk.key().release(); umk.down.clear(); umk.active.clear(); umk.mngd_active.clear(); umk.dbl_tap.clear(); umk.consumed.clear();
        });
        self.caps.down.clear(); self.caps.dbl_tap.clear();
    }


    pub fn proc_notice__caps_down (&self, ks:&KrustyState) {
        self.ordered_unif_modkeys() .iter() .for_each (|umk| umk.proc_notice__caps_down(ks));
    }
    pub fn proc_notice__caps_up (&self, ks:&KrustyState) {
        self.ordered_unif_modkeys() .iter() .for_each (|umk| umk.proc_notice__caps_up(ks));
    }

    pub fn setup_tracking (&self, k:&Krusty) {
        self.caps.setup_tracking (k);
        self.ordered_unif_modkeys() .iter() .for_each (|umk| umk.setup_tracking(k));
    }

}





/// Impl for the caps-lock key specific functionality
impl CapsModKey {
    // ^^ CMK : Caps-Modifier-Key type .. basically tracks caps state and sets up caps as the global Layer-2/3/qks etc modifier key

    pub fn instance () -> Self {
        // note that since ofc there's only one caps key, we'll set this up as singleton (unlike for the TMKs and SMKs below)
        static INSTANCE: OnceCell<CapsModKey> = OnceCell::new();
        INSTANCE .get_or_init ( ||
            CapsModKey ( Arc::new ( _CapsModKey {
                _private : (),
                down        : Flag::default(),
                dbl_tap     : Flag::default(),
                stamp       : EventStamp::default(),
            } ) )
        ) .clone()
    }

    fn handle_key_down (&self, ks:&KrustyState, ev:&Event) {
        //println!("Caps DOWN : {:?}, inj: {:?}", ev.key, ev.injected);

        // note that for caps, we completely block it from ever being sent up, and just manage internally
        if !self.down.is_set() {
            // capslock can come as repeats like other mod keys .. this was a fresh one
            self.down.set();
            update_stamp_key_dbl_tap (ev.stamp, &self.stamp, &self.dbl_tap);
            ks.proc_notice__modkey_down(caps);
        }
        if ks.mouse.lbtn.down.is_set() && !ks.mod_keys.lwin.down.is_set() {
            // caps w mouse lbtn down, should be managed ctrl down (via ensure_active()) .. (for ctrl-click, drag-drop etc)
            let ks = ks.clone();
            afq_send (Box::new (move || ks.mod_keys.lctrl.ensure_active()));
        }
    }

    fn handle_key_up (&self, ks:&KrustyState, _ev:&Event) {
        //println!("Caps UP : {:?}, inj: {:?}", _ev.key, _ev.injected);
        self.down.clear();
        self.dbl_tap.clear();
        ks.proc_notice__modkey_up(caps);
    }


    pub fn setup_tracking (&self, k:&Krusty) {
        // note that for caps, we completely block it from ever being sent up, and just manage internally
        use crate::{EvProp_D::*, KbdEv_MapKey_T::*, ComboProc_D::*, EvCbFn_T::*, KbdKey::CapsLock};

        // toggle off first if necessary (to clear key light)
        if CapsLock.is_toggled() { CapsLock.press_release() }

        let ks = k.ks.clone();
        let ev_proc_ds = EvProc_Ds::new (EvProp_Stop, ComboProc_Disable);
        let cb = EvCbFn_Inline ( Arc::new ( move |ev| { ks.mod_keys.caps.handle_key_down(&ks, &ev); ev_proc_ds } ) );
        k.iproc.input_bindings .bind_kbd_event (CapsLock, KeyEventCb_KeyDown, EvCbEntry { ev_proc_ds, cb } );

        let ks = k.ks.clone();
        let cb = EvCbFn_Inline ( Arc::new ( move |ev| { ks.mod_keys.caps.handle_key_up(&ks, &ev); ev_proc_ds } ) );
        k.iproc.input_bindings .bind_kbd_event (CapsLock, KeyEventCb_KeyUp, EvCbEntry { ev_proc_ds, cb } );
    }

}




/// impl for key handling for various ModKey_* behaviors
// note that there's some more common key handling in the UnifModKey itself, esp for physical states ..
// .. and only variations dealing w managing the external logical 'active' state should be in these

/// passthrough keyhandling only tracks state with no other management
impl KeyHandling for ModKey_Passthrough {

    fn handling_type(&self) -> ModKey_Mgmt { ModKey_Mgmt::MK_Mgmt_Passthrough }

    fn handle_key_down (&self, bmk:&UnifModKey, _:&KrustyState) -> EvProc_Ds {
        bmk.active.set();
        EvProc_Ds::new (EvProp_Continue, ComboProc_Disable)
    }

    fn handle_key_up (&self, bmk:&UnifModKey, _:&KrustyState) -> EvProc_Ds {
        bmk.active.clear();
        EvProc_Ds::new (EvProp_Continue, ComboProc_Disable)
    }

}


/// blocked modkey handling simply stops all propagation (physical states are tracked, but it should never be logically active)
impl KeyHandling for ModKey_Blocked {

    fn handling_type(&self) -> ModKey_Mgmt { ModKey_Mgmt::MK_Mgmt_Blocked }

    fn handle_key_down (&self, _:&UnifModKey, _:&KrustyState) -> EvProc_Ds {
        EvProc_Ds::new (EvProp_Stop, ComboProc_Disable)
    }

    fn handle_key_up (&self, _:&UnifModKey, _:&KrustyState) -> EvProc_Ds {
        EvProc_Ds::new (EvProp_Stop, ComboProc_Disable)
    }

}


/// doubled modkeys behave like regular when double-tapped, but single taps are monitored but blocked
impl KeyHandling for ModKey_Doubled {

    fn handling_type(&self) -> ModKey_Mgmt { ModKey_Mgmt::MK_Mgmt_Doubled }

    fn handle_key_down (&self, bmk:&UnifModKey, _:&KrustyState) -> EvProc_Ds {
        if bmk.dbl_tap.is_set() {
            bmk.active.set();
            EvProc_Ds::new (EvProp_Continue, ComboProc_Disable)
        } else {
            EvProc_Ds::new (EvProp_Stop, ComboProc_Disable)
        }
    }

    fn handle_key_up (&self, bmk:&UnifModKey, _:&KrustyState) -> EvProc_Ds {
        if bmk.active.is_set() {
            bmk.active.clear();
            EvProc_Ds::new (EvProp_Continue, ComboProc_Disable)
        } else {
            EvProc_Ds::new (EvProp_Stop, ComboProc_Disable)
        }
    }

}

/// key handling for fully managed modkeys .. check comments for details
impl KeyHandling for ModKey_Managed {

    /// key-down management for the complex 'managed' modkey type
    // NOTE: given that kbds seem to have idiosyncrasies with what scancode vs vk codes they sent, we end up getting out of sync w
    // what keys we let through and what we simulate .. e.g in my machine lshift comes in vk while rshift comes sc and so sending our
    // vk shift doesnt clear it out .. so we've decided to just block everything and send our uniform up/down reports instead!

    // beyond that, we'll block repeats to keep code logic (and keystream inspections) manageable
    // we'll also track and update both physical and externally expressed states and try and keep our model always in sync w the outside
    // we'll also track if we've used up the press so we can mask its release later (for things like win/alt that trigger menus on release)

    // so goal here is, any presses with caps active, we suppress mod-key going outside
    // and since we can have caps come in after the mod-key is already down, we'll have to capture disparity states ..
    //  .. as well as restoring them when either caps/alt gets released etc
    // (plus, if we're down and caps goes down, we'll get notification below so we're enforcing the disabled state from both sides)

    // re utilty of spawning threads even for a few send-inputs .. quick timing tests on laptop showed ..
    // - for our usage, modkey inline proc w/o send-inputs typically within ~~100us ..
    // - a send-input seemed to take very roughly ~~500us ..
    // - spawning a thread ~~50us .. thread pickup maybe ~~300-800us
    //   (so spawning here, even frivolously, does help keep hook-thread snappy, and w/o delaying the work much)
    // - sending to our af-queue ~~15-50us and queued task pickup ususally ~~50-100us ..
    //   (so even better, just send any (no-sleep) computation to the af-queue! .. faster, less overhead, and keeps sequential too)

    fn handling_type(&self) -> ModKey_Mgmt { ModKey_Mgmt::MK_Mgmt_Managed }

    fn handle_key_down (&self, bmk:&UnifModKey, ks:&KrustyState) -> EvProc_Ds {
        // we should clear the consumed flag, but not if mouse btns are down, so we'll just put mouse-btns state there
        //self.consumed.clear();
        bmk.consumed .store ( ks.mouse.lbtn.down.is_set() || ks.mouse.rbtn.down.is_set() );

        if ks.mod_keys.caps.down.is_clear() {
            // caps isnt down (and its repeat filtered), so record it and let it through (or send replacment as detailed above)
            bmk.active.set();
            let bmk = bmk.clone();
            //thread::spawn (move || bmk.mk.key().press());
            afq_send (Box::new (move || bmk.mk.key().press()));
        }
        // else if caps was down, we just block it
        EvProc_Ds::new (EvProp_Stop, ComboProc_Disable)
    }

    fn handle_key_up (&self, bmk:&UnifModKey, ks:&KrustyState) -> EvProc_Ds {
        // (note.. no more passing through of mod-keys, we'll instead send replacement ones if we need to (due to R/L sc-codes mismatch etc))

        // first off, we'll take care of pair managed flags
        // basically, if we're managed, and pair-mngd flag is set, but pair is up, and caps is also up ..
        // .. then we should clear the pair's mngd flag (as caps up doesnt clear mngd while any of the pair is down)
        // .. (this is ofc, coz we only typically set mngd flag on left-of-pair, but still want it cleared when neither pair nor caps are down)
        if !ks.mod_keys.caps.down.is_set() && bmk.paired_mngd_active() && !bmk.paired_down() {
            if let Some(p) = bmk.paired() {
                let p = p.clone();
                afq_send (Box::new (move || p.ensure_inactive()));
            }
        }

        if bmk.active.is_clear() || (bmk.mngd_active.is_set() && ks.mod_keys.caps.down.is_set()) {
            // if inactive (usually due to caps-down), or we're forced active while caps still down, we just suppress this keyup
        } else {
            if bmk.is_keyup_unified() && bmk.paired_down() {
                // for shift (w/ keyup state unified), ONLY send up a keyup if the other key isnt down .. so do nothing, not even clear active
            } else {
                let bmk = bmk.clone();
                afq_send ( Box::new (move || {
                    bmk.release_w_masking();  // this checks/updates flags too
                    if bmk.is_keyup_unified() { // and for up-unified, try and clear the other too
                        if let Some(p) = bmk.paired() {
                            if !p.down.is_set() && p.active.is_set() { p.release_w_masking(); }
                    } }
                } ) );
            }
        }
        EvProc_Ds::new (EvProp_Stop, ComboProc_Disable)
    }

    fn proc_notice__caps_down (&self, bmk:&UnifModKey, _ks:&KrustyState) {
        // we will immediately invalidate and clear any down mod-key found upon caps activation!
        // note that internally tracked physical is_down will continue to be down
        // note also that each of paired mod-keys will get their own notification too
        if bmk.down.is_set() && bmk.active.is_set() {
            bmk.consumed.set();
            // we want to release mod-key upon caps .. (unless it would interfere w/ switch alt-tab)
            if WinEventsListener::instance().fgnd_info.read().unwrap().exe != "Switche.exe" {
                let bmk = bmk.clone();
                afq_send (Box::new (move || bmk.release_w_masking()));
            }
        }
    }

    fn proc_notice__caps_up (&self, bmk:&UnifModKey, _ks:&KrustyState) {
        // for managed active (i.e active outside w/o down held), we want to clear it on caps release ..
        // note that only checking ourselves works even if the paired was held down, the pair would just reactivate itself afterwards
        // (because each of the pair gets its own caps-up/dn notification)
        if bmk.mngd_active.is_set() {
            bmk.mngd_active.clear();
            if bmk.active.is_set() && !bmk.down.is_set() {
                let bmk = bmk.clone();
                afq_send (Box::new (move || bmk.ensure_inactive()));
            }
        }
        // since we deactivate mod-keys on caps press, check to see if we want to reactivate them
        // note: we'll setup a delay for activation to allow for some sloppy combo releases etc
        // note also, that if inspecting (shift) in browser-key-events, this might appear unexpected coz browser does its own shift 'unifying'
        // .. so to check the logic here must use lower level key inspections like via ahk key history!!
        // plus if doing caps release while both shift down, on my machine even the raw events are wonky (no caps evnt until one releases!!)
        if bmk.down.is_set() {
            let umk = bmk.clone();
            // the delayed action ofc, we dont want to send off to af-queue, so we'll just spawn a thread
            thread::spawn ( move || {
                thread::sleep(time::Duration::from_millis(150));
                if umk.down.is_set() && !umk.active.is_set() {
                    umk.mk.key().press(); umk.active.set(); umk.consumed.set();
            } } );
        }
    }

}




/// base impl for shared functionality among all mod-key types
impl UnifModKey {

    fn new (mk: ModKey, mk_dbl: ModKey, handling: KH) -> Self {
        Self ( Arc::new ( _UnifModKey::new (mk, mk_dbl, handling) ) )
    }

    fn set_pair (&self, p:UnifModKey) {
        *self.pair .borrow_mut() = Some(p)
    }
    pub fn paired (&self) -> Option<&UnifModKey> {
        //self.pair.borrow().as_ref()
        unsafe { (*self.pair.as_ptr()) .as_ref() }
        // ^^ we access this without any guards since this never gets written to at runtime .. (and it is in hotpath)
    }

    /// NOTE re injected events .. we block our own (and ahk) injections at hook level .. so anything here is external
    // so we'll want to let them through, only updating our tracking of external state (not our physical state)

    fn handle_key_down (&self, ev: Event, ks:&KrustyState) -> EvProc_Ds {
        if ev.injected {
            self.active.set();
            return EvProc_Ds::new (EvProp_Continue, ComboProc_Disable)
        }
        if self.down.is_set() {     // repeats are blocked w/o further processing
            return EvProc_Ds::new (EvProp_Stop, ComboProc_Disable)
        }
        //println!("mod new DOWN : {:?}, inj: {:?}",ev.key, ev.injected);

        // now first lets do some common work (physical state etc) ..
        self.down.set();
        update_stamp_key_dbl_tap (ev.stamp, &self.stamp, &self.dbl_tap);
        ks.proc_notice__modkey_down (self.mk);

        // then for external active state etc updates, we'll call the mgmt specific fns
        self.handling.handle_key_down (self, ks)
    }


    fn handle_key_up (&self, ev: Event, ks:&KrustyState) -> EvProc_Ds {
        //println!("mod new DOWN : {:?}, inj: {:?}",ev.key, ev.injected);
        if ev.injected {
            self.active.clear();
            return EvProc_Ds::new (EvProp_Continue, ComboProc_Disable)
        }
        // lets do some common work (physical state etc) ..
        self.down.clear(); self.dbl_tap.clear();
        ks.proc_notice__modkey_up (self.mk);

        // then for external active state etc updates, we'll call the mgmt specific fns
        self.handling.handle_key_up (self, ks)
    }


    pub fn setup_tracking (&self, k:&Krusty) {
        // the setup for these is mostly just tracking their state flags ..
        // however, we will also disable repeats, not least to ease looking at keystreams
        use crate::{KbdEv_MapKey_T::*, EvCbFn_T::*};

        let umk = self.clone(); let ks = k.ks.clone();
        k.iproc.input_bindings .bind_kbd_event (
            self.mk.key(), KeyEventCb_KeyDown, EvCbEntry {
                ev_proc_ds: EvProc_Ds::new (EvProp_Undet, ComboProc_Disable),
                cb: EvCbFn_Inline ( Arc::new (move |ev| { umk.handle_key_down (ev, &ks) } ) )
        } );

        let umk = self.clone(); let ks = k.ks.clone();
        k.iproc.input_bindings .bind_kbd_event (
            self.mk.key(), KeyEventCb_KeyUp, EvCbEntry {
                ev_proc_ds: EvProc_Ds::new (EvProp_Undet, ComboProc_Disable),
                cb: EvCbFn_Inline ( Arc::new (move |ev| { umk.handle_key_up (ev, &ks) } ) )
        } );
    }


    pub fn proc_notice__caps_down (&self, ks:&KrustyState) { self.handling.proc_notice__caps_down (self, ks) }
    pub fn proc_notice__caps_up   (&self, ks:&KrustyState) { self.handling.proc_notice__caps_up   (self, ks) }




    // unassigned vks: 0x88-0x8F, 0x97-0x9F, 0xD8-0xDA, 0xE8 ..  undefined: 0x07, 0x0E-0x0F, 0x3A-0x40
    //fn mask (&self) -> Key { Key::Other(0xFF) }
    fn mask (&self) -> Key { Key::OtherKey(0x9A) }


    /// release masking for when key release can have side-effects (e.g for Win and Alt releases)
    fn is_rel_masking (&self) -> bool { matches! ( self.mk, lwin | rwin | lalt )  }  // excludes RAlt

    /// delayed-release is specifically for Win key,
    fn is_rel_delaying (&self) -> bool { matches! ( self.mk, lwin | rwin )  }

    /// unified-keyup is for shift where the OS behavior seems to be to ONLY release shift keys (both if applicable) once both shift keys are up
    fn is_keyup_unified (&self) -> bool { matches! ( self.mk, lshift | rshift )  }

    // todo ^^ the unified behavior is only browser level, not OS level .. so not sure if we should emulate it here ourselves
    // .. if it was here for some actual problem (maybe due to caps doing mk-rel etc) then ok, but most likely could just remove it

    fn paired_down        (&self) -> bool { self.paired() .is_some_and (|p| p.down.is_set()) }
    fn paired_active      (&self) -> bool { self.paired() .is_some_and (|p| p.active.is_set()) }
    fn paired_mngd_active (&self) -> bool { self.paired() .is_some_and (|p| p.mngd_active.is_set()) }

    //fn pair_any_down      (&self) -> bool { self.down.is_set() || self.paired_down() }
    fn pair_any_active      (&self) -> bool { self.active.is_set() || self.paired_active() }
    //fn pair_any_mngd_active (&self) -> bool { self.active.is_set() || self.paired_mngd_active() }

    pub fn release_w_masking (&self) {
        // masking w an unassigned key helps avoid/reduce focus loss to menu etc for alt/win
        self.active.clear();
        if !self.is_rel_masking() || !self.consumed.is_set() {
            self.mk.key().release();
        } else {
            self.mask().release(); self.mk.key().release();
        }
    }
    fn paired_release_w_masking (&self) {
        if let Some(p) = self.paired() { p.release_w_masking() }
    }

    fn reactivate (&self) {
        self.active.set(); self.mk.key().press();
    }
    fn paired_reactivate (&self) {
        if let Some(p) = self.paired() { p.reactivate() }
    }

    pub fn ensure_inactive (&self) {
        // utility to ensure modkey is inactive regardless if held down
        // shouldnt really be necessary since there are action wrappers available to set/restore mod-key for any need at any mod-key state
        self.consumed.set(); self.mngd_active.clear();
        if self.active.is_set() { self.release_w_masking(); } // rel call will clear active flag too
    }
    pub fn ensure_active (&self) {
        // utility to get the mod out reliably whether its currently pressed or not, while keeping state tracking updated
        // this should really ONLY be necessary where we want the mod to be left hanging on until later .. e.g. to simulate alt-tab
        self.consumed.set(); self.mngd_active.set();
        if !self.active.is_set() { self.active.set(); self.mk.key().press(); }
    }



    /// All mod-actions mark the mod-down consumed too, but if its a no-key action (like brightness etc), wrap with this to mark it consumed.
    /// The consumed flag marks it to have its later release be masked with control to avoid activating win-menu etc
    pub fn keydn_consuming_action (&self, af:AF) -> AF {
        let smk = self.clone();
        Arc::new ( move || { smk.consumed.set(); af(); } )
    }


    /// Use this to wrap activation action blindly (whether its already active or not) and without masking on release.
    /// ... Should be useful only in cases we explicitly dont expect any contention and want to avoid masking
    pub fn bare_action (&self, af:AF) -> AF {
        let k = self.mk.key();
        Arc::new ( move || { k.press(); af(); k.release() })
    }

    /// Use this to wrap actions when we want the mod-key to be ACTIVE in the combo .. can use for both self-mod-key combos or unrelated combos.
    /// .. e.g. if setting up alt-X to send alt-win-y, we'd set lalt-mapping on Key::X as k.alt.active_action(k.win.active_on_key(Key::Y))
    pub fn active_action (&self, af:AF) -> AF {
        let smk = self.clone();
        Arc::new ( move || {
            smk.consumed.set();
            if smk.pair_any_active() { af() }
            else { smk.mk.key().press(); af(); smk.release_w_masking(); }
        })
    }
    /// Use this to wrap actions when we want the mod-key to be ACTIVE in the combo .. can use for both self-mod-key combos or unrelated combos.
    /// .. e.g. if setting up alt-X to send alt-win-y, we'd set lalt-mapping on Key::X as k.alt.active_action(k.win.active_on_key(Key::Y))
    pub fn active_on_key (&self, key:Key) -> AF { self.active_action (key_utils::base_action(key)) }
    // ^^ some sugar to make common things simpler


    /// Use this for a forced masked-release to be sent before this action .. can be usedful for doubled-keys for robustness etc
    pub fn masked_released_action (&self, af:AF) -> AF {
        let smk = self.clone();
        Arc::new ( move || { smk.consumed.set(); smk.release_w_masking(); af(); } )
    }


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
                if smk.active.is_set() { smk.release_w_masking() }
                if smk.paired_active() { smk.paired_release_w_masking() }
                af();
                if !smk.is_rel_delaying() {  // (all but win keys)
                    // at least one of the pair was active before .. so we MUST reactivate (even if none are currently down!) ..
                    // .. first, any/both thats down should be reactivated ..
                    if smk.down.is_set() { smk.reactivate() }
                    if smk.paired_down() { smk.paired_reactivate() }
                    // .. but even if none are currently down, we still gotta reactivate self (which is the left one)
                    if !smk.pair_any_active() { smk.reactivate() }
                } else {
                    // we have post release reactivation delays (for win)
                    let smk = smk.clone(); // for the delay closure
                    thread::spawn ( move || {
                        thread::sleep(time::Duration::from_millis(100));
                        // since we're delayed, we'll check if the modkeys are still down before reactivating
                        if smk.down.is_set() { smk.reactivate() }
                        if smk.paired_down() { smk.paired_reactivate() }
                    } );
        } } } )
    }
    pub fn inactive_on_key (&self, key:Key) -> AF { self.inactive_action (key_utils::base_action(key)) }
    // ^^ some sugar to make common things simpler .. could add for ctrl etc too if there was use


}




