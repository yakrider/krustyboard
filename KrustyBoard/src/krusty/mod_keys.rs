#![ allow (non_snake_case, non_camel_case_types) ]


use std::time;
use std::thread;
use std::sync::Arc;
use std::fmt::Debug;
use std::mem::size_of;

use derive_deref::Deref;
use once_cell::sync::OnceCell;
use strum_macros::EnumIter;


use crate::{*, ModKey::*};





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



/// for composition of modkey behavior variations, we define a KeyHandling trait ..
/// then keep a box-dyn of the right variant in the modkey struct ..
/// it needs to be send/sync/'static for cross-thread usage, but shouldnt need to be cloned itself, hence just Box instead of Arc
pub type KH = Box <dyn KeyHandling + Send + Sync + 'static>;


/// We'll used this as the common struct for all types of modkeys whether simple blocked TMKs or the complex SMKs
/// (As with couple extra bytes of storage, we get to keep code close enough to switch easily between TMK/SMK e.g. for lwin)
# [ derive (Debug) ]
pub struct _ModKey {
    // this struct holds flags required for a outside-state syncd tracking of a modifier key (we only do this for alt and win)
    // the alt and win keys are now tracked identically and so doing this helps us avoid duplication of that bunch of related code
    // these flags track whether the key is down, if it has been sent out (active), and if we should release it masked (consumed)
    _private     : (),
    pub modkey   : ModKey,
    pub handling : KH,                 // box dyn w handling behavior for this key (passthrough/blocked/doubled/managed)
    pub pair     : Option <ModKey>,    // pairing to the left/right counterpart if desired
    pub down     : Flag,
    pub active   : Flag,
    pub consumed : Flag,
    pub stamp    : EventStamp,
    pub dbl_tap  : Flag,
}
impl _ModKey {
    pub fn new (modkey:ModKey, handling:KH, pair:Option<ModKey>) -> _ModKey { _ModKey {
        _private : (),
        modkey,
        handling,
        pair,
        down     : Flag::default(),
        active   : Flag::default(),
        consumed : Flag::default(),
        stamp    : EventStamp::default(),
        dbl_tap  : Flag::default(),
    } }
}





/// CapsModKey holds the caps-lock key and its impl as the base for most l2/l3 functionality
# [ derive (Debug, Clone, Deref) ]
pub struct CapsModKey ( Arc <_ModKey> );




/// Unified-Modifier-Key is now used for all modkeys regardless of ModKey_Mgmt behavior variation (other than for the capslock key)
// (for reference, we used to have a SyncedModKey for the fully managed type, and a TrackedModKey for all the others)
# [ derive (Debug, Clone, Deref) ]
pub struct UnifModKey ( Arc <_ModKey> );



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

    // utility fn to make prepping these binding/processing directives less cumbersome
    fn epds (&self, epd: EvProp_D) -> EvProc_Ds {
        EvProc_Ds::new (epd, ComboProc_D::ComboProc_Disable)
    }
    // note that the self is in params simply to allow calling w/o full impl specification
    // (and w that adding 'where Self:Sized' is no longer needed .. which otherwise we'd need for trait object associated fn )
    fn epds_blocked      (&self) -> EvProc_Ds { self.epds (EvProp_D::EvProp_Stop) }
    fn epds_continue     (&self) -> EvProc_Ds { self.epds (EvProp_D::EvProp_Continue) }
    fn epds_undetermined (&self) -> EvProc_Ds { self.epds (EvProp_D::EvProp_Undet) }

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






/// Holds representation for all modifier keys together, interlinked functionality etc impld here
impl ModKeys {

    pub fn new() -> Self {
        ModKeys {
            _private : (),
            // caps is a special singleton for itself
            caps   : CapsModKey::instance(),
            // ralt is fully blocked .. we'll typically use it as shift instead
            ralt   : UnifModKey::new (ralt, Box::new(ModKey_Blocked), None),
            // lwin/rwin are doubled, so their functionality is only activated on dbl-press
            lwin   : UnifModKey::new (lwin, Box::new(ModKey_Doubled), Some(rwin)),
            rwin   : UnifModKey::new (rwin, Box::new(ModKey_Doubled), Some(lwin)),
            // the other modifier-keys are fully managed
            lalt   : UnifModKey::new (lalt,   Box::new(ModKey_Managed), None),
            lctrl  : UnifModKey::new (lctrl,  Box::new(ModKey_Managed), Some(rctrl)),
            rctrl  : UnifModKey::new (rctrl,  Box::new(ModKey_Managed), Some(lctrl)),
            lshift : UnifModKey::new (lshift, Box::new(ModKey_Managed), Some(rshift)),
            rshift : UnifModKey::new (rshift, Box::new(ModKey_Managed), Some(lshift)),
        }
    }

    pub fn get_umk (&self, mk:ModKey) -> Option<UnifModKey> {
        self.mod_umk_pairs() .iter() .filter_map (|(pmk,pumk)| if mk == *pmk { Some((*pumk).clone()) } else { None } ) .next()
    }


    /// NOTE: this 'static' will be our source of ordering for the mod-keys in the combo-mod-keys-state bitmap!!
    pub fn static_combo_bits_mod_keys() -> [ModKey; size_of::<ComboStatesBits_ModKeys>()] {
        // Note that there are bits in combo-bitmap only for physical keys
        // (i.e. it excludes the virtual l/r agnostic enum-vals used during combo construction)
        static COMBO_STATE_BITS_MOD_KEYS: [ModKey; size_of::<ComboStatesBits_ModKeys>()] = { [
            caps,     lalt,     ralt,     lwin,     rwin,     lctrl,     rctrl,     lshift,     rshift,
            caps_dbl, lalt_dbl, ralt_dbl, lwin_dbl, rwin_dbl, lctrl_dbl, rctrl_dbl, lshift_dbl, rshift_dbl,
        ] };
        COMBO_STATE_BITS_MOD_KEYS
    }

    /// NOTE: the ordering in this 'static' tuples array will be used to expand the l/r agnostic combo keys into their specialized versions
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
    pub fn static_dbl_tap_mk_pairs () -> [(ModKey,ModKey);9] {
        let mks = ModKeys::static_combo_bits_mod_keys();
        mks[0..9] .iter().copied() .zip (mks[9..18].iter().copied()) .collect::<Vec<(ModKey,ModKey)>>() .try_into().unwrap()
    }


    pub fn mod_umk_pairs (&self) -> [(ModKey, &UnifModKey);8] { [
        (lalt,   &self.lalt  ), (ralt,   &self.ralt  ),
        (lctrl,  &self.lctrl ), (rctrl,  &self.rctrl ),
        (lshift, &self.lshift), (rshift, &self.rshift),
        (lwin,   &self.lwin  ), (rwin,   &self.rwin  )
    ] }

    pub fn mk_flag_pairs (&self) -> [(ModKey, Option<&Flag>); size_of::<ComboStatesBits_ModKeys>()] { [
        (caps,   Some(&self.caps.down)),
        (lalt,   Some(&self.lalt.down)),
        (ralt,   Some(&self.ralt.down)),
        (lwin,   Some(&self.lwin.down)),
        (rwin,   Some(&self.rwin.down)),
        (lctrl,  Some(&self.lctrl.down)),
        (rctrl,  Some(&self.rctrl.down)),
        (lshift, Some(&self.lshift.down)),
        (rshift, Some(&self.rshift.down)),
        //
        (caps_dbl,   Some(&self.caps.dbl_tap)),
        (lalt_dbl,   Some(&self.lalt.dbl_tap)),
        (ralt_dbl,   Some(&self.ralt.dbl_tap)),
        (lwin_dbl,   Some(&self.lwin.dbl_tap)),
        (rwin_dbl,   Some(&self.rwin.dbl_tap)),
        (lctrl_dbl,  Some(&self.lctrl.dbl_tap)),
        (rctrl_dbl,  Some(&self.rctrl.dbl_tap)),
        (lshift_dbl, Some(&self.lshift.dbl_tap)),
        (rshift_dbl, Some(&self.rshift.dbl_tap)),
        // ^^ note again, that for the combo bitmap construction, the l/r agnostic keys should have been expanded out and eliminated
    ] }

    pub fn get_cur_mod_keys_states_bitmap(&self) -> ComboStatesBits_ModKeys {
        self.mk_flag_pairs() .map (|(_,fgo)| fgo.filter(|fg| fg.is_set()).is_some())
    }
    pub fn make_combo_mod_keys_states_bitmap(mod_keys:&[ModKey]) -> ComboStatesBits_ModKeys {
        ModKeys::static_combo_bits_mod_keys() .map (|mk| mod_keys.contains(&mk))
    }

    // mouse btns notify here in case we need to do cleanup/markings for win move/resize setups
    //pub fn process_mbtn_down (&self, mbtn:MouseButton) { self.lwin.consumed.set(); }
    //pub fn process_mbtn_up   (&self, mbtn:MouseButton) { }

    pub fn some_shift_down (&self) -> bool { self.lshift.down.is_set() || self.rshift.down.is_set() }
    pub fn some_ctrl_down  (&self) -> bool { self.lctrl.down.is_set()  || self.rctrl.down.is_set() }
    pub fn some_alt_down   (&self) -> bool { self.lalt.down.is_set() } // ralt is disabled as an Alt key
    pub fn some_win_down   (&self) -> bool { self.lwin.down.is_set()  || self.rctrl.down.is_set() }

    pub fn some_shift_dbl (&self) -> bool { self.lshift.dbl_tap.is_set() || self.rshift.dbl_tap.is_set() }
    pub fn some_ctrl_dbl  (&self) -> bool { self.lctrl.dbl_tap.is_set()  || self.rctrl.dbl_tap.is_set()  }
    pub fn some_alt_dbl   (&self) -> bool { self.lalt.dbl_tap.is_set()   }  // ralt is disabled as an Alt key
    pub fn some_win_dbl   (&self) -> bool { self.lwin.dbl_tap.is_set()   || self.rctrl.dbl_tap.is_set()  }

    pub fn unstick_all (&self) {
        // all modkey states .. we'll do two loops to interleave them so they dont activate e.g. start-menu
        self.mod_umk_pairs() .iter() .for_each (|(_,umk)| umk.modkey.key().press() );
        // but since ctrl-alt-shift-win press-rel triggers ms-365, we'll insert a dummy key as well
        Key::OtherKey(0x9A).release();
        self.mod_umk_pairs() .iter() .for_each (|(_, umk)| {
            umk.modkey.key().release(); umk.down.clear(); umk.active.clear(); umk.dbl_tap.clear();
        });
        // clear capslock too
        if self.caps.modkey.key().is_toggled() { key_utils::press_release(self.caps.modkey.key().into()) }
        self.caps.down.clear();
    }


    pub fn proc_notice__caps_down (&self) {
        self.mod_umk_pairs() .iter() .for_each (|(_,smk)| smk.proc_notice__caps_down());
    }
    pub fn proc_notice__caps_up (&self) {
        self.mod_umk_pairs() .iter() .for_each (|(_,smk)| smk.proc_notice__caps_up());
    }


    pub fn proc_notice__mouse_btn_down (&self, _mbtn:MouseButton) {
        //self.lwin.consumed.set();  // its just a consumed flag, doesnt matter, we can set it for any mouse btn
        // ^^ leftover from when lwin was SMK instead of TMK_D .. we'll let it be to allow quick lwin TMK/SMK switch
    }
    pub fn proc_notice__mouse_btn_up (&self, _mbtn:MouseButton) {
        // nothing to do for btn-up
    }


    pub fn setup_tracking (&self, k:&Krusty) {
        self.caps.setup_tracking (k);
        self.mod_umk_pairs() .iter() .for_each (|(_, umk)| umk.setup_tracking(k));
    }

}





/// Impl for the caps-lock key specific functionality
impl CapsModKey {

    // ^^ CMK : Caps-Modifier-Key type .. basically tracks caps state and sets up caps as the global Layer-2/3/qks etc modifier key
    // (other mod-keys are via SMK (Syncd-Tracked modifier key: alt/win)  or TMK (Tracked modifier keys: ralt/lctrl/rctrl/lshift/rshift))

    pub fn instance () -> Self {
        // note that since ofc there's only one caps key, we'll set this up as singleton (unlike for the TMKs and SMKs below)
        // also note that while we use ModKey_Blocked here to satisfy construction, that field is irrelevant outside UniModKey
        static INSTANCE: OnceCell<CapsModKey> = OnceCell::new();
        INSTANCE .get_or_init (||
            Self ( Arc::new ( _ModKey::new (caps, Box::new(ModKey_Blocked), None) ) )
        ) .clone()
    }

    fn handle_key_down (&self, ks:&KrustyState, ev:&Event) {
        //println!("Caps DOWN : {:?}, inj: {:?}", ev.key, ev.injected);

        // note that for caps, we completely block it from ever being sent up, and just manage internally
        if !self.down.is_set() {
            // capslock can come as repeats like other mod keys .. this was a fresh one
            self.down.set();
            update_stamp_key_dbl_tap (ev.stamp, &self.stamp, &self.dbl_tap);

            // lets notify the synced tracked mod keys, so they can invalidate/release themselves
            ks.mod_keys.proc_notice__caps_down();
            ks.mouse.proc_notice__modkey_down (self.modkey, &ks);
        }
        if ks.mouse.lbtn.down.is_set() && !ks.mod_keys.lwin.down.is_set() && !ks.in_managed_ctrl_down_state.is_set() {
            // caps w mouse lbtn down, should be managed ctrl down (for ctrl-click, drag-drop etc)
            ks.in_managed_ctrl_down_state.set();
            ks.mod_keys.lctrl.ensure_active();
        }
    }

    fn handle_key_up (&self, ks:&KrustyState, _ev:&Event) {
        //println!("Caps UP : {:?}, inj: {:?}", _ev.key, _ev.injected);
        self.down.clear();
        self.dbl_tap.clear();
        ks.mouse.proc_notice__modkey_up(self.modkey, &ks);
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
        use crate::{EvProp_D::*, KbdEvCbMapKey_T::*, EvCbMapKey_Action::*, ComboProc_D::*, EvCbFn_T::*};

        if Key::CapsLock.is_toggled() { // toggle off first if necessary (to clear key light)
            key_utils::press_release (Key::CapsLock);
        }

        let ks = k.ks.clone();
        let ev_proc_ds = EvProc_Ds::new (EvProp_Stop, ComboProc_Disable);
        let cb = EvCbFn_Inline( Arc::new ( move |ev| { ks.mod_keys.caps.handle_key_down(&ks, &ev); ev_proc_ds } ) );
        k.iproc.input_bindings .bind_kbd_event (self.modkey.key(), KeyEventCb(KeyEventCb_KeyDown), EvCbEntry { ev_proc_ds, cb } );

        let ks = k.ks.clone();
        let cb = EvCbFn_Inline( Arc::new ( move |ev| { ks.mod_keys.caps.handle_key_up(&ks, &ev); ev_proc_ds } ) );
        k.iproc.input_bindings .bind_kbd_event (self.modkey.key(), KeyEventCb(KeyEventCb_KeyUp), EvCbEntry { ev_proc_ds, cb } );
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
        self.epds_continue()
    }

    fn handle_key_up (&self, bmk:&UnifModKey, _:&KrustyState) -> EvProc_Ds {
        bmk.active.clear();
        self.epds_continue()
    }

}


/// blocked modkey handling simply stops all propagation (physical states are tracked, but it should never be logically active)
impl KeyHandling for ModKey_Blocked {

    fn handling_type(&self) -> ModKey_Mgmt { ModKey_Mgmt::MK_Mgmt_Blocked }

    fn handle_key_down (&self, _:&UnifModKey, _:&KrustyState) -> EvProc_Ds {
        self.epds_blocked()
    }

    fn handle_key_up (&self, _:&UnifModKey, _:&KrustyState) -> EvProc_Ds {
        self.epds_blocked()
    }

}


/// doubled modkeys behave like regular when double-tapped, but single taps are monitored but blocked
impl KeyHandling for ModKey_Doubled {

    fn handling_type(&self) -> ModKey_Mgmt { ModKey_Mgmt::MK_Mgmt_Doubled }

    fn handle_key_down (&self, bmk:&UnifModKey, _:&KrustyState) -> EvProc_Ds {
        if bmk.dbl_tap.is_set() {
            bmk.active.set();
            self.epds_continue()
        } else {
            self.epds_blocked()
        }
    }

    fn handle_key_up (&self, bmk:&UnifModKey, _:&KrustyState) -> EvProc_Ds {
        if bmk.active.is_set() {
            bmk.active.clear();
            self.epds_continue()
        } else {
            self.epds_blocked()
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

    fn handling_type(&self) -> ModKey_Mgmt { ModKey_Mgmt::MK_Mgmt_Managed }

    fn handle_key_down (&self, bmk:&UnifModKey, ks:&KrustyState) -> EvProc_Ds {
        // we should clear the consumed flag, but not if mouse btns are down, so we'll just put mouse-btns state there
        //self.consumed.clear();
        bmk.consumed .store ( ks.mouse.lbtn.down.is_set() || ks.mouse.rbtn.down.is_set() );

        if ks.mod_keys.caps.down.is_clear() {
            // caps isnt down (and its repeat filtered), so record it and let it through (or send replacment as detailed above)
            bmk.active.set();
            let key = bmk.modkey.key();
            thread::spawn (move || key.press());
        } // else if caps was down, we just block it
        self.epds_blocked()
    }

    fn handle_key_up (&self, bmk:&UnifModKey, _:&KrustyState) -> EvProc_Ds {
        // if caps is pressed, or alt is already inactive (via masked-rel, press-rel etc), we block it
        // else if win was consumed, we release with mask, else we can actually pass it through unblocked
        // (note.. no more passing through of mod-keys, we'll instead send replacement ones if we need to (due to R/L sc-codes mismatch etc))
        if bmk.active.is_clear() { //|| ks.mod_keys.caps.down.is_set() {
            // ^^ since caps-dn releases mod-keys, we dont need to check that here ..
            //  .. EXCEPT for alt when switche is fgnd, in which case, we'd want to release it even w caps down anyway
            // if inactive (usually due to caps-down) we just suppress this keyup
        } else {
            if bmk.is_keyup_unified() && bmk.paired_down() {
                // for shift (w/ keyup state unified), ONLY send up a keyup if the other key isnt down .. so do nothing, not even clear active
            } else {
                let umk = bmk.clone();
                thread::spawn ( move || {
                    umk.release_w_masking();  // this checks/updates flags too
                    if umk.is_keyup_unified() { // and for up-unified, try and clear the other too
                        umk.paired() .iter() .for_each (|p| {
                            if !p.down.is_set() && p.active.is_set() { p.release_w_masking(); }
                    } ) }
                } );
        }  }
        self.epds_blocked()
    }

}




/// base impl for shared functionality among all mod-key types
impl UnifModKey {


    pub fn new (mk: ModKey, handling: KH, pair:Option<ModKey>) -> Self {
        Self ( Arc::new ( _ModKey::new (mk, handling, pair) ) )
    }


    pub fn paired (&self) -> Option<UnifModKey> {
        self.pair .map (|p| KrustyState::instance().mod_keys.get_umk(p)) .flatten()
    }


    /// NOTE re injected events .. we block our own (and ahk) injections at hook level .. so anything here is external
    // so we'll want to let them through, only updating our tracking of external state (not our physical state)

    fn handle_key_down (&self, ev: Event, ks:&KrustyState) -> EvProc_Ds {
        if ev.injected {
            self.active.set();
            return self.handling.epds_continue()
        }
        if self.down.is_set() {     // repeats are blocked w/o further processing
            return self.handling.epds_blocked()
        }
        //println!("mod new DOWN : {:?}, inj: {:?}",ev.key, ev.injected);

        // now first lets do some common work (physical state etc) ..
        self.down.set();
        update_stamp_key_dbl_tap (ev.stamp, &self.stamp, &self.dbl_tap);
        ks.mouse.proc_notice__modkey_down (self.modkey, &ks);

        // then for external active state etc updates, we'll call the mgmt specific fns
        self.handling.handle_key_down (self, ks)
    }


    fn handle_key_up (&self, ev: Event, ks:&KrustyState) -> EvProc_Ds {
        //println!("mod new DOWN : {:?}, inj: {:?}",ev.key, ev.injected);
        if ev.injected {
            self.active.clear();
            return self.handling.epds_continue()
        }
        // lets do some common work (physical state etc) ..
        self.down.clear(); self.dbl_tap.clear();
        ks.mouse.proc_notice__modkey_up (self.modkey, &ks);
        if self.modkey == lctrl || self.modkey == rctrl { ks.in_ctrl_tab_scroll_state.clear() }

        // then for external active state etc updates, we'll call the mgmt specific fns
        self.handling.handle_key_up (&self, ks)
    }


    pub fn setup_tracking (&self, k:&Krusty) {
        // the setup for these is mostly just tracking their state flags ..
        // however, we will also disable repeats, not least to ease looking at keystreams
        use crate::{KbdEvCbMapKey_T::*, EvCbMapKey_Action::*, EvCbFn_T::*};

        let umk = self.clone(); let ks = k.ks.clone();
        k.iproc.input_bindings .bind_kbd_event (
            self.modkey.key(), KeyEventCb(KeyEventCb_KeyDown), EvCbEntry {
                ev_proc_ds: self.handling.epds_undetermined(),
                cb: EvCbFn_Inline( Arc::new (move |ev| { umk.handle_key_down (ev, &ks) } ) )
        } );

        let umk = self.clone(); let ks = k.ks.clone();
        k.iproc.input_bindings .bind_kbd_event (
            self.modkey.key(), KeyEventCb(KeyEventCb_KeyUp), EvCbEntry {
                ev_proc_ds: self.handling.epds_undetermined(),
                cb: EvCbFn_Inline( Arc::new (move |ev| { umk.handle_key_up (ev, &ks) } ) )
        } );
    }



    pub fn proc_notice__caps_down(&self) {
        // we will immediately invalidate and clear any down mod-key found upon caps activation!
        // note that internally tracked physical is_down will continue to be down
        // note also that each of paired mod-keys will get their own notification too
        if self.handling.is_managed()  &&  self.down.is_set() && self.active.is_set() {
            self.consumed.set();
            let umk = self.clone();
            thread::spawn ( move || {
                // we want to release mod-key upon caps .. (unless it would interfere w/ switch alt-tab)
                if WinEventsListener::instance().fgnd_info.read().unwrap().exe != "Switche.exe" {
                    umk.release_w_masking()   // this will update flags too
                }
            } );
        }
    }
    pub fn proc_notice__caps_up (&self) {
        // since we deactivate mod-keys on caps press, check to see if we want to reactivate them
        // note: we'll setup a delay for activation to allow for some sloppy combo releases etc
        // note also, that if inspecting in browser-key-events, this might appear unexpected coz browser does its own 'unifying'
        // .. so to check the logic here must use lower level key inspections like via ahk key history!!
        // plus if doing caps release while both shift down, on my machine even the raw events are wonky (no caps evnt until one releases!!)
        if self.handling.is_managed()  &&  self.down.is_set() { //&& !KrustyState::instance().in_right_btn_scroll_state.is_set() {
            let umk = self.clone();
            thread::spawn ( move || {
                thread::sleep(time::Duration::from_millis(150));
                if umk.down.is_set() && !umk.active.is_set() {
                    umk.modkey.key().press(); umk.active.set(); umk.consumed.set();
            } } );
        }
    }





    // unassigned vks: 0x88-0x8F, 0x97-0x9F, 0xD8-0xDA, 0xE8 ..  undefined: 0x07, 0x0E-0x0F, 0x3A-0x40
    //fn mask (&self) -> Key { Key::Other(0xFF) }
    fn mask (&self) -> Key { Key::OtherKey(0x9A) }


    fn is_rel_masking   (&self) -> bool { self.modkey.key() == Key::LWin || self.modkey.key() == Key::RWin || self.modkey.key() == Key::LAlt } // excluding RAlt
    fn is_rel_delaying  (&self) -> bool { self.modkey.key() == Key::LWin || self.modkey.key() == Key::RWin }
    fn is_keyup_unified (&self) -> bool { self.modkey.key() == Key::LShift || self.modkey.key() == Key::RShift }

    fn paired_down     (&self) -> bool { self.paired() .iter() .any (|p| p.down.is_set()) }
    fn paired_active   (&self) -> bool { self.paired() .iter() .any (|p| p.active.is_set()) }
    fn pair_any_active (&self) -> bool { self.active.is_set() || self.paired_active() }

    pub fn release_w_masking(&self) {
        // masking w an unassigned key helps avoid/reduce focus loss to menu etc for alt/win
        self.active.clear();
        if !self.is_rel_masking() || !self.consumed.is_set() { self.modkey.key().release(); }
        else { self.mask().release(); self.modkey.key().release(); }
    }

    fn reactivate        (&self) { self.active.set(); self.modkey.key().press(); }
    fn paired_reactivate (&self) { self.paired() .iter() .for_each (|p| p.reactivate()) }

    pub fn ensure_inactive (&self) {
        // utility to ensure modkey is inactive regardless if held down
        // shouldnt really be necessary since there are action wrappers available to set/restore mod-key for any need at any mod-key state
        self.consumed.set();
        if self.active.is_set() { self.release_w_masking(); } // rel call will clear active flag too
    }
    pub fn ensure_active (&self) {
        // utility to get the mod out reliably whether its currently pressed or not, while keeping state tracking updated
        // this should really ONLY be necessary where we want the mod to be left hanging on until later .. e.g. to simulate alt-tab
        self.consumed.set();
        if !self.active.is_set() { self.active.set(); self.modkey.key().press(); }
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
        let k = self.modkey.key();
        Arc::new ( move || { k.press(); af(); k.release() })
    }

    /// Use this to wrap actions when we want the mod-key to be ACTIVE in the combo .. can use for both self-mod-key combos or unrelated combos.
    /// .. e.g. if setting up alt-X to send alt-win-y, we'd set lalt-mapping on Key::X as k.alt.active_action(k.win.active_on_key(Key::Y))
    pub fn active_action (&self, af:AF) -> AF {
        let smk = self.clone();
        Arc::new ( move || {
            smk.consumed.set();
            if smk.pair_any_active() { af() }
            else { smk.modkey.key().press(); af(); smk.release_w_masking(); }
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
                if smk.paired_active() { smk.paired() .iter() .for_each (|p| p.release_w_masking()) }
                af();
                if !smk.is_rel_delaying() { // post release reactivation delays (for win)
                    // basically any/both thats down is activated, but if none are down, still reactivate self (which is the left one)
                    if smk.paired_down() { smk.paired_reactivate() } else { smk.reactivate() }
                    if smk.down.is_set() && !smk.active.is_set() { smk.reactivate() } // if still not active from above when down, do it
                } else {
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




