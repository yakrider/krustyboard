#![ allow (non_snake_case) ]


use std::mem::size_of;
use std::time;
use std::thread;
use std::ops::Deref;
use std::sync::Arc;

use once_cell::sync::OnceCell;
use strum_macros::EnumIter;


use crate::{*, ModKey::*};





/// All the supported modifier-keys (incl some w incomplete impl like rwin)
# [ allow (non_camel_case_types) ]
# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter) ]
pub enum ModKey {
    no_mk,
    caps,     lalt,     ralt,     lwin,     rwin,     lctrl,     rctrl,     lshift,     rshift,
    caps_dbl, lalt_dbl, ralt_dbl, lwin_dbl, rwin_dbl, lctrl_dbl, rctrl_dbl, lshift_dbl, rshift_dbl,
    alt, win, ctrl, shift,
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



/// We'll used this as the common struct for all types of modkeys whether simple blocked TMKs or the complex SMKs
/// (As with couple extra bytes of storage, we get to keep code close enough to switch easily between TMK/SMK e.g. for lwin)
# [ derive (Debug) ]
pub struct _ModKey {
    // this struct holds flags required for a outside-state syncd tracking of a modifier key (we only do this for alt and win)
    // the alt and win keys are now tracked identically and so doing this helps us avoid duplication of that bunch of related code
    // these flags track whether the key is down, if it has been sent out (active), and if we should release it masked (consumed)
    _private     : (),
    pub modkey   : ModKey,
    pub pair     : Option <ModKey>,    // pairing to the left/right counterpart if desired
    pub down     : Flag,
    pub active   : Flag,
    pub consumed : Flag,
    pub stamp    : EventStamp,
    pub dbl_tap  : Flag,
}
impl _ModKey {
    pub fn new (modkey:ModKey, pair:Option<ModKey>) -> _ModKey { _ModKey {
        _private : (),
        modkey,
        pair,
        down     : Flag::default(),
        active   : Flag::default(),
        consumed : Flag::default(),
        stamp    : EventStamp::default(),
        dbl_tap  : Flag::default(),
    } }
}



/// CpasModKey holds the caps-lock key and its impl as the base for most l2/l3 functionality
# [ derive (Debug, Clone) ]
pub struct CapsModKey ( Arc <_ModKey> );

impl Deref for CapsModKey {
    type Target = _ModKey;
    fn deref(&self) -> &Self::Target { &self.0 }
}
// NOTE rest of the impl for this further down





/// TrackedModKeys only have key-down-state tracked, but no syncing against active-state externally
# [ derive (Debug, Clone) ]
pub struct TrackedModKey ( Arc <_ModKey> );

impl Deref for TrackedModKey {
    type Target = _ModKey;
    fn deref(&self) -> &Self::Target { &self.0 }
}
// NOTE rest of the impl for this further down


#[ allow (non_camel_case_types) ]
pub enum TrackedModKey_E {
    TrackedModKey_Passthrough,
    TrackedModKey_Blocked,
    TrackedModKey_Doubled,
}





/// Synced-Modifier-Key tracks modifier key down-states as well as externally active state (used for lalt/lwin/lctrl/rctrl/lshift/rshift)
// this struct holds flags required for a outside-state syncd tracking of a modifier key
// the alt (and win before) and now shift/ctrl keys are tracked identically and so doing this helps us avoid code duplication
# [ derive (Debug, Clone) ]
pub struct SyncdModKey ( Arc <_ModKey> );

impl Deref for SyncdModKey {
    type Target = _ModKey;
    fn deref(&self) -> &Self::Target { &self.0 }
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

    _private   : (),
    // capslock tracking
    pub caps   : CapsModKey,
    // ralt is only tracked, but not synced .. (and now lwin/rwin too as their native functionality is on double-tap)
    pub ralt   : TrackedModKey,
    pub lwin   : TrackedModKey,
    pub rwin   : TrackedModKey,
    // the rest are synced mode keys
    pub lalt   : SyncdModKey,
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
    fn deref(&self) -> &Self::Target { &self.0 }
}





/// Holds representation for all modifier keys together, interlinked functionality etc impld here
impl ModKeys {

    pub fn new() -> Self {
        Self ( Arc::new ( _ModKeys {
            _private : (),
            // caps is a special singleton for itself
            caps   : CapsModKey::instance(),
            // ralt (and now lwin/rwin) as TMK, for which we track but dont control if its in sync w outside
            ralt   : TrackedModKey::new (ralt, None),
            lwin   : TrackedModKey::new (lwin, Some(rwin)),
            rwin   : TrackedModKey::new (rwin, Some(lwin)),
            // the rest are SMKs (synced mod keys)
            lalt   : SyncdModKey::new (lalt,   None),
            lctrl  : SyncdModKey::new (lctrl,  Some(rctrl)),
            rctrl  : SyncdModKey::new (rctrl,  Some(lctrl)),
            lshift : SyncdModKey::new (lshift, Some(rshift)),
            rshift : SyncdModKey::new (rshift, Some(lshift)),
        } ) )
    }

    pub fn get_smk (&self, mk:ModKey) -> Option <SyncdModKey> {
        self.mod_smk_pairs() .iter() .filter_map (|(pmk,psmk)| if mk == *pmk { Some(psmk.clone()) } else { None } ) .cloned() .next()
    }
    // todo ^^ prob could make an enum-ordinal based array to avoid the looping in get_smk

    pub fn get_tmk (&self, mk:ModKey) -> Option <TrackedModKey> {
        self.mod_tmk_pairs() .iter() .filter_map (|(pmk,ptmk)| if mk == *pmk { Some(ptmk.clone()) } else { None } ) .cloned() .next()
    }

    pub fn is_lwin_smk (&self) -> bool {
        // this is simply to allow code to remain maximally compatible with switching lwin from TMK_D to SMK
        false
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
    pub fn static_dbl_tap_mk_pairs () -> [(ModKey,ModKey);9] {
        let mks = ModKeys::static_combo_bits_mod_keys();
        mks[0..9] .iter().copied() .zip (mks[9..18].iter().copied()) .collect::<Vec<(ModKey,ModKey)>>() .try_into().unwrap()
    }


    pub fn mod_smk_pairs (&self) -> [(ModKey, &SyncdModKey);5] { [ // note that ralt is TMK not SMK (and doesnt to activate/inactivate etc)
        (lalt,   &self.lalt  ),
        //(lwin,   &self.lwin  ), (rwin,   &self.rwin  ),
        (lctrl,  &self.lctrl ), (rctrl,  &self.rctrl ),
        (lshift, &self.lshift), (rshift, &self.rshift),
    ] }
    pub fn mod_tmk_pairs (&self) -> [(ModKey, &TrackedModKey);3] { [
        //(ralt, &self.ralt),
        (ralt, &self.ralt), (lwin, &self.lwin), (rwin, &self.rwin)
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
        // clear SMKs .. we'll do two loops to interleave them so they dont activate e.g. start-menu
        let smks = [ &self.lalt, &self.lctrl, &self.rctrl, &self.lshift, &self.rshift ];
        smks .iter() .for_each (|smk| smk.modkey.key().press());
        smks .iter() .for_each (|smk| { smk.modkey.key().release(); smk.down.clear(); smk.active.clear(); smk.dbl_tap.clear(); });
        // clear TMKs
        let tmks = [ &self.ralt, &self.lwin, &self.rwin ];
        tmks .iter() .for_each (|tmk| tmk.modkey.key().press() );
        tmks .iter() .for_each (|tmk| { tmk.modkey.key().release(); tmk.down.clear(); tmk.active.clear(); tmk.dbl_tap.clear(); });
        // clear capslock too
        if self.caps.modkey.key().is_toggled() { key_utils::press_release(self.caps.modkey.key().into()) }
        self.caps.down.clear();
    }

    pub fn proc_notice__caps_down (&self) {
        self.mod_smk_pairs() .iter() .for_each (|(_,smk)| smk.proc_notice__caps_down());
    }
    pub fn proc_notice__caps_up (&self) {
        self.mod_smk_pairs() .iter() .for_each (|(_,smk)| smk.proc_notice__caps_up());
    }

    pub fn proc_notice__mouse_btn_down (&self, _mbtn:MouseButton) {
        self.lwin.consumed.set();  // its just a consumed flag, doesnt matter, we can set it for any mouse btn
        // ^^ leftover from when lwin was SMK instead of TMK_D .. we'll let it be to allow quick lwin TMK/SMK switch
    }
    pub fn proc_notice__mouse_btn_up (&self, _mbtn:MouseButton) {
        // nothing to do for btn-up
    }



    pub fn setup_tracking (&self, k:&Krusty) {
        use TrackedModKey_E::*;
        // setup capslock, we'll completely disable it other than for krusty use
        self.caps.setup_tracking (k);

        // setup tracking for right-alt .. its completely blocked but tracked, we use it as shifts and combos
        // (ctrl/shift etc used to be like this, but they have been promoted to full synced-mod-key tracking since)
        self.ralt .setup_tracking (k, TrackedModKey_Blocked);      // for ralt can setup w doBlock=true

        // we've moved lwin/rwin from SMK to to only activating native funcctionality on double-tap
        self.lwin.setup_tracking (k, TrackedModKey_Doubled);
        self.rwin.setup_tracking (k, TrackedModKey_Doubled);

        // lalt (and lwin before) are set as syncd-modifier-key with special (but identical) impl .. more details under SyncdModKey impl
        self.lalt.setup_tracking (k);

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

    pub fn instance () -> Self {
        // note that since ofc there's only one caps key, we'll set this up as singleton (unlike for the TMKs and SMKs below)
        static INSTANCE: OnceCell<CapsModKey> = OnceCell::new();
        INSTANCE .get_or_init (||
            Self ( Arc::new ( _ModKey::new(caps,None) ) )
        ) .clone()
    }

    fn handle_key_down (&self, ks:&KrustyState, ev:&KbdEvent) {
        // note: ^^ redundantly passing KrustyState is cheaper than atomic lookups (of singleton) that coordinate across cpu cores
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

    fn handle_key_up (&self, ks:&KrustyState, _ev:&KbdEvent) {
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
        use crate::{EventPropagationDirective::*, KbdEventCbMapKeyType::*, KbdEvCbComboProcDirective::*, KbdEventCallbackFnType::*};

        if Key::CapsLock.is_toggled() { // toggle off first if necessary (to clear key light)
            key_utils::press_release (Key::CapsLock);
        }

        let ks = k.ks.clone();
        let event_proc_d = KbdEvProcDirectives::new (EventProp_Stop, ComboProc_Disable);
        let cb = KbdEvCbFn_InlineCallback ( Arc::new ( move |ev| { ks.mod_keys.caps.handle_key_down(&ks,&ev); event_proc_d } ) );
        k.iproc.kbd_bindings .bind_kbd_event (self.modkey.key(), KeyEventCb_KeyDown, KbdEventCallbackEntry { event_proc_d, cb } );

        let ks = k.ks.clone();
        let cb = KbdEvCbFn_InlineCallback ( Arc::new ( move |ev| { ks.mod_keys.caps.handle_key_up(&ks,&ev); event_proc_d } ) );
        k.iproc.kbd_bindings .bind_kbd_event (self.modkey.key(), KeyEventCb_KeyUp, KbdEventCallbackEntry { event_proc_d, cb } );
    }

}





/// Impl for Tracked-Modifier-Key functionality (currently only for R-Alt)
impl TrackedModKey {

    // ^^ TMK : Tracked-Modifier-Key type .. unlike the SMK, this only tracks the physical (is down) state of the modifier key
    // we should currently be doing this for left/right ctrl/shift, as well as right-alt

    pub fn new (mk: ModKey, pair:Option<ModKey>) -> Self {
        Self ( Arc::new ( _ModKey::new(mk,pair) ) )
    }

    # [ allow (dead_code) ]
    fn paired (&self) -> Option <&SyncdModKey> {
        static PAIRED : OnceCell <Option <SyncdModKey>> = OnceCell::new();
        PAIRED .get_or_init (||
            self.pair .map (|p| KrustyState::instance().mod_keys.get_smk(p)) .flatten()
        ) .as_ref()
    }


    fn epds (epd:EventPropagationDirective) -> KbdEvProcDirectives {
        KbdEvProcDirectives::new (epd, KbdEvCbComboProcDirective::ComboProc_Disable)
    }
    fn epds_blocked  () -> KbdEvProcDirectives { Self::epds (EventPropagationDirective::EventProp_Stop) }
    fn epds_continue () -> KbdEvProcDirectives { Self::epds (EventPropagationDirective::EventProp_Continue) }
    fn epds_checked  () -> KbdEvProcDirectives { Self::epds (EventPropagationDirective::EventProp_Undetermined) }


    pub fn handle_key_down__tmk_p (&self, ev:KbdEvent, ks:&KrustyState) -> KbdEvProcDirectives {
        if self.down.is_set() {
            Self::epds_blocked()
        } else {
            if !ev.injected {
                self.down.set(); self.active.set();
                update_stamp_key_dbl_tap (ev.stamp, &self.stamp, &self.dbl_tap);
                ks.mouse.proc_notice__modkey_down (self.modkey, &ks);
            }
            Self::epds_continue()
        }
    }
    pub fn handle_key_down__tmk_b (&self, ev:KbdEvent, ks:&KrustyState) -> KbdEvProcDirectives {
        if !ev.injected && !self.down.is_set() {
            self.down.set();
            update_stamp_key_dbl_tap (ev.stamp, &self.stamp, &self.dbl_tap);
            ks.mouse.proc_notice__modkey_down (self.modkey, &ks);
        }
        Self::epds_blocked()
    }
    pub fn handle_key_down__tmk_d (&self, ev:KbdEvent, ks:&KrustyState) -> KbdEvProcDirectives {
        if self.down.is_set() {
            // this is a repeat, we dont even want to update dbl-tap stamps on these
            Self::epds_blocked()
        } else {
            if !ev.injected {
                self.down.set();
                update_stamp_key_dbl_tap (ev.stamp, &self.stamp, &self.dbl_tap);
                ks.mouse.proc_notice__modkey_down (self.modkey, &ks);
                if self.dbl_tap.is_set() {
                    // a double tap .. we'll let this through
                    self.active.set();
                    Self::epds_continue()
                } else {
                    // for first taps for these tmk_doubled, we just block it
                    Self::epds_blocked()
                }
            } else {
                Self::epds_continue()
            }
        }
    }

    pub fn handle_key_up__tmk_p (&self, ev:KbdEvent) -> KbdEvProcDirectives {
        if !ev.injected { self.down.clear(); self.dbl_tap.clear(); self.active.clear() }
        Self::epds_continue()
    }
    pub fn handle_key_up__tmk_b (&self, ev:KbdEvent) -> KbdEvProcDirectives {
        if !ev.injected { self.down.clear(); self.dbl_tap.clear(); self.active.clear() }
        Self::epds_blocked()
    }
    pub fn handle_key_up__tmk_d (&self, ev:KbdEvent) -> KbdEvProcDirectives {
        if !ev.injected {
            self.down.clear(); self.dbl_tap.clear();
            if self.active.is_set() {
                self.active.clear();
                return Self::epds_continue()
            } else {
                return Self::epds_blocked()
            }
        }
        Self::epds_continue()
    }

    pub fn setup_tracking (&self, k:&Krusty, tmk_e:TrackedModKey_E) {
        // the setup for these is mostly just tracking their state flags ..
        // however, we will also disable repeats, not least to ease looking at keystreams
        use crate::{TrackedModKey_E::*, KbdEventCbMapKeyType::*, KbdEventCallbackFnType::*};

        let tmk = self.clone(); let ks = k.ks.clone();
        let (event_proc_d, cb_fn) = match tmk_e {
            TrackedModKey_Passthrough => {( Self::epds_continue(), Arc::new (move |ev| { tmk.handle_key_down__tmk_p (ev, &ks) }) as KbdEvCbFn_InThreadCb_T )},
            TrackedModKey_Blocked     => {( Self::epds_blocked(),  Arc::new (move |ev| { tmk.handle_key_down__tmk_b (ev, &ks) }) as KbdEvCbFn_InThreadCb_T )},
            TrackedModKey_Doubled     => {( Self::epds_checked(),  Arc::new (move |ev| { tmk.handle_key_down__tmk_d (ev, &ks) }) as KbdEvCbFn_InThreadCb_T )},
        };
        k.iproc.kbd_bindings .bind_kbd_event (self.modkey.key(), KeyEventCb_KeyDown, KbdEventCallbackEntry {
            event_proc_d, cb: KbdEvCbFn_InlineCallback (cb_fn)
        } );

        let tmk = self.clone();
        let (event_proc_d, cb_fn) = match tmk_e {
            TrackedModKey_Passthrough => {( Self::epds_continue(), Arc::new (move |ev| { tmk.handle_key_up__tmk_p (ev) }) as KbdEvCbFn_InThreadCb_T )},
            TrackedModKey_Blocked     => {( Self::epds_blocked(),  Arc::new (move |ev| { tmk.handle_key_up__tmk_b (ev) }) as KbdEvCbFn_InThreadCb_T )},
            TrackedModKey_Doubled     => {( Self::epds_checked(),  Arc::new (move |ev| { tmk.handle_key_up__tmk_d (ev) }) as KbdEvCbFn_InThreadCb_T )},
        };
        k.iproc.kbd_bindings .bind_kbd_event (self.modkey.key(), KeyEventCb_KeyUp, KbdEventCallbackEntry {
            event_proc_d, cb: KbdEvCbFn_InlineCallback (cb_fn)
        } );
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

    fn release_w_masking(&self) {
        // masking w an unassigned key helps avoid/reduce focus loss to menu etc for alt/win
        self.active.clear();
        if !self.is_rel_masking() || !self.consumed.is_set() { self.modkey.key().release(); }
        else { self.mask().press(); self.modkey.key().release(); self.mask().release(); }
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
    #[allow(dead_code)]
    pub fn inactive_on_key (&self, key:Key) -> AF { self.inactive_action (key_utils::base_action(key)) }
    // ^^ some sugar to make common things simpler .. could add for ctrl etc too if there was use


}






/// Impl for Synced-Modifier-Key functionality .. (e.g. for lalt/lwin/lctrl/rctrl/lshift/rshift)
impl SyncdModKey {

    // ^^ SMK : Synced-Modifier-Key type .. tracks internal (is down), external (is active), and to-mask (is consumed) states
    // .. we should currently be doing this for left-alt and left-win identically (.. expanded out to ctrl/shift too)
    // ^^ lwin/rwin moved out to new doubled-TMK, so now back to mostly for lalt but w ctrl/shift set this way too coz might as well

    pub fn new (mk: ModKey, pair: Option<ModKey>) -> Self {
        Self ( Arc::new ( _ModKey::new(mk,pair) ) )
    }

    fn _paired (&self) -> Option <&SyncdModKey> {
        static PAIRED : OnceCell <Option <SyncdModKey>> = OnceCell::new();
        PAIRED .get_or_init (|| {
            let p = self.pair .map (|p| KrustyState::instance().mod_keys.get_smk(p)) .flatten();
            println!("paired: {:?}",((self.modkey, p.as_ref())));
            p }
        ) .as_ref()
    }
    fn paired (&self) -> Option <SyncdModKey> {
        self.pair .map (|p| KrustyState::instance().mod_keys.get_smk(p)) .flatten()
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

    fn handle_key_down (&self, ks:&KrustyState, ev:KbdEvent) {
        if ks.mod_keys.caps.down.is_set() {
            // caps is down, record alt being down if not already, but either way block it (so no change to alt-active state)
            self.consumed.clear();
            //if !self.down.is_set() { ks.mouse.proc_notice__modkey_down (self.modkey, &ks) }
            if !ev.injected {
                if !self.down.is_set() {
                    self.down.set();
                    ks.mouse.proc_notice__modkey_down (self.modkey, &ks);
                    update_stamp_key_dbl_tap (ev.stamp, &self.stamp, &self.dbl_tap);
                }
            } else {  self.active.set() }
            // ^^ if its injected, we'll at least try to keep our state in sync w outside (otherwise w caps down, we never send mod press)
        } else {
            if self.down.is_set() {
                // caps isnt down, but we were, so its repeat .. we'll block it even if out-of-sync or its coming after combo caps release
                if ev.injected { self.active.set() }  // but for injected events, at least update update our internal model
            } else {
                // caps isnt down, and our mod key wasnt down i.e not a key-repeat, so record states and let it through
                if !ev.injected {
                    self.down.set();
                    update_stamp_key_dbl_tap (ev.stamp, &self.stamp, &self.dbl_tap);
                    ks.mouse.proc_notice__modkey_down (self.modkey, &ks);
                }
                self.active.set(); self.consumed.clear();
                let key = self.modkey.key();
                thread::spawn (move || key.press());
        } }
        if ks.mouse.lbtn.down.is_set() || ks.mouse.rbtn.down.is_set() { self.consumed.set() }
    }

    fn handle_key_up (&self, ks:&KrustyState, ev:KbdEvent) {
        // if caps is pressed, or alt is already inactive (via masked-rel, press-rel etc), we block it
        // else if win was consumed, we release with mask, else we can actually pass it through unblocked
        // (note.. no more passing through of mod-keys, we'll instead send replacement ones if we need to (due to R/L sc-codes mismatch etc))
        if !ev.injected {
            self.down.clear(); self.dbl_tap.clear();
            ks.mouse.proc_notice__modkey_up (self.modkey, &ks);
            if self.modkey == lctrl || self.modkey == rctrl { ks.in_ctrl_tab_scroll_state.clear() }
        }
        if !self.active.is_set() || ks.mod_keys.caps.down.is_set() {
            // if inactive or caps-down we just suppress this keyup
        } else {
            if self.is_keyup_unified() && self.paired_down() {
                // for shift (w/ keyup state unified), ONLY send up a keyup if the other key isnt down .. so do nothing, not even clear active
            } else {
                let smk = self.clone();
                thread::spawn ( move || {
                    smk.release_w_masking();  // this checks/updates flags too
                    if smk.is_keyup_unified() { // and for up-unified, try and clear the other too
                        smk.paired() .iter() .for_each (|p| {
                            if !p.down.is_set() && p.active.is_set() { p.release_w_masking(); }
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
        k.iproc.kbd_bindings .bind_kbd_event (self.modkey.key(), KeyEventCb_KeyDown, KbdEventCallbackEntry { event_proc_d, cb } );

        let (ks, smk) = (k.ks.clone(), self.clone());
        let cb = KbdEvCbFn_InlineCallback ( Arc::new ( move |e| { smk.handle_key_up(&ks, e); event_proc_d } ) );
        k.iproc.kbd_bindings .bind_kbd_event (self.modkey.key(), KeyEventCb_KeyUp, KbdEventCallbackEntry { event_proc_d, cb } );
    }



    pub fn proc_notice__caps_down(&self) {
        // we will immediately invalidate and clear any down mod-key found upon caps activation!
        // note that internally tracked physical is_down will continue to be down
        // note also that each of paired mod-keys will get their own notification too
        if self.down.is_set() && self.active.is_set() {
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
        if self.down.is_set() {
            let smk = self.clone();
            thread::spawn ( move || {
                thread::sleep(time::Duration::from_millis(200));
                if smk.down.is_set() && !smk.active.is_set() {
                    smk.modkey.key().press(); smk.active.set(); smk.consumed.set();
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

    fn release_w_masking(&self) {
        // masking w an unassigned key helps avoid/reduce focus loss to menu etc for alt/win
        self.active.clear();
        if !self.is_rel_masking() || !self.consumed.is_set() { self.modkey.key().release(); }
        else { self.mask().press(); self.modkey.key().release(); self.mask().release(); }
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


    // since the physical and effective state of these keys can differ and we manage internally, all press/releases should also be
    // >  guarded to ensure that our internal model is always in-sync with the outside state .. hence the util fns below
    // we'll also mark the down lalt/lwin as consumed and mask its release with ctrl so it doesnt activate menus/start-btn etc

    #[allow(dead_code)]
    fn do_w_mod (&self, key:Key, key_action:fn(Key)) {
        self.consumed.set();
        if self.active.is_set() { key_action(key) }
        else { self.modkey.key().press(); self.active.set(); key_action(key); self.release_w_masking(); }
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
        let k = self.modkey.key();
        Arc::new ( move || { k.press(); af(); k.release() })
    }

    /// Use this to wrap actions when we want the mod-key to be ACTIVE in the combo .. can use for both self-mod-key combos or unrelated combos.
    /// .. e.g. if setting up alt-X to send alt-win-y, we'd set lalt-mapping on Key::X as k.alt.active_action(k.win.active_on_key(Key::Y))
    pub fn active_action (&self, af:AF) -> AF {
        let smk = self.clone();
        Arc::new ( move || {
            smk.consumed.set();
            //if smk.pair_any_active() { dbg!((smk.modkey, &smk.paired())); af() }
            if smk.pair_any_active() { af() }
            else { smk.modkey.key().press(); af(); smk.release_w_masking(); }
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
    #[allow(dead_code)]
    pub fn inactive_on_key (&self, key:Key) -> AF { self.inactive_action (key_utils::base_action(key)) }
    // ^^ some sugar to make common things simpler .. could add for ctrl etc too if there was use


}

