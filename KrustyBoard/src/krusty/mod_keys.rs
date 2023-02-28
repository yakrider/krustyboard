
use std::{time};
use std::thread::{sleep, spawn};
use std::ops::Deref;
use std::sync::{Arc, RwLock};
use once_cell::sync::OnceCell;


use crate::{BlockInput, KbdEvntCbMapKeyType::*, krusty};

use crate::krusty::{key_utils, combo_maps, Key, Flag, KrS, AF};




# [ derive (Debug) ]
pub struct CapsModKey {
    // we've defined this just to be consistent with other mod-keys, but its really just a wrapper for caps key!
    _private : (),
    pub key  : Key,
    pub down : Flag,
}

# [ derive (Debug, Clone) ]
pub struct CMK (Arc<CapsModKey>);

impl Deref for CMK {
    type Target = CapsModKey;
    fn deref(&self) -> &CapsModKey { &self.0 }
}
// NOTE rest of the impl for this further down




# [ derive (Debug) ]
pub struct TrackedModKey {
    // we'll use this for modifier keys that we want to simply track down state, w/o attempting to track or sync their external active states
    // again, motivation for making this separate is to share fumctionality implementations
    _private : (),
    pub key  : Key,
    pub down : Flag,
}

# [ derive (Debug, Clone) ]
pub struct TMK (Arc<TrackedModKey>);

impl Deref for TMK {
    type Target = TrackedModKey;
    fn deref(&self) -> &TrackedModKey { &self.0 }
}
// NOTE rest of the impl for this further down




// note that we dont want this cloneable to avoid cloning all underlying, we'll work with it Arc-wrapped instead
# [ derive (Debug) ]
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
pub struct SMK (Arc<SyncdModKey>);
// ^^ wrapping like this lets us impl functionality direclty on this cheaply clonable new-type to pass around
// .. the alternate of making SyncdModKey clonable would be more costly as it would clone each of the underlying flags

// and we'll impl deref on it so we dont have to keep using smk.0.<bla-bla> etc .. and its kosher as its underlying is Arc anyway
impl Deref for SMK {
    type Target = SyncdModKey;
    fn deref(&self) -> &SyncdModKey { &self.0 }
}
// NOTE:  the impl for the actual SMK functionality is further down





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

    pub fn setup_tracking (&self, ksr:&KrS) {
        // note that for caps, we completely block it from ever being sent up, and just manage internally
        if Key::CapsLock.is_toggled() { // toggle off first if necessary (to clear key light)
            key_utils::press_release (Key::CapsLock);
        }

        let ks = ksr.clone();
        Key::CapsLock.block_bind(KeyDownCallback, move |_| {
            if !ks.caps.down.check() {
                // capslock can come as repeats like other mod keys .. this was a fresh one
                ks.caps.down.set();
                ks.is_wheel_spin_invalidated.set_if_clear();
                // lets notify the synced tracked mod keys, so they can invalidate/release themselves
                combo_maps::mod_smk_pairs(&ks) .iter() .for_each (|(_,smk)| smk.process_caps_down());
            }
            if ks.mouse_left_btn_down.check() && !ks.in_managed_ctrl_down_state.check() {
                ks.in_managed_ctrl_down_state.set();
                ks.lctrl.ensure_active();
            }
        });

        let ks = ksr.clone();
        Key::CapsLock.block_bind(KeyUpCallback, move |_| {
           ks.caps.down.clear_if_set();
            if ks.in_managed_ctrl_down_state.check() {
                ks.in_managed_ctrl_down_state.clear();
                ks.lctrl.ensure_inactive();
            }
            // the following isnt strictly necessary, but useful in case some keyup falls through
            ks.clear_mode_flags();
            // lets also notify the alt/win tracked mod keys so they can re-enable themselves if applicable
            combo_maps::mod_smk_pairs(&ks) .iter() .for_each (|(_,smk)| smk.process_caps_release());
        });
    }

}




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
        if do_block {
            // here we just block it completely and just track state .. e.g. for ralt
            self.key .block_bind (KeyDownCallback, move |_| { tmk.down.set_if_clear(); } );
        } else {
            // set it so if its already down, its a repeat, and we'll block it .. e.g. for lctrl/rctrl/lshift/rshift
            self.key .blockable_bind (KeyDownCallback, move |_| {
                if tmk.down.check() {
                    BlockInput::Block
                } else {
                    tmk.down.set();
                    BlockInput::DontBlock
                }
            } );
        }

        // for key-ups, we simply update flag and let them go through
        let tmk = self.clone();
        let cb = move |_| tmk.down.clear_if_set();
        if do_block {
            self.key .block_bind (KeyUpCallback, cb)
        } else {
            self.key .non_blocking_bind (KeyUpCallback, cb)
        }
    }

}




impl SMK {

    // ^^ SMK : Synced-Modifier-Key type .. tracks internal (is down), external (is active), and to-mask (is consumed) states
    // .. we should currently be doing this for left-alt and left-win identically

    // for the constructors, we'll create specific ones to create tracked-mod-keys for alt and win out of the initialized ks flags
    // .. note that we want the SMK struct which is Arc wrapped SyncdModKey with deref .. that gives us cheaper clone and good code ergo

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


    pub fn setup_tracking (&self, ksr:&KrS) {

        // NOTE: given that kbds seem to have idiosyncrasies with what scancode vs vk codes they sent, we end up getting out of sync w
        // what keys with let through and what we simulate .. e.g in my machine lshift comes in vk while rshift comes sc and so sending our
        // vk shift doesnt clear it out .. so we've decided to just block everything and send our uniform up/down reports instead!

        // beyond that, we'll block repeats to keep code logic (and keystream inspections) manageable
        // we'll also track and update both physical and externally expressed states and try and keep our model always in sync w the outside
        // we'll also track if we've used up the press so we can mask its release later (for things like win/alt that trigger menus on release)

        let (ks, smk) = (ksr.clone(), self.clone());
        smk.key.block_bind (KeyDownCallback, move |_| {
            // so goal here is, any presses with caps active, we suppress mod-key going outside
            // and since we can have caps come in after the mod-key is already down, we'll have to capture disparity states ..
            //  .. as well as restoring them when either caps/alt gets released etc
            // (plus, if we're down and caps goes down, we'll get notification below so we're enforcing the disabled state from both sides)
            if ks.caps.down.check() {
                // caps is down, record alt being down if not already, but either way block it (so no change to alt-active state)
                smk.down.set_if_clear();
                smk.consumed.clear_if_set();
            } else {
                if smk.down.check() {
                    // caps isnt down, but alt was, so its repeat .. we'll block it even if out-of-sync or its coming after combo caps release
                } else {
                    // caps isnt down, and alt wasnt down, so record states and let it through
                    smk.down.set();
                    smk.active.set_if_clear();
                    smk.consumed.clear_if_set();
                    ks.is_wheel_spin_invalidated.set_if_clear();
                    smk.key.press();
            } }
        } );

        let (ks, smk) = (ksr.clone(), self.clone());
        smk.key.block_bind(KeyUpCallback, move |_| {
            // if caps is pressed, or alt is already inactive (via masked-rel, press-rel etc), we block it
            // else if win was consumed, we release with mask, else we can actually pass it through unblocked
            smk.down.clear_if_set();
            if !smk.active.check() || ks.caps.down.check() {
                // if inactive or caps-down we just suppress this keyup
            } else {
                if smk.is_keyup_unified() && smk.paired_down() {
                    // for shift (w/ keyup state unified), ONLY send up a keyup if the other key isnt down .. so do nothing, not even clear active
                } else {
                    smk.release_w_masking();  // this checks/updates flags too
                    if smk.is_keyup_unified() { // and for up-unified, try and clear the other too
                        smk.pair.read().unwrap() .iter() .for_each (|p| {
                            if !p.down.check() && p.active.check() { p.release_w_masking(); }
                } ) } }
        } } );

    }


    pub fn process_caps_down (&self) {
        // we will immediately invalidate and clear any down lalt found upon caps activation!
        // note that internally tracked physical is_alt_down will continue to be down!
        // note also that each of paired mod-keys will get their own notification too
        if self.down.check() && self.active.check() {
            self.consumed.set_if_clear();
            self.release_w_masking(); // this will update flags too
        }
    }
    pub fn process_caps_release (&self) {
        // since we deactivate alt/win on caps press, check to see if we want to reactivate them
        // note: we'll setup a delay for activation to allow for some sloppy combo releases etc
        // note also, that if inspecting in browser-key-events, this might appear unexpected coz browser does its own 'unifying'
        // .. so to check the logic here must use lower level key inspections like via ahk key history!!
        if self.down.check() {
            let smk = self.clone();
            spawn ( move || {
                sleep(time::Duration::from_millis(200));
                if smk.down.check() && !smk.active.check() {
                    smk.key.press(); smk.active.set(); smk.consumed.set_if_clear();
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
        self.active.clear_if_set();
        if !self.is_rel_masking() || !self.consumed.check() { self.key.release(); }
        else { self.mask().press(); self.key.release(); self.mask().release(); }
    }

    fn reactivate        (&self) { self.active.set_if_clear(); self.key.press(); }
    fn paired_reactivate (&self) { self.pair.read().unwrap() .iter() .for_each (|p| p.reactivate()) }

    // we'll try an expt with masked re-activates to see if that can avoid having to do a delay before win reactivate
    // --> nah didnt seem to make a difference
    fn activate_w_masking (&self) {
        self.active.set_if_clear();
        if !self.is_rel_masking() { self.consumed.set_if_clear(); self.key.press(); }
        else { self.mask().press(); self.key.press(); self.mask().release(); }
    }
    fn paired_masked_activate (&self) {
        self.pair.read().unwrap() .iter() .for_each (|p| p.activate_w_masking())
    }


    pub fn ensure_inactive (&self) {
        // utility to ensure modkey is inactive regardless if held down
        // shouldnt really be necessary since there are action wrappers available to set/restore mod-key for any need at any mod-key state
        self.consumed.set_if_clear();
        if self.active.check() { self.release_w_masking(); } // rel call will clear active flag too
    }
    pub fn ensure_active (&self) {
        // utility to get the mod out reliably whether its currently pressed or not, while keeping state tracking updated
        // this should really ONLY be necessary where we want the mod to be left hanging on until later .. e.g. to simulate alt-tab
        self.consumed.set_if_clear();
        if !self.active.check() { self.active.set_if_clear(); self.key.press(); }
    }


    // since the physical and effective state of these keys can differ and we manage internally, all press/releases should also be
    // >  guarded to ensure that our internal model is always in-sync with the outside state .. hence the util fns below
    // we'll also mark the down lalt/lwin as consumed and mask its release with ctrl so it doesnt activate menus/start-btn etc

    fn do_w_mod (&self, key:Key, key_action:fn(Key)) {
        self.consumed.set_if_clear();
        if self.active.check() { key_action(key) }
        else { self.key.press(); self.active.set(); key_action(key); self.release_w_masking(); }
    }
    pub fn mod_press_release_guarded (&self, key:Key) { self.do_w_mod (key, key_utils::press_release) }
    // ^^ these direct guarding fns are left here if needed for complex actions later, but currently all is handled via action guards below


    /// all mod-actions mark the mod-down consumed too, but if its a no-key action (like brightness etc), wrap with this to mark consumed
    /// .. the consumed flag marks it to have its later release be masked with control to avoid activating win-menu etc
    pub fn keydn_consuming_action (&self, af:AF) -> AF {
        let smk = self.clone();
        Arc::new ( move || { smk.consumed.set_if_clear(); af(); } )
    }


    /// use this to wrap actions when we want the mod-key to be ACTIVE in the combo .. can use for both self-mod-key combos or unrelated combos
    /// .. e.g. if setting up alt-X to send alt-win-y, we'd set lalt-mapping on Key::X as k.alt.active_action(k.win.active_on_key(Key::Y))
    pub fn active_action (&self, af:AF) -> AF {
        let smk = self.clone();
        Arc::new ( move || {
            smk.consumed.set_if_clear();
            if smk.pair_any_active() { af() }
            else { smk.key.press(); af(); smk.release_w_masking(); }
        })
    }
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
                smk.consumed.set_if_clear();
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
    pub fn inactive_on_key (&self, key:Key) -> AF { self.inactive_action (key_utils::base_action(key)) }
    // ^^ some sugar to make common things simpler .. could add for ctrl etc too if there was use


}

