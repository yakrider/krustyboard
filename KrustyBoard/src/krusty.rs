use std::{thread, time, time::Instant, cell::RefCell};
use std::thread::{sleep, spawn};
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::sync::{Arc, RwLock};

use derivative::Derivative;

//use closure::closure;
// ^^ gives a nice macro that eliminates clutter from having to clone everywhere
// .. we'd use it a lot more, except IDE syntax highlighting doesnt work inside the macro .. which is waay too big a loss for the benefit!

use crate::{BlockInput, KbdEvent, KbdEvntCbMapKeyType::*, KbdKey, MouseButton, MouseEvent, MouseWheel, utils};
use crate::key_utils::base_action;




// we'll define some easier type aliases (CallBack, ActionFn etc) to pass around triggered actions and so on
type CB  = Box <dyn Fn(KbdEvent) + Send + Sync + 'static> ;
type AF  = Arc <dyn Fn() + Send + Sync + 'static> ;

type Key = KbdKey ;

//type L2S = Arc <Layer2State>;
//type SMK = Arc <SyncdModKey>;
//type TMK = Arc <TrackedModKey>;
// ^^ made these into new-type with deref impld, and then impld all functionality on those
// .. on gains on L2K, but on SMK, lets us impl functionality on the Arc wrapped SMK so we can clone and use it w/o doing smk.0.<bla-bla>




#[derive(Debug, Default, Clone)]
pub struct Flag (Arc<RwLock<bool>>);
// ^^ simple sugar that helps reduce a lot of clutter in code

impl Flag {
    pub fn check (&self) -> bool { *self.0.read().unwrap() }
    pub fn set   (&self) { *self.0.write().unwrap() = true }
    pub fn clear (&self) { *self.0.write().unwrap() = false }

    pub fn set_if_clear (&self) { if !self.check() { self.set() } }
    pub fn clear_if_set (&self) { if self.check() { self.clear() } }
}



#[derive(Debug)]
pub struct TrackedModKey {
    // we'll use this for modifier keys that we want to simply track down state, w/o attempting to track or sync their external active states
    // again, motivation for making this separate is to share fumctionality implementations
    _private : (),
    key  : Key,
    down : Flag,
}

#[derive(Debug, Clone)]
pub struct TMK (Arc<TrackedModKey>);

impl Deref for TMK {
    type Target = TrackedModKey;
    fn deref(&self) -> &TrackedModKey { &self.0 }
}
// NOTE rest of the impl for this further down




// note that we dont want this cloneable to avoid cloning all underlying, we'll work with it Arc-wrapped instead
#[derive(Debug)]
pub struct SyncdModKey {
    // this struct holds flags required for a outside-state syncd tracking of a modifier key (we only do this for alt and win)
    // the alt and win keys are now tracked identically and so doing this helps us avoid duplication of that bunch of related code
    // these flags track whether the key is down, if it has been sent out (active), and if we should release it masked (consumed)
    _private : (),
    key      : Key,
    down     : Flag,
    active   : Flag,
    consumed : Flag,

    l2s      : Arc<RwLock<Option<L2S>>>, // backlink we'll try and populate later
    // ^^ this is a lil hacky, but since we only use it for checking ctrl state to restore upon masked release, its not essential
    //      either way, we'll populate it at krusty creation time, so is reliable anyway
}

#[derive(Debug, Clone)]
pub struct SMK (Arc<SyncdModKey>);
// ^^ wrapping like this lets us impl functionality direclty on this cheaply clonable new-type to pass around
// .. the alternate of making SyncdModKey clonable would be more costly as it would clone each of the underlying flags


// and we'll impl deref on it so we dont have to keep using smk.0.<bla-bla> etc .. and its kosher as its underlying is Arc anyway
impl Deref for SMK {
    type Target = SyncdModKey;
    fn deref(&self) -> &SyncdModKey { &self.0 }
}
// NOTE:  the impl for the actual SMK functionality is further down





#[derive(Derivative)]
#[derivative(Debug, Default)]    // note that we def dont want this clonable (we'd rather clone its Arc than all underlying!)
pub struct Layer2State {
    // used for toggling key processing .. should only listen to turn-back-on combo
    in_disabled_state: Flag,

    // caps will be tracked for internal reference, and we'll assume we'll ALWAYS operate with caps-lock off
    is_caps_down: Flag,

    // we will track win and alt as syncd-modifier-keys, where we track the phys and logical states, as well as whether to mask their release
    // this allows us to add any composition of alt/win or other combos on the actual alt/win keys as well as any other key!
    #[ derivative ( Default ( value = "SMK::new(Key::LAlt)" ) ) ]
    lalt : SMK,
    #[ derivative ( Default ( value = "SMK::new(Key::LWin)" ) ) ]
    lwin : SMK,

    // now the tracked modifier keys (which we dont try to keep syncd w external state)
    // of these, at binding time, ralt is completely blocked and simply tracked, while others are pass through tracked
    #[ derivative ( Default ( value = "TMK::new(Key::RAlt)" ) ) ]
    ralt   : TMK,
    #[ derivative ( Default ( value = "TMK::new(Key::LControl)" ) ) ]
    lctrl  : TMK,
    #[ derivative ( Default ( value = "TMK::new(Key::RControl)" ) ) ]
    rctrl  : TMK,
    #[ derivative ( Default ( value = "TMK::new(Key::LShift)" ) ) ]
    lshift : TMK,
    #[ derivative ( Default ( value = "TMK::new(Key::RShift)" ) ) ]
    rshift : TMK,

    // note that we're not tracking rwin that some machines (not mine) have .. could add that later if need be


    // these track the word/fast nav and sel/del caret modes for l2 action
    in_caret_sel_mode:  Flag,
    in_caret_del_mode:  Flag,
    in_caret_word_mode: Flag,
    in_caret_fast_mode: Flag,

    // the quick keys (aka shortcuts) mode designates a shortcuts combo (ctrl-Q) in combo with which any other key combo can be mapped
    in_quick_keys_mode: Flag,

    is_mouse_left_btn_down:  Flag,
    is_mouse_right_btn_down: Flag,

    // for caps-ctrl eqv for caps-tab or caps-wheel, we'll send ctrl press/release at the right times, and will need to track that
    in_managed_ctrl_down_state: Flag,
    // and for right-mouse-btn-wheel switche support, we'll track that state too (and send switche specific keys)
    in_right_btn_scroll_state: Flag,

    #[derivative(Default(value = "RwLock::new(Instant::now())"))]
    last_wheel_stamp: RwLock<Instant>,
    is_wheel_spin_invalidated: Flag, // invalidated by e.g. mid-spin mod press, or spin stop (spacing > 120ms)
}


#[derive(Debug, Clone)]
pub struct L2S (Arc<Layer2State>);
// ^^ we'll use this wrapped type so cloning and passing around is cheap

impl Deref for L2S {
    type Target = Layer2State;
    fn deref(&self) -> &Layer2State { &self.0 }
}

impl L2S {
    pub fn new () -> L2S {
        L2S ( Arc::new ( Layer2State::default() ) )
    }
    fn clear_caret_mode_flags (&self) {
        self.in_caret_sel_mode.clear();
        self.in_caret_del_mode.clear();
        self.in_caret_word_mode.clear();
        self.in_caret_fast_mode.clear();
    }
    fn some_shift_down (&self) -> bool { self.lshift.down.check() || self.rshift.down.check() }
    fn some_ctrl_down (&self)  -> bool { self.lctrl.down.check()  || self.rctrl.down.check() }
}




#[derive(Default)]
pub struct ActionMaps {
    // this allows us to build maps to hold l2, win, l3 etc by group instead of spread around all over by key
    // the caps and caps-alt mapping comprise the l2 functionality, the win and caps-win comprise l3
    // the idea is to auto compose actual callbacks by key after all the mapping is filled out
    // note ofc that direct key callbacks can still be set for keys not put in these maps
    pub base_act_map       : HashMap<Key,AF>,
    pub caps_act_map       : HashMap<Key,AF>,
    pub lalt_act_map       : HashMap<Key,AF>,
    pub ralt_act_map       : HashMap<Key,AF>,
    pub win_act_map        : HashMap<Key,AF>,
    pub caps_lwin_act_map  : HashMap<Key,AF>,
    pub caps_lalt_act_map  : HashMap<Key,AF>,
    pub caps_ralt_act_map  : HashMap<Key,AF>,
    pub caps_shift_act_map : HashMap<Key,AF>,
}




pub struct Krusty {
    // this is mostly just a utility wrapper sugar to pass things around
    _private : (),   // prevents direct instantiation of this struct
    // L2S is Arc<Layer2State>, holds all state flags
    pub l2s : L2S,
    // then we'll hold all the actionmaps, this doesnt have to be clonable or sendable
    pub am : RefCell <ActionMaps>,
    // we'll also keep a mapping of caret-mode keys and their associated flags for l2 impl
    pub cmks: RefCell <HashMap <Key,Flag>>,
}



impl Krusty {

    // NOTE: we've decided to mostly impl things grouped by related functionality in disparate modules rather than under impl here
    // .. mostly because doing that allows easier piecemeal imports, as well as better pub/private management

    pub fn new() -> Krusty {
        let l2s = L2S::new();
        // we'll fill these with backlinks to parent (mostly to avoid always having to pass l2s into their impld fns)
        l2s.lalt.link_parent(&l2s);
        l2s.lwin.link_parent(&l2s);
        // now we can construct the Krusty struct we want
        Krusty {
            _private : (),
            l2s  : l2s,
            am   : RefCell::new (ActionMaps::default()),
            cmks : RefCell::new (HashMap::new()),
    } }

    fn setup_global_disable (&self) {
        // todo: wont be straight-forward if we want to handle this at hook receipt
        // .. will need a global check there, as well as allowing selectively for the re-enable combo!
    }

}




impl TMK {

    // ^^ TMK : Tracked-Modifier-Key type .. unlike the SMK, this only tracks the physical (is down) state of the modifier key
    // we should currently be doing this for left/right ctrl/shift, as well as right-alt

    pub fn new (key:Key) -> TMK { TMK ( Arc::new ( TrackedModKey {
        _private : (),
        key : key,
        down : Flag::default(),
    } ) ) }

    pub fn setup_tracking (&self, doBlock:bool) {
        // the setup for these is mostly just tracking their down state ..
        // however, we will also disable repeats, mostly for no functional reason than to ease looking at keystreams
        let tmk = self.clone();
        if doBlock {
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
        if doBlock {  self.key.block_bind(KeyUpCallback, cb) }
        else { self.key.non_blocking_bind(KeyUpCallback, cb) };
    }

}




impl SMK {

    // ^^ SMK : Synced-Modifier-Key type .. tracks internal (is down), external (is active), and to-mask (is consumed) states
    // .. we should currently be doing this for left-alt and left-win identically

    // for the constructors, we'll create specific ones to create tracked-mod-keys for alt and win out of the initialized l2s flags
    // .. note that we want the SMK struct which is Arc wrapped SyncdModKey with deref .. that gives us cheaper clone and good code ergo

    pub fn new (key:Key) -> SMK { SMK ( Arc::new ( SyncdModKey {
        _private:(), key : key,
        down: Flag::default(), active: Flag::default(), consumed: Flag::default(),
        l2s : Arc::new(RwLock::new(None)),
    } ) ) }

    pub fn link_parent (&self, l2s:&L2S) {
        let _ = self.l2s.write().unwrap().insert(l2s.clone());
    }


    pub fn setup_tracking (&self, l2sr:&L2S) {
        // for these mod keys, we'll let it go through, but instead of letting it spam on repeat on long presses, we just preserve the logical
        // >  state but suppress the repeats .. that helps us reason safely about its logical state in all the other combos, esp non-caps ones,
        // >  plus, it makes alt-combos for keys not handled here continue to work as expected
        // however, letting alt out unblocked, does cause issues at times (eg. when it moves focus to menu items .. so we'll track both its
        // >  internal, as well as external state for when we've want to suppress it while caps was down, or mask its release after its done
        let (l2s, smk) = (l2sr.clone(), self.clone());
        smk.key.blockable_bind(KeyDownCallback, move |_| {
            // so goal here is, any presses with caps active, we suppress alt/win mod-key going outside
            // and since we can have caps come in AFTER the mod-key is already down, we'll have to capture disparity states, as well as to
            // >  deal with restoring that when either caps/alt gets released etc
            // so, if caps is down, suppress alt/win, capture state, let specific combos deal with the disparity
            // plus, if we're down and caps goes down, we'll get notification below so we're enforcing the disabled state from both sides
            if l2s.is_caps_down.check() {
                // caps is down, record alt being down if not already, but either way block it (so no change to alt-active state)
                smk.down.set_if_clear();
                smk.consumed.clear_if_set();
                BlockInput::Block
            } else {
                if smk.down.check() {
                    // caps isnt down, but alt was, so its repeat .. we'll block it even if out-of-sync or its coming after combo caps release
                    BlockInput::Block
                } else {
                    // caps isnt down, and alt wasnt down, so record states and let it through
                    smk.down.set();
                    smk.active.set_if_clear();
                    smk.consumed.clear_if_set();
                    l2s.is_wheel_spin_invalidated.set_if_clear();
                    BlockInput::DontBlock
            } }
        } );

        let (l2s, smk) = (l2sr.clone(), self.clone());
        smk.key.blockable_bind(KeyUpCallback, move |_| {
            // if caps is pressed, or alt is already inactive (via masked-rel, press-rel etc), we block it
            // else if win was consumed, we release with mask, else we can actually pass it through unblocked
            smk.down.clear_if_set();
            if !smk.active.check() || l2s.is_caps_down.check() {
                BlockInput::Block
            } else {
                smk.active.clear();
                if !smk.consumed.check() {
                    BlockInput::DontBlock
                } else {
                    let smk = smk.clone();
                    spawn (move || smk.release_w_ctrl_masking() );
                    BlockInput::Block
            } }
        } );

    }


    pub fn process_caps_down (&self) {
        // we will immediately invalidate and clear any down lalt found upon caps activation!
        // note that internally tracked physical is_alt_down will continue to be down!
        if self.down.check() && self.active.check() {
            self.active.clear();
            self.consumed.set_if_clear();
            let smk = self.clone();
            spawn (move || smk.release_w_ctrl_masking() );
        }
    }
    pub fn process_caps_release (&self) {
        // since we deactivate alt/win on caps press, check to see if we want to reactivate them
        // note: we'll setup a delay for activation to allow for some sloppy combo releases etc
        if self.down.check() {
            let smk = self.clone();
            spawn ( move || {
                sleep(time::Duration::from_millis(200));
                if smk.down.check() && !smk.active.check() {
                    smk.key.press(); smk.active.set(); smk.consumed.set_if_clear();
            } } );
        }
    }


    pub fn release_w_ctrl_masking (&self) {
        // masking w ctrl helps avoid/reduce focus loss to menu
        Key::LControl.press(); self.key.release(); Key::LControl.release();
        //if self.l2s.some_ctrl_down() { Key::Ctrl.press() }
        self.l2s.read().unwrap() .iter() .filter (|l2s| l2s.some_ctrl_down()) .for_each (|_| Key::Ctrl.press())
    }
    pub fn ensure_inactive (&self) {
        // utility to ensure lalt is inactive regardless if held down
        // shouldnt really be necessary since there are action wrappers available to set/restore alt/win for any need at any alt/win state
        self.consumed.set_if_clear();
        if self.active.check() { self.active.clear(); self.release_w_ctrl_masking(); }
    }
    pub fn ensure_active (&self) {
        // utility to get alt out reliably whether its currently pressed or not, while keeping state tracking updated
        // this should really ONLY be necessary where we want alt to be left hanging on until later .. e.g. to simulate alt-tab
        self.consumed.set_if_clear();
        if !self.active.check() { self.active.set(); Key::LAlt.press(); }
    }


    // since the physical and effective state of these keys can differ and we manage internally, all press/releases should also be
    // >  guarded to ensure that our internal model is always in-sync with the outside state .. hence the util fns below
    // we'll also mark the down lalt/lwin as consumed and mask its release with ctrl so it doesnt activate menus/start-btn etc

    fn checked_masked_release (&self) {
        if !self.down.check() { self.key.release() }
        else { self.release_w_ctrl_masking() }
    }
    fn do_w_mod (&self, key:Key, key_action:fn(Key)) {
        self.consumed.set_if_clear();
        if self.active.check() { key_action(key) }
        else { self.key.press(); key_action(key); self.checked_masked_release(); }
    }
    pub fn mod_press_release_guarded (&self, key:Key) { self.do_w_mod (key, key_utils::press_release) }
    // ^^ we can add similar for ctrl, shift or ctrl_shift if need be


    /// all mod-actions mark the mod-down consumed too, but if its a no-key action (like brightness etc), wrap with this to mark consumed
    /// .. the consumed flag marks it to have its later release be masked with control to avoid activating win-menu etc
    fn down_mod_consuming_action (&self, af:AF) -> AF {
        let smk = self.clone();
        Arc::new ( move || { smk.consumed.set_if_clear(); af(); } )
    }


    /// use this to wrap actions when we want the mod-key to be ACTIVE in the combo .. can use for both self-mod-key combos or unrelated combos
    /// .. e.g. if setting up alt-X to send alt-win-y, we'd set lalt-mapping on Key::X as k.alt.active_action(k.win.active_on_key(Key::Y))
    pub fn active_action (&self, af:AF) -> AF {
        let smk = self.clone();
        Arc::new (move || {
            smk.consumed.set_if_clear();
            if smk.active.check() { af() }
            else { smk.key.press(); af(); smk.checked_masked_release(); }
        })
    }
    pub fn active_on_key (&self, key:Key) -> AF { self.active_action(base_action(key)) }
    // ^^ some sugar to make common things simpler .. could add for ctrl etc too if there was use


    /// use this to wrap actions ONLY when setting combos with this mod key itself AND we want the mod-key to be INACTIVE in the combo
    /// .. e.g. if setting up alt-X to send win-y, we'd set lalt-mapping on Key::X as k.alt.inactive_action(k.win.inactive_on_key(Key::Y))
    pub fn inactive_action (&self, af:AF) -> AF {
        // in theory, we should be able to just do a masked release here, and that work for alt .. win however is finicky
        // apparently win start menu triggers unless there's some timing gap betwewn the masked release and another press
        // .. and from quick expts apparently even 80ms is sometimes too little .. not sure if also machine dependent
        let smk = self.clone();
        Arc::new (move || {
            smk.consumed.set_if_clear();
            if !smk.active.check() { af() }
            else {
                //smk.checked_masked_release(); af(); smk.key.press();  // as per above, we'll do a delayed release instead
                let smk = smk.clone(); let af = af.clone();
                spawn (move || {
                    smk.checked_masked_release(); af();
                    sleep(time::Duration::from_millis(100));
                    smk.key.press();
                } );
        } } )
    }
    pub fn inactive_on_key (&self, key:Key) -> AF { self.inactive_action(base_action(key)) }
    // ^^ some sugar to make common things simpler .. could add for ctrl etc too if there was use


}



pub mod caps_setup {

    // setting up caps as the global Layer-2 modifier key
    // .. other mod-keys are via SMK (Syncd-Tracked modifier key: alt/win)  or TMK (Tracked modifer keys: ralt/lctrl/rctrl/lshift/rshift)

    use crate::krusty::*;

    pub fn setup_caps_tracking (k:&Krusty) {
        // note that for caps, we completely block it from ever being sent up, and just manage internally
        if Key::CapsLock.is_toggled() {
            Key::CapsLock.press();  // toggle off first if necessary (to clear key light)
        }

        let l2s = k.l2s.clone();
        Key::CapsLock.block_bind(KeyDownCallback, move |_| {
            if !l2s.is_caps_down.check() {
                // capslock can come as repeats like other mod keys .. this was a fresh one
                l2s.is_caps_down.set();
                l2s.is_wheel_spin_invalidated.set_if_clear();
                // lets notify the alt/win tracked mod keys, so they can invalidate/release themselves
                l2s.lalt.process_caps_down();
                l2s.lwin.process_caps_down();
            }
            if l2s.is_mouse_left_btn_down.check() && !l2s.in_managed_ctrl_down_state.check() {
                l2s.in_managed_ctrl_down_state.set();
                spawn ( || Key::LControl.press() );
            }
        });

        let l2s = k.l2s.clone();
        Key::CapsLock.block_bind(KeyUpCallback, move |_| {
           l2s.is_caps_down.clear_if_set();
            if l2s.in_managed_ctrl_down_state.check() {
                l2s.in_managed_ctrl_down_state.clear();
                spawn ( || Key::LControl.release() );
            }
            // the following isnt strictly necessary, but useful in case some keyup falls through
            l2s.clear_caret_mode_flags();
            // lets also notify the alt/win tracked mod keys so they can re-enable themselves if applicable
            l2s.lalt.process_caps_release();
            l2s.lwin.process_caps_release();
        });

    }



}



pub mod key_utils {

    use crate::krusty::*;

    pub fn press_release        (key:Key) { key.press(); key.release(); }
    pub fn double_press_release (key:Key) { press_release(key); press_release(key); }

    fn wrapped_press_release (wrap_key:Key, key_action:fn(Key), key:Key) {
        wrap_key.press(); key_action(key); wrap_key.release();
    }
    // ctrl and shift are untracked pure mod keys, so we can blindly send those presses on demand
    pub fn ctrl_press_release       (key:Key) { wrapped_press_release (Key::Ctrl,  press_release, key) }
    pub fn shift_press_release      (key:Key) { wrapped_press_release (Key::Shift, press_release, key) }
    pub fn ctrl_shift_press_release (key:Key) { wrapped_press_release (Key::Ctrl,  shift_press_release, key) }


    // we'll define some arc-wrapper util fns, but really, its just as easy to just use arcs directly
    /// wraps a given unitary function with NO input args into an Arc Fn
    pub fn action (f:fn()) -> AF { Arc::new (move || f()) }

    /// wraps a given unitary function with ONE input arg into an Arc Fn
    pub fn action_p1<T> (f:fn(T), t:T) -> AF where T: Copy + Send + Sync + 'static { Arc::new (move || f(t)) }

    pub fn no_action                () -> AF { Arc::new ( || {} ) }
    pub fn base_action       (key:Key) -> AF { action_p1 (press_release,            key) }
    pub fn fast_action       (key:Key) -> AF { action_p1 (double_press_release,     key) }

    pub fn ctrl_action       (key:Key) -> AF { action_p1 (ctrl_press_release,       key) }
    pub fn shift_action      (key:Key) -> AF { action_p1 (shift_press_release,      key) }
    pub fn ctrl_shift_action (key:Key) -> AF { action_p1 (ctrl_shift_press_release, key) }


    // note: ^^ we dont have these versions for alt/win combos, as we need to check state before blindly sending those
    // .. instead, can use the smart state restoring action wrappers defined under the SyncdModKey (SMK) impl

}



pub mod mouse_btn_setups { // setups for mouse btn handling

    use crate::krusty::*;

    pub fn setup_mouse_left_btn_handling (k:&Krusty) {
        let l2s = k.l2s.clone();
        MouseButton::LeftButton.block_bind(true, move |_| {
            l2s.is_mouse_left_btn_down.set_if_clear();
            if l2s.is_caps_down.check() {
                l2s.in_managed_ctrl_down_state.set_if_clear();
                Key::LControl.press(); // this allows caps-as-ctrl for drag drop etc
            }
            MouseButton::LeftButton.press()
        });
        let l2s = k.l2s.clone();
        MouseButton::LeftButton.block_bind(false, move |_| {
            l2s.is_mouse_left_btn_down.clear_if_set();
            sleep(time::Duration::from_millis(10));
            MouseButton::LeftButton.release();
        });
        // ^^ in theory could do non-blocking bind for release, but that can occasionally expose us to timing issues as press is on block
        //      and that will be slower to send the actual press from the spawned thread (so making release slower too helps)
    }

    pub fn setup_mouse_right_btn_handling (k:&Krusty) {
        // tracking btn down is for ctrl-wheel (zoom) etc, and right-btn-down-state is for switche scroll (via F21/F22/F23)
        let l2s = k.l2s.clone();
        MouseButton::RightButton.non_blocking_bind(true, move |_| {
            l2s.is_mouse_right_btn_down.set_if_clear();
        });
        let l2s = k.l2s.clone();
        let switche_action = k.l2s.lalt.active_on_key(Key::F23);
        MouseButton::RightButton.non_blocking_bind(false, move |_| {
            l2s.is_mouse_right_btn_down.clear_if_set();
            if l2s.in_right_btn_scroll_state.check() {
                l2s.in_right_btn_scroll_state.clear();
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

    pub fn handle_wheel_guarded (delta:i32, l2sr:&L2S) {
        // this is mostly to make the super-fast inertial smooth-scroll wheel on my (MX3) mouse a lil more usable by spacing things out
        // also, the invalidation setup prevents things like caps down when wheel is still unintentionally inertially spinning to trigger zooms etc
        let last_stamp = *l2sr.last_wheel_stamp.read().unwrap();
        *l2sr.last_wheel_stamp.write().unwrap() = Instant::now();
        //let gap = l2sr.last_wheel_stamp.read().unwrap().duration_since(last_stamp);
        //println!("{:#?}", dur.as_millis());
        const GUARD_DUR_MS: u128 = 120;  // from dur printouts above, looked like max inertial gap is 120 (min 7ms, usually <100)
        if !l2sr.is_wheel_spin_invalidated.check() {
            handle_wheel_action(delta, l2sr);
        } else if GUARD_DUR_MS < l2sr.last_wheel_stamp.read().unwrap().duration_since(last_stamp).as_millis() {
            l2sr.is_wheel_spin_invalidated.clear();
            handle_wheel_action(delta, l2sr);
        } else {
            // if its invalidated AND wheel-spin spacing is below guard-dur, we suppress the wheel event
        }
    }

    pub fn handle_wheel_action (delta:i32, l2sr:&L2S) {
        let incr = delta / WHEEL_DELTA as i32;
        if l2sr.is_mouse_right_btn_down.check() {
            // right-mouse-btn-wheel support for switche task switching
            l2sr.in_right_btn_scroll_state.set_if_clear();
            let key = if incr.is_positive() { Key::F22 } else { Key::F21 };
            l2sr.lalt.mod_press_release_guarded(key);
        } else  if l2sr.lalt.down.check() {
            // wheel support for scrolling in windows native alt-tab task-switching screen
            // this requires a system call to check alt-tab window, so push it out to thread
            l2sr.lalt.consumed.set_if_clear();
            let lalt = l2sr.lalt.clone();
            spawn ( move || {
                if get_fgnd_win_class() == "MultitaskingViewFrame" { // alt-tab states
                    lalt.ensure_active();
                    handle_alt_tab_wheel(incr)
                } else { incr_volume(incr); } // simple alt wheel for volume
            } );
        } else if l2sr.lwin.down.check() {
            l2sr.lwin.consumed.set_if_clear();
            incr_brightness(incr); // win-wheel for brightness
        } else if l2sr.is_caps_down.check() {
            l2sr.in_managed_ctrl_down_state.set_if_clear();
            Key::LControl.press();
            MouseWheel::DefaultWheel.scroll(delta); // caps-wheel as ctrl-wheel (zoom etc)
        } else if l2sr.some_shift_down() {
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

    pub fn handle_horiz_scroll_wheel (incr:i32) {
    }

    pub fn setup_mouse_wheel_handling (k:&Krusty) {
        let l2s = k.l2s.clone();
        MouseWheel::DefaultWheel.block_bind(true, move |ev| {
            ev .wheel_ev_data .iter() .for_each (|d| handle_wheel_guarded(d.delta, &l2s) );
        });
        let l2s = k.l2s.clone();
        MouseWheel::DefaultWheel.block_bind(false, move |ev| {
            ev .wheel_ev_data .iter() .for_each (|d| handle_wheel_guarded(d.delta, &l2s) );
        })
    }

}



pub mod special_keys_setups { // special keys handling


    // NOTE: so far we have decided not to hold mappings for ctrl or shift (and cascadingly for ctrl-alt, ctrl-win, shift-alt, shift-win etc)
    // further, we make the action map composition default to pure ctrl/shift action when those natural combos are pressed
    // >  which is better than not checking for those, as this will allow for natural ctrl-alt, shift-alt, ctrl-win etc to pass through
    // however, that means if we need to setup combos that want to use shift or ctrl keys, we'll have to do it directly

    use crate::krusty::{*, key_utils::*};

    pub fn setup_direct_binding_keys (k:&Krusty) {

        // currently, we dont need to do it for anything
        // .. even for shift-tick, its handled by checking for shift in that base-action itself

        // HOWEVER, for anything that needs ctrl/shift while alt/win/ralt is down, we have no easy way currently to do it via action maps
        // .. and thats coz during am cb composition, shift/ctrl check defaulting to base needs to be before alt/ralt/win conditionals
        // the messy alternative if we wanted to do it via action maps, we'd have base action check alt/ctrl/shift etc etc and set that for all actions

        // ALSO, for things that require actual action in key-up callbacks, we'd do it direclty here because key-dn actions arent registered in maps
        // >  and so action-maps composed callbacks can only either set key-up to do nothing, or at most clear caret flags

        // IMPORTANT : note that whatever we put here, we will NOT want to include them in ANY action maps !!


    }


}




pub mod action_map_utils {

    use crate::krusty::{*, key_utils::*};

    /// note: registering a caret mode key auto sets their caps and caps actions to nothing, others combos can be set as usual
    pub fn register_caret_mode_key (k: &Krusty, key:Key, mode_flag:&Flag) {
        k.cmks.borrow_mut() .insert(key, mode_flag.clone());
        add_caps_mapping (k, key, no_action());  // caret mode keys trigger w caps down, cant send anything during that
    }

    /// if the key is registered for caret mode, this will generate the (key-dn, key-up) caret-flag setting actions
    fn gen_key_caret_mode_actions (k:&Krusty, key:Key) -> (AF,AF) {
        if let Some(cmfr) = k.cmks.borrow().get(&key) {
            let (cmf_c1, cmf_c2) = (cmfr.clone(), cmfr.clone()); // gotta clone to move into AF
            return ( Arc::new (move || cmf_c1.set_if_clear()), Arc::new (move || cmf_c2.clear_if_set()) )
            // ^^ key-dn and key-up caret-mode-actions for a caret-mode-key is to set and clear its caret-mode-flag
        }
        (no_action(), no_action()) // empty (dn,up) caret-flag-actions for non-caret keys
    }


    fn compose_action_maps_cb (k:&Krusty, key:Key) -> (CB, CB) {
        // lets first make a utility closure that extracts the AF for this key from some specified am map
        let af = move |m:&HashMap<Key,AF>| { m.get(&key).map(|afr| afr.clone()) };

        // then retrieve all the registered actions for this key
        let am = &k.am.borrow();
        let (mba, mca, mlaa, mraa, mwa, mcwa, mclaa, mcraa, mcsa, mqka) = (
            af(&am.base_act_map), af(&am.caps_act_map), af(&am.lalt_act_map), af(&am.ralt_act_map), af(&am.win_act_map),
            af(&am.caps_lwin_act_map), af(&am.caps_lalt_act_map), af(&am.caps_ralt_act_map), af(&am.caps_shift_act_map), af(&am.quick_keys_map)
        );
        // we cant move local options to closure, just the Arcs inside, so do that part of composition before making closure
        let (pba, ba) = (base_action(key), mba.unwrap_or(base_action(key))); // pure-base-action vs possibly mapped base-action

        // ralt is by default mapped to shift unless specifically set otherwise!
        let raa = mraa.unwrap_or(shift_action(key));

        // we'll set caps-as-ctrl and caps-shift as ctrl-shift unless specifically set otherwise
        let (ca, csa) = ( mca.unwrap_or(ctrl_action(key)), mcsa.unwrap_or(ctrl_shift_action(key)) );

        // for alt/win, we'll default to base to give natural alt/win-key combos (as the mod key alt/win press will already be out)
        // however, the actual alt/win state might not be active, so we'll wrap them to ensure active states
        let laa = mlaa.unwrap_or (k.l2s.lalt.active_on_key(key));
        let wa  = mwa.unwrap_or  (k.l2s.lwin.active_on_key(key));;

        // we'll set caps-as-ctrl to work with lalt/lwin combos unless specifically set otherwise!
        let claa = mclaa.unwrap_or (k.l2s.lalt.active_action(ctrl_action(key)));
        let cwa  = mcwa.unwrap_or  (k.l2s.lwin.active_action(ctrl_action(key)));

         // for our remaining combo of caps with ralt (which is always suppressed), or quick-keys, we'll default to no-action
        let (craa, qka) = (mcraa.unwrap_or(no_action()), mqka.unwrap_or(no_action()));

        // for caret-mode support, we'll check if the key is in caret-mode keys, and create flag update actions
        let (cma_dn, cma_up) = gen_key_caret_mode_actions (k, key);

        // now we can finally compose these Arc action clones into the key action closure we need
        let l2s = k.l2s.clone();
        let cb_dn = Box::new ( move |_| { // note that we're ignoring the KbdEvent passed in (it'd have KbdEventType and key vk/sc codes)
            cma_dn();
            if l2s.is_caps_down.check() {
                // supports caps-mod combos .. if need caps-mod1-mod2 combos, can code the third mod check directly in that key's caps-mod action
                if      l2s.lwin.down.check()           { cwa  () }
                else if l2s.lalt.down.check()           { claa () }
                else if l2s.ralt.down.check()           { craa () }
                else if l2s.some_shift_down()           { csa  () }
                else if l2s.some_ctrl_down()            { ba   () }
                else if l2s.in_quick_keys_mode.check()  { qka  () }
                else                                    { ca   () }
            } // note: for the rest (lwin, lalt, ralt), we wont define multi-combos for now .. can add if need arises
            else if l2s.ralt.down.check() { raa() }
            // ^^ dont care about natural action for this, its completely suppressed
            else if l2s.some_ctrl_down() || l2s.some_shift_down() { ba() }  // might as well do ba() here instead of pba()
            // ^ for natural combos with ctrl or shift (both for alt and win), this ensures mapped alt-? combos dont mess those
            //   .. and sending base action is adequate since the ctrl/shift/alt will already be active
            else if l2s.lalt.down.check() && l2s.lwin.down.check() { ba() }
            // ^^ similarly, this allows natural win-alt combos to go through
            else if l2s.lalt.down.check() { laa() }
            else if l2s.lwin.down.check() { wa() }
            else { ba() }
        } );

        // and finally, the key up action, which is really just caret mode flag action if any
        let cb_up = Box::new ( move |_| { cma_up() } );

        // aight, these are ready to be binded .. ship it!
        (cb_dn, cb_up)
    }


    pub fn bind_all_from_action_maps (k:&Krusty) {
        // first we want to register all the keys in all the action maps into a keyset
        let mut keys = HashSet::new(); let ks = &mut keys; // ref to be consumed by closure below
        let mut reg = move |m:&HashMap<Key,AF>| { m.keys() .for_each (|kr| { ks.insert(*kr); } ) };
        let am = &k.am.borrow();
        reg(&am.base_act_map); reg(&am.caps_act_map); reg(&am.lalt_act_map); reg(&am.ralt_act_map); reg(&am.win_act_map);
        reg(&am.caps_lwin_act_map); reg(&am.caps_lalt_act_map); reg(&am.caps_ralt_act_map); reg(&am.caps_shift_act_map); reg(&am.quick_keys_map);

        // then for each key in the set, we'll gen the composed action and bind it
        for key in keys.iter() {
            let (cb_dn, cb_up) = compose_action_maps_cb (k, *key);
            key.block_bind (KeyDownCallback, cb_dn);
            key.block_bind (KeyUpCallback,   cb_up);
        }
    }



    // base and ralt mapping are straightforward, and we dont worry about combos for these either
    pub fn add_base_mapping (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .base_act_map .insert (key, action); }
    pub fn add_ralt_mapping (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .ralt_act_map .insert (key, action); }

    // lalt/lwin are used as special modifier keys setup identically via SyncdModKey functionality
    // their active state and physical states might not match, so we need more housekeeping to keep our model in sync w outside
    pub fn add_lalt_mapping (k:&Krusty, key:Key, action:AF) {
        k.am.borrow_mut() .lalt_act_map .insert (key, k.l2s.lalt.down_mod_consuming_action(action));
    }
    pub fn add_lwin_mapping (k:&Krusty, key:Key, action:AF) {
        k.am.borrow_mut() .win_act_map .insert (key, k.l2s.lwin.down_mod_consuming_action(action));
    }

    // for the caps mapping, and all of its combos that we support (dont have to worry about state, as w/ caps alt/win are masked/suppressed)
    // NOTE: we dont need one for caps-ctrl, as caps default is ctrl ... (though if we really ran out of combos, lol(!), we could add it)
    pub fn add_caps_mapping       (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .caps_act_map       .insert (key, action); }
    pub fn add_caps_ralt_mapping  (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .caps_ralt_act_map  .insert (key, action); }
    pub fn add_caps_lalt_mapping  (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .caps_lalt_act_map  .insert (key, action); }
    pub fn add_caps_lwin_mapping  (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .caps_lwin_act_map  .insert (key, action); }
    pub fn add_caps_shift_mapping (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .caps_shift_act_map .insert (key, action); }


    // finally we have the special shortcuts mode l4 quick-keys mapping
    pub fn add_quick_keys_mapping (k:&Krusty, key:Key, action:AF) { k.am.borrow_mut() .quick_keys_map .insert (key, action); }




}



pub mod l2_utils {

    /* setup options summary:
     - only j/k for left/right get f-for-word-nav mode speedup (native word nav by sending ctrl)
     - those and i/comma for up/down get r-for-double-speed nav mode (2x nav) .. i/comma get that for f too
     - h/l/u/m for home/end/pgup/pgdown get no speedup modes
     - e/d do sel/del modes, and those can be freely combined with the f/r word/fast nav modes
     - in del mode, left/home/up/pgup get ExtBackspace, right/end/down/pgdn get ExtDelete
     - in del mode, left/right do direct bksp/del, but others get select then bksp/del
     */
    use crate::krusty::{*, key_utils::*, action_map_utils::*};

    /// action-function generator type for l2 configuration
    pub type AFG = fn(Key) -> AF ;

    /// composes the navigation action for the l2 key, incl fast/word nav options
    fn l2_nav_afg (l2s:L2S, l2k:Key, wafg:AFG, fafg:AFG) -> AF {
        let (base_af, word_af, fast_af) = (base_action(l2k), wafg(l2k), fafg(l2k));
        Arc::new ( move || {
            if l2s.in_caret_word_mode.check() { word_af() }
            else if l2s.in_caret_fast_mode.check() { fast_af() }
            else { base_af() }
    } ) }

    /// delete action with specified bksp/del key, supports the fast/word nav options
    pub fn l2_del_afg (l2s:L2S, del_key:Key, _:AF) -> AF {
        // note : this is here instead of using del-sel for everything because this gives better undo/redo behavior in editors
        l2_nav_afg (l2s, del_key, ctrl_action, fast_action)
    }
    /// delete action as first select and then delete w the specified bksp/del key
    pub fn l2_del_sel_afg (_:L2S, del_key:Key, nav_af:AF) -> AF {
        Arc::new ( move || {
            Key::Shift.press(); nav_af(); Key::Shift.release();
            press_release(del_key);
    } ) }
    /// generator type for deletion AFs
    pub type DAFG = fn(L2S, Key, AF) -> AF ;

    /// l2 selection action, supports any configured fast/word nav options for the key
    fn l2_sel_afg (nav_af:AF) -> AF {
        Arc::new ( move || { Key::Shift.press(); nav_af(); Key::LShift.release() } )
    }

    /// composes the complete l2 action for the key
    fn l2_afg (l2s:L2S, nav_af:AF, sel_af:AF, del_af:AF) -> AF {
        Arc::new ( move || {
            if l2s.in_caret_sel_mode.check() { sel_af() }
            else if l2s.in_caret_del_mode.check() { del_af() }
            else { nav_af() }
    } ) }

    /// setup layer-2 behavior for a given 'key' <br>
    /// *l2k* : layer-2 eqv key to press for regular/nav actions <br>
    /// *dk* : key to use for delete action (backspace or delete) <br>
    /// *wafg* : word-action-gen,  specifies regular/fast/word-nav when word-mode-key (f) held <br>
    /// *fafg* : fast-action-gen, specifies regular/fast nav when fast-mode-key (r) held <br>
    /// *dafg* : del-action-gen,  specifies del action when 'd' held, either direct 'dk' or select-then-dk <br>
    /// (note that there's default selection action when 'e' is held)
    pub fn setup_l2k (k:&Krusty, key:Key, l2k:Key, dk:Key, aafg:AFG, fafg:AFG, dafg:DAFG) {
        // first lets assemble our component actions
        let nav_af = l2_nav_afg (k.l2s.clone(), l2k, aafg, fafg);
        let sel_af = l2_sel_afg (nav_af.clone());
        let del_af = dafg (k.l2s.clone(), dk, nav_af.clone());

        // then compose the complete l2 action for the key
        let l2_af  = l2_afg (k.l2s.clone(), nav_af, sel_af, del_af);

        // finally we can bind the l2 behavior on caps key (which has to be held down for the layer-2 behavior)
        add_caps_mapping      (k, key, l2_af.clone());

        //add_caps_lalt_mapping (k, key, l2_af.clone());
        add_caps_lalt_mapping (k, key, no_action());
        // ^^ lalt is no longer a l2s mode key, but to train for a bit, we'll set it to do nothing for these keys instead
        // todo: ^^ remove training-disabled things like these when no longer useful (so can use those high value combos for other purposes)
    }

    // note: actual setup for l2k keys can be done (importing utils this mod) along with other krustyboard setup steps

}


pub fn setup_krusty_board () {

    use crate::krusty::{*, key_utils::*, caps_setup::*, special_keys_setups::*,
                        mouse_btn_setups::*, mouse_wheel_setups::*, action_map_utils::*};

    use crate::utils::{window_utils::*, process_utils::*};


    let k = Krusty::new();

    let (alt, win) = (&k.l2s.lalt, &k.l2s.lwin);
    // ^^ just shorthands to reduce some code clutter



    // setup capslock, we'll completely disable it other than for krusty use
    setup_caps_tracking (&k);

    // setup tracking for right-alt .. its completely blocked but tracked, we use it as shifts and combos
    k.l2s.ralt .setup_tracking (true);      // for ralt can setup w doBlock=true

    // and shift .. this is simple non-blocking tracking, only tracked to enable a few combos with caps
    k.l2s.lshift.setup_tracking(false);     // these others have doBlock = false
    k.l2s.rshift.setup_tracking(false);

    // and even ctrl .. again simple non-blocking, just tracking flags to allow for natural ctrl-alt-? when alt-? is remapped
    k.l2s.lctrl.setup_tracking(false);
    k.l2s.rctrl.setup_tracking(false);

    // lalt and lwin are set as syncd-tracked-modifier-key with special (but identical) impl .. more details under SyncdModKey impl
    k.l2s.lalt.setup_tracking(&k.l2s);
    k.l2s.lwin.setup_tracking(&k.l2s);


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

    // lets start out by setting up bulk keys (which might get overriden further down, though we'd prefer to not rely on that)

    // IMPORTANT: default action maps composition per key, if no explicit mapping, sets ralt-as-shift, caps-as-ctrl, caps-alt--as--ctrl-alt
    // HOWEVER, action maps only come into play for a key if SOME mapping is made
    // and by now, we pretty much want to map all keys in some way or another, so might as well put em all here ourselves
    // .. EXCEPT for cases we find we need to directly bind some keys (special-keys-setups) .. which we have none so far !!

    let char_keys: Vec<Key> = "qwertasdfgzxcvb`123456yuiop[]\\hjkl;\'nm,./7890-=" .chars() .map (|c| Key::from_char(c)) .flatten() .collect();
    let fnum_keys: Vec<Key> = (u64::from(Key::F1) .. u64::from(Key::F24)) .map (|v| Key::from(v)) .collect();
    let nav_keys   = vec![Key::Left, Key::Right, Key::Up, Key::Down, Key::PageUp, Key::PageDown, Key::Home, Key::End];
    let spcl_keys  = vec![Key::Backspace, Key::Delete, Key::Space, Key::Tab, Key::Enter, Key::Escape, Key::Insert, Key::Apps];
    //let media_keys = vec![Key::BrowserBack, Key::BrowserForward, Key::BrowserRefresh, Key::VolumeMute, Key::VolumeDown, Key::VolumeUp,
    //                      Key::MediaNextTrack, Key::MediaPrevTrack, Key::MediaStop, Key::MediaPlayPause];

    vec![char_keys, fnum_keys, nav_keys, spcl_keys] .concat() .into_iter() .for_each ( |key| {
        add_base_mapping (&k, key, base_action(key))
    } );
    // ^^ we can ofc override these later .. as we will for some like backquote-as-delete F1-as-F21 etc



    // a bunch of caps-as-shift mappings, basically nums or kbd-right symbols not otherwise involved in l2
    "1234567890-=[]\\;\'/." .chars() .for_each ( |c| {
        Key::from_char(c) .into_iter() .for_each ( |key| add_caps_mapping (&k, key, shift_action(key)) )
    } );



    // lets also disable the win-number combos as they annoyingly activate/minimize items from taskbar
    "1234567890" .chars() .map (|c| Key::from_char(c)) .flatten() .for_each ( |key| {
        add_lwin_mapping       (&k, key, no_action());
        add_caps_lwin_mapping  (&k, key, no_action());
    } ); // some of ^^ these will get overwritten by specific win-combos added later .. which is fine

    // note that at least for now, we're choosing to ignore caps-shift, caps-ctrl etc combos, though ofc could impl if need arises






    // setup keys for layer-2 caret nav sel/del/fast modes
    // note: registering as caret-mode key will set caps and actions to nothing, other combos can be set as usual elsewhere
    register_caret_mode_key (&k, Key::E, &k.l2s.in_caret_sel_mode);
    register_caret_mode_key (&k, Key::D, &k.l2s.in_caret_del_mode);
    register_caret_mode_key (&k, Key::F, &k.l2s.in_caret_word_mode);
    register_caret_mode_key (&k, Key::R, &k.l2s.in_caret_fast_mode);

    // setup the key for l4 shortcuts mode, but the mechanism is same as for the caret modes
    register_caret_mode_key (&k, Key::Q, &k.l2s.in_quick_keys_mode);


    // f in caret mode, so we'll remap some of the other combos to replace ctr-f etc
    add_lalt_mapping      (&k, Key::F, alt.inactive_action(ctrl_action(Key::F)));        // alt-f to ctrl-f
    add_caps_lalt_mapping (&k, Key::F, alt.active_on_key(Key::F));          // caps-alt-f to alt-f

    // e in caret mode, so we'll put our left-handed-enter on alt-e instead .. caps-alt-e can continue to do alt-enter
    add_lalt_mapping      (&k, Key::E, alt.inactive_on_key(Key::Enter));     // alt-e -> enter
    add_caps_lalt_mapping (&k, Key::E, alt.active_on_key(Key::Enter));       // caps-alt-e -> alt-enter

    // todo ^^ all the experimental sections above, clean up when sufficiently trained/settled






    // setup backquote .. make normal case be Delete, caps do back-tick, and shift do its tilde, alt will do quick switch via Alt-F20
    // note that since base is remapped, it will change ctrl/shift combos w/ alt/win etc too .. e.g. ctrl-alt-tick gives ctrl-alt-delete !
    let l2s = k.l2s.clone();
    add_base_mapping (&k, Key::Backquote, Arc::new ( move || {
        if l2s.some_shift_down() { press_release(Key::Backquote) } // uses already down shift to give tilde
        else { press_release(Key::ExtDelete) }
    } ) );
    add_caps_mapping (&k, Key::Backquote, base_action(Key::Backquote));
    add_lalt_mapping (&k, Key::Backquote, alt.active_on_key(Key::F20));
    //add_ralt_mapping (&k, Key::Backquote, shift_action(Key::Backquote));
    // ^^ not strictly necessary as cb composition now defaults to this, but also useful to see here for reference



    // setup tab .. caps-as-ctrl for caps-tab switching, incl for ctrl-shift-tab .. also ralt-tab for shift-tab
    let l2s = k.l2s.clone();
    let cb = Arc::new (move || {
        if l2s.is_caps_down.check() {
            l2s.in_managed_ctrl_down_state.set_if_clear();
            Key::LControl.press();  // this enables caps-as-ctrl for caps-tab switching
            // ^^ we're not gonna release ctrl immediately, but keep track and release when caps is released
            press_release(Key::Tab)
        }
    } );
    add_caps_mapping       (&k, Key::Tab, cb.clone());
    add_caps_shift_mapping (&k, Key::Tab, cb);
    // ^^ this enables the natural ctrl-shift-tab to do backwards tablist nav
    //add_ralt_mapping (&k, Key::Tab, shift_action(Key::Tab));
    // ^^ not strictly necessary, as cb compostion now defaults to this, but also useful to see here for reference


    // setup space key .. ralt-space as enter, caps-space as ctrl-space, caps-lalt-space as alt-enter for intellij
    add_ralt_mapping      (&k, Key::Space, base_action(Key::Enter));
    add_caps_lalt_mapping (&k, Key::Space, alt.active_on_key(Key::Enter));
    //add_caps_mapping      (&k, Key::Space, ctrl_action(Key::Space));
    // ^^ not strictly necessary as cb composition now defaults to this, but also useful to see here for reference



    // win-m by default minimized all windows .. we just want to disable it
    add_lwin_mapping (&k, Key::M, no_action());

    // win-i should start irfanview
    add_lwin_mapping (&k, Key::I, Arc::new (|| start_irfanview()));

    // win-n should start chrome-incognito
    add_lwin_mapping (&k, Key::N, Arc::new (|| start_chrome_incognito()));

    // we'll setup win-C (and caps-alt-C) to quickly bring up chrome Tabs-Outliner via switche Alt-F24 hotkey
    add_lwin_mapping (&k, Key::C, win.inactive_action(alt.active_on_key(Key::F24)));   // win-c -> alt-F24
    add_caps_lalt_mapping (&k, Key::C, alt.active_on_key(Key::F24));                   // caps-lalt-c -> alt-F24 .. one-handed


    // in cur laptop, Fn-F6/F7 do brightness, but at +10 incrs .. set them to do small incrs with win combos
    add_lwin_mapping (&k, Key::F6, Arc::new (|| incr_brightness(-1)));
    add_lwin_mapping (&k, Key::F7, Arc::new (|| incr_brightness(1)));

    // might as well do that for alt as well, since we use alt for most other such shortcuts
    add_lalt_mapping (&k, Key::F6, Arc::new (|| incr_brightness(-1)));
    add_lalt_mapping (&k, Key::F7, Arc::new (|| incr_brightness(1)));

    // actually, since we use alt-2/3 as vol down/up, might as well also set win-2/3 for brightness down/up
    add_lwin_mapping (&k, Key::Numrow_2, Arc::new (|| incr_brightness(-1)));
    add_lwin_mapping (&k, Key::Numrow_3, Arc::new (|| incr_brightness(1)));


    // alt-2 is vol down, alt-3 is vol up
    add_lalt_mapping (&k, Key::Numrow_2, alt.inactive_on_key(Key::VolumeDown));
    add_lalt_mapping (&k, Key::Numrow_3, alt.inactive_on_key(Key::VolumeUp));

    // alt-f1 play/pause, caps-f1 toggle mute, base-case alt-F21 for switche caller, ralt for actual F1
    add_base_mapping (&k, Key::F1, alt.active_on_key(Key::F21));
    add_ralt_mapping (&k, Key::F1, base_action(Key::F1));
    add_caps_mapping (&k, Key::F1, base_action(Key::VolumeMute));
    add_lalt_mapping (&k, Key::F1, alt.inactive_on_key(Key::MediaPlayPause));


    // want al-f2 for next with some initial skip .. we'll use caps-alt-f2 for prev, so we'll set it up for both
    // note that our mechanism for wrapping alt-state restoring guards operates via AFs, hence setting those up (instead of fns)

    // skips work by alt-ctrl-volUp, so gotta wrap with alt_active_action
    fn media_skips_action (n_skips:u32, l2s:&L2S) -> AF {
        l2s.lalt.active_action ( Arc::new ( move || {
            (0 .. n_skips) .into_iter() .for_each (|_| { ctrl_press_release(Key::VolumeUp) });
    } ) ) }
    // ^^ gives an AF with specified number of skips

    // media next key shouldnt have alt on it, so should use alt_inactive-action .. (esp given we're on alt combo)
    let l2s = k.l2s.clone();
    let media_next_action = alt.inactive_action ( Arc::new ( move || {
        if !l2s.is_caps_down.check() { press_release(Key::MediaNextTrack) }
        else { press_release(Key::MediaPrevTrack) }
        let l2s = l2s.clone();  // clone again to allow moving into spawned thread closure
        spawn ( move || { sleep(time::Duration::from_millis(2000));  media_skips_action(3,&l2s)(); } );
    } ) );

    // al-f2 for next with some initial skip
    add_lalt_mapping      (&k, Key::F2, media_next_action.clone());
    add_caps_lalt_mapping (&k, Key::F2, media_next_action);

    // alt-f3 for skip forward a bit
    add_lalt_mapping (&k, Key::F3, media_skips_action(1,&k.l2s));



    // escape is just escape, but we just want it to do press-release immediately (so switche is faster)
    add_base_mapping (&k, Key::Escape, base_action(Key::Escape));

    // use the apps key to send shift-escape .. or caps-escape too
    add_base_mapping (&k, Key::Apps,   shift_action(Key::Escape));
    add_caps_mapping (&k, Key::Escape, shift_action(Key::Escape));

    // similar to backquote, lets also add alt-1 for switche next-win (via its Alt-F20 hotkey)
    add_lalt_mapping (&k, Key::Numrow_1, alt.active_on_key(Key::F20));




    // caps-lalt-F for full screen ..(but from bulk mapping above, caps-f should still give ctrl-f)
    //add_caps_lalt_mapping (&k, Key::F, base_action(Key::F11));
    // ^^ no longer available as we made F a caret mode key, and set caps-alt-F to alt-F instead

    // 'w' should have caps-ctrl mapping, but when w/ alt, send alt-f4 (to close all-tabs, windows etc)
    //add_caps_shift_mapping (&k, Key::W, alt_action(Key::F4));
    // ^^ note: initially we wanted this with caps-shift-w, but turns out (at least on my kbd, turns out caps+shift+[F1, 2, w, s, x]
    // >  dont produce any key event at the hook at all .. nothing .. its like the keyboard driver not sending those out
    // funnily enough, there's a bunch of complaints about specifically those keys for dell/hp laptops .. looks like hardware
    // >  appears to be a common kbd pcb layout issue .. heres from 2007: (https://www.joachim-breitner.de/blog/250-Shift-Caps-2)
    // sooo .. to makeup, we'll do alt-caps-w do the alt-f4 business instead
    add_caps_lalt_mapping (&k, Key::W, alt.active_on_key(Key::F4));




    // filling out l2 actions (incl w caps-alt combos) .. the l2_utils module and setup_l2k fn below have more details in doc-comments
    use crate::krusty::l2_utils::*;
    setup_l2k ( &k,  Key::J,     Key::ExtLeft,   Key::Backspace,   ctrl_action,   fast_action,   l2_del_afg );
    setup_l2k ( &k,  Key::K,     Key::ExtRight,  Key::ExtDelete,   ctrl_action,   fast_action,   l2_del_afg );
    setup_l2k ( &k,  Key::I,     Key::ExtUp,     Key::Backspace,   fast_action,   fast_action,   l2_del_sel_afg );
    setup_l2k ( &k,  Key::Comma, Key::ExtDown,   Key::ExtDelete,   fast_action,   fast_action,   l2_del_sel_afg );
    setup_l2k ( &k,  Key::H,     Key::ExtHome,   Key::Backspace,   base_action,   base_action,   l2_del_sel_afg );
    setup_l2k ( &k,  Key::L,     Key::ExtEnd,    Key::ExtDelete,   base_action,   base_action,   l2_del_sel_afg );
    setup_l2k ( &k,  Key::U,     Key::ExtPgUp,   Key::Backspace,   base_action,   base_action,   l2_del_sel_afg );
    setup_l2k ( &k,  Key::M,     Key::ExtPgDn,   Key::ExtDelete,   base_action,   base_action,   l2_del_sel_afg );



    // then the caps-win combo l3 actions :

    // caps-win-U should vert-max (via shift-win-up) if not already, or else restore window from vert-max
    add_caps_lwin_mapping (&k, Key::U, Arc::new (|| win_fgnd_toggle_vertmax()));
    // caps-win-m should maximize (via win-m) if not, else restore from max
    add_caps_lwin_mapping (&k, Key::M, Arc::new (|| win_fgnd_toggle_max()));

    // caps-win-[j,k,i,,] should  move window [left, right, top, bottom] respectively
    add_caps_lwin_mapping (&k, Key::J,     Arc::new (|| win_fgnd_move(-80, 0) ));
    add_caps_lwin_mapping (&k, Key::K,     Arc::new (|| win_fgnd_move(80, 0) ));
    add_caps_lwin_mapping (&k, Key::I,     Arc::new (|| win_fgnd_move(0, -50) ));
    add_caps_lwin_mapping (&k, Key::Comma, Arc::new (|| win_fgnd_move(0, 50) ));

    // caps-win-[h,;,.,o] should stretch window [narrower, wider, shorter, taller] respectively
    // .. note that caps-win-L gets unavoidably captured by windows to lock laptop, hence the semicolon
    add_caps_lwin_mapping (&k, Key::H,       Arc::new (|| win_fgnd_stretch(-30, 0) ));
    add_caps_lwin_mapping (&k, Key::O,       Arc::new (|| win_fgnd_stretch(0, -30) ));
    add_caps_lwin_mapping (&k, Key::Period,  Arc::new (|| win_fgnd_stretch(0, 30) ));
    //add_caps_lwin_mapping (&k, Key::L,     Arc::new (|| win_fgnd_stretch(30, 0) ));
    // ^^ any win-L combo is hardcoded at OS level to lock machine, cant override that, so we'll make semicolon do that instead
    add_caps_lwin_mapping (&k, Key::Semicolon, Arc::new (|| win_fgnd_stretch(30, 0) ));


    // some additional caps-win combos
    // caps-win-c being used to launch winmerge diff from last two clipboard entries
    add_caps_lwin_mapping (&k, Key::C, Arc::new (|| start_winmerge_clipboard() ));
    // gaah we'll just throw in iDEA diff for drag-drop diffing (just coz winmerge doesnt do dark mode)
    //add_lwin_mapping (&k, Key::C, Arc::new (|| start_idea_diff() ));
    // ^^ cant do from here, turns out idea diff from cmd line can ONLY be opened with two files pointed, unlike empty from Idea shortcut!




    // then we can add in any l4 quick-keys shortcuts combos we want

    // first we'll just add some qev for what we set as win combos .. should be tmp just to try things out
    add_quick_keys_mapping (&k, Key::I, action (start_irfanview));
    add_quick_keys_mapping (&k, Key::N, action (start_chrome_incognito));



    // we could also set up specific r-alt combos (for F<?> keys etc other than the default ralt-as-shift)
    // hmm.. cant think of any so far?


    // could also setup caps-ralt combos (for non l2/caret keys), which can be separate from caps-lalt combos!
    // hah .. nothing here yet either huh .. well these are two hand combos, so not preferable anyway


    // if really want/need to, could do completely independent lalt_ralt_<key> combos too (with minimal extra coding)
    // not yet implemented as dont see any need or much utlity for doing so given there are other simpler unused combos avalable


    // also fyi re free combos: caps-win-<non-l3>, win-<num>, caps-alt-<num>, caps-ralt<non-l2>
    // .. even for caps-lalt-<?> defaulting to ctr-alt-<?> most are still free (other than l2, caret, e, f, w, space, f2)
    // .. and almost all F<num> combos with caps, caps-win, caps-lalt, ralt, caps-ralt, even w just lalt


    // plus, for additional l3+ setup, (e.g. moving windows across monitors), could impl 'mode' in l3 (w/ caps-win) like for l2 (w/ caps-alt)
    // .. or add additional mode keys in l2 (caps-alt) .. although not a lot of free keys there .. maybe q .. could reuse mod keys w/ caps-win though




    // finally bind everything from action-maps !!
    bind_all_from_action_maps (&k);



    // and we'll put any direct special key setups after all this
    // > which is for safety in case anything above accidently included those, although ofc we dont want to rely on that!
    setup_direct_binding_keys (&k);


    // note: the handle_input_events to start the whole shebang should be being called somewhere in main after this setup

}

