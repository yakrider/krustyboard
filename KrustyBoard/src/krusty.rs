use std::{time, time::Instant, cell::RefCell};
use std::thread::{sleep, spawn};
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::sync::{Arc, RwLock};

use derivative::Derivative;
use once_cell::sync::Lazy;

//use closure::closure;
// ^^ gives a nice macro that eliminates clutter from having to clone everywhere
// .. we'd use it a lot more, except IDE syntax highlighting doesnt work inside the macro .. which is waay too big a loss for the benefit!

use crate::{BlockInput, KbdEvent, KbdEvntCbMapKeyType::*, KbdKey, MouseButton, MouseEvent, MouseWheel, utils};



// todo : just a reminder that we added some hacky meddling into keycodes and sending key events to get L/R scancodes out on alt/ctrl/shift


// we'll define some easier type aliases (CallBack, ActionFn etc) to pass around triggered actions and so on
type CB  = Box <dyn Fn(KbdEvent) + Send + Sync + 'static> ;
type AF  = Arc <dyn Fn() + Send + Sync + 'static> ;

type Key = KbdKey ;

//type L2S = Arc <Layer2State>;
//type SMK = Arc <SyncdModKey>;
//type TMK = Arc <TrackedModKey>;
// ^^ made these into new-type with deref impld, and then impld all functionality on those
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



# [ derive (Debug) ]
pub struct TrackedModKey {
    // we'll use this for modifier keys that we want to simply track down state, w/o attempting to track or sync their external active states
    // again, motivation for making this separate is to share fumctionality implementations
    _private : (),
    key  : Key,
    down : Flag,
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
    _private : (),
    key      : Key,
    down     : Flag,
    active   : Flag,
    consumed : Flag,

    l2s      : Arc<RwLock<Option<L2S>>>, // backlink for parent l2s we'll try and populate later
    pair     : Arc<RwLock<Option<SMK>>>, // backlink to populate to the left/right counterpart if desired
    // ^^ not the most ideal to have these cyclic backlinks, but its mostly for non-essential checking ctrl state to restore upon masked release etc
    // >  either way, we'll populate it at krusty creation time, so is reliable anyway
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





# [ derive (Derivative) ]
# [ derivative (Debug, Default) ]    // note that we def dont want this clonable (we'd rather clone its Arc than all underlying!)
pub struct Layer2State {
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





use strum::IntoEnumIterator;
use strum_macros::EnumIter;

# [ allow (non_camel_case_types) ]
# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter) ]
pub enum ModKey {
    caps, lalt, ralt, lwin, rwin, lctrl, rctrl, lshift, rshift, alt, win, ctrl, shift
    // ^^ the last four (Alt/Win/Ctrl/Shfit) are intended to imply either of the L/R versions
}
pub type MK = ModKey;

# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter) ]
pub enum ModeState {
    sel, del, word, fast, qks, qks2, qks3, rght_ms_scrl, mngd_ctrl_dn
}

static K_MOD_IDXS: Lazy<HashMap<ModKey,usize>> = Lazy::new ( || {
    ModKey::iter() .enumerate() .map (|(i,x)| (x, i)) .collect()
} );
static MODE_IDXS : Lazy<HashMap<ModeState,usize>> = Lazy::new ( || {
    ModeState::iter() .enumerate() .map (|(i,x)| (x,i)) .collect()
} );

pub const COMBO_STATE_N_BITS: usize = 22;
// ^^ 9 mod keys, 4 for lr-(alt/win/shift/ctrl), 4+3+2 for modes (sel/del/word/fast, qks/qks2/qks3, rght-ms-scrl/mngd-ctrl-dn)

# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone) ]
pub struct Combo {
    _private:(),
    key : Key,
    state : [bool; COMBO_STATE_N_BITS],
}

impl Combo {
    pub fn new (key:Key, mod_keys:&[ModKey], modes:&[ModeState]) -> Combo {
        // lets populate all the modifier key states
        let mut state : [bool; COMBO_STATE_N_BITS] = [false; COMBO_STATE_N_BITS];
        mod_keys .iter() .for_each (|key| {
            K_MOD_IDXS.get(key) .iter() .for_each (|i| { state[**i] = true; })
        });
        // and populate the mode states that are specified too
        modes .iter() .for_each (|mode| {
            MODE_IDXS .get(mode) .iter() .for_each (|i| { state[13 + **i] = true; })
        });
        Combo {_private:(), key, state }
    }
}


// CG_K and CG_AF comprise the Combo-Generation fluent api structs, with Key or with supplied AF
// .. once populated it can be used for generation of either a combo or a (possibly guarded) action

# [ derive (Clone) ]
pub struct CG_K<'a> { _private:(), l2s:&'a L2S, k_mods:Vec<MK>, modes:Vec<ModeState>, key:Key }

# [ derive (Clone) ]
pub struct CG_AF<'a> { _private:(), l2s:&'a L2S, k_mods:Vec<MK>, af:AF }

impl<'a> CG_K<'a> {
    pub fn new (key:Key, l2s:&L2S) -> CG_K {
        CG_K { _private:(), l2s, k_mods:Vec::new(), modes:Vec::new(), key }
    }
    pub fn m (&'a mut self, mk:MK) -> &'a mut CG_K {
        self.k_mods.push(mk); self
    }
    pub fn s (&'a mut self, md:ModeState) -> &'a mut CG_K {
        self.modes.push(md); self
    }
    pub fn gen_combos (&self) -> Vec<Combo> {
        combo_maps::cg_gen_combos (&self.k_mods, &self.modes, self.key)
    }
    pub fn gen_af (&self) -> AF {
        combo_maps::cg_gen_af (&self.k_mods, key_utils::base_action(self.key), self.l2s)
    }
}

impl<'a> CG_AF<'a> {
    pub fn new  (af:AF,   l2s:&L2S) -> CG_AF {
        CG_AF { _private:(), l2s, k_mods:Vec::new(), af }
    }
    pub fn m (&'a mut self, mk:MK) -> &'a mut CG_AF {
        self.k_mods.push(mk); self
    }
    pub fn gen_af (&self) -> AF {
        combo_maps::cg_gen_af (&self.k_mods, self.af.clone(), self.l2s)
    }
}


mod combo_maps {
    use crate::krusty::{*, ModKey::*, ModeState::*};

    pub static LR_MODS : [(MK,MK,MK);4] = [
        (ctrl,  lctrl,  rctrl),
        (shift, lshift, rshift),
        (alt,   lalt,   ralt),
        (win,   lwin,   rwin),
    ]; // note ^^ this order is relied on elsewhere, plus we want the fn composition to have ctrl innermost (as others use ctrl masking)
    // .. and the successive wrapping means the first one on this list is innermost .. and so it state will be updated with others masking
    // also, we'd rather have win at the end here (and wrap outermost), because that has a spawn and delay in reactivation .. still ok but still

    // this will give the SMKs in order defined at LR_MODS so they can be matched up at runtime
    fn lrmk_smks (l2s:&L2S) -> [&SMK;4] { [&l2s.lctrl, &l2s.lshift, &l2s.lalt, &l2s.lwin] }

    fn mod_smk_pairs (l2s:&L2S) -> [(MK,&SMK);6] { [ // note that ralt is TMK not SMK (and doesnt to activate/inactivate etc)
        (lwin, &l2s.lwin), (lalt, &l2s.lalt), (lctrl, &l2s.lctrl), (rctrl, &l2s.rctrl), (lshift, &l2s.lshift), (rshift, &l2s.rshift),
    ] }


    /// generate the combo bit-map for the current runtime state (incl the active key and l2s state flags)
    pub fn gen_cur_combo (key:Key, l2s:&L2S) -> Combo {
        // we'll match up all the flags for the mod keys and put them into our mods vec to gen combo
        let mut mods : Vec<MK> = Vec::new();
        [lwin, lalt, lshift, rshift, lctrl, rctrl, ralt, caps] .to_vec() .iter() .zip (
            [&l2s.lwin.down, &l2s.lalt.down, &l2s.lshift.down, &l2s.rshift.down,
                &l2s.lctrl.down, &l2s.rctrl.down, &l2s.ralt.down, &l2s.caps_down] .to_vec() .iter()
        ) .collect::<Vec<(&MK,&&Flag)>>() .iter() .for_each ( |(mk, b)| { if b.check() { mods.push(**mk); } } );
        // also populate the mode states
        let mut modes : Vec<ModeState> = Vec::new();
        [sel, del, word, fast, qks, qks2, qks3] .to_vec() .iter() .zip (
            [ &l2s.l2_sel_mode_key_down, &l2s.l2_del_mode_key_down, &l2s.l2_word_mode_key_down, &l2s.l2_fast_mode_key_down,
                &l2s.qks_mode_key_down, &l2s.qks2_mode_key_down, &l2s.qks3_mode_key_down
            ] .to_vec() .iter()
        ) .collect::<Vec<(&ModeState,&&Flag)>>() .iter() .for_each (|(ms, b)| {
            if b.check() && mods.len()==1 && mods.contains(&caps) { modes.push(**ms); }
        } ); // ^^ note that these modes can only be on when caps is the only mod down
        // finally lets add the two other non-mode like global flag states too
        if l2s.in_right_btn_scroll_state.check()  { modes.push (rght_ms_scrl) }
        if l2s.in_managed_ctrl_down_state.check() { modes.push (mngd_ctrl_dn) }
        Combo::new(key, &mods as &[ModKey], &modes as &[ModeState])
    }



    fn fan_lrmk (k_mods:&Vec<MK>, lrmk:&MK, lmk:&MK, rmk:&MK) -> Vec<Vec<MK>> {
        // if a mod vec had this l/r mod (e.g. Alt), we'll instead gen three mod vecs having (LAlt, RAlt, LAlt && RAlt)
        let mut mvs: Vec<Vec<MK>> = Vec::new();
        if k_mods.contains(lrmk) {
            let others = k_mods.iter() .filter (|m| *m != lrmk && *m != lmk && *m != rmk) .map (|m| *m) .collect::<Vec<MK>>();
            let (mut va, mut vb, mut vab) : (Vec<MK>,Vec<MK>,Vec<MK>) = (Vec::new(), Vec::new(), Vec::new());
            va.append(&mut others.clone()); vb.append(&mut others.clone()); vab.append(&mut others.clone());
            va.push(*lmk); vb.push(*rmk); vab.push(*lmk); vab.push(*rmk);
            mvs.push(va); mvs.push(vb); mvs.push(vab);
        } else {
            mvs.push(k_mods.clone())
        }
        mvs
    }
    fn fan_lrmks (mvs: &Vec<Vec<MK>>, lrmk:&MK, lmk:&MK, rmk:&MK) -> Vec<Vec<MK>> {
        // this just does the fan_lrmk above for each of the expanded mods-vec in the vec-of-vecs
        let mut mvsf : Vec<Vec<MK>> = Vec::new();
        mvs .iter() .for_each (|mv| {
           fan_lrmk (mv, lrmk, lmk, rmk) .iter() .for_each (|mv| mvsf.push(mv.clone()));
        });
        mvsf
    }
    pub fn fan_lr (k_mods:&Vec<MK>) -> Vec<Vec<MK>> {
        // we'll expand out this mod-vec into vec-of-vec with all L/R optional mods expanded into piles of L/R/L+R mod-vecs
        let mut mvsf : Vec<Vec<MK>> = Vec::new();
        mvsf.push(k_mods.clone());
        LR_MODS .iter() .for_each (|(lrmk, lmk, rmk)| {
            mvsf = fan_lrmks (&mvsf, lrmk, lmk, rmk);
        } );
        mvsf
    }



    pub fn cg_gen_combos (k_mods:&Vec<MK>, modes:&Vec<ModeState>, key:Key) -> Vec<Combo> {
        fan_lr(k_mods) .iter() .map(|mv| {Combo::new (key, &mv[..], modes)}) .collect::<Vec<Combo>>()
    }
    pub fn cg_gen_af (k_mods:&Vec<MK>, af:AF, l2s:&L2S) -> AF {
        // note that this will only wrap actions using L-mod-keys .. hence there's still utility in wrapping consuming AF after this
        let mc = |mk:MK| k_mods.contains(&mk) ;
        let mut af = af;
        let lr_zip = LR_MODS.iter() .zip(lrmk_smks(l2s).into_iter()) .collect::<Vec<(&(MK,MK,MK),&SMK)>>();
        lr_zip .iter() .for_each ( |((lrmk,lmk,rmk),smk)| {
            af = if mc(*lrmk) || mc(*lmk) || mc(*rmk) { smk.active_action(af.clone()) } else { smk.inactive_action(af.clone()) }
        });
        af
    }
    pub fn gen_consuming_af (k_mods:&Vec<MK>, af:AF, l2s:&L2S) -> AF {
        let mc = |mk:MK| k_mods.contains(&mk) ;
        let mut af = af;
        mod_smk_pairs(l2s) .iter() .for_each ( |(kmod,smk)| {
            if mc(*kmod) { af = smk.keydn_consuming_action(af.clone()) }
        });
        af
    }


    /// use this fn to register combos that will auto wrap activation/inactivation AFs around any mods in the combo
    pub fn add_combo (k:&Krusty, cgc:&CG_K, cga:&CG_K) {
        // note that although the active/inactive will also mark mk keydn consumed, we only do that for L-keys, hence wrapping consuming AF too
        let af = gen_consuming_af (&cgc.k_mods, cga.gen_af(), &k.l2s);
        for c in cgc.gen_combos() {
            //println! ("{:?}, {:?}, {:?}, {:?} -> {:?} {:?}", c.state, cgc.key, cgc.mods, cgc.modes, cga.key, cga.mods );
            k .combos_map .borrow_mut() .insert (c, af.clone());
    } }
    /// note that this will wrap the AF in with mod-actions for all mods either active (if specified) or inactive (if not)
    pub fn add_af_combo (k:&Krusty, cgc:&CG_K, cgaf:&CG_AF) {
        let af = gen_consuming_af (&cgc.k_mods, cgaf.gen_af(), &k.l2s);
        for c in cgc.gen_combos() {
            //println! ("{:?}, {:?}, {:?} {:?} -> {:?}", c.state, cgc.key, cgc.mods, cgc.modes, cgaf.mods );
            k .combos_map .borrow_mut() .insert (c, af.clone());
    } }
    /// use this consuming bare fn to add combos if the AF supplied shouldnt be wrapped for active/inactive by mod, but just marked for consumption
    pub fn add_cnsm_bare_af_combo(k:&Krusty, cgc:&CG_K, af:AF) {
        let af = gen_consuming_af (&cgc.k_mods, af, &k.l2s);
        for c in cgc.gen_combos () {
            k .combos_map .borrow_mut() .insert (c, af.clone());
    } }
    /// use this bare fn to add combos if the AF supplied needs special care to avoid wrapping with mod active/inactive .. e.g ctrl/tab etc
    pub fn add_bare_af_combo(k:&Krusty, cgc:&CG_K, af:AF) {
        for c in cgc.gen_combos () { k .combos_map .borrow_mut() .insert (c, af.clone()); }
    }


    /// combo Action-Function generator type for the cb composition time combo-AF-gen fn below
    pub type CAFG =  Arc<dyn Fn(&Combo) -> Option<AF> + Send + Sync + 'static>;

    /// this will gen the runtime combo action closure for each key to register in callbacks ..
    /// note that since each key can be in many combos, it will extract and move a mini subset combo hashmap into the closure!
    pub fn get_combo_af_gen_for_key (k:&Krusty, key:Key) -> CAFG {
        let mut ckm : HashMap<Combo,AF> = HashMap::new();
        k .combos_map .borrow() .iter() .for_each ( |(c,af)| {
            if c.key.eq(&key) { ckm.insert(*c, af.clone()); }
        } );
        Arc::new ( move |cc:&Combo| {
            ckm .get (cc) .map (|af| af.clone())
        } )
    }


}






pub struct Krusty {
    // this is mostly just a utility wrapper sugar to pass things around
    _private : (),   // prevents direct instantiation of this struct
    // L2S is Arc<Layer2State>, holds all state flags
    pub l2s : L2S,
    // we'll also keep a mapping of caret-mode keys and their associated flags for l2 impl
    pub mode_keys_map : RefCell <HashMap <Key, Flag>>,
    // we'll have a combos map to register all combos (key + modifiers + modes) to their mapped actions
    pub combos_map : RefCell <HashMap <Combo, AF>>,
    // instead of polluting combos_map, we'll hold a registry for keys to gen default bindings for
    pub default_bind_keys : RefCell <HashSet <Key>>,
}



impl Krusty {

    // NOTE: we've decided to mostly impl things grouped by related functionality in disparate modules rather than under impl here
    // .. mostly because doing that allows easier piecemeal imports, as well as better pub/private management

    pub fn new() -> Krusty {
        let l2s = L2S::new();
        // we'll fill these with backlinks to parent (mostly to avoid always having to pass l2s into their impld fns)
        l2s.lalt .link_parent (&l2s);
        l2s.lwin .link_parent (&l2s);
        // and we'll pair up the left/right values where applicable
        l2s.lctrl  .link_pair (&l2s.rctrl);  l2s.rctrl  .link_pair (&l2s.lctrl);
        l2s.lshift .link_pair (&l2s.rshift); l2s.rshift .link_pair (&l2s.lshift);
        // now we can construct the Krusty struct we want
        Krusty {
            _private : (),
            l2s : l2s,
            mode_keys_map : RefCell::new (HashMap::new()),
            combos_map : RefCell::new (HashMap::new()),
            default_bind_keys : RefCell::new (HashSet::new()),
    } }

    pub fn cg    (&self, key:Key) -> CG_K  { CG_K::new  (key, &self.l2s) }
    pub fn cg_af (&self, af:AF)   -> CG_AF { CG_AF::new (af,  &self.l2s) }

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

    // for the constructors, we'll create specific ones to create tracked-mod-keys for alt and win out of the initialized l2s flags
    // .. note that we want the SMK struct which is Arc wrapped SyncdModKey with deref .. that gives us cheaper clone and good code ergo

    pub fn new (key:Key) -> SMK { SMK ( Arc::new ( SyncdModKey {
        _private : (),
        key      : key,
        down     : Flag::default(),
        active   : Flag::default(),
        consumed : Flag::default(),
        l2s      : Arc::new(RwLock::new(None)),
        pair     : Arc::new(RwLock::new(None)),
    } ) ) }

    pub fn link_parent (&self, l2s:&L2S) {
        let _ = self.l2s.write().unwrap().insert(l2s.clone());
    }
    pub fn link_pair (&self, smk:&SMK) {
        let _ = self.pair.write().unwrap().insert(smk.clone());
    }


    pub fn setup_tracking (&self, l2sr:&L2S) {

        // NOTE: given that kbds seem to have idiosyncrasies with what scancode vs vk codes they sent, we end up getting out of sync w
        // what keys with let through and what we simulate .. e.g in my machine lshift comes in vk while rshift comes sc and so sending our
        // vk shift doesnt clear it out .. so we've decided to just block everything and send our uniform up/down reports instead!

        // beyond that, we'll block repeats to keep code logic (and keystream inspections) manageable
        // we'll also track and update both physical and externally expressed states and try and keep our model always in sync w the outside
        // we'll also track if we've used up the press so we can mask its release later (for things like win/alt that trigger menus on release)

        let (l2s, smk) = (l2sr.clone(), self.clone());
        smk.key.block_bind (KeyDownCallback, move |_| {
            // so goal here is, any presses with caps active, we suppress mod-key going outside
            // and since we can have caps come in after the mod-key is already down, we'll have to capture disparity states ..
            //  .. as well as restoring them when either caps/alt gets released etc
            // (plus, if we're down and caps goes down, we'll get notification below so we're enforcing the disabled state from both sides)
            if l2s.caps_down.check() {
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
                    l2s.is_wheel_spin_invalidated.set_if_clear();
                    smk.key.press();
            } }
        } );

        let (l2s, smk) = (l2sr.clone(), self.clone());
        smk.key.block_bind(KeyUpCallback, move |_| {
            // if caps is pressed, or alt is already inactive (via masked-rel, press-rel etc), we block it
            // else if win was consumed, we release with mask, else we can actually pass it through unblocked
            smk.down.clear_if_set();
            if !smk.active.check() || l2s.caps_down.check() {
                // if inactive or caps-down we just suppress this keyup
            } else {
                smk.active.clear();
                if smk.is_keyup_unified() && smk.paired_active() {
                    // for shift (w/ keyup state unified), ONLY send up a keyup if the other key isnt down .. so do nothing
                } else {
                    if !smk.consumed.check() { smk.key.release() } else { smk.release_w_masking() }
                    // and follow-on for up-unified, now we gotta try and clear the other one too
                    if smk.is_keyup_unified() {
                        smk.pair.read().unwrap() .iter() .for_each (|p| p.key.release())
                        // ^ and no unified key (which is just shift really), needs masked releases
                } }
        } } );

    }


    pub fn process_caps_down (&self) {
        // we will immediately invalidate and clear any down lalt found upon caps activation!
        // note that internally tracked physical is_alt_down will continue to be down!
        if self.down.check() && self.active.check() {
            let smk = self.clone();
            smk.release_w_masking(); // this will update flags too
            // ^^ note that cant spawn thread for this as on caps down this will get called for all down mods, so threads could cause race
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

    pub fn is_rel_masking   (&self) -> bool { self.key == Key::LWin || self.key == Key::RWin || self.key == Key::LAlt } // excluding RAlt
    pub fn is_rel_delaying  (&self) -> bool { self.key == Key::LWin || self.key == Key::RWin }
    pub fn is_keyup_unified (&self) -> bool { self.key == Key::LShift || self.key == Key::RShift }

    pub fn paired_down   (&self) -> bool { self.pair.read() .unwrap() .iter() .any (|p| p.down.check()) }
    pub fn paired_active (&self) -> bool { self.pair.read() .unwrap() .iter() .any (|p| p.active.check()) }
    pub fn paired_any_active (&self) -> bool { self.active.check() || self.paired_active() }

    pub fn reactivate (&self) { self.key.press(); self.active.set_if_clear(); }
    pub fn paired_reactivate (&self) { self.pair.read().unwrap() .iter() .for_each (|p| p.reactivate()) }

    pub fn release_w_masking(&self) {
        // masking w ctrl helps avoid/reduce focus loss to menu etc for alt/win
        self.active.clear_if_set();
        if !self.is_rel_masking() { self.key.release(); } // note that this could be ctrl itself, which does non-masked release
        else {
            // remember that if we're here doing a ctrl-masked-rel, we're obv not ctrl ourselves (hence can freely call ctrl restoration)
            Key::LCtrl.press(); self.key.release(); Key::LCtrl.release();
            // we should have backlink to l2s populated, can use that to check and restore ctrl states
            self.l2s.read().unwrap() .iter() .filter (|l2s| l2s.lctrl.active.check()) .for_each (|_| Key::LCtrl.press());
            self.l2s.read().unwrap() .iter() .filter (|l2s| l2s.rctrl.active.check()) .for_each (|_| Key::RCtrl.press());
        }
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
    fn keydn_consuming_action (&self, af:AF) -> AF {
        let smk = self.clone();
        Arc::new ( move || { smk.consumed.set_if_clear(); af(); } )
    }


    /// use this to wrap actions when we want the mod-key to be ACTIVE in the combo .. can use for both self-mod-key combos or unrelated combos
    /// .. e.g. if setting up alt-X to send alt-win-y, we'd set lalt-mapping on Key::X as k.alt.active_action(k.win.active_on_key(Key::Y))
    pub fn active_action (&self, af:AF) -> AF {
        let smk = self.clone();
        Arc::new ( move || {
            smk.consumed.set_if_clear();
            if smk.paired_any_active() { af() }
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
            if !smk.paired_any_active() { af() }
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
                        // now re-set flag as the release call will have cleared it, but only if its still down!
                        // however, its delayed, so we should only reactivate if things are still down .. oh well
                        if smk.down.check() { smk.reactivate() }
                        if smk.paired_down() { smk.paired_reactivate() }
                    } );
        } } } )
    }
    pub fn inactive_on_key (&self, key:Key) -> AF { self.inactive_action (key_utils::base_action(key)) }
    // ^^ some sugar to make common things simpler .. could add for ctrl etc too if there was use


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

        let l2s = k.l2s.clone();
        Key::CapsLock.block_bind(KeyDownCallback, move |_| {
            if !l2s.caps_down.check() {
                // capslock can come as repeats like other mod keys .. this was a fresh one
                l2s.caps_down.set();
                l2s.is_wheel_spin_invalidated.set_if_clear();
                // lets notify the synced tracked mod keys, so they can invalidate/release themselves
                [ &l2s.lalt, &l2s.lwin, &l2s.lctrl, &l2s.rctrl, &l2s.lshift, &l2s.rshift
                ] .iter() .for_each ( |smk| smk.process_caps_down() );
            }
            if l2s.mouse_left_btn_down.check() && !l2s.in_managed_ctrl_down_state.check() {
                l2s.in_managed_ctrl_down_state.set();
                l2s.lctrl.ensure_active();
            }
        });

        let l2s = k.l2s.clone();
        Key::CapsLock.block_bind(KeyUpCallback, move |_| {
           l2s.caps_down.clear_if_set();
            if l2s.in_managed_ctrl_down_state.check() {
                l2s.in_managed_ctrl_down_state.clear();
                l2s.lctrl.ensure_inactive();
            }
            // the following isnt strictly necessary, but useful in case some keyup falls through
            l2s.clear_caret_mode_flags();
            // lets also notify the alt/win tracked mod keys so they can re-enable themselves if applicable
            [ &l2s.lalt, &l2s.lwin, &l2s.lctrl, &l2s.rctrl, &l2s.lshift, &l2s.rshift
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
        let l2s = k.l2s.clone();
        MouseButton::LeftButton.block_bind(true, move |_| {
            l2s.mouse_left_btn_down.set_if_clear();
            if l2s.caps_down.check() {
                l2s.in_managed_ctrl_down_state.set_if_clear();
                l2s.lctrl.ensure_active();   // this allows caps-as-ctrl for drag drop etc
            }
            MouseButton::LeftButton.press()
        });
        let l2s = k.l2s.clone();
        MouseButton::LeftButton.block_bind(false, move |_| {
            l2s.mouse_left_btn_down.clear_if_set();
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
            l2s.mouse_right_btn_down.set_if_clear();
        });
        let l2s = k.l2s.clone();
        let switche_action = base_action(Key::F23);
        MouseButton::RightButton.non_blocking_bind(false, move |_| {
            l2s.mouse_right_btn_down.clear_if_set();
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
        if l2sr.mouse_right_btn_down.check() {
            // right-mouse-btn-wheel support for switche task switching
            l2sr.in_right_btn_scroll_state.set_if_clear();
            let key = if incr.is_positive() { Key::F22 } else { Key::F21 };
            //l2sr.lalt.mod_press_release_guarded(key);
            press_release(key);
        } else  if l2sr.lalt.down.check() {
            // wheel support for scrolling in windows native alt-tab task-switching screen
            // this requires a system call to check alt-tab window, so push it out to thread
            l2sr.lalt.consumed.set_if_clear();
            let lalt = l2sr.lalt.clone();
            spawn ( move || {
                if get_fgnd_win_class() == "MultitaskingViewFrame" { // alt-tab states
                    lalt.ensure_active(); // we're already down but just in case its down/inactive
                    handle_alt_tab_wheel(incr)
                } else { incr_volume(incr); } // simple alt wheel for volume
            } );
        } else if l2sr.lwin.down.check() {
            l2sr.lwin.consumed.set_if_clear();
            incr_brightness(incr); // win-wheel for brightness
        } else if l2sr.caps_down.check() {
            l2sr.in_managed_ctrl_down_state.set_if_clear();
            //Key::LCtrl.press();
            l2sr.lctrl.ensure_active();
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


    use crate::krusty::*;

    pub fn setup_direct_binding_keys (k:&Krusty) {

        // currently, we dont need to do it for anything .. (was more essential when mod-tracking / combo-maps mechanisms were more limited)

        // HOWEVER, there might be cases where it might be simpler to just do direct bindings than to populate all the combo maps etc etc
        // ALSO, for things that require actual action in key-up callbacks, we'd do it direclty here because key-dn actions arent registered
        // > specifically in maps and so combo-maps composed callbacks can only either set key-up to do nothing, or at most clear mode flags

        // IMPORTANT : note that whatever we put here, we will NOT want to include them in ANY action maps (or overwrite w these at the end) !!

    }


}




pub mod action_map_utils {
    use crate::krusty::{*, key_utils::*, combo_maps::*};

    /// note: registering a caret mode key auto sets their caps-only action to nothing, others combos can be set as usual
    pub fn register_caret_mode_key (k: &Krusty, key:Key, ms:ModeState, mode_flag:&Flag) {
        k .mode_keys_map .borrow_mut() .insert(key, mode_flag.clone());
        //add_af_combo_bare (k, k.cg(key).m(MK::caps).s(ms), &no_action());
        // ^^ not adequate as we'll have all sorts of mode combinations .. better to just disable it in runtime fallbacks
    }
    /// if the key is registered as mode keys, this will generate  (is_mode_key, key-dn, key-up) caret-flag setting actions
    fn gen_key_caret_mode_actions (k:&Krusty, key:Key) -> (bool, AF,AF) {
        if let Some(cmfr) = k.mode_keys_map.borrow().get(&key) {
            let (cmf_c1, cmf_c2) = (cmfr.clone(), cmfr.clone()); // gotta clone to move into AF
            return (true, Arc::new (move || cmf_c1.set_if_clear()), Arc::new (move || cmf_c2.clear_if_set()) )
        } // ^^ key-dn and key-up caret-mode-actions for a caret-mode-key is to set and clear its caret-mode-flag
        (false, no_action(), no_action()) // empty (dn,up) caret-flag-actions for non-caret keys
    }


    type BFK = Box <dyn Fn(Key)>;
    fn wrapped_bfn (wk:Key, kbf:BFK) -> BFK {
        Box::new ( move |key:Key| { wk.press(); kbf(key); wk.release(); } )
    }
    fn caps_fallback_action_fn (key:Key, l2s:&L2S) {
        // if no combo found while caps down, we want to support most multi-mod combos treating caps as ctrl..
        // however, we have caps-dn suppress all mod-keys, so we'll have to wrap mod-key up/dn here as necessary
        let mut kf : BFK = Box::new(|key:Key| press_release(key));
        // ^^ stable rust doesnt let me use existential/dyn stuff in vars to wrap progressively, so use boxes at minimal cost
        if l2s.caps_down.check() || l2s.some_ctrl_down()  { kf = wrapped_bfn (Key::LCtrl, kf) }
        if l2s.ralt.down.check() || l2s.some_shift_down() { kf = wrapped_bfn (Key::LShift, kf) }
        if l2s.lalt.down.check() { kf = wrapped_bfn (Key::LAlt, kf) }
        if l2s.some_win_down()   { kf = wrapped_bfn (Key::LWin, kf) }
        kf(key);
    }

    fn compose_action_maps_cb (k:&Krusty, key:Key) -> (CB, CB) {
        let (is_cmk, cma_dn, cma_up) = gen_key_caret_mode_actions (k, key);
        let cmafg = get_combo_af_gen_for_key (k, key);
        let l2s = k.l2s.clone();
        let cb_dn = Box::new ( move |_| {
            // note that we're ^^ ignoring the KbdEvent passed in (it'd have KbdEventType and key vk/sc codes)
            cma_dn();
            if let Some(cmaf) = cmafg (&gen_cur_combo(key,&l2s)) {
                cmaf();  // yay found combo match!
            } else if l2s.only_caps_mod_key_down() && (is_cmk || l2s.some_qks_mode_key_down()) {
                // during quick-keys modes, or for actual mode keys when caps is down, default action is to do nothing
            } else if l2s.caps_down.check() { caps_fallback_action_fn (key,&l2s); }  // caps has extensive remapping fallback
            else if l2s.ralt.down.check() { shift_press_release(key) }               // unmapped ralt is set to shift
            else { press_release(key) }                                              // all else works naturally via passthrough
        } );
        // and finally, the key up action, which is really just caret mode flag action if any
        let cb_up = Box::new ( move |_| { cma_up() } );
        // aight, these are ready to be bound .. ship it!
        (cb_dn, cb_up)
    }


    pub fn bind_all_from_action_maps (k:&Krusty) {
        // first we want to register all the keys the combo maps and in default-binds list into a keyset
        let mut keys = HashSet::new();
        k .combos_map        .borrow() .iter() .for_each ( |(c,_)|   { keys.insert(c.key); } );
        k .mode_keys_map     .borrow() .iter() .for_each ( |(key,_)| { keys.insert(*key); } );
        k .default_bind_keys .borrow() .iter() .for_each ( |key|     { keys.insert(*key); } );

        // then for each key in the set, we'll gen the composed action and bind it
        for key in keys.iter() {
            let (cb_dn, cb_up) = compose_action_maps_cb (k, *key);
            key.block_bind (KeyDownCallback, cb_dn);
            key.block_bind (KeyUpCallback,   cb_up);
        }
        // might as well clear up some setup resources now that done setting things up for runtime!
        k.combos_map.borrow_mut().clear();
        k.mode_keys_map.borrow_mut().clear();
        k.default_bind_keys.borrow_mut().clear();
    }



}



pub fn setup_krusty_board () {

    use crate::krusty::{*, key_utils::*, caps_setup::*, special_keys_setups::*, combo_maps::*,
                        mouse_btn_setups::*, mouse_wheel_setups::*, action_map_utils::*};

    use crate::utils::{window_utils::*, process_utils::*};

    use crate::krusty::{ModKey::*, KbdKey::*, ModeState::*};
    // ^^ a little extreme, but we'll see if its tolerable


    let k = Krusty::new();

    // setup capslock, we'll completely disable it other than for krusty use
    setup_caps_tracking (&k);

    // setup tracking for right-alt .. its completely blocked but tracked, we use it as shifts and combos
    k.l2s.ralt .setup_tracking (true);      // for ralt can setup w doBlock=true

    // and shift .. this is simple non-blocking tracking, only tracked to enable a few combos with caps
    k.l2s.lshift.setup_tracking (&k.l2s);     // these others have doBlock = false
    k.l2s.rshift.setup_tracking (&k.l2s);

    // and even ctrl .. again simple non-blocking, just tracking flags to allow for natural ctrl-alt-? when alt-? is remapped
    k.l2s.lctrl.setup_tracking (&k.l2s);
    k.l2s.rctrl.setup_tracking (&k.l2s);

    // lalt and lwin are set as syncd-tracked-modifier-key with special (but identical) impl .. more details under SyncdModKey impl
    k.l2s.lalt.setup_tracking (&k.l2s);
    k.l2s.lwin.setup_tracking (&k.l2s);


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
    // note: registering as caret-mode key will set caps-only actions to nothing, other combos can be set as usual elsewhere
    register_caret_mode_key (&k, E, sel,  &k.l2s.l2_sel_mode_key_down);
    register_caret_mode_key (&k, D, del,  &k.l2s.l2_del_mode_key_down);
    register_caret_mode_key (&k, F, word, &k.l2s.l2_word_mode_key_down);
    register_caret_mode_key (&k, R, fast, &k.l2s.l2_fast_mode_key_down);

    // setup the key for l4 shortcuts mode, but the mechanism is same as for the caret modes
    register_caret_mode_key (&k, Q,        qks,  &k.l2s.qks_mode_key_down);
    register_caret_mode_key (&k, Numrow_2, qks2, &k.l2s.qks2_mode_key_down);
    register_caret_mode_key (&k, Numrow_3, qks3, &k.l2s.qks3_mode_key_down);



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
    let l2s = k.l2s.clone();
    let cb : AF = Arc::new (move || {
        if l2s.caps_down.check() {
            l2s.in_managed_ctrl_down_state.set_if_clear();
            l2s.lctrl.ensure_active();  // this enables caps-as-ctrl for caps-tab switching
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
    fn media_skips_action (n_skips:u32, l2s:&L2S) -> AF {
        l2s.lalt.active_action ( Arc::new ( move || {
            (0 .. n_skips) .into_iter() .for_each (|_| { ctrl_press_release(VolumeUp) });
    } ) ) }
    // ^^ gives an AF with specified number of skips

    // media next key shouldnt have alt on it, so should use alt_inactive-action .. (esp given we're on alt combo)
    let l2s = k.l2s.clone();
    let media_next_action =  k.l2s.lalt.inactive_action ( Arc::new ( move || {
        if !l2s.caps_down.check() { press_release(MediaNextTrack) }
        else { press_release(MediaPrevTrack) }
        let l2s = l2s.clone();  // clone again to allow moving into spawned thread closure
        spawn ( move || { sleep(time::Duration::from_millis(2000));  media_skips_action(3,&l2s)(); } );
    } ) );

    // al-f2 for next with some initial skip
    add_cnsm_bare_af_combo (&k, &k.cg(F2).m(lalt),         media_next_action.clone());
    add_cnsm_bare_af_combo (&k, &k.cg(F2).m(caps).m(lalt), media_next_action);

    // alt-f3 for skip forward a bit
    add_cnsm_bare_af_combo (&k, &k.cg(F3).m(lalt), media_skips_action(1, &k.l2s));



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

    // caps-win-[j,k,i,,] should  move window [left, right, top, bottom] respectively
    add_cnsm_bare_af_combo (&k, &k.cg(J).    m(caps).m(lwin), Arc::new (|| win_fgnd_move(-80, 0) ));
    add_cnsm_bare_af_combo (&k, &k.cg(K).    m(caps).m(lwin), Arc::new (|| win_fgnd_move(80, 0) ));
    add_cnsm_bare_af_combo (&k, &k.cg(I).    m(caps).m(lwin), Arc::new (|| win_fgnd_move(0, -50) ));
    add_cnsm_bare_af_combo (&k, &k.cg(Comma).m(caps).m(lwin), Arc::new (|| win_fgnd_move(0, 50) ));

    // caps-win-[h,;,.,o] should stretch window [narrower, wider, shorter, taller] respectively
    // .. note that caps-win-L gets unavoidably captured by windows to lock laptop, hence the semicolon
    add_cnsm_bare_af_combo (&k, &k.cg(H).     m(caps).m(lwin), Arc::new (|| win_fgnd_stretch(-30, 0) ));
    add_cnsm_bare_af_combo (&k, &k.cg(O).     m(caps).m(lwin), Arc::new (|| win_fgnd_stretch(0, -30) ));
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
    add_combo (&k, &k.cg(I)    .m(caps).s(qks2), &k.cg(ExtUp  ).m(lalt).m(lctrl).m(lshift));
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

