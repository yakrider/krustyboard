

use std::collections::{HashMap, HashSet};

use once_cell::sync::Lazy;

use strum::IntoEnumIterator;
use strum_macros::EnumIter;


use crate::krusty::{*, mod_keys::*, key_utils::*};

use crate::krusty::combo_maps::{ModKey::*, ModeState::*};

use crate::{KbdEventCallbackEntry, KbdEventCallbackFnType::*, KbdEvCbComboProcDirective::*, EventPropagationDirective::*};


# [ allow (non_camel_case_types) ]
# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter) ]
pub enum ModKey {
    caps, lalt, ralt, lwin, rwin, lctrl, rctrl, lshift, rshift, alt, win, ctrl, shift
    // ^^ the last four (Alt/Win/Ctrl/Shfit) are intended to imply either of the L/R versions
}
pub type MK = ModKey;


# [ allow (non_camel_case_types) ]
# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter) ]
pub enum ModeState {
    sel, del, word, fast, qks, qks1, qks2, qks3, rght_ms_scrl, mngd_ctrl_dn
}


static L2_MODES:   Lazy <HashSet <ModeState>> = Lazy::new (||
    [sel, del, word, fast] .into_iter() .collect()
);
static QKS_MODES:   Lazy <HashSet <ModeState>> = Lazy::new (||
    [qks, qks1, qks2, qks3] .into_iter() .collect()
);
static CAPS_MODES: Lazy <HashSet <ModeState>> = Lazy::new (||
    [sel, del, word, fast, qks, qks1, qks2, qks3, rght_ms_scrl] .into_iter() .collect()
);

static K_MOD_IDXS: Lazy <HashMap <ModKey,usize>> = Lazy::new ( || {
    ModKey::iter() .enumerate() .map (|(i,x)| (x, i)) .collect()
} );

static MODE_IDXS : Lazy<HashMap<ModeState,usize>> = Lazy::new ( || {
    ModeState::iter() .enumerate() .map (|(i,x)| (x,i)) .collect()
} );


pub const COMBO_STATE_N_BITS: usize = 23;
// ^^ 13 mod keys from above list, 4+4+2=10 for modes from above list (sel/del/word/fast, qks/qks1/qks2/qks3, rght-ms-scrl/mngd-ctrl-dn)


pub static LR_MODS : [(MK,MK,MK);4] = [
    (ctrl,  lctrl,  rctrl),
    (shift, lshift, rshift),
    (alt,   lalt,   ralt),
    (win,   lwin,   rwin),
];
// note ^^ this order is relied on elsewhere, plus we want the fn composition to have ctrl innermost (if we use ctrl masking, which we dont anymore)
// .. and the successive wrapping means the first one on this list is innermost .. and so its state will be updated with others masking
// also, we'd rather have win at the end here (and wrap outermost), because that has a spawn and delay in reactivation .. still ok but still


// this will give the SMKs in order defined at LR_MODS so they can be matched up at runtime
fn lrmk_smks (ks:&KrS) -> [&SMK;4] { [&ks.lctrl, &ks.lshift, &ks.lalt, &ks.lwin] }


pub fn mod_smk_pairs (ks:&KrS) -> [(MK, &SMK);6] { [ // note that ralt is TMK not SMK (and doesnt to activate/inactivate etc)
    (lwin, &ks.lwin), (lalt, &ks.lalt), (lctrl, &ks.lctrl), (rctrl, &ks.rctrl), (lshift, &ks.lshift), (rshift, &ks.rshift),
] }

pub fn l2_mode_flag_pairs (ks:&KrS) -> [(ModeState, &Flag);4] { [
    (sel, &ks.l2_sel_mode_key_down), (del,  &ks.l2_del_mode_key_down), (word, &ks.l2_word_mode_key_down), (fast, &ks.l2_fast_mode_key_down),
] }
pub fn qks_mode_flag_pairs (ks:&KrS) -> [(ModeState, &Flag);4] { [
    (qks, &ks.qks_mode_key_down),    (qks1, &ks.qks1_mode_key_down),   (qks2, &ks.qks2_mode_key_down),    (qks3, &ks.qks3_mode_key_down),
] }
pub fn mode_flag_pairs (ks:&KrS) -> [(ModeState, &Flag);8] { [
    (sel, &ks.l2_sel_mode_key_down), (del,  &ks.l2_del_mode_key_down), (word, &ks.l2_word_mode_key_down), (fast, &ks.l2_fast_mode_key_down),
    (qks, &ks.qks_mode_key_down),    (qks1, &ks.qks1_mode_key_down),   (qks2, &ks.qks2_mode_key_down),    (qks3, &ks.qks3_mode_key_down),
] }





# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone) ]
pub struct Combo {
    _private:(),
    pub key : Key,
    pub state : [bool; COMBO_STATE_N_BITS],
}


impl Combo {

    pub fn new (key:Key, mod_keys:&[ModKey], modes:&[ModeState]) -> Combo {

        let mut state : [bool; COMBO_STATE_N_BITS] = [false; COMBO_STATE_N_BITS];

        // lets populate all the modifier key states
        mod_keys .iter() .for_each (|key| {
            K_MOD_IDXS.get(key) .iter() .for_each (|i| { state[**i] = true; })
        });

        // and populate the mode states that are specified too
        modes .iter() .for_each (|mode| {
            MODE_IDXS .get(mode) .iter() .for_each (|i| { state[K_MOD_IDXS.len() + **i] = true; })
        });

        Combo {_private:(), key, state }
    }

}


// CG_K and CG_AF comprise the Combo-Generation fluent api structs, with Key or with supplied AF
// .. once populated it can be used for generation of either a combo or a (possibly guarded) action

# [ allow (non_camel_case_types) ]
# [ derive (Clone) ]
pub struct CG_K<'a> {
    _private : (),
    ks       : &'a KrS,
    k_mods   : Vec<MK>,
    modes    : Vec<ModeState>,
    key      : Key,
}


# [ allow (non_camel_case_types) ]
# [ derive (Clone) ]
pub struct CG_AF<'a> {
    _private : (),
    ks       : &'a KrS,
    k_mods   : Vec<MK>,
    af       : AF,
}


impl<'a> CG_K<'a> {
    pub fn new (key:Key, ks:&KrS) -> CG_K {
        CG_K { _private:(), ks, k_mods:Vec::new(), modes:Vec::new(), key }
    }
    pub fn m (&'a mut self, mk:MK) -> &'a mut CG_K {
        self.k_mods.push(mk); self
    }
    pub fn s (&'a mut self, md:ModeState) -> &'a mut CG_K {
        self.modes.push(md); self
    }
    pub fn gen_combos (&self) -> Vec<Combo> {
        cg_gen_combos (&self.k_mods, &self.modes, self.key)
    }
    pub fn gen_af (&self) -> AF {
        cg_gen_af (&self.k_mods, base_action(self.key), self.ks)
    }
}


impl<'a> CG_AF<'a> {
    pub fn new  (af:AF, ks:&KrS) -> CG_AF {
        CG_AF { _private:(), ks, k_mods:Vec::new(), af }
    }
    pub fn m (&'a mut self, mk:MK) -> &'a mut CG_AF {
        self.k_mods.push(mk); self
    }
    pub fn gen_af (&self) -> AF {
        cg_gen_af (&self.k_mods, self.af.clone(), self.ks)
    }
}





/// generate the combo bit-map for the current runtime state (incl the active key and ks state flags)
pub fn gen_cur_combo (key:Key, ks:&KrS) -> Combo {
    // we'll match up all the flags for the mod-keys and mode-states and put them into our mods vec to gen combo
    // (note that ordering does not matter here .. bitmap indices will be looked up for each key to set the correct combo-bitmap bit)
    let mut mks : Vec<MK> = Vec::new();
    let mut modes : Vec<ModeState> = Vec::new();

    // lets add the lone TMK mod-key (ralt) state
    if ks.ralt.down.check() { mks.push (ralt) }

    // and add in all the SMK mod-key states
    mod_smk_pairs(ks) .iter() .for_each (|(kmod,smk)| { if smk.down.check() { mks.push (*kmod) } });

    // and populate all the mode states (and the caps that they require)
    // (note that these modes are active when caps is down, regardless of any other modkey state!)
    if ks.caps.down.check() {
        mks.push (caps);
        mode_flag_pairs(ks) .iter() .for_each (|(ms,mf)| { if mf.check() { modes.push (*ms) } });
    }
    // finally lets add the two other non-mode global flag states too
    if ks.in_right_btn_scroll_state.check()  { modes.push (rght_ms_scrl) }
    if ks.in_managed_ctrl_down_state.check() { modes.push (mngd_ctrl_dn) }

    Combo::new(key, &mks as &[ModKey], &modes as &[ModeState])
}



fn fan_lrmk (k_mods:Vec<MK>, lrmk:&MK, lmk:&MK, rmk:&MK) -> Vec<Vec<MK>> {
    // if a mod vec had this l/r mod (e.g. Alt), we'll instead gen three mod vecs having (LAlt, RAlt, LAlt && RAlt)
    let mut mvs: Vec<Vec<MK>> = Vec::new();
    if k_mods.contains(lrmk) {
        let mut vlr = k_mods.iter() .filter (|m| *m != lrmk && *m != lmk && *m != rmk) .map (|m| *m) .collect::<Vec<MK>>();
        let (mut vl, mut vr) = (vlr.clone(), vlr.clone());
        vl.push(*lmk); vr.push(*rmk); vlr.push(*lmk); vlr.push(*rmk);
        mvs.push(vl); mvs.push(vr); mvs.push(vlr);
    } else {
        mvs.push(k_mods)
    }
    mvs
}
fn fan_lr (k_mods:Vec<MK>) -> Vec<Vec<MK>> {
    // we'll expand out this mod-vec into vec-of-vec with all L/R optional mods fanned out into piles of L/R/L+R mod-vecs
    let mut mvs : Vec<Vec<MK>> = Vec::new();
    mvs.push(k_mods);      // prepare seed vec-of-vec with initial mods-vec
    LR_MODS .iter() .for_each ( |(lrmk, lmk, rmk)| {
        // expand repeatedly for each lrmk, consuming the list and replacing with expanded version (w/o cloning)
        mvs = mvs .drain(..) .flat_map (|mv| fan_lrmk(mv, lrmk, lmk, rmk)) .collect();
    } );
    mvs
}


pub fn cg_gen_combos (k_mods:&Vec<MK>, modes:&Vec<ModeState>, key:Key) -> Vec<Combo> {
    let is_qk = modes .iter() .any (|m| CAPS_MODES.contains(m));
    let mut kms = k_mods.clone();
    if is_qk && !kms.contains(&caps) { kms.push(caps); }
    // ^^ we'll auto add caps to any caps based modes (for easier setup)
    fan_lr(kms) .iter() .map(|mv| {Combo::new (key, &mv[..], modes)}) .collect::<Vec<Combo>>()
}

pub fn cg_gen_af (k_mods:&Vec<MK>, af:AF, ks:&KrS) -> AF {
    // note that this will only wrap actions using L-mod-keys .. hence there's still utility in wrapping consuming AF after this
    let mc = |mk:MK| k_mods.contains(&mk) ;
    let mut af = af;
    let lr_zip = LR_MODS.iter() .zip(lrmk_smks(ks).into_iter()) .collect::<Vec<(&(MK,MK,MK),&SMK)>>();
    lr_zip .iter() .for_each ( |((lrmk,lmk,rmk),smk)| {
        af = if mc(*lrmk) || mc(*lmk) || mc(*rmk) {
            smk.active_action(af.clone())
        } else { smk.inactive_action(af.clone()) }
    });
    af
}

pub fn gen_consuming_af (k_mods:&Vec<MK>, af:AF, ks:&KrS) -> AF {
    let mc = |mk:MK| k_mods.contains(&mk) ;
    let mut af = af;
    mod_smk_pairs(ks) .iter() .for_each ( |(kmod,smk)| {
        if mc(*kmod) { af = smk.keydn_consuming_action(af.clone()) }
    });
    af
}



/// use this fn to register combos that will auto wrap activation/inactivation AFs around any mods in the combo
pub fn add_combo (k:&Krusty, cgc:&CG_K, cga:&CG_K) {
    // note that although the active/inactive will also mark mk keydn consumed, we only do that for L-keys, hence wrapping consuming AF too
    let af = gen_consuming_af (&cgc.k_mods, cga.gen_af(), &k.ks);
    for c in cgc.gen_combos() {
        //println! ("{:?}, {:?}, {:?}, {:?} -> {:?} {:?}", c.state, cgc.key, cgc.mods, cgc.modes, cga.key, cga.mods );
        k .combos_map .write().unwrap() .insert (c, af.clone());
} }

/// note that this will wrap the AF in with mod-actions for all mods either active (if specified) or inactive (if not)
pub fn add_af_combo (k:&Krusty, cgc:&CG_K, cgaf:&CG_AF) {
    let af = gen_consuming_af (&cgc.k_mods, cgaf.gen_af(), &k.ks);
    for c in cgc.gen_combos() {
        //println! ("{:?}, {:?}, {:?} {:?} -> {:?}", c.state, cgc.key, cgc.mods, cgc.modes, cgaf.mods );
        k .combos_map .write().unwrap() .insert (c, af.clone());
} }

/// use this consuming bare fn to add combos if the AF supplied shouldnt be wrapped for active/inactive by mod, but just marked for consumption
pub fn add_cnsm_bare_af_combo(k:&Krusty, cgc:&CG_K, af:AF) {
    let af = gen_consuming_af (&cgc.k_mods, af, &k.ks);
    for c in cgc.gen_combos () {
        k .combos_map .write().unwrap() .insert (c, af.clone());
} }

/// use this bare fn to add combos if the AF supplied needs special care to avoid wrapping with mod active/inactive .. e.g ctrl/tab etc
pub fn add_bare_af_combo(k:&Krusty, cgc:&CG_K, af:AF) {
    for c in cgc.gen_combos () { k .combos_map .write().unwrap() .insert (c, af.clone()); }
}






/// note: registering a mode key auto sets their caps-only action to nothing, others combos can be set as usual
pub fn register_mode_key (k: &Krusty, key:Key, ms:ModeState) {

    // registering is simply putting an entry into mod-keys-map .. (we could make multiple keys trigger same mode too!)
    k .mode_keys_map .write().unwrap() .insert(key, ms);

    //add_bare_af_combo (k, k.cg(key).m(caps).s(ms), no_action());
    // ^^ not adequate as we'll have multi-mode and multi-modkey combos .. better to just disable it in runtime fallbacks

    // however, we can add in some replacement actions here .. basically ralt w/ those can give the expected w/caps actions
    add_combo (&k, &k.cg(key).s(ms).m(ralt),           &k.cg(key).m(lctrl));
    add_combo (&k, &k.cg(key).s(ms).m(ralt).m(lalt),   &k.cg(key).m(lctrl).m(lalt));
    add_combo (&k, &k.cg(key).s(ms).m(ralt).m(lshift), &k.cg(key).m(lctrl).m(lshift));
    add_combo (&k, &k.cg(key).s(ms).m(ralt).m(lwin),   &k.cg(key).m(lctrl).m(lwin));

}

/// if the key is registered as mode keys, this will generate  (is_mode_key, key-dn, key-up) mode-flag setting actions
fn gen_mode_key_actions (k:&Krusty, key:Key) -> (bool, AF,AF) {
    if let Some(ms) = k.mode_keys_map.read().unwrap().get(&key) {
        if let Some((_, mf)) = mode_flag_pairs(&k.ks) .iter() .find (|(msr,_)| *msr == *ms) {
            let (is_l2, is_qks) = (L2_MODES.contains(ms), QKS_MODES.contains(ms));
            // on key-down, we'll want to set its mode flag, but also collective flags if applicable so runtime checks are reduced
            let (mfc, ks) = ((*mf).clone(), k.ks.clone());
            let keydn_action = Arc::new (move || {
                mfc.set();
                if is_l2  { ks.some_l2_mode_active.set() }
                if is_qks { ks.some_qks_mode_active.set() }
                ks.some_caps_mode_active.set();
            });
            // and on key-up, we'll want to clear its flag, and any collective flags if applicable too
            let (mfc, ks) = ((*mf).clone(), k.ks.clone());
            let keyup_action = Arc::new (move || {
                mfc.clear();
                if is_qks && qks_mode_flag_pairs(&ks).iter().find(|(_,mf)| mf.check()).is_none() { ks.some_qks_mode_active.clear() }
                if is_l2  &&  l2_mode_flag_pairs(&ks).iter().find(|(_,mf)| mf.check()).is_none() { ks.some_l2_mode_active .clear() }
                if !ks.some_qks_mode_active.check() && !ks.some_l2_mode_active.check() { ks.some_caps_mode_active.clear() }
            });
            return (true, keydn_action, keyup_action)
    } }
    (false, no_action(), no_action()) // empty (dn,up) mode-flag-actions for non-mode keys
}

/// for mode-key btns we'll want additional individual binding callbacks that update flags
/// note that after these binding callbacks process, they will still go through bulk processing for their default/combo actions
/// (this is as opposed to default-keys/combos that are handled in bulk w/o individual callback bindings)
pub fn bind_mode_key_actions (k:&Krusty) {
    use crate::KbdEventCbMapKeyType::*;
    k .mode_keys_map .read().unwrap() .iter() .for_each (|(key,_)| {
        let (is_cmk, cma_dn, cma_up) = gen_mode_key_actions (k, *key);
        if is_cmk {
            key.bind ( KeyDownCallback, KbdEventCallbackEntry {
                event_prop_directive: EventProp_Continue,
                combo_proc_directive: ComboProc_Enable,
                cb : KbdEvCbFn_InlineCallback {
                    cb : Arc::new ( move |_| { cma_dn(); EventProp_Continue } )
                },
            } );
            key.bind ( KeyUpCallback, KbdEventCallbackEntry {
                event_prop_directive: EventProp_Continue,
                combo_proc_directive: ComboProc_Enable,
                cb : KbdEvCbFn_InlineCallback {
                    cb : Arc::new ( move |_| { cma_up(); EventProp_Continue } )
                },
            } );
        }
    } );
}





type BFK = Box <dyn Fn(Key)>;
fn wrapped_bfn (wk:Key, bfk:BFK) -> BFK {
    Box::new ( move |key:Key| { wk.press(); bfk(key); wk.release(); } )
}
fn caps_fallback_action_fn (key:Key, ks:&KrS) {
    // if no combo found while caps down, we want to support most multi-mod combos treating caps as ctrl..
    // however, we have caps-dn suppress all mod-keys, so we'll have to wrap mod-key up/dn here as necessary
    let mut bfk: BFK = Box::new(|key:Key| press_release(key));
    // ^^ stable rust doesnt let me use existential/dyn stuff in vars to wrap progressively, so use boxes at some minimal cost
    if ks.caps.down.check() || ks.some_ctrl_down()  { bfk = wrapped_bfn (Key::LCtrl, bfk) }
    if ks.ralt.down.check() || ks.some_shift_down() { bfk = wrapped_bfn (Key::LShift, bfk) }
    if ks.lalt.down.check() { bfk = wrapped_bfn (Key::LAlt, bfk) }
    if ks.some_win_down()   { bfk = wrapped_bfn (Key::LWin, bfk) }
    bfk(key);
}


pub fn combo_maps_handle_kbd_event (key:Key, ks:&KrS, cm:&Arc<RwLock<HashMap<Combo,AF>>> ) {
    // note that we assume by the time we're here, callbacks for modifier-keys and mode-keys have already been called (and so flags updated)
    // note also, that from binding setup, we shouldnt get modifier keys or caps sent here for processing
    if let Some(cmaf) = cm.read().unwrap() .get(&gen_cur_combo(key,&ks)) {
        cmaf()  // found a combo matched action .. we're done!
    } else if ks.caps.down.check() {
        if ks.some_caps_mode_active.check() {
            // when caps down, we'll suppress caps-mode-trigger keys (incl in combo w other modkeys)
            // we'll also suppress all unregistered caps-mode combos
        } else {
            // .. but for other caps combos, there are extensive fallback setups
            caps_fallback_action_fn (key,ks);
        }
    } else if ks.ralt.down.check() {
        // unmapped ralt is set to shift (other mods pass through)
        shift_press_release(key)
    } else {
        // all else works naturally via passthrough
        press_release(key)
    }
}

pub fn gen_combo_maps_processor (k:&Krusty) -> KbdEventCallbackEntry {
    use crate::{KbdEventType::*, EventPropagationDirective::*};
    // we'll assume that by the time we're here, callbacks for modifier-keys and mode-keys have already updated their flags
    // and for all keys whitelisted for combo-maps style handling, we do complete block on both keydown/keyup and gen all events ourselves!
    let mut handled_keys = HashSet::new();
    k .combos_map        .read().unwrap() .iter() .for_each ( |(c,_)|   { handled_keys.insert(c.key); } );
    k .mode_keys_map     .read().unwrap() .iter() .for_each ( |(key,_)| { handled_keys.insert(*key); } );
    k .default_bind_keys .read().unwrap() .iter() .for_each ( |key|     { handled_keys.insert(*key); } );

    let (ks, cm) = (k.ks.clone(), k.combos_map.clone());
    let cb = Arc::new ( move |e:KbdEvent| {
        if handled_keys.contains(&e.key) {
            if e.event == KbdEvent_KeyDown || e.event == KbdEvent_SysKeyDown {
                let (ks, cm) = (ks.clone(), cm.clone());
                spawn (move || combo_maps_handle_kbd_event(e.key, &ks, &cm));
                return EventProp_Stop
            } else if e.event == KbdEvent_KeyUp || e.event == KbdEvent_SysKeyUp {
                // nothing to do on keyup, as anything we want to send, we send in keydn/keyup pairs at keydown itself
                return EventProp_Stop
            } // all else fall back down to pass-through
        }
        return EventProp_Continue
    });
    KbdEventCallbackEntry {
        event_prop_directive: EventProp_Undetermined,
        combo_proc_directive: ComboProc_Enable,  // (doesnt actually matter, as this is in combo proc itself!)
        cb: KbdEvCbFn_InlineCallback {cb}
    }
}



