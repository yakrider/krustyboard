#![ allow (non_camel_case_types) ]

use std::collections::{HashMap, HashSet};
use std::borrow::Borrow;
use std::ops::Deref;
use std::sync::{Arc, RwLock};

use once_cell::sync::OnceCell;


use crate::{*, key_utils::*, ModeState_T::*};



pub type ComboStatesBits_ModKeys = [bool; 9];
pub type ComboStatesBits_Modes   = [bool; 8];
pub type ComboStatesBits_Flags   = [bool; 3];
// ^^ 9 mod-keys (caps,l/r-(alt,ctrl,win,shift)), 4+4=8 modes (sel/del/word/fast, qks/qks1/qks2/qks3), 3 flags (mngd-ctrl-dn/ctrl-tab-scrl/rght-ms-scrl)
// note that l/r unspecified keys (ctrl/alt/shift/win) get mapped out to l/r/lr expansions, so mod-key-bits only need the l/r bits





# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone) ]
/// represents the actual Combo, and impls generation from ComboGens (to store in combo-map) or from active states-flags
pub struct Combo {
    _private:(),
    pub key : Key,
    pub mk_state   : ComboStatesBits_ModKeys,
    pub mode_state : ComboStatesBits_Modes,
    pub flags_state: ComboStatesBits_Flags,
}





// The following generators comprise the Combo and Action Generation fluent api structs, with Key or with supplied AF
// .. once populated they can be used for generation of either a combo or a (possibly guarded) action

# [ allow (non_camel_case_types) ]
# [ derive (Clone) ]
/// Combo-Generator for a specified Key Combo (as opposed to a combo that triggers non-key action etc)
pub struct ComboGen<'a> {
    ks    : &'a KrustyState,
    key   : Key,                // combo-trigger key
    mks   : Vec<ModKey>,        // modifier keys that should be down to trigger this combo
    modes : Vec<ModeState_T>,   // mode-states that should match for this combo to trigger
    // the modifier-key consume flag marks that the release of mod-keys in this combo should be masked
    mod_key_no_consume  : bool,
    // the mode-ken consume flag marks that key-repeats on mode-keys in this combo should be suppressed until they are released
    mode_kdn_no_consume : bool,

}

# [ allow (non_camel_case_types) ]
# [ derive (Clone) ]
/// Combo-Action-Generator for a combo targeting a key-action (e.g. mapping a combo to another key)
pub struct ActionGen_wKey<'a> {
    ks    : &'a KrustyState,
    key   : Key,            // key on which the output combo-action will be built
    mks   : Vec<ModKey>,    // modifier-keys to send out with this combo-action
    // the wrap_mod_key_guard flag will set the generated action for this combo to be wrapped in activation/inactivation guards
    wrap_mod_key_guard : bool,
}

# [ allow (non_camel_case_types) ]
# [ derive (Clone) ]
/// Combo-Action-Generator for a a combo targeting a non-key action (e.g. moving windows etc)
pub struct ActionGen_wAF<'a> {
    ks  : &'a KrustyState,
    mks : Vec<ModKey>,      // modifier-keys to wrap the specified action-function for this combo-action
    af  : AF,               // the action-function to build this combo-output-action with
    // the wrap_mod_key_guard flag will set the generated action for this combo to be wrapped in activation/inactivation guards
    wrap_mod_key_guard : bool,
}





# [ derive () ]
/// holds the actual combo-map, and impls functionality on adding combos and matching/handling runtime combos
pub struct _CombosMap {
    _private : (),
    // the combos map itself, mapping key+mod-key+mode-state combos to actions
    combos_map : Arc <RwLock <HashMap <Combo, AF>>>,
    // we'll maintain a (redundant) mapping of l2-keys for quick checks during fallback processing
    l2_keys_map : Arc <RwLock <HashMap <Key, Key>>>,
    // and we'll hold a registry for keys that only need default/fallback bindings
    default_bind_keys : Arc <RwLock <HashSet <Key>>>,
}
# [ derive (Clone) ]
pub struct CombosMap ( Arc <_CombosMap> );

impl Deref for CombosMap {
    type Target = _CombosMap;
    fn deref (&self) -> &Self::Target { &self.0 }
}







/// represents the actual Combo, and impls generation from ComboGens (to store in combo-map) or from active states-flags
impl Combo {

    pub fn new (key:Key, mod_keys:&[ModKey], modes:&[ModeState_T]) -> Combo {
        let mk_state    = ModKeys::make_combo_mod_keys_states_bitmap(mod_keys);
        let mode_state  = ModeStates::make_combo_mode_states_bitmap(modes);
        let flags_state = Combo::make_combo_flags_states_bitmap(modes);
        Combo { _private:(), key, mk_state, mode_state, flags_state }
    }


    // while the mod-keys and mode-states are handled by their own objects, we'll handle combo bits gen for flag states ourselves
    pub fn static_flags_modes () -> [ModeState_T;3] {
        // note that this will be the source of ordering for the flags-state bits in our combo flags-bitmap field
        static FLAGS_MODES : [ModeState_T;3] = [mngd_ctrl_dn, ctrl_tab_scrl, rght_ms_scrl];
        FLAGS_MODES
    }
    fn get_cur_flags_states_bitmap (ks:&KrustyState) -> ComboStatesBits_Flags {
        // note that the order of these must match the order given by the static_flag_modes fn above
        [ks.in_managed_ctrl_down_state.is_set(), ks.in_ctrl_tab_scroll_state.is_set(), ks.in_right_btn_scroll_state.is_set()]
    }
    fn make_combo_flags_states_bitmap (modes:&[ModeState_T]) -> ComboStatesBits_Flags {
        Combo::static_flags_modes() .map (|ms| modes.contains(&ms))
    }


    /// generate the combo bit-map for the current runtime state (incl the active key and ks state flags)
    pub fn gen_cur_combo (key:Key, ks:&KrustyState) -> Combo {
        // note: this is in runtime hot-path .. (unlike the make_combo_*_states_bitmap fns used while building combos-table)
        let mode_state  = ks.mode_states.get_cur_mode_states_bitmap();
        let mk_state    = ks.mod_keys.get_cur_mod_keys_states_bitmap();
        let flags_state = Combo::get_cur_flags_states_bitmap(ks);
        // NOTE that we're not including mouse btn down keys in states bitmap
        // .. this way, we can use them freely with other states .. else we'd need whole set of combos to support when mouse btn down
        // .. (which might not be a bad idea if we decide we do want to restrict down to specific combos to enable when mouse btns held!)

        //println! ("{:?}", Combo { _private:(), key, mk_state, mode_state, flags_state });
        Combo { _private:(), key, mk_state, mode_state, flags_state }
    }


    fn fan_lr (mks:Vec<ModKey>) -> Vec<Vec<ModKey>> {
        // we'll expand out this mod-vec into vec-of-vec with all L/R optional mods fanned out into vec-of-vecs with L, R, or L+R versions
        let mut mvs : Vec<Vec<ModKey>> = Vec::new();
        mvs.push(mks);      // prepare seed vec-of-vec with initial mods-vec
        ModKeys::static_lr_mods_triplets() .iter() .for_each ( |(lrmk, lmk, rmk)| {
            // expand repeatedly for each lrmk, consuming the list and replacing with expanded version (w/o cloning)
            let mut mvs_exp: Vec<Vec<ModKey>> = Vec::new();
            mvs .drain(..) .for_each ( |mv| {
                if mv.contains(lrmk) {
                    // if a vec had this l/r mod (e.g. alt), we'll instead gen three mod vecs having (lalt, ralt, lalt && ralt)
                    let mut vlr = mv.iter() .filter (|m| *m != lrmk && *m != lmk && *m != rmk) .map (|m| *m) .collect::<Vec<ModKey>>();
                    let (mut vl, mut vr) = (vlr.clone(), vlr.clone());
                    vl.push(*lmk); mvs_exp.push(vl);
                    vr.push(*rmk); mvs_exp.push(vr);
                    vlr.push(*lmk); vlr.push(*rmk); mvs_exp.push(vlr);
                } else {
                    mvs_exp.push(mv)
                }
            } );
            // swap in this expanded vec-of-vec for the next loop iteration (w the next lrmk)
            mvs = mvs_exp;
        } );
        mvs
    }

    fn gen_combos (mks:&Vec<ModKey>, modes:&Vec<ModeState_T>, key:Key, ks:&KrustyState) -> Vec<Combo> {
        // we'll auto add any mode-key's state to its own combos
        // (note however, that since this is way downstream, these additions wont have consumption guards ..)
        // (.. which actually is what we want, as that lets us avoid consumption for mode-trigger-keys by not explicitly incl their modes)
        let mut modes = modes.clone();
        if let Some(ms_t) = ks.mode_states.get_mode_t(key) {
            if !modes.contains(&ms_t) { modes.push(ms_t) }
        }
        // we want to expand the combos for any L/R agnostic mod-keys specified
        Combo::fan_lr(mks.clone()) .iter() .map(|mv| {Combo::new (key, mv, modes.borrow())}) .collect::<Vec<Combo>>()
    }

    fn gen_af (mks:&Vec<ModKey>, af:AF, wrap_mod_key_guard:bool, ks:&KrustyState) -> AF {
        // note that this will only wrap actions using L-mod-keys .. hence there's still utility in wrapping consuming AF after this
        let mut af = af;
        let lr_zip = ModKeys::static_lr_mods_triplets() .into_iter() .zip(ks.mod_keys.lrmk_smks().into_iter()) .collect::<Vec<((ModKey, ModKey, ModKey), &SyncdModKey)>>();
        lr_zip .iter() .for_each ( |((lrmk,lmk,rmk),smk)| {
            af = if mks.contains(lrmk) || mks.contains(lmk) || mks.contains(rmk) {
                if wrap_mod_key_guard { smk.active_action(af.clone()) } else { smk.bare_action(af.clone()) }
            } else {
                if wrap_mod_key_guard { smk.inactive_action(af.clone()) } else { af.clone() }
            }
        });
        af
    }

}





/// Combo-Generator for a specified Key Combo (as opposed to a combo that triggers non-key action etc)
impl<'a> ComboGen<'a> {
    pub fn new (key:Key, ks:&KrustyState) -> ComboGen {
        ComboGen { ks, key, mks:Vec::new(), modes:Vec::new(), mod_key_no_consume:false, mode_kdn_no_consume:false }
    }
    /// Add a modifier key to the combo
    pub fn m (mut self, mk:ModKey) -> ComboGen<'a> {
        self.mks.push(mk); self
    }
    /// Add a mode-state to the combo
    pub fn s (mut self, md: ModeState_T) -> ComboGen<'a> {
        self.modes.push(md); self
    }
    /// Disable consuming mod-key key-downs for this combo. <br>
    /// (The default is to consume (i.e. do masking when releasing modkey) any modkey kdn on registered combos)
    pub fn mk_nc (mut self) -> ComboGen<'a> {
        self.mod_key_no_consume = true; self
    }
    /// Disable consuming mode-trigger-key key-downs for this combo. <br>
    /// (The default is to consume (i.e. disable further key-events until released) any mode-trigger-key kdn on registered combos)
    pub fn msk_nc (mut self) -> ComboGen<'a> {
        self.mode_kdn_no_consume = true; self
    }

    fn kdn_consume_wrap (&self, af:AF) -> AF {
        let mut afc = af;
        if !self.mod_key_no_consume {
            self.ks.mod_keys.mod_smk_pairs() .iter() .for_each ( |(mk, smk)| {
                if self.mks.contains(mk) { afc = smk.keydn_consuming_action (afc.clone()) }
            });
        }
        if !self.mode_kdn_no_consume {
            self.ks.mode_states.mode_flag_pairs() .iter() .for_each ( |(ms_t, ms)| {
                if self.modes.contains(ms_t) { afc = ms.mode_key_consuming_action (afc.clone()); }
            } );
        }
        afc
    }
    /// Generate one or more combos from this ComboGen w/ key-dwn consuming behavior as specified during construction
    pub fn gen_combos (&self) -> Vec<Combo> {
        Combo::gen_combos (&self.mks, &self.modes, self.key, self.ks)
    }
}


/// Combo-Generator for a specified Key Combo (as opposed to a combo that triggers non-key action etc)
impl<'a> ActionGen_wKey<'a> {
    pub fn new (key:Key, ks:&KrustyState) -> ActionGen_wKey {
        ActionGen_wKey { ks, key, mks:Vec::new(), wrap_mod_key_guard:true }
    }
    /// Add a modifier key to the combo
    pub fn m (mut self, mk:ModKey) -> ActionGen_wKey<'a> {
        self.mks.push(mk); self
    }
    /// Disable wrapping with mod-key guard actions. <br>
    /// (The default for ActionGen_wKey is enabled)
    pub fn mkg_nw (mut self) -> ActionGen_wKey<'a> {
        self.wrap_mod_key_guard = false; self
    }
    /// Generate the action for this ActionGen, w mod-key guard wrapping as specified during construction
    pub fn gen_af (&self) -> AF {
        Combo::gen_af (&self.mks, base_action(self.key), self.wrap_mod_key_guard, self.ks)
    }
}


/// Combo-Generator for a a combo targeting a non-key action (e.g. moving windows etc)
impl<'a> ActionGen_wAF<'a> {
    pub fn new  (af:AF, ks:&KrustyState) -> ActionGen_wAF {
        ActionGen_wAF { ks, mks:Vec::new(), af, wrap_mod_key_guard:false }
    }
    pub fn m (mut self, mk:ModKey) -> ActionGen_wAF<'a> {
        self.mks.push(mk); self
    }
    /// Enable wrapping with mod-key guard actions <br>
    /// (The default for ActionGen_wAF is disabled)
    pub fn mkg_w (mut self) -> ActionGen_wAF<'a> {
        self.wrap_mod_key_guard = true; self
    }
    /// Generate the action for this ActionGen, w mod-key guard wrapping as specified during construction
    pub fn gen_af (&self) -> AF {
        Combo::gen_af (&self.mks, self.af.clone(), self.wrap_mod_key_guard, self.ks)
    }
}







/// holds the actual combo-map, and impls functionality on adding combos and matching/handling runtime combos
impl CombosMap {

    pub fn instance () -> CombosMap {
        static INSTANCE: OnceCell<CombosMap> = OnceCell::new();
        INSTANCE .get_or_init (||
            CombosMap ( Arc::new ( _CombosMap {
                _private : (),
                combos_map  : Arc::new ( RwLock::new ( HashMap::new() ) ),
                l2_keys_map : Arc::new ( RwLock::new ( HashMap::new() ) ),
                default_bind_keys : Arc::new ( RwLock::new ( HashSet::new() ) ),
            } ) )
        ) .clone()
    }

    /// registers a key for layer-2 functionality, which is used during fallback to layer any pressed mod-keys onto the l2-key
    pub fn register_l2_key (&self, key:Key, l2k:Key) {
        self.l2_keys_map.write().unwrap() .insert (key, l2k);
    }
    /// registers a key for default binding (without a specific combo)
    pub fn register_default_binding_key (&self, key:Key) {
        self.default_bind_keys .write().unwrap() .insert (key);
    }


    /// use this fn to register combos that will output other keys/combos
    pub fn add_combo (&self, cg:ComboGen, ag:ActionGen_wKey) {
        let af = cg.kdn_consume_wrap (ag.gen_af());
        for c in cg.gen_combos() {
            //println! ("+C: mks:{:?}, ms:{:?}, k:{:?}, mks:{:?}, ms:{:?} -> k:{:?} mks:{:?}", c.mk_state, c.mode_state, cg.key, cg.mks, cg.modes, ag.key, ag.mks );
            self.combos_map.write().unwrap() .insert (c, af.clone());
    } }

    /// use this fn to register combos that will trigger the supplied action
    pub fn add_combo_af (&self, cg:ComboGen, ag:ActionGen_wAF) {
        let af = cg.kdn_consume_wrap (ag.gen_af());
        for c in cg.gen_combos() {
            //println! ("+C: mks:{:?}, ms:{:?}, k:{:?}, mks:{:?}, ms:{:?} -> AF, mks:{:?}", c.mk_state, c.mode_state, cg.key, cg.mks, cg.modes, ag.mks );
            self.combos_map.write().unwrap() .insert (c, af.clone());
    } }



    fn wrapped_bfn (wk:Key, bfn: Box <dyn Fn()>) -> Box <dyn Fn()> {
        Box::new ( move || { wk.press(); bfn(); wk.release(); } )
    }
    fn handle_caps_combo_fallback (&self, key:Key, ks:&KrustyState) {
        // - if no combo found while caps down, we want to support most multi-mod combos treating caps as ctrl..
        // (however, we have caps-dn suppress all mod-keys, so we'll have to wrap mod-key up/dn here as necessary)
        // - as to l2 keys (for l3 fallback), we want l2-key expected behavior with other mod key combos ..
        // in particular, since caps is used up just to trigger l2, we'll let qks1 do ctrl, (and ralt do shift) for the l2 key combos
        // - and for the mode-keys, we need most caps combos to be silent, so we'll do fallback only when qks1 active, treating that as ctrl
        // (note that for mode-keys, w/o caps down we can ofc do arbitrary mix of natural ctrl/alt/shift/win etc combos)

        // if its the actual qks-1 trigger key, its always disabled (as qks1-down is when we want other l2/mode keys to do their l2-eqv modes)
        if let Some(msk) = ks.mode_states.qks1.key() { if msk==key { return } }

        // if we're in some caps mode-state, but not qks1, we do nothing for fallback (i.e. only registered combos allowed)
        let qks1_active = ks.mode_states.qks1.down.check();
        if ks.mode_states.some_combo_mode_active.check() && !qks1_active { return }

        // else, we do fallback for the key, but if its l2k, the fallback output should be on its l2-key
        let l2k_opt = self.l2_keys_map.read().unwrap().get(&key).copied();
        let fb_key = l2k_opt.unwrap_or(key);

        // and for either l2k or mode-keys, we wont wrap w ctrl just from being in caps fallback here (unless actual ctrl or qks1-down)
        let qks1_ctrl = ks.mode_states.check_if_mode_key(key) || l2k_opt.is_some();

        // note that stable rust doesnt let us use existential/dyn stuff in vars to wrap progressively, so use boxes at some minimal cost
        let mut bfn: Box <dyn Fn()> = Box::new (move || press_release(fb_key));
        if ks.mod_keys.some_ctrl_down() || qks1_active || !qks1_ctrl { bfn = CombosMap::wrapped_bfn (Key::LCtrl, bfn) }
        // ^^ if its l2-key or mode-key (i.e qks1-ctrl), we wont ctrl wrap just from being here (unless there's actual ctrl or qks1-down)
        if ks.mod_keys.some_shift_down() || ks.mod_keys.ralt.down.check() { bfn = CombosMap::wrapped_bfn (Key::LShift, bfn) }
        if ks.mod_keys.lalt.down.check() { bfn = CombosMap::wrapped_bfn (Key::LAlt, bfn) }
        if ks.mod_keys.some_win_down()   { bfn = CombosMap::wrapped_bfn (Key::LWin, bfn) }
        bfn();
    }


    /// combos (and fallback) action handler for current key-event, based on current modes/mod-key states
    pub fn combo_maps_handle_key_down (&self, key:Key, ks:&KrustyState) {
        // note that we assume by the time we're here, callbacks for modifier-keys and mode-keys have already been called (and so flags updated)
        // note also, that from binding setup, we shouldnt get modifier keys or caps sent here for processing
        if let Some(cmaf) = self.combos_map.read().unwrap() .get(&Combo::gen_cur_combo(key, &ks)) {
            cmaf()  // found a combo matched action .. we're done!
        } else if ks.mod_keys.caps.down.check() {
            // unregistered caps-combos have extensive fallback setups
            self.handle_caps_combo_fallback (key, ks);
        } else if ks.mod_keys.ralt.down.check() {
            // unmapped ralt w/o caps is set to shift (other mods pass through)
            shift_press_release(key)
        } else {
            // all else works naturally via passthrough
            press_release(key)
        }
    }


    /// generates the full combo-maps processing AF for use by lower level events processor (which sets the processor enabled)
    pub fn enable_combos_map_events_processor (&self, k:&Krusty) {
        use crate::{EventPropagationDirective::*, KbdEventType::*};
        // we'll assume that by the time we're here, callbacks for modifier-keys and mode-keys have already updated their flags
        // and for all keys whitelisted for combo-maps style handling, we do complete block on both keydown/keyup and gen all events ourselves!
        // note that we want a whitelist instead of covering everything since unknown apps (incl switche) send unknown keys for valid reasons!
        let mut handled_keys = HashSet::new();
        self.combos_map .read().unwrap() .iter() .for_each ( |(c,_)| { handled_keys.insert(c.key); } );
        self.default_bind_keys .read().unwrap() .iter() .for_each ( |key| { handled_keys.insert(*key); } );
        k .ks .mode_states .mode_flag_pairs() .iter() .for_each ( |(_,ms)| ms.key() .iter() .for_each (|key| {handled_keys.insert(*key);}) );

        // lets prep the AF to send into kbd-events-queue
        let (ks, cm) = (k.ks.clone(), self.clone());
        let cm_cb = Arc::new ( move |e:KbdEvent| { cm.combo_maps_handle_key_down (e.key, &ks) } );

        // then we'll build the actual combo-processor AF
        let kbd_af_queue = k.iproc.kbd_af_queue.clone();
        let cb = Arc::new ( move |e:KbdEvent| {
            // if its not in the combo-proc handled-keys whitelist, we should just let it pass through
            if !handled_keys.contains(&e.key) { return EventProp_Continue }
            // we'll also just let injected events pass through (both kdn/kup) .. note that modifier keys deal w injected events separately
            if e.injected { return EventProp_Continue }
            // all combo proc is only on keydown and we'll queue them up (instead of spawning out) so they dont get out of sequence
            if e.ev_t == KbdEvent_KeyDown || e.ev_t == KbdEvent_SysKeyDown {
                let _ = kbd_af_queue.send((cm_cb.clone(), e.clone()));
            }
            // either way, combo-proc-handled keys should be completely blocked past combo-proc (both keydn and keyup)
            return EventProp_Stop
        } );
        k.iproc.set_combo_processor(cb);
    }
    pub fn disable_combos_map_events_processor (&self, k:&Krusty) {
        k.iproc.clear_combo_processor()
    }


}
