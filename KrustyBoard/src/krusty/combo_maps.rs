#![ allow (non_camel_case_types) ]

use std::collections::{HashMap, HashSet};
use std::mem::size_of;
use std::ops::Deref;
use std::sync::{Arc, RwLock};
use std::thread;

use once_cell::sync::OnceCell;


use crate::{*, key_utils::*, ModeState_T::*, ModKey::*};



pub type ComboStatesBits_ModKeys = [bool; 9];
pub type ComboStatesBits_Modes   = [bool; 8];
pub type ComboStatesBits_Flags   = [bool; 2];
// ^^ 9 mod-keys (caps,l/r-(alt,ctrl,win,shift)), 4+4=8 modes (sel/del/word/fast, qks/qks1/qks2/qks3), 2 flags (rght-ms-scrl/mngd-ctrl-dn)
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





// CG_K and CG_AF comprise the Combo-Generation fluent api structs, with Key or with supplied AF
// .. once populated it can be used for generation of either a combo or a (possibly guarded) action

# [ allow (non_camel_case_types) ]
# [ derive (Clone) ]
/// Combo-Generator for a specified Key Combo (as opposed to a combo that triggers non-key action etc)
pub struct ComboGen_wKey<'a> {
    ks    : &'a KrustyState,
    key   : Key,
    mks   : Vec<ModKey>,
    modes : Vec<ModeState_T>,
}


# [ allow (non_camel_case_types) ]
# [ derive (Clone) ]
/// Combo-Generator for a a combo targeting a non-key action (e.g. moving windows etc)
pub struct ComboGen_wAF<'a> {
    ks  : &'a KrustyState,
    mks : Vec<ModKey>,
    af  : AF,
}





# [ derive () ]
/// holds the actual combo-map, and impls functionality on adding combos and matching/handling runtime combos
pub struct _CombosMap {
    _private : (),
    cm      : Arc <RwLock <HashMap <Combo, AF>>>,
    l2ks    : Arc <RwLock <HashMap <Key, Key>>>,
    cm_proc : Arc <RwLock <Option <KbdEvCbFn_ComboProc_T>>>,
    // note that ^^ none of these need to be public as we should only need to access these from within the module
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
    pub fn static_flags_modes () -> [ModeState_T;2] {
        // note that this will be the source of ordering for the flags-state bits in our combo flags-bitmap field
        static FLAGS_MODES : [ModeState_T;2] = [mngd_ctrl_dn, rght_ms_scrl];
        FLAGS_MODES
    }
    fn get_cur_flags_states_bitmap (ks:&KrustyState) -> ComboStatesBits_Flags {
        // note that the order of these must match the order given by the static_flag_modes fn above
        [ks.in_managed_ctrl_down_state.check(), ks.in_right_btn_scroll_state.check()]
    }
    fn make_combo_flags_states_bitmap (modes:&[ModeState_T]) -> ComboStatesBits_Flags {
        Combo::static_flags_modes() .map (|ms| modes.contains(&ms))
    }


    /// generate the combo bit-map for the current runtime state (incl the active key and ks state flags)
    pub fn gen_cur_combo (key:Key, ks:&KrustyState) -> Combo {
        // note: this is in runtime hot-path .. (unlike the one above which is used while populating the combo-table)
        let mode_state  = {
            if !ks.mod_keys.caps.down.check() { [false; size_of::<ComboStatesBits_Modes>()] }
            else { ks.mode_states.get_cur_mode_states_bitmap() }
        };
        let mk_state    = ks.mod_keys.get_cur_mod_keys_states_bitmap();
        let flags_state = Combo::get_cur_flags_states_bitmap(ks);
        // NOTE that we're not including mouse btn down keys in states bitmap
        // .. this way, we can use them freely with other states .. else we'd need whole set of combos to support when mouse btn down
        // .. (which might not be a bad idea if we decide we do want to restrict down to specific combos to enable when mouse btns held!)
        // todo: circle back here

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

    pub fn gen_combos (mks:&Vec<ModKey>, modes:&Vec<ModeState_T>, key:Key) -> Vec<Combo> {
        // we'll auto add caps to any caps based modes (for easier setup)
        let mut mks = mks.clone();
        let is_qk = modes .iter() .any (|m| ModeStates::static_caps_modes().contains(m));
        if is_qk && !mks.contains(&caps) { mks.push(caps); }
        // and expand the combos for any L/R agnostic mod-keys specified
        Combo::fan_lr(mks) .iter() .map(|mv| {Combo::new (key, mv, modes)}) .collect::<Vec<Combo>>()
    }

    pub fn gen_af (mks:&Vec<ModKey>, af:AF, ks:&KrustyState) -> AF {
        // note that this will only wrap actions using L-mod-keys .. hence there's still utility in wrapping consuming AF after this
        let mc = |mk:ModKey| mks.contains(&mk) ;
        let mut af = af;
        let lr_zip = ModKeys::static_lr_mods_triplets() .into_iter() .zip(ks.mod_keys.lrmk_smks().into_iter()) .collect::<Vec<((ModKey, ModKey, ModKey), &SyncdModKey)>>();
        lr_zip .iter() .for_each ( |((lrmk,lmk,rmk),smk)| {
            af = if mc(*lrmk) || mc(*lmk) || mc(*rmk) {
                smk.active_action(af.clone())
            } else { smk.inactive_action(af.clone()) }
        });
        af
    }

}





/// Combo-Generator for a specified Key Combo (as opposed to a combo that triggers non-key action etc)
impl<'a> ComboGen_wKey<'a> {
    pub fn new (key:Key, ks:&KrustyState) -> ComboGen_wKey {
        ComboGen_wKey { ks, key, mks:Vec::new(), modes:Vec::new() }
    }
    pub fn m (&'a mut self, mk:ModKey) -> &'a mut ComboGen_wKey {
        self.mks.push(mk); self
    }
    pub fn s (&'a mut self, md: ModeState_T) -> &'a mut ComboGen_wKey {
        self.modes.push(md); self
    }
    pub fn gen_combos (&self) -> Vec<Combo> {
        Combo::gen_combos (&self.mks, &self.modes, self.key)
    }
    fn gen_mod_keydn_consuming_af (&self, af:AF, ks:&KrustyState) -> AF {
        let mut afc = af;
        ks.mod_keys.mod_smk_pairs() .iter() .for_each ( |(mk, smk)| {
            if self.mks.contains(mk) { afc = smk.keydn_consuming_action (afc.clone()) }
        });
        afc
    }
    pub fn gen_mode_keydn_consuming_af (&self, af:AF, ks:&KrustyState) -> AF {
        let mut afc = af;
        ks.mode_states.mode_flag_pairs() .iter() .for_each ( |(ms_t, ms)| {
            if self.modes.contains(ms_t) { afc = ms.mode_key_consuming_action (afc.clone()); }
        } );
        afc
    }
    pub fn gen_af (&self) -> AF {
        Combo::gen_af (&self.mks, base_action(self.key), self.ks)
    }
}


/// Combo-Generator for a a combo targeting a non-key action (e.g. moving windows etc)
impl<'a> ComboGen_wAF<'a> {
    pub fn new  (af:AF, ks:&KrustyState) -> ComboGen_wAF {
        ComboGen_wAF { ks, mks:Vec::new(), af }
    }
    pub fn m (&'a mut self, mk:ModKey) -> &'a mut ComboGen_wAF {
        self.mks.push(mk); self
    }
    pub fn gen_af (&self) -> AF {
        Combo::gen_af (&self.mks, self.af.clone(), self.ks)
    }
}





/// holds the actual combo-map, and impls functionality on adding combos and matching/handling runtime combos
impl CombosMap {

    pub fn instance () -> CombosMap {
        static INSTANCE: OnceCell<CombosMap> = OnceCell::new();
        INSTANCE .get_or_init (||
            CombosMap ( Arc::new ( _CombosMap {
                _private : (),
                cm       : Arc::new ( RwLock::new ( HashMap::new() ) ),
                l2ks     : Arc::new ( RwLock::new ( HashMap::new() ) ),
                cm_proc  : Arc::new ( RwLock::new ( None) ),
            } ) )
        ) .clone()
    }

    /// returns true if the combos map processor is enabled (meaning the processor has been generated from the combos map)
    pub fn is_enabled (&self) -> bool {
        self.cm_proc .read().unwrap() .is_some()
    }
    /// supplies an (Arc) copy of the generated and cached combos map based (and fallback defaults) events processor
    pub fn get_processor (&self) -> Option<KbdEvCbFn_ComboProc_T> {
        self.cm_proc .read().unwrap() .as_ref() .map(|cb| cb.clone())
    }
    /// registers a key for layer-2 functionality, which is used during fallback to layer any pressed mod-keys onto the l2-key
    pub fn register_l2_key (&self, key:Key, l2k:Key) {
        self.l2ks .write().unwrap() .insert (key, l2k);
    }


    /// use this fn to register combos that will auto wrap activation/inactivation AFs around any mods in the combo
    pub fn add_combo (&self, ksr:&KrustyState, cgc:&ComboGen_wKey, cga:&ComboGen_wKey) {
        // note that although the active/inactive will also mark mk keydn consumed, we only do that for L-keys, hence wrapping consuming AF too
        let af = cgc.gen_mod_keydn_consuming_af ( cgc.gen_mode_keydn_consuming_af (cga.gen_af(), ksr), ksr);
        for c in cgc.gen_combos() {
            //println! ("{:?}, {:?}, {:?}, {:?}, {:?} -> {:?} {:?}", c.mk_state, c.mode_state, cgc.key, cgc.mks, cgc.modes, cga.key, cga.mks );
            self.cm .write().unwrap() .insert (c, af.clone());
    } }

    /// note that this will wrap the AF in with mod-actions for all mods either active (if specified) or inactive (if not)
    pub fn add_af_combo (&self, ksr:&KrustyState, cgc:&ComboGen_wKey, cgaf:&ComboGen_wAF) {
        let af = cgc.gen_mod_keydn_consuming_af ( cgc.gen_mode_keydn_consuming_af (cgaf.gen_af(), ksr), ksr);
        for c in cgc.gen_combos() {
            //println! ("{:?}, {:?}, {:?} {:?} -> {:?}", c.state, cgc.key, cgc.mods, cgc.modes, cgaf.mods );
            self.cm .write().unwrap() .insert (c, af.clone());
    } }

    /// use this consuming bare fn to add combos if the AF supplied shouldnt be wrapped for active/inactive by mod, but just marked for consumption
    pub fn add_cnsm_bare_af_combo (&self, ksr:&KrustyState, cgc:&ComboGen_wKey, af:AF) {
        let af = cgc.gen_mod_keydn_consuming_af ( cgc.gen_mode_keydn_consuming_af (af, ksr), ksr);
        for c in cgc.gen_combos () {
            self.cm .write().unwrap() .insert (c, af.clone());
    } }

    /// use this bare fn to add combos if the AF supplied needs special care to avoid wrapping with mod active/inactive .. e.g ctrl/tab etc
    pub fn add_bare_af_combo (&self, ksr:&KrustyState, cgc:&ComboGen_wKey, af:AF) {
        let af = cgc.gen_mode_keydn_consuming_af (af, ksr);
        for c in cgc.gen_combos () { self.cm .write().unwrap() .insert (c, af.clone()); }
    }


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

        // if its the actual qks-1 trigger key, its always disabled (as qks1-down is when the other l2/mode keys to do their l2= modes)
        if let Some(msk) = ks.mode_states.qks1.key() { if msk==key { return } }

        // if we're in some caps mode-state, but not qks1, we do nothing for fallback (i.e. only registered combos allowed)
        let qks1_active = ks.mode_states.qks1.down.check();
        if ks.mode_states.some_caps_mode_active.check() && !qks1_active { return }

        // else, we do fallback for the key, but if its l2k, the fallback output should be on its l2-key
        let l2k_opt = self.l2ks.read().unwrap().get(&key).copied();
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
    pub fn combo_maps_handle_kbd_event (&self, key:Key, ks:&KrustyState) {
        // note that we assume by the time we're here, callbacks for modifier-keys and mode-keys have already been called (and so flags updated)
        // note also, that from binding setup, we shouldnt get modifier keys or caps sent here for processing
        if let Some(cmaf) = self.cm.read().unwrap() .get(&Combo::gen_cur_combo(key,&ks)) {
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
        self.cm .read().unwrap() .iter() .for_each ( |(c,_)| { handled_keys.insert(c.key); } );
        k .default_bind_keys .read().unwrap() .iter() .for_each ( |key| { handled_keys.insert(*key); } );
        k .ks .mode_states .mode_flag_pairs() .iter() .for_each ( |(_,ms)| ms.key() .iter() .for_each (|key| {handled_keys.insert(*key);}) );

        let (ks, cm) = (k.ks.clone(), self.clone());
        let cb = Arc::new ( move |e:KbdEvent| {
            // if its not in the combo-proc handled-keys whitelist, we should just let it pass through
            if !handled_keys.contains(&e.key) { return EventProp_Continue }
            // all combo proc is only on keydown and we'll spawn then out (when needed, we'll send both keydn/keyup pairs at keydown itself)
            if e.ev_t == KbdEvent_KeyDown || e.ev_t == KbdEvent_SysKeyDown {
                let (ks, cm) = (ks.clone(), cm.clone());
                thread::spawn (move || cm.combo_maps_handle_kbd_event(e.key, &ks));
            }
            // either way, combo-proc-handled keys should be completely blocked past combo-proc (both keydn and keyup)
            return EventProp_Stop
        } );
        *self.cm_proc .write().unwrap() = Some(cb);
    }
    pub fn disable_combos_map_events_processor (&self) {
        *self.cm_proc .write().unwrap() = None;
    }


}
