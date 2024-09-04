#![ allow (non_camel_case_types) ]

use std::mem::size_of;
use std::sync::Arc;
use std::time::Instant;
use atomic_refcell::AtomicRefCell;

use derive_deref::Deref;
use once_cell::sync::OnceCell;
use rustc_hash::{FxHashMap, FxHashSet};


use crate::{*, key_utils::*};



pub type ComboStatesBits_ModKeys = [bool; 18];
pub type ComboStatesBits_Modes   = [bool; 8];
pub type ComboStatesBits_Latches = [bool; 4];
pub type ComboStatesBits_Flags   = [bool; 0];
// ^^ 9 mod-keys (caps,l/r-(alt,ctrl,win,shift)), x2 adding double-taps,
// 4+4=8 modes ( sel / del / word / fast,  qks / qks1 / qks2 / qks3),
// 0 flags () .. mngd-ctrl-dn, ctrl-tab-scrl, right-ms-scrl no longer included in bitmap
// 4 latched layer combo states
//
// note that l/r unspecified keys (ctrl/alt/shift/win) get mapped out to l/r/lr expansions, so mod-key-bits only need the l/r bits
// note also that rght-ms-scrl and some other flags are not included in combo maps .. just check them at use





# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone) ]
/// represents the actual Combo, and impls generation from ComboGens (to store in combo-map) or from active states-flags
pub struct Combo {
    _private:(),
    pub cmk : EvCbMapKey,
    pub modkey_states : ComboStatesBits_ModKeys,
    pub mode_states   : ComboStatesBits_Modes,
    pub latch_states  : ComboStatesBits_Latches,
    pub flags_states  : ComboStatesBits_Flags,
}





/// represents arc wrapped condition fn to trigger combo .. should return true if combo trigger condition is satisfied
pub type ComboCond = Arc < dyn Fn (&KrustyState, &Event) -> bool + Send + Sync + 'static >;


# [ derive () ]
/// ComboValue is the 'value' part of the combos_map entry that holds the AF, the combo creation time, and the optional trigger condition
pub struct ComboValue {
    _private  : (),
    pub stamp : Instant,
    pub cond  : Option <ComboCond>,
    pub af    : AF,
}

impl ComboValue {
    pub fn new (af:AF) -> ComboValue {
        ComboValue::new_w_cond (af, None)
    }
    pub fn new_w_cond (af:AF, cond:Option<ComboCond>) -> ComboValue {
        ComboValue { _private:(), stamp:Instant::now(), cond, af }
    }
}





# [ derive () ]
/// holds the actual combo-map, and impls functionality on adding combos and matching/handling runtime combos
pub struct _CombosMap {
    // Note that we're using AtomicRefCell instead of Arc-RwLock because we should be donig all the building before
    //   we start running it, so there should never be a write attempted while some other thread is trying to read these
    _private : (),
    // the combos map itself, mapping key+mod-key+mode-state combos to actions
    combos_map : AtomicRefCell <FxHashMap <Combo, Vec<ComboValue>>>,
    // we'll maintain a (redundant) mapping of l2-keys for quick checks during fallback processing
    l2_keys_map : AtomicRefCell <FxHashMap <Key, Key>>,
    // and we'll hold a registry for keys that only need default/fallback bindings
    default_bind_keys : AtomicRefCell <FxHashSet <Key>>,
}
# [ derive (Clone, Deref) ]
pub struct CombosMap ( Arc <_CombosMap> );







// The following generators comprise the Combo and Action Generation fluent api structs, with Key or with supplied AF
// .. once populated they can be used for generation of either a combo or a (possibly guarded) action

# [ derive (Clone) ]
/// Combo-Generator for a specified Key Combo (as opposed to a combo that triggers non-key action etc)
pub struct ComboGen<'a> {
    ks    : &'a KrustyState,
    cmk   : EvCbMapKey,         // combo-trigger key .. the same structure as we would have as key in input bindings map
    mks   : Vec<ModKey>,        // modifier keys that should be down to trigger this combo
    modes : Vec<ModeState_T>,   // mode-states that should match for this combo to trigger
    cond  : Option<ComboCond>,  // optional condition to check before triggering this combo
    // the modifier-key consume flag marks that the release of mod-keys in this combo should be masked
    mod_key_no_consume  : bool,
    // the mode-ken consume flag marks that key-repeats on mode-keys in this combo should be suppressed until they are released
    mode_kdn_no_consume : bool,

}

# [ derive (Clone) ]
/// Combo-Action-Generator for a combo targeting a key-action (e.g. mapping a combo to another key)
pub struct ActionGen_wKey<'a> {
    ks    : &'a KrustyState,
    key   : Key,            // key on which the output combo-action will be built
    mks   : Vec<ModKey>,    // modifier-keys to send out with this combo-action
    // the wrap_mod_key_guard flag will set the generated action for this combo to be wrapped in activation/inactivation guards
    wrap_mod_key_guard : bool,
}

# [ derive (Clone) ]
/// Combo-Action-Generator for a a combo targeting a non-key action (e.g. moving windows etc)
pub struct ActionGen_wAF<'a> {
    ks  : &'a KrustyState,
    mks : Vec<ModKey>,      // modifier-keys to wrap the specified action-function for this combo-action
    af  : AF,               // the action-function to build this combo-output-action with
    // the wrap_mod_key_guard flag will set the generated action for this combo to be wrapped in activation/inactivation guards
    wrap_mod_key_guard : bool,
}






/// represents the actual Combo, and impls generation from ComboGens (to store in combo-map) or from active states-flags
impl Combo {

    pub fn new (cmk:EvCbMapKey, mod_keys:&[ModKey], modes:&[ModeState_T]) -> Combo {
        let modkey_states = ModKeys::make_combo_mod_keys_states_bitmap(mod_keys);
        let mode_states   = ModeStates::make_combo_mode_states_bitmap(modes);
        let latch_states  = ModeStates::make_combo_latch_states_bitmap(modes);
        let flags_states  = Combo::make_combo_flags_states_bitmap(modes);
        Combo { _private:(), cmk, modkey_states, mode_states, latch_states, flags_states }
    }


    // while the mod-keys and mode-states are handled by their own objects, we'll handle combo bits gen for flag states ourselves
    pub fn static_flags_modes () -> [ModeState_T; size_of::<ComboStatesBits_Flags>()] {
        // note that this will be the source of ordering for the flags-state bits in our combo flags-bitmap field
        // NOTE again we want minimal flags in bitmap, as we dont want a flag to change the combo state so other combos w/o flags get invalidated
        static FLAGS_MODES : [ModeState_T; size_of::<ComboStatesBits_Flags>()] = {
            //[mngd_ctrl_dn, ctrl_tab_scrl, rght_ms_scrl];
            //[mngd_ctrl_dn, ctrl_tab_scrl]; // rght_ms_scrl];
            //[mngd_ctrl_dn]; //, ctrl_tab_scrl]; // rght_ms_scrl];
            [] //mngd_ctrl_dn]; //, ctrl_tab_scrl]; // rght_ms_scrl];
        };
        FLAGS_MODES
    }
    fn get_cur_flags_states_bitmap (_:&KrustyState) -> ComboStatesBits_Flags {
        // note that the order of these must match the order given by the static_flag_modes fn above
        // NOTE again we want minimal flags in bitmap, as we dont want a flag to change the combo state so other combos w/o flags get invalidated
        //[ks.in_managed_ctrl_down_state.is_set(), ks.in_ctrl_tab_scroll_state.is_set(), ks.in_right_btn_scroll_state.is_set()]
        //[ks.in_managed_ctrl_down_state.is_set(), ks.in_ctrl_tab_scroll_state.is_set()] //, ks.in_right_btn_scroll_state.is_set()]
        //[ks.in_managed_ctrl_down_state.is_set()] //, ks.in_ctrl_tab_scroll_state.is_set()] //, ks.in_right_btn_scroll_state.is_set()]
        [] //ks.in_managed_ctrl_down_state.is_set()] //, ks.in_ctrl_tab_scroll_state.is_set()] //, ks.in_right_btn_scroll_state.is_set()]
    }
    fn make_combo_flags_states_bitmap (modes:&[ModeState_T]) -> ComboStatesBits_Flags {
        Combo::static_flags_modes() .map (|ms| modes.contains(&ms))
    }


    /// generate the combo bit-map for the current runtime state (incl the active key and ks state flags)
    pub fn gen_cur_combo (cmk:EvCbMapKey, ks:&KrustyState) -> (Combo, Combo) {
        // note: this is in runtime hot-path .. (unlike the make_combo_*_states_bitmap fns used while building combos-table)
        let modkey_states = ks.mod_keys.get_cur_mod_keys_states_bitmap();
        let mode_states   = ks.mode_states.get_cur_mode_states_bitmap();
        let latch_states  = ks.mode_states.get_cur_latch_states_bitmap();
        let flags_states  = Combo::get_cur_flags_states_bitmap(ks);
        // NOTE that we're not including mouse btn down keys in states bitmap
        // .. this way, we can use them freely with other states .. else we'd need whole set of combos to support when mouse btn down
        // .. (which might not be a bad idea if we decide we do want to restrict down to specific combos to enable when mouse btns held!)

        //println! ("{:?}", Combo { _private:(), cmk, mk_state, mode_state, flags_state });
        let combo_w_latch  = Combo { _private:(), cmk, modkey_states, mode_states, latch_states, flags_states };
        let combo_no_latch = Combo { _private:(), cmk, modkey_states, mode_states, latch_states: ComboStatesBits_Latches::default(), flags_states };
        (combo_w_latch, combo_no_latch)
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

    fn gen_combos (mks:&Vec<ModKey>, modes:&Vec<ModeState_T>, cmk:EvCbMapKey, _ks:&KrustyState) -> Vec<Combo> {
        // we'll auto add any mode-key's state to its own combos
        // (note however, that since this is way downstream, these additions wont have consumption guards ..)
        // (.. which actually is what we want, as that lets us avoid consumption for mode-trigger-keys by not explicitly incl their modes)
        //let mut modes = modes.clone();
        //if let Some(ms_t) = ks.mode_states.get_mode_t(key) {
        //    if !modes.contains(&ms_t) { modes.push(ms_t) }
        //}
        // ^^ these were moved at gen-combo creation since we started supporting mbtn/wheel combos

        // we'll also add mod-keys to their double-tap combos too (our dbl-tap combos only fire while the second tap isnt released yet)
        let mut mksc = mks.clone();
        let dt_pairs = ModKeys::static_dbl_tap_mk_pairs();
        dt_pairs .iter() .filter (|(mk,dmk)| mks.contains(dmk) && !mks.contains(mk)) .for_each (|(mk,_dmk)| mksc.push(*mk));

        // we want to expand the combos for any L/R agnostic mod-keys specified
        Combo::fan_lr(mksc) .iter() .map(|mv| {Combo::new (cmk, mv, &modes)}) .collect::<Vec<Combo>>()
    }

    fn gen_af (mks:&Vec<ModKey>, af:AF, wrap_mod_key_guard:bool, ks:&KrustyState, cgo:Option<&ComboGen>) -> AF {
        // note-1: there's inefficiency below (gets by using static lists rather than a map), but it's just for ahead-of-time AF gen
        // note-2: this will only wrap actions using L-mod-keys .. hence there's still utility in wrapping consuming AF after this
        // note-3: this left-mk wrapping would be amiss if we had a left-blocked but right-managed mk pair (which we dont intend to have)
        // note-4: reminder that e.g. we have ralt blocked, and lalt managed .. and its still ok to specify ralt in combo-gen (sending out)
        fn triplet_contains (mks:&Vec<ModKey>, lrmk:&ModKey, lmk:&ModKey, rmk:&ModKey) -> bool {
            mks.contains(lrmk) || mks.contains(lmk) || mks.contains(rmk)
        }
        let mut af = af;
        ModKeys::static_lr_mods_triplets() .iter() .for_each ( |(lrmk,lmk,rmk)| { // for each triplet
            ks.mod_keys.mod_umk_pairs() .iter() .filter (|(mk,_)| *mk == *lmk) .for_each (|(_, umk)| { // for the left-matching umk
                // ^^ we filtered for the modkey match on the triplet as the 'left' key (so we'll only ever wrap left mks)
                if triplet_contains (mks, lrmk, lmk, rmk) {
                    // so we're on a triplet where one among its lr/l/r is in the modkeys set of this combo ..
                    // so if this is managed mk and the wrapping flag is set, we'll wrap in active action, else just direct action
                    if umk.handling.is_managed() && wrap_mod_key_guard {
                        af = umk.active_action (af.clone())
                    } else {
                        af = umk.bare_action (af.clone())
                    }
                } else {
                    // we're in a triplet where neither of lr/l/r is in the modkeys set for this combo
                    if umk.handling.is_managed() && wrap_mod_key_guard {
                        af = umk.inactive_action(af.clone())
                        // ^^ for managed mk, as this mod-key was not in the list, we wrap inactive action around it
                    } else if umk.handling.is_doubled() && wrap_mod_key_guard && cgo.is_some_and (|cg| triplet_contains (&cg.mks, lrmk, lmk, rmk) ) {
                        af = umk.masked_released_action (af.clone())
                        // ^^ for doubled-mk (e.g. lwin) specified in combo-gen mks but not in action mks, we'll do a masked release here for robustness
                        // .. in theory, we shouldnt need it, but the OS might have gotten at the held key earlier than our hook, so this helps
                    }
                }
            })
        });
        // finally we can return the AF, whether it got wrapped above or not
        af
    }

}





/// Combo-Generator for a specified Key Combo (as opposed to a combo that triggers non-key action etc)
impl<'a> ComboGen<'a> {
    /// Create ComboGen around a input-event-callback-map-key
    pub fn new (ks:&KrustyState, cmk:EvCbMapKey) -> ComboGen {
        ComboGen { ks, cmk, mks:Vec::new(), modes:Vec::new(), cond:None, mod_key_no_consume:false, mode_kdn_no_consume:false }
    }
    /// Create ComboGen around a kbd-key .. (will create a kbdkey-down combo)
    pub fn new_w_key (ks:&KrustyState, k:Key) -> ComboGen {
        // combos are based on full combo-map-key instead of just key .. so before we gen that
        // .. we'll also check if its a mode-registered key and auto pre-populate modes w that key
        let mut modes : Vec<ModeState_T> = Vec::new();
        if let Some(ms_t) = ks.mode_states.get_mode_t(k) { modes.push(ms_t) }
        use {EvCbMapKey_Src::*, EvCbMapKey_Action::*, KbdEvCbMapKey_T::*};
        let cmk = EvCbMapKey::new ( key(k), KeyEventCb(KeyEventCb_KeyDown) );
        ComboGen { ks, cmk, mks:Vec::new(), modes, cond:None, mod_key_no_consume:false, mode_kdn_no_consume:false }
    }
    /// Create ComboGen around a mouse button .. (will create a mouse-btn-down combo)
    pub fn new_w_mbtn (ks:&KrustyState, mbtn:MouseButton) -> ComboGen {
        use {EvCbMapKey_Src::*, EvCbMapKey_Action::*, MouseBtnEvent_T::*};
        ComboGen::new (ks, EvCbMapKey::new (btn(mbtn), BtnEventCb(BtnDown)) )
    }
    /// Create ComboGen around mouse vertical wheel
    pub fn new_w_whl (ks:&KrustyState) -> ComboGen {
        use {EvCbMapKey_Src::*, EvCbMapKey_Action::*};
        ComboGen::new (ks, EvCbMapKey::new (wheel(MouseWheel::DefaultWheel), WheelEventCb) )
    }

    /// Set the combo to be for release instead of press (for a kbd-key or mouse-btn)
    pub fn rel (mut self) -> ComboGen<'a> {
        use {EvCbMapKey_Src::*, EvCbMapKey_Action::*, KbdEvCbMapKey_T::*, MouseBtnEvent_T::*};
        match self.cmk.ev_src {
            key(..) => { self.cmk.ev_action = KeyEventCb(KeyEventCb_KeyUp) }
            btn(..) => { self.cmk.ev_action = BtnEventCb(BtnUp) }
            _ => { }
        }
        self
    }
    /// Add a modifier key to the combo
    pub fn m (mut self, mk:ModKey) -> ComboGen<'a> {
        if !self.mks.contains(&mk) { self.mks.push(mk) }; self
    }
    /// Add a mode-state to the combo
    pub fn s (mut self, md: ModeState_T) -> ComboGen<'a> {
        if !self.modes.contains(&md) { self.modes.push(md) }; self
    }
    /// Add a condition to the combo
    pub fn c (mut self, cond:ComboCond) -> ComboGen<'a> {
        if self.cond.is_none() {
            self.cond = Some(cond);
        } else {
            let cond_old = self.cond.take().unwrap();
            self.cond = Some ( Arc::new ( move |ks,e| cond_old(ks,e) && cond(ks,e) ) );
        }
        self
    }
    /// Add a fgnd-exe condition to the combo
    pub fn fg (self, exe:&str) -> ComboGen<'a> {
        let exe = exe.to_string();
        self.c ( Arc::new ( move |_,_| WinEventsListener::instance().fgnd_info.read().is_ok_and(|fi| fi.exe == exe) ) )
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
            self.ks.mod_keys.mod_umk_pairs() .iter() .for_each ( |(mk, umk)| {
                if umk.handling.is_managed() && self.mks.contains(mk) { afc = umk.keydn_consuming_action (afc.clone()) }
            });
        }
        if !self.mode_kdn_no_consume {
            self.ks.mode_states.mode_flag_pairs() .iter() .for_each ( |(ms_t, ms)| {
                if self.modes.contains(ms_t) { afc = ms.mode_key_consuming_action (afc.clone()); }
            } );
        }
        afc
    }

    /// Generate one or more combos/combo-value entries from this ComboGen w/ key-dwn consuming behavior as specified during construction
    pub fn gen_combo_entries (&self, af:AF) -> Vec<(Combo, ComboValue)> {
        Combo::gen_combos (&self.mks, &self.modes, self.cmk, self.ks) .into_iter() .map ( |c|
            (c, ComboValue::new_w_cond (af.clone(), self.cond.clone()))
        ) .collect()
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
        Combo::gen_af (&self.mks, base_action(self.key), self.wrap_mod_key_guard, self.ks, None)
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
        Combo::gen_af (&self.mks, self.af.clone(), self.wrap_mod_key_guard, self.ks, None)
    }
}







/// holds the actual combo-map, and impls functionality on adding combos and matching/handling runtime combos
impl CombosMap {

    pub fn instance () -> CombosMap {
        static INSTANCE: OnceCell<CombosMap> = OnceCell::new();
        INSTANCE .get_or_init (||
            CombosMap ( Arc::new ( _CombosMap {
                _private : (),
                combos_map  : AtomicRefCell::new ( FxHashMap::default() ),
                l2_keys_map : AtomicRefCell::new ( FxHashMap::default() ),
                default_bind_keys : AtomicRefCell::new ( FxHashSet::default() )
            } ) )
        ) .clone()
    }

    /// registers a key for layer-2 functionality, which is used during fallback to layer any pressed mod-keys onto the l2-key
    pub fn register_l2_key (&self, key:Key, l2k:Key) {
        self.l2_keys_map .borrow_mut() .insert (key, l2k);
    }
    /// registers a key for default binding (without a specific combo)
    pub fn register_default_binding_key (&self, key:Key) {
        self.default_bind_keys .borrow_mut() .insert (key);
    }


    /// use this fn to register combos that will output other keys/combos
    pub fn add_combo (&self, cg:ComboGen, ag:ActionGen_wKey) {
        // instead of calling ag.gen_af() directly (which would call Combo::gen_af), we'll ourselves call Combo:gen_af while passing in CG as well
        // .. this will be useful in case we need trigger-specific wrapping (e.g. for mk_dbl present in combo but not in AF)
        let af = Combo::gen_af (&ag.mks, base_action(ag.key), ag.wrap_mod_key_guard, cg.ks, Some(&cg));
        let af = cg.kdn_consume_wrap (af);
        for (c, cv) in cg.gen_combo_entries(af) {
            //println! ("+C: mks:{:?}, ms:{:?}, k:{:?}, mks:{:?}, ms:{:?} -> k:{:?} mks:{:?}", c.mk_state, c.mode_state, cg.key, cg.mks, cg.modes, ag.key, ag.mks );
            self.add_to_combos_map (c, cv);
    } }

    /// use this fn to register combos that will trigger the supplied action
    pub fn add_combo_af (&self, cg:ComboGen, ag:ActionGen_wAF) {
        let af = Combo::gen_af (&ag.mks, ag.af, ag.wrap_mod_key_guard, cg.ks, Some(&cg));
        let af = cg.kdn_consume_wrap (af);
        for (c, cv) in cg.gen_combo_entries(af) {
            //println! ("+C: mks:{:?}, ms:{:?}, k:{:?}, mks:{:?}, ms:{:?} -> AF, mks:{:?}", c.mk_state, c.mode_state, cg.key, cg.mks, cg.modes, ag.mks );
            self.add_to_combos_map (c, cv);
    } }

    fn add_to_combos_map (&self, c:Combo, cv:ComboValue) {
        let mut cm = self.combos_map.borrow_mut();
        //self.combos_map.write().unwrap() .insert (c, cv);
        if let Some(cvs) = cm.get_mut(&c) {
            // if we already had combo-values for this combo, we can add new conditional, but must replace any prior non-conditional AF entries
            if cv.cond.is_none() {
                if let Some(idx) = cvs.iter().position (|cv| cv.cond.is_none()) {
                    let _ = cvs.remove(idx);
            }  }
            cvs.push(cv);
            cvs.sort_by_cached_key (|cv| (cv.cond.is_none(), cv.stamp));
            // ^^ we want to sort such that conditionals are up top sorted by timestamp .. (hence the boolean supplied as cond.is_none())
        } else {
            cm.insert (c, vec![cv]);
        }
    }




    fn handle_caps_combo_fallback (&self, fbaf:AF, e:&Event, ks:&KrustyState) {
        // - if no combo found while caps down, we want to support most multi-mod combos treating caps as ctrl..
        // (however, we have caps-dn suppress all mod-keys, so we'll have to wrap mod-key up/dn here as necessary)
        // - as to l2 keys (for l3 fallback), we want l2-key expected behavior with other mod key combos ..
        // in particular, since caps is used up just to trigger l2, we'll let qks1 do ctrl, (and ralt do shift) for the l2 key combos
        // - and for the mode-keys, we need most caps combos to be silent, so we'll do fallback only when qks1 active, treating that as ctrl
        // (note that for mode-keys, w/o caps down we can ofc do arbitrary mix of natural ctrl/alt/shift/win etc combos)

        // if we're in some caps mode-state, but not qks1, we do nothing for fallback (i.e. only registered combos allowed)
        let qks1_active = ks.mode_states.qks1.down.is_set();
        if ks.mode_states.some_combo_mode_active.is_set() && !qks1_active { return }

        // now for all non-kbd events, we can simply use the provided default action ..
        let (mut af, mut qks1_ctrl) = (fbaf, false);
        // .. but for kbd events, we do special handling for mode-keys
        if let EventDat::key_event {src_key, ..} = e.dat {
            // if its the actual qks-1 trigger key, its always disabled (as qks1-down is when we want other l2/mode keys to do their l2-eqv modes)
            if let Some(msk) = ks.mode_states.qks1.key() { if msk == src_key { return } }
            // else, we do fallback for the key, but if its l2k, the fallback output should be on its l2-key
            let l2k_opt = self.l2_keys_map.borrow().get(&src_key).copied();
            af = base_action (l2k_opt.unwrap_or(src_key));
            // and for either l2k or mode-keys, we wont wrap w ctrl just from being in caps fallback here (unless actual ctrl or qks1-down)
            qks1_ctrl = ks.mode_states.check_if_mode_key(src_key) || l2k_opt.is_some();
        }
        // we can now start progressively wrapping the actions with the appropriate mod-key actions
        if ks.mod_keys.some_ctrl_down() || qks1_active || !qks1_ctrl { af = ks.mod_keys.lctrl.active_action(af) } //CombosMap::wrapped_bfn (Key::LCtrl, bfn) }
        // ^^ if its l2-key or mode-key (i.e qks1-ctrl), we wont ctrl wrap just from being here (unless there's actual ctrl or qks1-down)
        if ks.mod_keys.some_shift_down() || ks.mod_keys.ralt.down.is_set() { af = ks.mod_keys.lshift.active_action(af) }
        if ks.mod_keys.lalt.down.is_set() { af = ks.mod_keys.lalt.active_action(af) }
        if ks.mod_keys.some_win_down()   { af = ks.mod_keys.lwin.active_action(af) }

        // aight, now we exec the layered action we built, and we're done
        af();
    }

    /// applies applicable combo-actions (that meet conditional requirements if any), returns whether any combo AF was executed
    fn process_combo_afs (&self, ks:&KrustyState, cvs:&Vec<ComboValue>, e:&Event) -> bool {
        // note that we have previously ordered combo-values for each combo such that conditional ones are up top sorted by timestamp
        // this ensures determinism, and since the non-conditional af is at the end, allows us to only run that if no conditions matched
        let mut cond_matched = false;
        cvs.iter() .for_each ( |cv| {
            if cv.cond.as_ref() .is_some_and (|c| c(ks,e)) || (cv.cond.is_none() && !cond_matched) {
                cond_matched = true;
                (cv.af)();
            }
        } );
        cond_matched
    }

    /// generates appropriate fallback actions for a given input-event type (if no matching entry was found in combo maps)
    /// (note that since non-mod keys are not tracked, and press -> up/dn while rel -> ignored, they can have simple fallbacks)
    /// (.. however mouse-btns have tracked states, and separated out press/rel .. so fallback AFs are more involved)
    fn gen_fallback_base_af (&self, ks:KrustyState, ev:&Event) -> AF {
        match ev.dat {
            EventDat::key_event {src_key, ..} => {
                Arc::new (move || src_key.press_release() )
            }
            EventDat::wheel_event {src_wheel, delta} => {
                Arc::new (move || src_wheel.scroll(delta) )
            }
            EventDat::btn_event {src_btn, ev_t} => { match ev_t {
                MouseBtnEvent_T::BtnDown => {
                    Arc::new ( move || {
                        ks.mouse.get_btn_state(src_btn) .iter().for_each (|bs| {
                            bs.active.set(); bs.btn.press();
                    } ) } )
                }
                MouseBtnEvent_T::BtnUp   => {
                    Arc::new ( move || {
                        ks.mouse.get_btn_state(src_btn) .iter().for_each (|bs| {
                            if bs.active.is_set() { bs.active.clear(); bs.btn.release(); }
                    } ) } )
                }
                _ => no_action()
            } }
            _ => no_action()
        }
    }

    /// combos (and fallback) action handler for current key-event, based on current modes/mod-key states
    fn combo_maps_handle_input (&self, cmk:EvCbMapKey, ev:&Event, ks:&KrustyState) {
        //println! ("combo-map-key: {:#?}", cmk);
        // note that we assume by the time we're here, callbacks for modifier-keys and mode-keys have already been called (and so flags updated)
        // note also, that from binding setup, we shouldnt get modifier keys or caps sent here for processing

        let (combo_w_latch, combo_no_latch) = Combo::gen_cur_combo (cmk, &ks);
        let mut combo_triggered = false;

        //let cm = self.combos_map.borrow();
        let cm = unsafe { & *self.combos_map.as_ptr() };
        // ^^ the borrow would be fine too, but there's really no need for any guarding as we dont do any writes at runtime ..
        // .. hence we might as well directly read from the map and avoid the (minor) atomic borrow-check overhead

        if let Some(cvs) = cm.get(&combo_w_latch) {
            // if we find a combo w latch active, that overrides everything else
            combo_triggered = self.process_combo_afs (ks, &cvs, ev);
        } else if let Some(cvs) = cm.get(&combo_no_latch) {
            // else we're now ok w just a combo ignoring latches
            combo_triggered = self.process_combo_afs (ks, &cvs, ev);
        }
        if combo_triggered { return }
        // ^^ if either of those actually had registered combos with either a matching conditional, or an un-conditional one, we're done

        // fallback processing ..
        let fbaf = self.gen_fallback_base_af (ks.clone(), &ev);
        if ks.mod_keys.caps.dbl_tap.is_set() {
            // no fallback for double-tap combos that arent explicitly registered
            // in the few cases (like maybe caps/ctrl-f etc) we can set them individually in code ourselves
        } else if ks.mod_keys.caps.down.is_set() {
            // unregistered caps-combos have extensive fallback setups
            self.handle_caps_combo_fallback (fbaf, ev, ks);
        } else if ks.mod_keys.ralt.dbl_tap.is_set() {
            // no fallback for double-tap combos that arent explicitly registered
        } else if ks.mod_keys.ralt.down.is_set() {
            // unmapped ralt w/o caps is set to shift (other mods pass through)
            ks.mod_keys.lshift.active_action(fbaf)()
        } else if ks.mod_keys.some_win_dbl() {
            // we want to check this first before just win-down, but we'll let this work naturally via passthrough (as win will be active)
            fbaf()
        } else if ks.mod_keys.some_win_down() { // note that win is set to only be active upon dbl-tap
            // so .. for non-dbl win-combo, we could leave it empty or fallback to actual win-combo if its not too annoying
            ks.mod_keys.lwin.active_action(fbaf)()       // <-- temp hopefully until we get used to double-tap ??
        } else {    //println!("passthrough: {:?}",key);
            // others, incl single/double ctrl/shift/no-mod presses should all work naturally via passthrough
            fbaf()
        }
    }


    /// generates the full combo-maps processing AF for use by lower level events processor (which sets the processor enabled)
    pub fn enable_combos_map_events_processor (&self, k:&Krusty) {
        use crate::{EvProp_D::*, KbdEvent_T::*};

        // we'll assume that by the time we're here, callbacks for modifier-keys and mode-keys have already updated their flags
        // and for all keys whitelisted for combo-maps style handling, we do complete block on both keydown/keyup and gen all events ourselves!
        // note that we want a whitelist instead of covering everything since unknown apps (incl switche) send unknown keys for valid reasons!

        // and for non-key events (mouse btn/wheel/move), we want to allow combo proc only for those that have binding entries registered
        // (this means they would have had combo-proc directives specified in the binding anyway)
        // (this is 'friendlier' as w/o explicitly configuring the bindings, btns etc wont auto get combo-checked to potentially unpopulated table)

        let mut handled_keys : FxHashSet<Key> = FxHashSet::default();
        self.combos_map .borrow() .keys() .for_each ( |c| {
            if let EvCbMapKey_Src::key(key) = c.cmk.ev_src { handled_keys.insert(key); }
        } );
        self.default_bind_keys .borrow() .iter() .for_each ( |key| { handled_keys.insert(*key); } );
        k .ks .mode_states .mode_flag_pairs()  .iter() .for_each ( |(_,ms)| ms.key() .iter() .for_each (|key| {handled_keys.insert(*key);}) );
        k .ks .mode_states .latch_flag_pairs() .iter() .for_each ( |(_,ms)| ms.key() .iter() .for_each (|key| {handled_keys.insert(*key);}) );

        // lets prep the AF to send into kbd-events-queue
        let (ks, cm) = (k.ks.clone(), self.clone());
        let cm_cb = Arc::new ( move |cmk:EvCbMapKey, e:&Event| { cm.combo_maps_handle_input (cmk, e, &ks) } );

        // then we'll build the actual combo-processor AF
        let input_af_queue = k.iproc.input_af_queue.clone();
        let cb = Arc::new ( move |cmk:EvCbMapKey, had_binding:bool, e:Event| {  //println! ("combo-map-key: {:#?}", cmk);
            match e.dat {
                EventDat::key_event {src_key, ev_t, ..} => {
                    // we'll let injected events pass through (both kdn/kup) .. note that modifier keys deal w injected events separately
                    if e.injected { return EvProp_Continue }
                    // if its not in the combo-proc handled-keys whitelist, we should just let it pass through
                    if !handled_keys.contains(&src_key) { return EvProp_Continue }
                    // for keys, only press events get queued for combo proc (we send key press-rels, so rel-events should be suppresed)
                    if ! (ev_t == KbdEvent_KeyDown || ev_t == KbdEvent_SysKeyDown) { return EvProp_Stop }
                }
                _ => {
                    // note that we're not rejecting injected events here, as looks like x1/x2 mbtns come as injected, (at least in MX mouse)
                    // note also, that unlike for keys we're letting both press/rel go through separately for mouse-btns here
                    // (.. meaning, its fully upto the bindings set up to manage sensible mix between bindings and combo entries)
                    if !had_binding { return EvProp_Continue }
                }
            }
            // we queue all combo actions (instead of spawning out) so they dont get out of sequence
            // todo : warning : note that we're using the same input-af-queue ..
            // (.. and its non-ideal as some other event might have snuck in between event and its combo proc)
            // (.. a separate queue woudlnt fix it either, but eitherway shoudlnt be a problem if queue clearance is fast enough)
            let cm_cb = cm_cb.clone();
            let _ = input_af_queue .send (Box::new (move || cm_cb (cmk, &e)));

            // either way, combo-proc-handled keys should be completely blocked past combo-proc (both keydn and keyup etc)
            // (not least because the actual combo proc is queued for later .. so either we bail early, or we combo-proc and stop cur event)
            EvProp_Stop
        } );
        k.iproc.set_combo_processor(cb);
    }

    pub fn disable_combos_map_events_processor (&self, k:&Krusty) {
        // Note that this is not really intended to be called at runtime ..
        // if we do want this to be dyanmic behavior, we should replace AtomicRefCell with RwLock in the CombosMap struct for robustness
        k.iproc.clear_combo_processor()
    }


}
