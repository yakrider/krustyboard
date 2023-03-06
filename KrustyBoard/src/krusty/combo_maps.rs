#![ allow (non_camel_case_types) ]

use std::collections::{HashMap};

//use once_cell::sync::Lazy;
//use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::krusty::{*, mod_keys::*, key_utils::*};
use crate::krusty::{combo_maps::ModeState_T::*, mod_keys::ModKey::*};
use crate::{KbdEventCallbackEntry, KbdEventCallbackFnType::*, KbdEvCbComboProcDirective::*, EventPropagationDirective::*};






# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone, EnumIter) ]
/// all the supported mode-states, (whether they have triggering keys registered or not)
pub enum ModeState_T {
    sel, del, word, fast,
    qks, qks1, qks2, qks3,
    mngd_ctrl_dn, rght_ms_scrl,
}


pub const COMBO_STATE__MOD_KEY_BITS_N : usize = 9;
pub const COMBO_STATE__MODE_STATE_BITS_N : usize = 8;
pub const COMBO_STATE__FLAG_BITS_N : usize = 2;
// ^^ 9 mod keys from above list, 4+4=8 +2=10 for modes from above list (sel/del/word/fast, qks/qks1/qks2/qks3, rght-ms-scrl/mngd-ctrl-dn)
// note that l/r unspecified keys (ctrl/alt/shift/win) get mapped out to l/r/lr expansions, so mod-key-bits only need the l/r bits!

pub type ComboStatesBits_ModKeys = [bool; COMBO_STATE__MOD_KEY_BITS_N];
pub type ComboStatesBits_Modes   = [bool; COMBO_STATE__MODE_STATE_BITS_N];
pub type ComboStatesBits_Flags   = [bool; COMBO_STATE__FLAG_BITS_N];




# [ derive (Debug) ]
/// ModeState representation for mode-flags (and any associated trigger keys they have)
pub struct ModeState {
    _private    : (),
    pub ms_t    : ModeState_T,
    key         : Arc <RwLock <Option<KbdKey>>>,
    pub mk_down : Flag,
}

# [ derive (Debug, Clone) ]
/// implements the (Arc wrapped) ModeState functionality
pub struct MS (Arc<ModeState>);

impl Deref for MS {
    type Target = ModeState;
    fn deref(&self) -> &ModeState { &self.0 }
}





# [ derive (Debug) ]
/// holds all the ModeStates together, common functionality is impld here
pub struct ModeStates {
    _private : (),
    // l2 mode states
    pub sel  : MS,
    pub del  : MS,
    pub word : MS,
    pub fast : MS,
    // quick-keys mode states
    pub qks  : MS,
    pub qks1 : MS,
    pub qks2 : MS,
    pub qks3 : MS,
    // then the computed flags
    pub some_l2_mode_active   : Flag,
    pub some_qks_mode_active  : Flag,
    pub some_caps_mode_active : Flag,

}

# [ derive (Debug, Clone) ]
/// implements the (Arc wrapped) ModeStates-holder functionality
pub struct MSS (Arc<ModeStates>);

impl Deref for MSS {
    type Target = ModeStates;
    fn deref(&self) -> &ModeStates { &self.0 }
}





# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone) ]
/// represents the actual Combo, and impls generation from ComboGens (to store in combo-map) or from active states-flags
pub struct Combo {
    _private:(),
    pub key : Key,
    pub mk_state   : [bool; COMBO_STATE__MOD_KEY_BITS_N],
    pub mode_state : [bool; COMBO_STATE__MODE_STATE_BITS_N],
    pub flags_state: [bool; COMBO_STATE__FLAG_BITS_N],
    //pub state : [bool; COMBO_STATE_N_BITS],
}





// CG_K and CG_AF comprise the Combo-Generation fluent api structs, with Key or with supplied AF
// .. once populated it can be used for generation of either a combo or a (possibly guarded) action

# [ allow (non_camel_case_types) ]
# [ derive (Clone) ]
/// Combo-Generator for a specified Key Combo (as opposed to a combo that triggers non-key action etc)
pub struct ComboGen_wKey<'a> {
    ks    : &'a KrS,
    key   : Key,
    mks   : Vec<MK>,
    modes : Vec<ModeState_T>,
}


# [ allow (non_camel_case_types) ]
# [ derive (Clone) ]
/// Combo-Generator for a a combo targeting a non-key action (e.g. moving windows etc)
pub struct ComboGen_wAF<'a> {
    ks  : &'a KrS,
    mks : Vec<MK>,
    af  : AF,
}





/// combos-map module internal type (used to build successively mod-key wrapped fallback key-presses)
type BFK = Box <dyn Fn(Key)>;

# [ derive (Clone) ]
/// holds the actual combo-map, and impls functionality on adding combos and matching/handling runtime combos
pub struct CombosMap ( Arc <RwLock <HashMap <Combo, AF>>> );

impl Deref for CombosMap {
    type Target = RwLock <HashMap <Combo, AF>>;
    fn deref (&self) -> &Self::Target { &self.0 }
}






/// implements the (Arc wrapped) ModeState functionality
impl MS {
    pub fn new (ms_t: ModeState_T) -> MS { MS ( Arc::new ( ModeState {
        _private : (),
        ms_t,
        key     : Arc::new(RwLock::new(None)),
        mk_down : Flag::default()
    } ) ) }

    pub fn key (&self) -> Option<KbdKey> {
        self.key.read().unwrap().clone()
    }
    pub fn register_key (&self, key:KbdKey) {
        *self.key.write().unwrap() = Some(key);
    }


    /// generate mode-key flag update action for key-down on registered mode-key
    fn gen_mode_key_down_action(&self, mss:MSS) -> AF {
        let flag = self.mk_down.clone();
        let mss_cba : AF = {
            if MSS::static_l2_modes() .contains(&self.ms_t) {
                Arc::new ( move || { mss.some_l2_mode_active.set();  mss.some_caps_mode_active.set(); } )
            } else if MSS::static_qks_modes() .contains(&self.ms_t) {
                Arc::new ( move || { mss.some_qks_mode_active.set(); mss.some_caps_mode_active.set(); } )
            } else if MSS::static_caps_modes() .contains(&self.ms_t) {
                Arc::new ( move || { mss.some_caps_mode_active.set(); } )
            } else { Arc::new (move || { }) }
        };
        Arc::new (move || { flag.set(); mss_cba(); })
    }
    /// generate mode-key flag update action for key-up on registered mode-key
    fn gen_mode_key_up_action(&self, mss:MSS) -> AF {
        let flag = self.mk_down.clone();
        let mss_cba : AF = {
            if      MSS::static_l2_modes()   .contains(&self.ms_t) { Arc::new ( move || mss.refresh_l2_mode_active_flag() ) }
            else if MSS::static_qks_modes()  .contains(&self.ms_t) { Arc::new ( move || mss.refresh_qks_mode_active_flag() ) }
            else if MSS::static_caps_modes() .contains(&self.ms_t) { Arc::new ( move || mss.refresh_caps_mode_active_flag() ) }
            else { Arc::new ( || { } ) }
        };
        Arc::new ( move || { flag.clear(); mss_cba() } )
    }

    /// For mode-key btns (in addition to any combo maps action) we'll want individual binding callbacks that update flags.
    /// Note that after these binding callbacks process, they will still go through bulk processing for their default/combo actions.
    /// (This is as opposed to default-keys/combos that are handled in bulk w/o individual callback bindings)
    pub fn bind_mode_key_action (&self, mss:MSS) {
        use crate::KbdEventCbMapKeyType::*;
        let (cma_dn, cma_up) = (self.gen_mode_key_down_action(mss.clone()), self.gen_mode_key_up_action(mss.clone()));
        if let Some(key) = self.key() {
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
    } }

}





/// implements the (Arc wrapped) ModeStates-holder functionality
impl MSS {

    pub fn new() -> MSS {
        let (_sel, _del,  _word, _fast) = (MS::new(sel), MS::new(del), MS::new(word), MS::new(fast));
        let (_qks, _qks1, _qks2, _qks3) = (MS::new(qks), MS::new(qks1), MS::new(qks2), MS::new(qks3));
        let (some_l2, some_qks, some_caps) = (Flag::default(), Flag::default(), Flag::default() );

        MSS ( Arc::new ( ModeStates {
            _private : (),
            sel: _sel, del:  _del,  word: _word, fast: _fast,
            qks: _qks, qks1: _qks1, qks2: _qks2, qks3: _qks3,
            some_l2_mode_active: some_l2, some_qks_mode_active: some_qks, some_caps_mode_active: some_caps,
        } ) )
    }

    // we'll just define all enum subsets we need rather than trying to partly/fully iterating through ModeState_T
    pub fn static_l2_modes () -> [ModeState_T;4] {
        static L2_MODES : [ModeState_T;4]  = [sel, del, word, fast];
        L2_MODES
    }
    pub fn static_qks_modes () -> [ModeState_T;4] {
        static QKS_MODES : [ModeState_T;4] = [qks, qks1, qks2, qks3];
        QKS_MODES
    }
    pub fn static_l2_qks_modes () -> [ModeState_T;8] {
        // NOTE that this will be our source of ordering for the mode-states bits in combo bitmap!
        static L2_QKS_MODES : [ModeState_T;8]  = [sel, del, word, fast, qks, qks1, qks2, qks3];
        L2_QKS_MODES
    }
    pub fn static_caps_modes () -> [ModeState_T;9] {
        // note that this also includes mngd_ctrl_dn which is a flag in KrS (i.e. not managed here)
        static CAPS_MODES : [ModeState_T;9] = [sel, del, word, fast, qks, qks1, qks2, qks3, mngd_ctrl_dn];
        CAPS_MODES
    }


    pub fn mode_flag_pairs (&self) -> [(ModeState_T, &MS);8] { [
        // NOTE that the ordering here MUST match that given by the static_l2_qks_modes above
        // .. as this is what we will use to populate the combo bitmap and compare to current combo-mode-states!
        (sel, &self.sel), (del,  &self.del),  (word, &self.word), (fast, &self.fast),
        (qks, &self.qks), (qks1, &self.qks1), (qks2, &self.qks2), (qks3, &self.qks3),
    ] }

    pub fn get_cur_mode_states_bitmap (&self) -> ComboStatesBits_Modes {
        self.mode_flag_pairs() .map (|(_,ms)| ms.mk_down.check())
    }
    pub fn make_combo_mode_states_bitmap (modes:&[ModeState_T]) -> ComboStatesBits_Modes {
        MSS::static_l2_qks_modes() .map (|ms| modes.contains(&ms))
    }

    pub fn refresh_qks_mode_active_flag (&self) {
        if !self.qks.mk_down.check() && !self.qks1.mk_down.check() && !self.qks2.mk_down.check() && !self.qks3.mk_down.check() {
            self.some_qks_mode_active.clear()
        }
        self.refresh_caps_mode_active_flag();
    }
    pub fn refresh_l2_mode_active_flag (&self) {
        if !self.sel.mk_down.check() && !self.del.mk_down.check() && !self.word.mk_down.check() && !self.fast.mk_down.check() {
            self.some_l2_mode_active.clear()
        }
        self.refresh_caps_mode_active_flag();
    }
    pub fn refresh_caps_mode_active_flag (&self) {
        if !self.some_qks_mode_active.check() && !self.some_l2_mode_active.check() {
            self.some_caps_mode_active.clear()
        }
    }
    pub fn clear_flags (&self) {
        self.mode_flag_pairs() .iter() .for_each (|(_,ms)| ms.mk_down.clear());
        self.some_l2_mode_active.clear(); self.some_qks_mode_active.clear(); self.some_caps_mode_active.clear();
    }

    pub fn bind_mode_keys_actions (&self) {
        self.mode_flag_pairs() .iter() .for_each (|(_,ms)| ms.bind_mode_key_action(self.clone()))
    }

}





/// represents the actual Combo, and impls generation from ComboGens (to store in combo-map) or from active states-flags
impl Combo {

    pub fn new (key:Key, mod_keys:&[ModKey], modes:&[ModeState_T]) -> Combo {
        let mk_state    = MKS::make_combo_mod_keys_states_bitmap(mod_keys);
        let mode_state  = MSS::make_combo_mode_states_bitmap(modes);
        let flags_state = Combo::make_combo_flags_states_bitmap(modes);
        Combo { _private:(), key, mk_state, mode_state, flags_state }
    }


    // while the mod-keys and mode-states are handled by their own objects, we'll handle combo bits gen for flag states ourselves
    pub fn static_flags_modes () -> [ModeState_T;2] {
        // note that this will be the source of ordering for the flags-state bits in our combo flags-bitmap field
        static FLAGS_MODES : [ModeState_T;2] = [mngd_ctrl_dn, rght_ms_scrl];
        FLAGS_MODES
    }
    fn get_cur_flags_states_bitmap (ks:&KrS) -> ComboStatesBits_Flags {
        // note that the order of these must match the order given by the static_flag_modes fn above
        [ks.in_managed_ctrl_down_state.check(), ks.in_right_btn_scroll_state.check()]
    }
    fn make_combo_flags_states_bitmap (modes:&[ModeState_T]) -> ComboStatesBits_Flags {
        Combo::static_flags_modes() .map (|ms| modes.contains(&ms))
    }


    /// generate the combo bit-map for the current runtime state (incl the active key and ks state flags)
    pub fn gen_cur_combo (key:Key, ks:&KrS) -> Combo {
        // note: this is in runtime hot-path .. (unlike the one above which is used while populating the combo-table)
        let mode_state  = {
            if !ks.mod_keys.caps.down.check() { [false; COMBO_STATE__MODE_STATE_BITS_N] }
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


    fn fan_lrmk (mks:Vec<MK>, lrmk:&MK, lmk:&MK, rmk:&MK) -> Vec<Vec<MK>> {
        // if a mod vec had this l/r mod (e.g. Alt), we'll instead gen three mod vecs having (LAlt, RAlt, LAlt && RAlt)
        let mut mvs: Vec<Vec<MK>> = Vec::new();
        if mks.contains(lrmk) {
            let mut vlr = mks.iter() .filter (|m| *m != lrmk && *m != lmk && *m != rmk) .map (|m| *m) .collect::<Vec<MK>>();
            let (mut vl, mut vr) = (vlr.clone(), vlr.clone());
            vl.push(*lmk); vr.push(*rmk); vlr.push(*lmk); vlr.push(*rmk);
            mvs.push(vl); mvs.push(vr); mvs.push(vlr);
        } else {
            mvs.push(mks)
        }
        mvs
    }
    fn fan_lr (mks:Vec<MK>) -> Vec<Vec<MK>> {
        // we'll expand out this mod-vec into vec-of-vec with all L/R optional mods fanned out into piles of L/R/L+R mod-vecs
        let mut mvs : Vec<Vec<MK>> = Vec::new();
        mvs.push(mks);      // prepare seed vec-of-vec with initial mods-vec
        MKS::static_lr_mods_triplets() .iter() .for_each ( |(lrmk, lmk, rmk)| {
            // expand repeatedly for each lrmk, consuming the list and replacing with expanded version (w/o cloning)
            mvs = mvs .drain(..) .flat_map (|mv| Combo::fan_lrmk(mv, lrmk, lmk, rmk)) .collect();
        } );
        mvs
    }

    pub fn gen_combos (mks:&Vec<MK>, modes:&Vec<ModeState_T>, key:Key) -> Vec<Combo> {
        // we'll auto add caps to any caps based modes (for easier setup)
        let mut mks = mks.clone();
        let is_qk = modes .iter() .any (|m| MSS::static_caps_modes().contains(m));
        if is_qk && !mks.contains(&caps) { mks.push(caps); }
        // and expand the combos for any L/R agnostic mod-keys specified
        Combo::fan_lr(mks) .iter() .map(|mv| {Combo::new (key, mv, modes)}) .collect::<Vec<Combo>>()
    }

    pub fn gen_af (mks:&Vec<MK>, af:AF, ks:&KrS) -> AF {
        // note that this will only wrap actions using L-mod-keys .. hence there's still utility in wrapping consuming AF after this
        let mc = |mk:MK| mks.contains(&mk) ;
        let mut af = af;
        let lr_zip = MKS::static_lr_mods_triplets() .into_iter() .zip(ks.mod_keys.lrmk_smks().into_iter()) .collect::<Vec<((MK,MK,MK),&SMK)>>();
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
    pub fn new (key:Key, ks:&KrS) -> ComboGen_wKey {
        ComboGen_wKey { ks, key, mks:Vec::new(), modes:Vec::new() }
    }
    pub fn m (&'a mut self, mk:MK) -> &'a mut ComboGen_wKey {
        self.mks.push(mk); self
    }
    pub fn s (&'a mut self, md: ModeState_T) -> &'a mut ComboGen_wKey {
        self.modes.push(md); self
    }
    pub fn gen_combos (&self) -> Vec<Combo> {
        Combo::gen_combos (&self.mks, &self.modes, self.key)
    }
    pub fn gen_af (&self) -> AF {
        Combo::gen_af (&self.mks, base_action(self.key), self.ks)
    }
}


/// Combo-Generator for a a combo targeting a non-key action (e.g. moving windows etc)
impl<'a> ComboGen_wAF<'a> {
    pub fn new  (af:AF, ks:&KrS) -> ComboGen_wAF {
        ComboGen_wAF { ks, mks:Vec::new(), af }
    }
    pub fn m (&'a mut self, mk:MK) -> &'a mut ComboGen_wAF {
        self.mks.push(mk); self
    }
    pub fn gen_af (&self) -> AF {
        Combo::gen_af (&self.mks, self.af.clone(), self.ks)
    }
}





/// holds the actual combo-map, and impls functionality on adding combos and matching/handling runtime combos
impl CombosMap {

    pub fn new () -> CombosMap {
        CombosMap ( Arc::new ( RwLock::new ( HashMap::new() ) ) )
    }


    fn gen_consuming_af (mks:&Vec<MK>, af:AF, ks:&KrS) -> AF {
        let mc = |mk:MK| mks.contains(&mk) ;
        let mut af = af;
        ks.mod_keys.mod_smk_pairs() .iter() .for_each ( |(mk,smk)| {
            if mc(*mk) { af = smk.keydn_consuming_action(af.clone()) }
        });
        af
    }

    /// use this fn to register combos that will auto wrap activation/inactivation AFs around any mods in the combo
    pub fn add_combo (&self, ksr:&KrS, cgc:&ComboGen_wKey, cga:&ComboGen_wKey) {
        // note that although the active/inactive will also mark mk keydn consumed, we only do that for L-keys, hence wrapping consuming AF too
        let af = CombosMap::gen_consuming_af (&cgc.mks, cga.gen_af(), ksr);
        for c in cgc.gen_combos() {
            //println! ("{:?}, {:?}, {:?}, {:?}, {:?} -> {:?} {:?}", c.mk_state, c.mode_state, cgc.key, cgc.mks, cgc.modes, cga.key, cga.mks );
            self .write().unwrap() .insert (c, af.clone());
    } }

    /// note that this will wrap the AF in with mod-actions for all mods either active (if specified) or inactive (if not)
    pub fn add_af_combo (&self, ksr:&KrS, cgc:&ComboGen_wKey, cgaf:&ComboGen_wAF) {
        let af = CombosMap::gen_consuming_af (&cgc.mks, cgaf.gen_af(), ksr);
        for c in cgc.gen_combos() {
            //println! ("{:?}, {:?}, {:?} {:?} -> {:?}", c.state, cgc.key, cgc.mods, cgc.modes, cgaf.mods );
            self .write().unwrap() .insert (c, af.clone());
    } }

    /// use this consuming bare fn to add combos if the AF supplied shouldnt be wrapped for active/inactive by mod, but just marked for consumption
    pub fn add_cnsm_bare_af_combo (&self, ksr:&KrS, cgc:&ComboGen_wKey, af:AF) {
        let af = CombosMap::gen_consuming_af (&cgc.mks, af, ksr);
        for c in cgc.gen_combos () {
            self .write().unwrap() .insert (c, af.clone());
    } }

    /// use this bare fn to add combos if the AF supplied needs special care to avoid wrapping with mod active/inactive .. e.g ctrl/tab etc
    pub fn add_bare_af_combo (&self, _:&KrS, cgc:&ComboGen_wKey, af:AF) {
        for c in cgc.gen_combos () { self .write().unwrap() .insert (c, af.clone()); }
    }



    fn wrapped_bfn (wk:Key, bfk:BFK) -> BFK {
        Box::new ( move |key:Key| { wk.press(); bfk(key); wk.release(); } )
    }
    fn caps_fallback_action_fn (key:Key, ks:&KrS) {
        // if no combo found while caps down, we want to support most multi-mod combos treating caps as ctrl..
        // however, we have caps-dn suppress all mod-keys, so we'll have to wrap mod-key up/dn here as necessary
        let mut bfk: BFK = Box::new(|key:Key| press_release(key));
        // ^^ stable rust doesnt let me use existential/dyn stuff in vars to wrap progressively, so use boxes at some minimal cost
        if ks.mod_keys.caps.down.check() || ks.mod_keys.some_ctrl_down()  { bfk = CombosMap::wrapped_bfn (Key::LCtrl, bfk) }
        if ks.mod_keys.ralt.down.check() || ks.mod_keys.some_shift_down() { bfk = CombosMap::wrapped_bfn (Key::LShift, bfk) }
        if ks.mod_keys.lalt.down.check() { bfk = CombosMap::wrapped_bfn (Key::LAlt, bfk) }
        if ks.mod_keys.some_win_down()   { bfk = CombosMap::wrapped_bfn (Key::LWin, bfk) }
        bfk(key);
    }


    /// combos (and fallback) action handler for current key-event, based on current modes/mod-key states
    pub fn combo_maps_handle_kbd_event (&self, key:Key, ks:&KrS) {
        // note that we assume by the time we're here, callbacks for modifier-keys and mode-keys have already been called (and so flags updated)
        // note also, that from binding setup, we shouldnt get modifier keys or caps sent here for processing
        if let Some(cmaf) = self.read().unwrap() .get(&Combo::gen_cur_combo(key,&ks)) {
            cmaf()  // found a combo matched action .. we're done!
        } else if ks.mod_keys.caps.down.check() {
            if ks.mode_states.some_caps_mode_active.check() {
                // when caps down, we'll suppress caps-mode-trigger keys (incl in combo w other modkeys)
                // we'll also suppress all unregistered caps-mode combos
            } else {
                // .. but for other caps combos, there are extensive fallback setups
                CombosMap::caps_fallback_action_fn (key,ks);
            }
        } else if ks.mod_keys.ralt.down.check() {
            // unmapped ralt is set to shift (other mods pass through)
            shift_press_release(key)
        } else {
            // all else works naturally via passthrough
            press_release(key)
        }
    }


    /// generate the full combo-maps processing AF to pass into lower level events processor
    pub fn gen_combo_maps_processor (&self, k:&Krusty) -> KbdEventCallbackEntry {
        use crate::{KbdEventType::*, EventPropagationDirective::*, KbdEvCbComboProcDirective::*, KbdEventCallbackFnType::*};
        // we'll assume that by the time we're here, callbacks for modifier-keys and mode-keys have already updated their flags
        // and for all keys whitelisted for combo-maps style handling, we do complete block on both keydown/keyup and gen all events ourselves!
        let mut handled_keys = HashSet::new();
        self .read().unwrap() .iter() .for_each ( |(c,_)| { handled_keys.insert(c.key); } );
        k .default_bind_keys .read().unwrap() .iter() .for_each ( |key| { handled_keys.insert(*key); } );
        k .ks .mode_states .mode_flag_pairs() .iter() .for_each ( |(_,ms)| ms.key() .iter() .for_each (|key| {handled_keys.insert(*key);}) );

        let (ks, cm) = (k.ks.clone(), self.clone());
        let cb = Arc::new ( move |e:KbdEvent| {
            if handled_keys.contains(&e.key) {
                if e.event == KbdEvent_KeyDown || e.event == KbdEvent_SysKeyDown {
                    // all combo-handling is done on key-dn and spawned out
                    let (ks, cm) = (ks.clone(), cm.clone());
                    spawn (move || cm.combo_maps_handle_kbd_event(e.key, &ks));
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



}
