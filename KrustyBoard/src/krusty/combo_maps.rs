#![ allow (non_camel_case_types) ]

use std::mem::size_of;
use std::sync::Arc;
use std::time::Instant;
use atomic_refcell::AtomicRefCell;

use derive_deref::Deref;
use once_cell::sync::OnceCell;
use rustc_hash::{FxHashMap, FxHashSet};


use crate::{*, key_utils::*};


#[derive (Eq, PartialEq, Hash, Copy, Clone, Default)]
pub enum ComboStatesBits_T {
    #[default]
    Bit_Absent  = 0,
    Bit_Present = 1,
    Bit_Ignored = 2,
}
impl std::fmt::Debug for ComboStatesBits_T {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
       write!(f, "{:?}", *self as u8)
    }
}

pub type ComboStatesBits_ModKeys = [ComboStatesBits_T; 18];
pub type ComboStatesBits_Modes   = [ComboStatesBits_T; 8];
pub type ComboStatesBits_Latches = [ComboStatesBits_T; 4];
pub type ComboStatesBits_Flags   = [ComboStatesBits_T; 0];
// ^^ 9 mod-keys (caps,l/r-(alt,ctrl,win,shift)), x2 adding double-taps,
// 4+4=8 modes ( msE / msD / msF / msR,  qks / qks1 / qks2 / qks3),
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
    // we'll also maintain a separate set (of keys) for combos w wildcards .. (for more efficient wildcard matching)
    wildcard_combos : AtomicRefCell <FxHashMap <EvCbMapKey, Vec<Combo>>>,
    // we'll maintain a (redundant) mapping of l2-keys for quick checks during fallback processing
    l2_keys_map : AtomicRefCell <FxHashMap <Key, Key>>,
    // and we'll hold a registry for keys that only need default/fallback bindings
    default_bind_keys : AtomicRefCell <FxHashSet <Key>>,
}
# [ derive (Clone, Deref) ]
pub struct CombosMap ( Arc <_CombosMap> );






/// represents the actual Combo, and impls generation from ComboGens (to store in combo-map) or from active states-flags
impl Combo {

    // pub fn new (cmk, ??) -> Combo { }
    // ^^ no new fn, as we only want to gen combos via gen_combos which does a bunch of proc first

    pub fn has_wildcards (&self) -> bool {
        use ComboStatesBits_T::*;
        self.modkey_states.contains(&Bit_Ignored) || self.mode_states.contains(&Bit_Ignored) ||
            self.latch_states.contains(&Bit_Ignored) || self.flags_states.contains(&Bit_Ignored)
    }
    pub fn strip_wildcards (&self) -> Combo {
        use ComboStatesBits_T::*;
        let strip = |v| if v == Bit_Ignored { Bit_Absent } else { v };
        Combo { _private:(), cmk:self.cmk,
            modkey_states : self.modkey_states.map(strip), mode_states  : self.mode_states.map(strip),
            latch_states  : self.latch_states.map(strip),  flags_states : self.flags_states.map(strip),
        }
    }
    pub fn check_wildcard_eqv (&self, c:&Combo) -> bool {
        use ComboStatesBits_T::*;
        fn wc_eqv (a:&[ComboStatesBits_T], b:&[ComboStatesBits_T]) -> bool {
            a .iter().zip (b) .all (|(&sa, &sb)| {
                sa == sb || sa == Bit_Ignored || sb == Bit_Ignored
        } ) }
        wc_eqv (&self.modkey_states, &c.modkey_states) && wc_eqv (&self.mode_states, &c.mode_states) &&
            wc_eqv (&self.latch_states, &c.latch_states) && wc_eqv (&self.flags_states, &c.flags_states)
    }


    // while the mod-keys and mode-states are handled by their own objects, we'll handle combo bits gen for flag states ourselves
    pub fn static_flags_modes () -> [ModeState_T; size_of::<ComboStatesBits_Flags>()] {
        // note that this will be the source of ordering for the flags-state bits in our combo flags-bitmap field
        // NOTE again we want minimal flags in bitmap, as we dont want a flag to change the combo state so other combos w/o flags get invalidated
        static FLAGS_MODES : [ModeState_T; size_of::<ComboStatesBits_Flags>()] = {
            //[mngd_ctrl_dn, ctrl_tab_scrl, rght_ms_scrl];
            //[mngd_ctrl_dn, ctrl_tab_scrl];
            //[mngd_ctrl_dn];
            []
        };
        FLAGS_MODES
    }
    fn get_cur_flags_states_flags (_:&KrustyState) -> [&Flag; size_of::<ComboStatesBits_Flags>()] {
        // note that the order of these must match the order given by the static_flag_modes fn above
        // NOTE again we want minimal flags in bitmap, as we dont want a flag to change the combo state so other combos w/o flags get invalidated
        //[&ks.in_managed_ctrl_down_state, &ks.in_ctrl_tab_scroll_state, &ks.in_right_btn_scroll_state]
        //[&ks.in_managed_ctrl_down_state, &ks.in_ctrl_tab_scroll_state]
        //[&ks.in_managed_ctrl_down_state]
        []
    }



    /// generate the combo bit-map for the current runtime state (incl the active key and ks state flags)
    pub fn gen_cur_combo (cmk:EvCbMapKey, ks:&KrustyState) -> Combo {
        // note: this is in runtime hot-path .. (unlike the make_combo_*_states_bitmap fns used while building combos-table)

        use ComboStatesBits_T::*;
        let modkey_states = ks.mod_keys.mk_flag_pairs() .map ( |(_,fgo)|
            if fgo .filter (|fg| fg.is_set()) .is_some() { Bit_Present } else { Bit_Absent }
        );
        let mode_states = ks.mode_states.mode_flag_pairs() .map ( |(_,ms)|
            if ms.down.is_set() { Bit_Present } else { Bit_Absent }
        );
        let latch_states = ks.mode_states.latch_flag_pairs() .map ( |(_,ms)|
            if ms.active.is_set() { Bit_Present } else { Bit_Absent }
        );
        let flags_states  = Self::get_cur_flags_states_flags(ks) .map ( |flag|
            if flag.is_set() { Bit_Present } else { Bit_Absent }
        );

        Combo { _private:(), cmk, modkey_states, mode_states, latch_states, flags_states }
    }
    pub fn gen_no_latch_combo (combo:Combo) -> Combo {
        Combo { latch_states: ComboStatesBits_Latches::default(), ..combo }
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

    fn gen_combos (cg:&CG) -> Vec<Combo> {
        // we'll have to convert the states bits/flags to the wildcard-supporting enum
        use ComboStatesBits_T::*;
        fn get_modkey_enum (cg:&CG, emks:&Vec<ModKey>, mk:ModKey) -> ComboStatesBits_T {
            if let Some(v) = cg.wc_mks.as_ref() {
                if (v.is_empty() && !emks.contains(&mk)) || v.contains(&mk) { return Bit_Ignored }
            }
            if emks.contains(&mk) { Bit_Present } else { Bit_Absent }
        }
        fn get_mode_enum (cg:&CG, md:ModeState_T) -> ComboStatesBits_T {
            if let Some(v) = cg.wc_modes.as_ref() {
                if (v.is_empty() && !cg.modes.contains(&md)) || v.contains(&md) { return Bit_Ignored }
            }
            if cg.modes.contains(&md) { Bit_Present } else { Bit_Absent }
        }
        // we want to expand the combos for any L/R agnostic mod-keys specified
        Combo::fan_lr(cg.mks.clone()) .iter() .map ( |emks| {
            let modkey_states = ModKeys::static_combo_bits_mod_keys() .map (|mk| get_modkey_enum (cg,emks,mk));
            let mode_states   = ModeStates::static_combo_modes()  .map (|md| get_mode_enum (cg,md));
            let latch_states  = ModeStates::static_latch_states() .map (|md| get_mode_enum (cg,md));
            let flags_states  = Self::static_flags_modes()        .map (|md| get_mode_enum (cg,md));

            Combo { _private:(), cmk: cg._state.cmk, modkey_states, mode_states, latch_states, flags_states }
        } ) .collect::<Vec<Combo>>()
    }

    /// will wrap an action-gen (typically for key-type), with appropriate active/inactive actions for any
    /// modkeys specified (or not-specified) in the ActionGen builder. <br>
    /// Further, if a combo-gen is provided, will appropriately wrap modkey or mode-key consumption wrappers around the action
    /// (the consumption wrapper marks the keys as consumed, which typically suppresses their key-repeat and/or release events)
    pub fn gen_af (ag:&AG, cgo:Option<&CG>) -> AF {
        // note-1: there's inefficiency below (gets by using static lists rather than a map), but it's just for ahead-of-time AF gen
        // note-2: this will only wrap actions using L-mod-keys .. hence there's still utility in wrapping consuming AF after this
        // note-3: this left-mk wrapping would be amiss if we had a left-blocked but right-managed mk pair (which we dont intend to have)
        // note-4: reminder that e.g. we have ralt blocked, and lalt managed .. and its still ok to specify ralt in combo-gen (sending out)
        fn triplet_contains (mks:&Vec<ModKey>, lrmk:&ModKey, lmk:&ModKey, rmk:&ModKey) -> bool {
            mks.contains(lrmk) || mks.contains(lmk) || mks.contains(rmk)
        }
        let ks = KrustyState::instance();
        let mut af = ag._state.af.clone();
        ModKeys::static_lr_mods_triplets() .iter() .for_each ( |(lrmk,lmk,rmk)| { // for each triplet
            ks.mod_keys.mod_umk_pairs() .iter() .filter (|(mk,_)| *mk == *lmk) .for_each (|(_, umk)| { // for the left-matching umk
                // ^^ we filtered for the modkey match on the triplet as the 'left' key (so we'll only ever wrap left mks)
                if triplet_contains (&ag.mks, lrmk, lmk, rmk) {
                    // so we're on a triplet where one among its lr/l/r is in the modkeys set of this combo ..
                    // so if this is managed mk and the wrapping flag is set, we'll wrap in active action, else just direct action
                    if umk.handling.is_managed() && ag.wrap_mod_key_guard {
                        af = umk.active_action (af.clone())
                    } else {
                        af = umk.bare_action (af.clone())
                    }
                } else {
                    // we're in a triplet where neither of lr/l/r is in the modkeys set for this combo
                    if umk.handling.is_managed() && ag.wrap_mod_key_guard {
                        af = umk.inactive_action(af.clone())
                        // ^^ for managed mk, as this mod-key was not in the list, we wrap inactive action around it
                    } else if umk.handling.is_doubled() && ag.wrap_mod_key_guard && cgo.is_some_and (|cg| triplet_contains (&cg.mks, lrmk, lmk, rmk) ) {
                        af = umk.masked_released_action (af.clone())
                        // ^^ for doubled-mk (e.g. lwin) specified in combo-gen mks but not in action mks, we'll do a masked release here for robustness
                        // .. in theory, we shouldnt need it, but the OS might have gotten at the held key earlier than our hook, so this helps
                    }
                }
            })
        });
        // now, if we were generating this standalone without a combo-gen, we're done
        if cgo.is_none() { return af }

        // else, if we did have a combo-gen, we'll try to wrap it with any specified mod-key/mode-key consume actions
        let cg = cgo.unwrap();
        if !cg.mod_key_no_consume {
            ks.mod_keys.mod_umk_pairs() .iter() .for_each ( |(mk, umk)| {
                if umk.handling.is_managed() && cg.mks.contains(mk) { af = umk.keydn_consuming_action (af.clone()) }
            });
        }
        if !cg.mode_kdn_no_consume {
            ks.mode_states.mode_flag_pairs() .iter() .for_each ( |(ms_t, ms)| {
                if cg.modes.contains(ms_t) { af = ms.mode_key_consuming_action (af.clone()); }
            } );
        }
        af
    }

    /// Generate one or more combos/combo-value entries from this ComboGen (w/ key-dwn consuming behavior as specified during construction)
    fn gen_combo_entries (mut cg:CG, ag:AG) -> Vec<(Combo, ComboValue)> {
        // before we gen combos/AFs from these, lets make useful updates to the combo-gen as the final prep step
        // first we'll auto-add any mode-keys's state to its own key-down combos (as the flags will be set on before we get to combo proc)
        // (note that these can still be set to no-consume if key-repeat is desired)
        if let EvCbMapKey::key_ev_t (key, KbdEvCbMapKey_T::KeyEventCb_KeyDown) = cg._state.cmk {
            if let Some(ms_t) = KrustyState::instance().mode_states.get_mode_t(key) {
               if !cg.modes.contains(&ms_t) { cg.modes.push(ms_t) }
            }
            // next, we'll also add mod-keys to their double-tap combos (as our dbl-tap combos fire while the second tap is still held down)
            ModKeys::static_dbl_tap_mk_pairs() .iter() .filter ( |(mk,dmk)|
                cg.mks.contains(dmk) && !cg.mks.contains(mk)
            ) .map(|(mk,_)| mk) .collect::<Vec<_>>() .into_iter() .for_each (|mk| cg.mks.push(*mk) );
        }
        // finally we're ready to gen the combo-AF and the actual combo-entries
        let af = Combo::gen_af (&ag, Some(&cg));
        Self::gen_combos(&cg) .into_iter() .map ( |c|
            (c, ComboValue::new_w_cond (af.clone(), cg.cond.clone()))
        ) .collect()
    }

}




/// holds the actual combo-map, and impls functionality on adding combos and matching/handling runtime combos
impl CombosMap {

    pub fn instance () -> CombosMap {
        static INSTANCE: OnceCell<CombosMap> = OnceCell::new();
        INSTANCE .get_or_init (||
            CombosMap ( Arc::new ( _CombosMap {
                _private : (),
                combos_map        : AtomicRefCell::new ( FxHashMap::default() ),
                wildcard_combos   : AtomicRefCell::new ( FxHashMap::default() ),
                l2_keys_map       : AtomicRefCell::new ( FxHashMap::default() ),
                default_bind_keys : AtomicRefCell::new ( FxHashSet::default() ),
            } ) )
        ) .clone()
    }

    /// Registers a key for layer-2 functionality, which is used during fallback to layer any pressed mod-keys onto the l2-key
    pub fn register_l2_key (&self, key:Key, l2k:Key) {
        self.l2_keys_map .borrow_mut() .insert (key, l2k);
    }
    /// Registers a key for default binding (without a specific combo)
    pub fn register_default_binding_key (&self, key:Key) {
        self.default_bind_keys .borrow_mut() .insert (key);
    }


    /// Use this fn to register combos <br>
    /// The expectation is to progressively (fluently) build the ComboGen and ActionGen params, and pass them here.
    pub fn add_combo (&self, cg: impl Into<CG>, ag: impl Into<AG>) {
        for (c, cv) in Combo::gen_combo_entries(cg.into(), ag.into()) {
            self.add_to_combos_map (c, cv);
        }
    }


    fn add_to_wildcards_map (&self, c:Combo) {
        let mut wcm = self.wildcard_combos.borrow_mut();
        if let Some(cmks) = wcm.get_mut(&c.cmk) {
            if !cmks.contains(&c) { cmks.push(c) }
        } else {
            wcm.insert (c.cmk, vec![c]);
        }
    }

    fn add_to_combos_map (&self, c:Combo, cv:ComboValue) {
        // we'll check if combo has wildcards, and if so, add to wildcard-combos map
        // (.. plus, strip the wildcards and add to the regular map too)
        let mut c = c;
        if c.has_wildcards() {
            self.add_to_wildcards_map(c);
            c = c.strip_wildcards();
        }
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
        if let EventDat::key_event {key, ev_t, ..} = e.dat {
            // caps fallback for kbd-key-up is to do nothing (we typically do both press/rel action on press)
            if ev_t == KbdEvent_T::KbdEvent_KeyUp || ev_t == KbdEvent_T::KbdEvent_SysKeyUp { return }
            // if its the actual qks-1 trigger key, its always disabled (as qks1-down is when we want other l2/mode keys to do their l2-eqv modes)
            if let Some(msk) = ks.mode_states.qks1.key() { if msk == key { return } }
            // else, we do fallback for the key, but if its l2k, the fallback output should be on its l2-key
            let l2k_opt = self.l2_keys_map.borrow().get(&key).copied();
            af = base_action (l2k_opt.unwrap_or(key));
            // and for either l2k or mode-keys, we wont wrap w ctrl just from being in caps fallback here (unless actual ctrl or qks1-down)
            qks1_ctrl = ks.mode_states.check_if_mode_key(key) || l2k_opt.is_some();
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
    fn gen_fallback_base_af (&self, ks:KrustyState, ev:&Event) -> Option<AF> {
        match ev.dat {
            EventDat::key_event {key, ev_t, ..} => { match ev_t {
                KbdEvent_T::KbdEvent_KeyDown | KbdEvent_T::KbdEvent_SysKeyDown => {
                    Some ( Arc::new (move || key.press_release()) )
                }
                _ => None   // no default fallback for key-releas types (w/ or w/o syskey)
            } }
            EventDat::btn_event {btn, ev_t} => { match ev_t {
                // (note below that physical params like btn.{down, dbl_tap, stamp) are typically updated in binding itself)
                MouseBtnEv_T::BtnDown => {
                    Some ( Arc::new ( move || {
                        ks.mouse.get_btn_state(btn) .iter().for_each (|bs| {
                            bs.active.set(); bs.btn.press();
                    } ) } ) )
                }
                MouseBtnEv_T::BtnUp   => {
                    Some ( Arc::new ( move || {
                        ks.mouse.get_btn_state(btn) .iter().for_each (|bs| {
                            if bs.active.is_set() { bs.active.clear(); bs.btn.release(); }
                    } ) } ) )
                }
            } }
            EventDat::wheel_event {wheel, delta} => {
                Some ( Arc::new (move || wheel.scroll(delta) ) )
            }
            EventDat::move_event {..} => None
            // ^^ move events wont even get here, but eitherway we'd do nothing
        }
    }

    /// combos (and fallback) action handler for current key-event, based on current modes/mod-key states
    fn combo_maps_handle_input (&self, cmk:EvCbMapKey, ev:&Event, ks:&KrustyState) {
        //println! ("combo-map-key: {:?}", cmk);
        // note that we assume by the time we're here, callbacks for modifier-keys and mode-keys have already been called (and so flags updated)
        // note also, that from binding setup, we shouldnt get modifier keys or caps sent here for processing

        // we'll use these helper fns to repeatedly attempt combo matches .. if this returns true, we can immdtly return
        fn try_proc_combo_afs (combo:&Combo, ev:&Event, cm:&CombosMap, ks:&KrustyState) -> bool {
            //let pcm = self.combos_map.borrow();
            // ^^ the borrow would be fine too, but there's really no need for any guarding as we dont do any writes at runtime ..
            // .. hence we might as well directly read from the map and avoid the (minor) atomic borrow-check overhead
            let pcm  = unsafe { & *cm.combos_map.as_ptr() };
            if let Some(cvs) = pcm.get(&combo) {
                cm.process_combo_afs (ks, &cvs, ev)
            } else { false }
        }
        fn try_proc_wc_combo_afs (cmk:&EvCbMapKey, combo:&Combo, ev:&Event, cm:&CombosMap, ks:&KrustyState) -> bool {
            let cwm = unsafe { & *cm.wildcard_combos.as_ptr() };
            if let Some(cs) = cwm.get(cmk) {
                if let Some(c) = cs.iter().find (|c| c.check_wildcard_eqv (&combo)) {
                    // todo: ^^ should be ok for minimal wildcard use, but in theory we could make it much more efficient by doing things like
                    // .. using a column-wise bitmap (as in databases), or even just membership maps and progressively filtering matching combos etc
                    if try_proc_combo_afs (&c.strip_wildcards(), ev, &cm, ks) { return true }
                }
            }
            false
        }

        let combo = Combo::gen_cur_combo (cmk, &ks);

        // if we find an exact match combo, that overrides everything else
        if try_proc_combo_afs (&combo, ev, &self, ks) { return }

        // else, we'll check if there are some wildcard combos that might match
        // Note that we ONLY run the first matching wildcarded combo (not all matching ones)
        if try_proc_wc_combo_afs (&cmk, &combo, ev, &self, ks) { return }

        // else if some latch state was active, we can try to match a combo ignoring latches (as fallback)
        // note that we're doing the 'check again w/o latch' coz its more efficient than trying to default all latch combos to have no-latch wildcards ..
        // .. coz the cur impl requires linear search to match wildcard combos (within the subset for that particular cmk w wildcard combos)
        let combo_no_latch = Combo::gen_no_latch_combo(combo);
        if ks.mode_states.some_latch_state_active.is_set() {
            // again, first we check exact match
            if try_proc_combo_afs (&combo_no_latch, ev, &self, ks) { return }
            // else we check for wildcard matches
            if try_proc_wc_combo_afs (&cmk, &combo_no_latch, ev, &self, ks) { return }
        }

        // fallback processing ..
        let fbaf = self.gen_fallback_base_af (ks.clone(), &ev);
        if fbaf.is_none() { return }
        // ^^ if we explicitly didnt want to do anything, no point trying to wrap mods below etc
        let fbaf = fbaf.unwrap();

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
        use crate::EvProp_D::*;

        // we'll assume that by the time we're here, callbacks for modifier-keys and mode-keys have already updated their flags
        // and for all keys whitelisted for combo-maps style handling, we do complete block on both keydown/keyup and gen all events ourselves!
        // note that we want a whitelist instead of covering everything since unknown apps (incl switche) send unknown keys for valid reasons!

        // and for non-key events (mouse btn/wheel/move), we want to allow combo proc only for those that have binding entries registered
        // (this means they would have had combo-proc directives specified in the binding anyway)
        // (this is 'friendlier' as w/o explicitly configuring the bindings, btns etc wont auto get combo-checked to potentially unpopulated table)

        let mut handled_keys : FxHashSet<Key> = FxHashSet::default();
        //self.combos_map .borrow() .keys() .map(|c| format!("{:?}",c)) .sorted() .for_each (|c| println!("{:?}",c));
        self.combos_map .borrow() .keys() .for_each ( |c| {
            if let EvCbMapKey::key_ev_t (key, ..) = c.cmk { handled_keys.insert(key); }
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
                EventDat::key_event {key, ..} => {
                    // we'll let injected events pass through (both kdn/kup) .. note that modifier keys deal w injected events separately
                    if e.injected { return EvProp_Continue }
                    // if its not in the combo-proc handled-keys whitelist, we should just let it pass through
                    if !handled_keys.contains(&key) { return EvProp_Continue }
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
