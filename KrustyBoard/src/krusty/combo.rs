#![ allow (non_camel_case_types) ]

use std::mem::size_of;
use std::sync::Arc;
use std::time::Instant;

use crate::*;


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
            if let Some(v) = cg.dat.wc_mks.as_ref() {
                if (v.is_empty() && !emks.contains(&mk)) || v.contains(&mk) { return Bit_Ignored }
            }
            if emks.contains(&mk) { Bit_Present } else { Bit_Absent }
        }
        fn get_mode_enum (cg:&CG, md:ModeState_T) -> ComboStatesBits_T {
            if let Some(v) = cg.dat.wc_modes.as_ref() {
                if (v.is_empty() && !cg.dat.modes.contains(&md)) || v.contains(&md) { return Bit_Ignored }
            }
            if cg.dat.modes.contains(&md) { Bit_Present } else { Bit_Absent }
        }
        // we want to expand the combos for any L/R agnostic mod-keys specified
        Combo::fan_lr (cg.dat.mks.clone()) .iter() .map ( |emks| {
            let modkey_states = ModKeys::static_combo_bits_mod_keys() .map (|mk| get_modkey_enum (cg,emks,mk));
            let mode_states   = ModeStates::static_combo_modes()  .map (|md| get_mode_enum (cg,md));
            let latch_states  = ModeStates::static_latch_states() .map (|md| get_mode_enum (cg,md));
            let flags_states  = Self::static_flags_modes()        .map (|md| get_mode_enum (cg,md));

            Combo { _private:(), cmk: cg.st.cmk, modkey_states, mode_states, latch_states, flags_states }
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
        let mut af = ag.st.af.clone();
        ModKeys::static_lr_mods_triplets() .iter() .for_each ( |(lrmk,lmk,rmk)| { // for each triplet
            ks.mod_keys.mod_umk_pairs() .iter() .filter (|(mk,_)| *mk == *lmk) .for_each (|(_, umk)| { // for the left-matching umk
                // ^^ we filtered for the modkey match on the triplet as the 'left' key (so we'll only ever wrap left mks)
                if triplet_contains (&ag.mks, lrmk, lmk, rmk) {
                    // so we're on a triplet where one among its lr/l/r is in the modkeys set of this combo ..
                    // so if this is managed mk and the wrapping flag is set, we'll wrap in active action, else just direct action
                    if umk.handling.is_managed() && ag.wrap_mkg {
                        af = umk.active_action (af.clone())
                    } else {
                        af = umk.bare_action (af.clone())
                    }
                } else {
                    // we're in a triplet where neither of lr/l/r is in the modkeys set for this combo
                    if umk.handling.is_managed() && ag.wrap_mkg {
                        af = umk.inactive_action(af.clone())
                        // ^^ for managed mk, as this mod-key was not in the list, we wrap inactive action around it
                    } else if umk.handling.is_doubled() && ag.wrap_mkg && cgo.is_some_and (|cg| triplet_contains (&cg.dat.mks, lrmk, lmk, rmk) ) {
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
        if !cg.dat.mod_key_no_consume {
            ks.mod_keys.mod_umk_pairs() .iter() .for_each ( |(mk, umk)| {
                if umk.handling.is_managed() && cg.dat.mks.contains(mk) { af = umk.keydn_consuming_action (af.clone()) }
            });
        }
        if !cg.dat.mode_kdn_no_consume {
            ks.mode_states.mode_flag_pairs() .iter() .for_each ( |(ms_t, ms)| {
                if cg.dat.modes.contains(ms_t) { af = ms.mode_key_consuming_action (af.clone()); }
            } );
        }
        af
    }


    /// Generate one or more combos/combo-value entries from this ComboGen (w/ key-dwn consuming behavior as specified during construction)
    pub fn gen_combo_entries (mut cg:CG, ag:AG) -> Vec<(Combo, ComboValue)> {
        // before we gen combos/AFs from these, lets make useful updates to the combo-gen as the final prep step
        // first we'll auto-add any mode-keys's state to its own key-down combos (as the flags will be set on before we get to combo proc)
        // (note that these can still be set to no-consume if key-repeat is desired)
        if let EvCbMapKey::key_ev_t (key, KbdEvCbMapKey_T::KeyEventCb_KeyDown) = cg.st.cmk {
            if let Some(ms_t) = KrustyState::instance().mode_states.get_mode_t(key) {
               if !cg.dat.modes.contains(&ms_t) { cg.dat.modes.push(ms_t) }
            }
            // next, we'll also add mod-keys to their double-tap combos (as our dbl-tap combos fire while the second tap is still held down)
            ModKeys::static_dbl_tap_mk_pairs() .iter() .filter ( |(mk,dmk)|
                cg.dat.mks.contains(dmk) && !cg.dat.mks.contains(mk)
            ) .map(|(mk,_)| mk) .collect::<Vec<_>>() .into_iter() .for_each (|mk| cg.dat.mks.push(*mk) );
        }
        // finally we're ready to gen the combo-AF and the actual combo-entries
        let af = Combo::gen_af (&ag, Some(&cg));
        Self::gen_combos(&cg) .into_iter() .map ( |c|
            (c, ComboValue::new_w_cond (af.clone(), cg.dat.cond.clone()))
        ) .collect()
    }

}



