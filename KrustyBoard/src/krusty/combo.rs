#![ allow (non_camel_case_types, non_upper_case_globals) ]

use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;

use crate::*;



pub const N__COMBO_STATES_BITS__MODKEYS : usize = 9;    // x2 = 18  (from dbl-tap flags)
pub const N__COMBO_STATES_BITS__MODES   : usize = 9;    // x2 = 18  (from dbl-tap flags)
pub const N__COMBO_STATES_BITS__LATCHES : usize = 4;
pub const N__COMBO_STATES_BITS__FLAGS   : usize = 0;
// ^^ 9 mod-keys (caps,l/r-(alt,ctrl,win,shift)), x2 adding double-taps,
// 4+5=9 modes ( msE, msD, msF, msR,  qks, qks1, qks2, qks3, qks4), x2 adding double-taps
// 4 latched layer combo states (latch_1, latch_2, latch_3, latch_4)
// 0 flags () .. mngd-ctrl-dn, ctrl-tab-scrl, right-ms-scrl no longer included in bitmap
//
// note that l/r unspecified keys (ctrl/alt/shift/win) get mapped out to l/r/lr expansions, so mod-key-bits only need the l/r bits
// note also that rght-ms-scrl and some other flags are not included in combo maps .. just check them at use



# [ derive (Eq, PartialEq, Hash, Copy, Clone) ]
/// represents the actual Combo, and impls generation from ComboGens (to store in combo-map) or from active states-flags
pub struct Combo {
    _private:(),
    pub cmk : EvCbMapKey,
    pub states_bits  : u64,
    pub wc_mask_bits : u64,
}

pub const FULL_WILDCARDS_MASK: u64 = 0xFFFFFFFFFFFFFFFF;



/// ComboCond is an arc wrapped fn that should return true if the combo's trigger pre-condition is satisfied
pub type ComboCond = Arc < dyn Fn (&KrustyState, &Event) -> bool + Send + Sync + 'static >;




/// ComboHash is a simple new-type containing the hash value of the combo
#[derive (Debug, Default, Copy, Clone, Eq, PartialEq)]
pub struct ComboHash ( pub(self) u64 );

impl ComboHash {
    pub fn is_empty (&self) -> bool { self.0 == 0 }
}


/// ComboHash-Atomic holds a ComboHash value atomically
#[derive (Debug, Default)]
pub struct ComboHashAtomic ( AtomicU64 );

impl ComboHashAtomic {
    pub fn is_empty (&self) -> bool {
        ComboHash ( self.0 .load (Ordering::Relaxed) ) .is_empty()
    }
    pub fn store (&self, ch:ComboHash) {
        self.0 .store (ch.0, Ordering::Relaxed)
    }
    pub fn clear (&self) {
        self.0 .store (ComboHash::default().0, Ordering::Relaxed)
    }
    pub fn check_match (&self, ch:ComboHash) -> bool {
        ch == ComboHash ( self.0.load (Ordering::Relaxed) )
    }
}




# [ derive () ]
/// ComboValue is the 'value' part of the combos_map entry that holds the AF, the combo creation time, and the optional trigger condition
pub(crate) struct ComboValue {
    _private : (),

    /// The timestamp Instant of creation .. useful for sorting
    pub(crate) stamp : Instant,

    /// The action function to be executed when this combo triggers
    pub(crate) af : AF,

    /// Optional condition that must be valid for this combo to trigger
    pub(crate) cond : Option <ComboCond>,

    /// Optional ComboHash of any first-stroke that must be active for this combo to trigger <br>
    /// (A default zeroed ComboHash indicates no first-stroke requirement)
    pub(crate) fsc : ComboHash,

    /// The no_rpt flag when enabled, suppresses activation of this combo for triggering key-repeats
    pub(crate) no_rpt : bool,

    /// The is_fsc flag is simply a marker for fsc-registration AFs .. only used for info-printout analysis
    pub(crate) is_fsc : bool,
}

impl ComboValue {
    fn new (af:AF, cond:Option<ComboCond>, fsc:ComboHash, no_rpt:bool, is_fsc:bool) -> ComboValue {
        ComboValue { _private:(), stamp:Instant::now(), af, cond, fsc, no_rpt, is_fsc }
    }
}




/// represents the actual Combo, and impls generation from ComboGens (to store in combo-map) or from active states-flags
impl Combo {

    // pub fn new (cmk, ??) -> Combo { }
    // ^^ no new fn, as we only want to gen combos via gen_combos which does a bunch of proc first

    pub(crate) fn has_wildcards (&self) -> bool {
        self.wc_mask_bits < FULL_WILDCARDS_MASK
    }
    pub(crate) fn strip_wildcards (&self) -> Combo {
        Combo { wc_mask_bits: FULL_WILDCARDS_MASK, ..*self }
    }
    pub(crate) fn check_wildcard_eqv (&self, c:&Combo) -> bool {
        self.wc_mask_bits & c.states_bits == self.states_bits
    }


    // while the mod-keys and mode-states are handled by their own objects, we'll handle combo bits gen for flag states ourselves
    fn static_flags_modes () -> [ModeState_T; N__COMBO_STATES_BITS__FLAGS] {
        // note that this will be the source of ordering for the flags-state bits in our combo flags-bitmap field
        // NOTE again we want minimal flags in bitmap, as we dont want a flag to change the combo state so other combos w/o flags get invalidated
        static FLAGS_MODES : [ModeState_T; N__COMBO_STATES_BITS__FLAGS] = {
            //[mngd_ctrl_dn, ctrl_tab_scrl, rght_ms_scrl];
            //[mngd_ctrl_dn, ctrl_tab_scrl];
            //[mngd_ctrl_dn];
            []
        };
        FLAGS_MODES
    }
    fn get_cur_flags_states_flags (_:&KrustyState) -> [&Flag; N__COMBO_STATES_BITS__FLAGS] {
        // note that the order of these must match the order given by the static_flag_modes fn above
        // NOTE again we want minimal flags in bitmap, as we dont want a flag to change the combo state so other combos w/o flags get invalidated
        //[&ks.in_managed_ctrl_down_state, &ks.in_ctrl_tab_scroll_state, &ks.in_right_btn_scroll_state]
        //[&ks.in_managed_ctrl_down_state, &ks.in_ctrl_tab_scroll_state]
        //[&ks.in_managed_ctrl_down_state]
        []
    }



    /// generate the combo bit-map for the current runtime state (incl the active key and ks state flags)
    pub(crate) fn gen_cur_combo (cmk:EvCbMapKey, ks:&KrustyState) -> Combo {
        // note: this is in runtime hot-path .. (unlike the make_combo_*_states_bitmap fns used while building combos-table)
        let wc_mask_bits = FULL_WILDCARDS_MASK;
        let states_bits = ks.mod_keys.mk_flag_pairs()    .map (|(_,fg)| fg.is_set()) .iter()
            .chain ( & ks.mode_states.mode_flag_pairs()  .map (|(_,ms)| ms.down.is_set()) )
            .chain ( & ks.mod_keys.mk_dbl_flag_pairs()   .map (|(_,fg)| fg.is_set()) )
            .chain ( & ks.mode_states.mode_flag_pairs()  .map (|(_,ms)| ms.dbl_tap.is_set()) )
            .chain ( & ks.mode_states.latch_flag_pairs() .map (|(_,ms)| ms.active.is_set()) )
            .chain ( & Self::get_cur_flags_states_flags(ks) .map (|fg| fg.is_set()) )
            .enumerate() .fold ( 0, |a, (ei,e)| a | ((*e as u64) << (ei as u8)) );

        Combo { _private:(), cmk, states_bits, wc_mask_bits }
    }
    pub fn gen_no_latch_combo (combo:Combo) -> Combo {
        const LATCH_MASK_SHIFT : usize = 2 * N__COMBO_STATES_BITS__MODKEYS + 2 * N__COMBO_STATES_BITS__MODES;
        const LATCH_MASK : u64 = ((1 << N__COMBO_STATES_BITS__LATCHES) -1) << LATCH_MASK_SHIFT;
        // ^^ note that bits were progressively packed leftwards, so latches are towards leftmost, not rightmost
        let states_bits = combo.states_bits  & (FULL_WILDCARDS_MASK ^ LATCH_MASK);
        Combo { states_bits, ..combo }
    }



    fn fan_lr (mks:Vec<ModKey>) -> Vec<Vec<ModKey>> {
        // we'll expand out this mod-vec into vec-of-vec with all L/R optional mods fanned out into vec-of-vecs with L, R, or L+R versions
        let mut mvs : Vec<Vec<ModKey>> = Vec::new();
        mvs.push(mks);      // prepare seed vec-of-vec with initial mods-vec
        ModKeys::static_lr_mods_triplets() .iter() .for_each ( |&(lrmk, lmk, rmk)| {
            // expand repeatedly for each lrmk, consuming the list and replacing with expanded version (w/o cloning)
            let mut mvs_exp: Vec<Vec<ModKey>> = Vec::new();
            mvs .drain(..) .for_each ( |mv| {
                if mv.contains(&lrmk) {
                    // if a vec had this l/r mod (e.g. alt), we'll instead gen three mod vecs having (lalt, ralt, lalt && ralt)
                    let mut vlr = mv.iter() .filter (|&&m| m != lrmk && m != lmk && m != rmk) .copied().collect::<Vec<_>>();
                    let (mut vl, mut vr) = (vlr.clone(), vlr.clone());
                    vl.push(lmk); mvs_exp.push(vl);
                    vr.push(rmk); mvs_exp.push(vr);
                    vlr.push(lmk); vlr.push(rmk); mvs_exp.push(vlr);
                } else {
                    mvs_exp.push(mv)
                }
            } );
            // swap in this expanded vec-of-vec for the next loop iteration (w the next lrmk)
            mvs = mvs_exp;
        } );
        mvs
    }

    fn finalize_combo_gen (mut cg:CG) -> CG {
        // before we gen combos from these, lets make useful updates to the combo-gen as the final prep step ..
        // first we'll auto-add any mode-keys's state to its own key-down combos (as the flags will be set on before we get to combo proc)
        // .. and also set it to no-consume .. (so the key can repeat itself, unless disabled via no_rpt)
        if let EvCbMapKey::key_ev_t (key, KbdEvCbMapKey_T::KeyEventCb_KeyDown) = cg.get_cmk() {
            if let Some(ms_t) = KrustyState::instance().mode_states.get_mode_t(key) {
               if !cg.dat.modes.contains(&ms_t) { cg.dat.modes.push(ms_t) }
                cg = cg.msk_nc();
            }
        }
        // next, we'll also add mod-keys to their double-tap combos (as our dbl-tap combos fire while the second tap is still held down)
        for (mk,dmk) in ModKeys::static_ordered_mod_keys().iter() .zip (ModKeys::static_ordered_mod_keys_dbl().iter()) {
            if cg.dat.mks.contains(dmk) && !cg.dat.mks.contains(mk) { cg.dat.mks.push(*mk) }
        }
        // and for double-taps on mode-states too
        for (ms,dms) in ModeStates::static_ordered_modes().iter() .zip (ModeStates::static_ordered_modes_dbl().iter()) {
            if cg.dat.modes.contains(dms) && !cg.dat.modes.contains(ms) { cg.dat.modes.push(*ms) }
        }
        cg
    }


    /// Generate one or more combos from this ComboGen (w/ key-dwn consuming behavior as specified during construction)
    pub(crate) fn gen_combos (mut cg:CG) -> Vec<Combo> {
        // we'll set up helper functions to get the bits for the states bitmap, and the wildcards mask
        fn get_modkey_bit_and_wc (cg:&CG, emks:&[ModKey], mk:ModKey) -> (bool, bool) {
            let mut wc = false;
            if let Some(v) = cg.dat.wc_mks.as_ref() {
                if (v.is_empty() && !emks.contains(&mk)) || v.contains(&mk) { wc = true }
            }
            (wc, emks.contains(&mk))
        }
        fn get_mode_bit_and_wc (cg:&CG, md:ModeState_T) -> (bool, bool) {
            let mut wc = false;
            if let Some(v) = cg.dat.wc_modes.as_ref() {
                if (v.is_empty() && !cg.dat.modes.contains(&md)) || v.contains(&md) { wc = true }
            }
            (wc, cg.dat.modes.contains(&md))
        }

        // and a helper fn to generate a combo given a set of lrmk expanded modkeys
        fn gen_exp_mks_combo (cg:&CG, emks:&[ModKey]) -> Combo {
            let (wc_bits, states_bits) = {
                ModKeys::static_ordered_mod_keys()                .map (|mk| get_modkey_bit_and_wc (cg,emks,mk)) .iter()
                .chain ( & ModeStates::static_ordered_modes()     .map (|md| get_mode_bit_and_wc (cg,md)) )
                .chain ( & ModKeys::static_ordered_mod_keys_dbl() .map (|mk| get_modkey_bit_and_wc (cg,emks,mk)) )
                .chain ( & ModeStates::static_ordered_modes_dbl() .map (|md| get_mode_bit_and_wc (cg,md)) )
                .chain ( & ModeStates::static_latch_states()      .map (|md| get_mode_bit_and_wc (cg,md)) )
                .chain ( & Combo::static_flags_modes()            .map (|md| get_mode_bit_and_wc (cg,md)) )
                .enumerate() .fold ( (0,0) , |(aw,ab), (ei, (w,b))| {
                    let acc_w = aw | ((*w as u64) << (ei as u8));  // accumulate the mask bits
                    let acc_b = ab | ((*b as u64) << (ei as u8));  // accumulate the data bits
                    (acc_w, acc_b)
                } )
            };
            let wc_mask_bits = FULL_WILDCARDS_MASK ^ wc_bits;
            Combo { _private:(), cmk: cg.get_cmk(), states_bits, wc_mask_bits }
        }

        // before sending off to combo lrmk expansion, lets expand any specified wildcard L/R agnostic modkeys
        // (note that leaving the l/r agnostic versions (alt,ctrl,shift,win) in the vec is fine, as they get ignored during bitmap generation
        if let Some(ref mut mks) = cg.dat.wc_mks {
            ModKeys::static_lr_mods_triplets() .iter() .for_each ( |&(lrmk, lmk, rmk)| {
                if mks.contains(&lrmk) {
                    if !mks.contains(&lmk) { mks.push(lmk) };
                    if !mks.contains(&rmk) { mks.push(rmk) };
                }
            } );
        }
        //cg.dat.wc_mks = cg.dat.wc_mks .map(Self::exp_wildcard_lrmks);
        // finally, we can expand on specified L/R agnostic mod-keys if any, and collect the generated combos
        Combo::fan_lr (cg.dat.mks.clone()) .iter() .map (|emks| gen_exp_mks_combo(&cg,emks)) .collect::<Vec<Combo>>()
    }



    /// will wrap an action-gen (typically for key-type), with appropriate active/inactive actions for any
    /// modkeys specified (or not-specified) in the ActionGen builder. <br>
    /// Further, if a combo-gen is provided, will appropriately wrap modkey or mode-key consumption wrappers around the action
    /// (the consumption wrapper marks the keys as consumed, which typically suppresses their key-repeat and/or release events)
    pub(crate) fn gen_af (ag:&AG, cgo:Option<&CG>) -> AF {
        // note-1: there's inefficiency below (gets by using static lists rather than a map), but it's just for ahead-of-time AF gen
        // note-2: this will only wrap actions using L-mod-keys .. hence there's still utility in wrapping consuming AF after this
        // note-3: this left-mk wrapping would be amiss if we had a left-blocked but right-managed mk pair (which we dont intend to have)
        // note-4: reminder that e.g. we have ralt blocked, and lalt managed .. and its still ok to specify ralt in combo-gen (sending out)
        fn triplet_contains (mks:&[ModKey], lrmk:&ModKey, lmk:&ModKey, rmk:&ModKey) -> bool {
            mks.contains(lrmk) || mks.contains(lmk) || mks.contains(rmk)
        }
        fn ag_triplet_contains (ag:&AG, lrmk:&ModKey, lmk:&ModKey, rmk:&ModKey) -> bool {
            ag.check_mks_contains(lrmk) || ag.check_mks_contains(lmk) || ag.check_mks_contains(rmk)
        }
        let ks = KrustyState::instance();
        let mut af = ag.get_af();
        ModKeys::static_lr_mods_triplets() .iter() .for_each ( |(lrmk,lmk,rmk)| { // for each triplet
            ks.mod_keys.mod_umk_pairs() .iter() .filter (|(mk,_)| *mk == *lmk) .for_each (|(_, umk)| { // for the left-matching umk
                // ^^ we filtered for the modkey match on the triplet as the 'left' key (so we'll only ever wrap left mks)
                if ag_triplet_contains (ag, lrmk, lmk, rmk) {
                    // so we're on a triplet where one among its lr/l/r is in the modkeys set of this combo ..
                    // so if this is managed mk and the wrapping flag is set, we'll wrap in active action, else just direct action
                    if umk.handling.is_managed() && ag.check_mkg_wrap() {
                        af = umk.active_action (af.clone())
                    } else {
                        af = umk.bare_action (af.clone())
                    }
                } else {
                    // we're in a triplet where neither of lr/l/r is in the modkeys set for this combo
                    if umk.handling.is_managed() && ag.check_mkg_wrap() {
                        af = umk.inactive_action(af.clone())
                        // ^^ for managed mk, as this mod-key was not in the list, we wrap inactive action around it
                    } else if umk.handling.is_doubled()
                        && ag.check_mkg_wrap()
                        && cgo.is_some_and (|cg| triplet_contains (&cg.dat.mks, lrmk, lmk, rmk) )
                    {
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


    pub(crate) fn gen_fsc_hash (cg:CG) -> ComboHash {
        // gen combos will generate a bunch of l/r expanded combos, but for matching up a first-stroke, we just need one shared truth
        let hash = Self::gen_combos (Self::finalize_combo_gen(cg)) .first() .map (|c| {
            use std::hash::*;
            let mut hasher = DefaultHasher::new();
            c.hash (&mut hasher);
            hasher.finish()
        } ) .unwrap_or_default();
        ComboHash (hash)
    }

    /// Generate one or more combos/combo-value entries from this ComboGen (w/ key-dwn consuming behavior as specified during construction)
    pub(crate) fn gen_combo_entries (cg:CG, ag:AG, is_fsc:bool) -> Vec<(Combo, ComboValue)> {
        let cg = Self::finalize_combo_gen(cg);
        let af = Self::gen_af (&ag, Some(&cg));
        let cond = cg.dat.cond.clone();
        let fsc = cg.dat.first_stroke;
        let no_rpt = cg.dat.repeat_suppressed;

        Self::gen_combos(cg) .into_iter() .map ( |c|
            (c, ComboValue::new (af.clone(), cond.clone(), fsc, no_rpt, is_fsc))
        ) .collect()
    }

}




impl std::fmt::Debug for Combo {
    fn fmt (&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use colored::*;
        const bits_chars_uc: &str = "CACSWACSWEDFRQ1234CACSWACSWEDFRQ1234LLLL";
        const bits_chars_lc: &str = "cacswacswedfrq1234cacswacswedfrq1234llll";
        fn bits_str (bits:u64) -> String {
            format! ("{:064b}",bits) .chars().rev()
                .zip (bits_chars_uc.chars())
                .zip (bits_chars_lc.chars())
                .enumerate() .map ( |(i,((b,uc),lc))| {
                    let c = if b=='1' {uc.to_string().yellow()} else {lc.to_string().dimmed()};
                    let sp = if i==0 || i==4 || i==8 || i==12 || i==17 || i==18 || i==22 || i==26 || i==30 || i==35 {"."} else {""};
                    c.to_string() + &sp.dimmed().to_string()
                } ) .collect::<String>()
        }
        let mask_str = if self.wc_mask_bits != FULL_WILDCARDS_MASK {
            bits_str (self.wc_mask_bits ^ FULL_WILDCARDS_MASK)
        } else { "".into() };

        write! ( f, "{:20} {}  {}", format! ("{:?}", &self.cmk).magenta(), bits_str(self.states_bits).trim(), mask_str.trim() )
    }
}

