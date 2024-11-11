#![ allow (non_camel_case_types) ]

use std::sync::Arc;
use std::{thread, time::Duration};

use atomic_refcell::AtomicRefCell;
use derive_deref::Deref;
use itertools::Itertools;
use once_cell::sync::OnceCell;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::*;



# [ derive () ]
/// holds the actual combo-map, and impls functionality on adding combos and matching/handling runtime combos
pub struct _CombosMap {
    // Note that we're using AtomicRefCell instead of Arc-RwLock because we should be doing all the building before
    //   we start running it, so there should never be a write attempted while some other thread is trying to read these
    _private : (),

    /// maps key+mod-key+mode-state combos to actions
    combos_map : AtomicRefCell <FxHashMap <Combo, Vec<ComboValue>>>,

    /// maintains a separate set (of keys) for combos w wildcards .. (for more efficient wildcard matching)
    /// (the second Combo in the stored pair is a precomputed wildcard-stripped version to lookup in the actual combos_map)
    wildcard_combos : AtomicRefCell <FxHashMap <EvCbMapKey, Vec<(Combo,Combo)>>>,

    /// holds a registry for keys that only need default/fallback bindings
    handled_keys_set : AtomicRefCell <FxHashSet <Key>>,
}
# [ derive (Clone, Deref) ]
pub struct CombosMap ( Arc <_CombosMap> );


# [ derive (Default) ]
struct ProcCVsResult {
    combo_execd : bool,
    fscs_found  : bool,
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
                handled_keys_set  : AtomicRefCell::new ( FxHashSet::default() ),
            } ) )
        ) .clone()
    }


    /// Add key to the handled-keys-set ..<br>
    /// A key in the handled-set w/o bindings will go through combo processing with fallback handling
    pub fn add_to_handled_keys_set (&self, key:Key) {
        self.handled_keys_set .borrow_mut() .insert (key);
    }

    /// Check if a key is in the handled-keys-set of the combos-map
    pub fn check_if_handled_key (&self, key:&Key) -> bool {
        self.handled_keys_set .borrow() .contains (key)
    }


    /// Compiles and adds a combo to the combo-mappings table <br>
    /// The expectation is to progressively (fluently) build the ComboGen and ActionGen params, and pass them here.
    pub fn add_combo (&self, cg: impl Into<CG>, ag: impl Into<AG>) {
        self._add_combo (cg, ag, false)
    }
    fn _add_combo (&self, cg: impl Into<CG>, ag: impl Into<AG>, is_fsc:bool) {
        for (c, cv) in Combo::gen_combo_entries (cg.into(), ag.into(), is_fsc) {
            self.add_to_combos_map (c, cv);
        }
    }


    /// Registers a combo as a possible 'first-stroke-combo' (fsc), and returns its combo-hash. <br>
    /// The combo-hash returned by this fn must be provided as the first-stroke when defining two-stroke-combos. <br>
    /// Note that fscs are active only while some-modkey is held, and therefore only a combo with some modkey can be a valid fsc
    pub fn register_first_stroke_combo (&self, cg: impl Into<CG>) -> ComboHash {
        let cg = cg.into();
        let fsc = Combo::gen_fsc_hash(cg.clone());
        self.setup_first_stroke_af (cg, fsc);
        fsc
    }
    /// Co-Registers a possible first-stroke combo as an alternate for another first-stroke with the combo-hash supplied
    pub fn co_register_first_stroke_combo (&self, cg: impl Into<CG>, fsc:ComboHash) {
        self.setup_first_stroke_af (cg.into(), fsc);
    }
    fn setup_first_stroke_af (&self, cg:CG, fsc:ComboHash) {
        let ks = KrustyState::instance();
        let af = Arc::new (move || {
            if ks.mod_keys.some_mk_down() {
                // ^^ the check is for safety, as any recorded fsc only clears on all-modkeys-released ..
                // (fscs are required to have some mod-key in them and are active until all modkeys are released)
                ks.first_stroke.store(fsc);
                jiggle_cursor()
            }
        } );
        self._add_combo (cg, ag().af(af), true);
    }

    fn add_to_wildcards_map (&self, c:Combo) {
        let mut wcm = self.wildcard_combos.borrow_mut();
        let pair = (c, c.strip_wildcards());
        if let Some(cs) = wcm.get_mut(&c.cmk) {
            if !cs.contains(&pair) { cs.push(pair) }
        } else {
            wcm.insert (c.cmk, vec![pair]);
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
        // we'll also add the KbdKey of this combo (if any) to our handled keys cache
        if let EvCbMapKey::key_ev_t (key, ..) = c.cmk {
            self.add_to_handled_keys_set (key);
        }
        // and finally, we can add the combo to our combos map
        let mut cm = self.combos_map.borrow_mut();
        //self.combos_map.write().unwrap() .insert (c, cv);
        if let Some(cvs) = cm.get_mut(&c) {
            // note that we allow multiple conditional or mult non-conditional combos to trigger ..
            // .. but if any conditional combo triggers, then non-conditional combos for that are ignored
            cvs.push(cv);
            cvs.sort_by_cached_key (|cv| (cv.fsc.is_empty(), cv.cond.is_none(), cv.stamp));
            // ^^ we want to sort such that fscs and  conditionals are up top .. (hence the booleans supplied)
        } else {
            cm.insert (c, vec![cv]);
        }
    }





    pub fn debug_print_combos_map (&self) {
        let cm = self.clone();
        thread::spawn ( move || {
            println! ("\nCombo entries and their combo-values counts \n# (first-stroke-combos, combos-total, two-stroke-combos, conditional-combos)");
            cm.combos_map .borrow() .iter() .map ( |(c,cvs)| {
                let conds = cvs.iter().filter(|cv| cv.cond.is_some()).count();
                let tscs = cvs.iter().filter(|cv| !cv.fsc.is_empty()).count();
                let fscs = cvs.iter().filter(|cv| cv.is_fsc).count();
                //note : same combos might be both tscs and cond .. i.e adding the ones above can be > cvs.len()
                format! ("fsc: {:?}, tot: {:?}, tsc: {:?}, cond: {:?}   {:?}", fscs, cvs.len(), tscs, conds, c)
            } ) .sorted() .for_each (|s| println!("{}",s));
            println! ("nTot = {:?}", cm.combos_map.borrow().len());

            println! ("combo counts by combo-map-key:");
            cm.combos_map .borrow() .keys() .map(|c| c.cmk) .counts() .iter()
                .map (|(cmk,n)| format!("  {:3}  {:?}", n, cmk)) .sorted() .for_each (|s| println!("{}",s));

            println! ("\nwildcarded combos:");
            cm.wildcard_combos.borrow() .values() .flatten()
                .map (|(c,_cs)| format!("  {:?}",c)) .sorted() .for_each (|s| println!("{}",s));

            println! ("\nfirst-stroke-combo registrations:");
            cm.combos_map .borrow() .iter()
                .filter (|(_c,cvs)| cvs.iter().any(|cv| cv.is_fsc))
                .map (|(c,_v)| format!("  {:?}",c)) .sorted() .for_each (|s| println!("{}",s));

            cm.info_print_simult_active_combos_check();
        } );
    }

    pub fn info_print_simult_active_combos_check (&self) {
        let cm = self.clone();
        thread::spawn ( move || {
            thread::sleep (Duration::from_millis(10));  // just to avoid printout garbling at startup
            println! ("## total combos count: {:?}", cm.combos_map.borrow().len());
            println! ("## total combo-map-keys count: {:?}", cm.combos_map.borrow().keys().map(|c| c.cmk).unique().count());
            println! ("## combo-map-keys with wildcards: {:?}", cm.wildcard_combos.borrow().len());

            let fscs_count = cm.combos_map .borrow() .iter() .filter (|(_c,cvs)| cvs.iter().any (|cv| cv.is_fsc)) .count();
            println! ("## first-stroke registrations: {:?}", fscs_count);

            let combos_w_mult_non_cond_cvs = cm .combos_map .borrow() .iter() .map ( |(c,cvs)| {
                (*c, cvs.iter() .filter (|cv| cv.cond.is_none() && cv.fsc.is_empty() && !cv.is_fsc) .count())
            } ) .filter (|(_,n)| *n > 1) .sorted_by_key (|(_,n)| *n) .collect_vec();
            println! ("## combos with multiple non-cond, non-fsc combo value entries each: {:?}", combos_w_mult_non_cond_cvs.len());
            combos_w_mult_non_cond_cvs .iter() .for_each (|(c,n)| println!("  n={:?} : {:?}", n, c));
        } );
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
                _ => None   // no default fallback for key-release types (w/ or w/o syskey)
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



    fn handle_caps_combo_fallback (&self, fbaf:AF, _e:&Event, ks:&KrustyState) {
        // if no combo found while caps down, we want to support most multi-mod combos treating caps as ctrl..
        // (however, we have caps-dn suppress all mod-keys, so we'll have to wrap mod-key up/dn here as necessary)
        // Note that caps combo with mode-state active (incl modekeys themselves) have no fallbacks, they wont even get here
        // further, we'll assume all l2k keys are configured through combo, and we need no fallbacks for them here

        // we'll progressively wrap the actions with the appropriate mod-key actions ..
        let mut af = ks.mod_keys.lctrl.active_action(fbaf);
        if ks.mod_keys.lalt.down.is_set() { af = ks.mod_keys.lalt.active_action(af) }
        if ks.mod_keys.some_win_down()    { af = ks.mod_keys.lwin.active_action(af) }
        if ks.mod_keys.some_shift_down() || ks.mod_keys.ralt.down.is_set() {
            af = ks.mod_keys.lshift.active_action(af)
        }
        // and finally we just exec the layered action
        af();
    }


    // For the actual matched combo, we still filter actual execution by any specified repeat-supression
    fn exec_combo_value (&self, cv:&ComboValue, ev:&Event) {
        if cv.no_rpt {
            if let EventDat::key_event {is_repeat, ..} = ev.dat {
                if is_repeat { return }
            }
        }
        cv.af.as_ref()();
    }


    // Matched Combo-Values/AFs processing : exec applicable combo-actions (w/ conditionals if any), return whether any AF was executed.
    //
    // Note that combo-values for each combo-key are prior sorted by .. first-stroke req, then regular cond, then non-cond by timestamp
    //  .. this ensures determinism, and since the non-conditional AFs are at the end, allows us to only run those if no conditions matched.
    // First-Stroke Combo (fsc) req matching rules :
    // - combo-values with fsc req get sorted topmost .. (but there could be fsc w/ or w/o additional cond)
    // - if fsc present and doesnt match, we skip that cv
    // - if fsc matched and execd, we can only go through subset that also have the matching fsc specified
    // - (note .. given fsc w cond, where fsc matched but cond did not, we'd still continue checking non-fsc .. follows least-surprise)
    // - (note also, that our fscs are modkey-sticky .. ie, a triggered fsc remains active until all held-modkeys are released .. non-modkey fscs are pointless)
    // Condition Matching (cond) rules :
    // - we execute AFs for all matching coditional-combos OR all matching non-conditional combos
    // - however, if any conditional combo triggers, then any remaining non-conditional combos are ignored
    // (This allows for ergonomic declaration and use of base-case actions and special conditional-case actions).
    //
    fn process_combo_afs (&self, cvs:&Vec<ComboValue>, ev:&Event, ks:&KrustyState) -> ProcCVsResult {
        let mut cond_matched = false;
        let mut proc_res = ProcCVsResult::default();
        for cv in cvs {
            // first we enforce rules for combos with first-stroke-combo reqs specified
            if !cv.fsc.is_empty() {
                proc_res.fscs_found = true;
                if !ks.first_stroke.check_match(cv.fsc) { continue }
                // ^^ we found a fsc for this cv .. so if we dont match it, we should skip it
            }
            else if proc_res.fscs_found && proc_res.combo_execd {
                // ^^ this cv didnt have a fsc, but some prior fsc existed, and we've execd on some cv for this combo earlier ..
                // .. and since fscs sort up top, nothing afterwards is now worth checking ..
                return proc_res
            }
            // so by here, either we're the a matched combo, or no fsc existed or matched for this and we're checking non-fsc cvs
            // so now we can simply process based on regular conditonals rules
            if let Some(cond) = cv.cond.as_ref() {
                // all conditional combos that are satisfied can be run
                if cond(ks,ev) {
                    cond_matched = true;
                    proc_res.combo_execd = true;
                    self.exec_combo_value (cv, ev);
                }
            } else if !cond_matched {
                // all non-conditional combos can also be run, but only if no conditional combos (which sort above them) were satisfied
                proc_res.combo_execd = true;
                self.exec_combo_value (cv, ev);
            }
        }
        proc_res
    }

    // Exact Combo Matching : we try directly looking up a combo and executing it
    fn try_proc_combo_afs (&self, combo:&Combo, ev:&Event, ks:&KrustyState) -> ProcCVsResult {
        //let pcm = self.combos_map.borrow();
        // ^^ the borrow would be fine too, but there's really no need for any guarding as we dont do any writes at runtime ..
        // .. hence we might as well directly read from the map and avoid the (minor) atomic borrow-check overhead
        let pcm  = unsafe { & *self.combos_map.as_ptr() };
        let mut proc_res = ProcCVsResult::default();
        if let Some(cvs) = pcm.get(combo) {
            proc_res = self.process_combo_afs (cvs, ev, ks);
        }
        proc_res
    }

    // Wild-Card Combo Matching :
    // - we first check the wildcard-combos map to get wildcard combos (if any) for this particular combo-maps-key
    //   .. this keeps it efficient for most typical use-cases (which have no wildcards)
    // - the wildcards, and the base bits are bit-packed, so a simple bit-and with the wildcards and cur-combo should match the base combo bits
    // - actual combos used as keys in combo-maps are stripped of wildcards (mask set to FFs)
    // - the actual wildcarded combos are stored in the wildcard_combos table, with the EvCbMapKey alone as key (no bit-fields)
    // - so for wc proc, we check cur cmk in wc-table, if found, we search through the wc combos under that cmk for wc-match w cur combo
    // - then if we found a cur-combo matching wc-combo, we use its wc-stripped version to lookup the actual combos_map for the combo-values!
    //
    fn try_proc_wildcard_combo_afs (&self, cmk:&EvCbMapKey, combo:&Combo, ev:&Event, ks:&KrustyState) -> ProcCVsResult {
        let cwm = unsafe { & *self.wildcard_combos.as_ptr() };
        let mut proc_res = ProcCVsResult::default();
        if let Some(cs) = cwm.get(cmk) {    // get list of wildcard combos (if any) for this particular combo-maps-key
            cs .iter() .filter (|(c,_wcsc)| c.check_wildcard_eqv (combo)) .for_each (|(_c,wcsc)| {
                // found a match in wc-combos table, now gotta lookup into actual combo table w its wc-stripped version as key
                // (the wc-stripped-match != cur-combo below is because then we'd have already found/execd it earlier w/o wc-matching)
                if *wcsc != *combo {
                    proc_res = self.try_proc_combo_afs (wcsc, ev, ks)
                }
            } );
        }
        proc_res
    }


    /// combos (and fallback) action handler for current key-event, based on current modes/mod-key states
    pub fn combo_maps_handle_input (&self, cmk:EvCbMapKey, ev:&Event) {
        //println! ("combo-map-key: {:?}", cmk);
        // we'll assume that by the time we're here, callbacks for modifier-keys and mode-keys have already updated their flags
        // note also, that from binding setup, we shouldnt get modifier keys or caps sent here for processing

        let ks = &KrustyState::instance();
        let combo = Combo::gen_cur_combo (cmk, ks);

        //println! ("{:?}",combo);

        // Combo-processing order :
        // - First we run any exact match combos .. (any first-stroke reqd combos, then conditionals if satisfied, else non-conditionals if none matched)
        // - Next, we'll run any wildcard combos that might match .. (same with conditionals exclusivity among them)
        // - First-stroke-reqd combos have exclusivity, so if they match first, wildcard combos matching isnt performed (treats fscs as a 'mode')
        // if we found/executed something so far, we dont need any fallback processing, and can return

        let proc_res = self.try_proc_combo_afs (&combo, ev, ks);

        if proc_res.fscs_found && proc_res.combo_execd { return }

        let wc_proc_res = self.try_proc_wildcard_combo_afs (&cmk, &combo, ev, ks);

        if proc_res.combo_execd || wc_proc_res.combo_execd { return }

        // - Else if some latch state was active, we can try to match a combo ignoring latches (as fallback)
        //   (And we'll do the same as above here, w direct matches first, w global/local exclusivity to fscs/conditionals, then check wildcards similarly)
        // Note that we're doing the 'check again w/o latch' coz its more efficient than trying to default all latch combos to have no-latch wildcards ..
        // .. coz the cur impl requires linear search to match wildcard combos (within the subset for that particular cmk w wildcard combos)

        if ks.mode_states.some_latch_state_active.is_set() {

            let combo_no_latch = Combo::gen_no_latch_combo(combo);

            let proc_res = self.try_proc_combo_afs (&combo_no_latch, ev, ks);

            if proc_res.fscs_found && proc_res.combo_execd { return }

            let wc_proc_res = self.try_proc_wildcard_combo_afs (&cmk, &combo_no_latch, ev, ks);

            if proc_res.combo_execd || wc_proc_res.combo_execd { return }
        }

        // - And finally, if neither direct lookups, nor lookups ignoring any active latch state found anything to run (with and without wildcards)
        // .. then we'll resort to fallback action generation and processing

        // but first, lets also filter out any automatic fallbacks for ..
        // .. caps-dbl, ralt-dbl combos in all cases .. and some mode-state (and mode-state-dbl) when with caps down
        // .. (reminder that [EDFRQ1234]_dbl, potentially w shift/ralt etc can trigger during normal typing and must be allowed)
        if ks.mod_keys.caps.dbl_tap.is_set()
            || ks.mod_keys.ralt.dbl_tap.is_set()
            || ( ks.mode_states.some_mode_state_active.is_set() && ks.mod_keys.caps.down.is_set() )
        { return }

        let fbaf = self.gen_fallback_base_af (ks.clone(), ev);
        if fbaf.is_none() { return }
        // ^^ if we explicitly didnt want to do anything, no point trying to wrap mods below etc
        let fbaf = fbaf.unwrap();

        if ks.mod_keys.caps.down.is_set() {
            // unregistered caps-combos have extensive fallback setups
            self.handle_caps_combo_fallback (fbaf, ev, ks);
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
            // others, incl single/double ctrl/shift/lalt/no-mod presses should all work naturally via passthrough
            fbaf()
        }
    }



}
