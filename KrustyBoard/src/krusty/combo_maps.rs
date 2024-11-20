#![ allow (non_camel_case_types) ]

use std::sync::Arc;
use std::{thread, time::Duration};

use atomic_refcell::AtomicRefCell;
use derive_deref::Deref;
use itertools::Itertools;
use once_cell::sync::OnceCell;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::*;


# [ derive (Debug, Eq, PartialEq, Hash, Copy, Clone) ]
pub struct WcCombosMapKey {
    bmk : BindingsMapKey,
    first_stroke : ComboHash,
}
impl WcCombosMapKey {
    pub fn new (bmk:BindingsMapKey, first_stroke:ComboHash) -> WcCombosMapKey {
        WcCombosMapKey { bmk, first_stroke }
    }
}

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
    wildcard_combos : AtomicRefCell <FxHashMap <WcCombosMapKey, Vec<(Combo,Combo)>>>,

    /// holds a registry for keys that only need default/fallback bindings
    handled_keys_set : AtomicRefCell <FxHashSet <Key>>,
}
# [ derive (Clone, Deref) ]
pub struct CombosMap ( Arc <_CombosMap> );




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
        //self.handled_keys_set .borrow() .contains (key)
        unsafe { & *self.handled_keys_set.as_ptr() } .contains(key)
        // ^^ we access this without guards as it never gets written to during runtime (and it is in hotpath)
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



    /// Registers a combo as a possible 'sticky-first-stroke-combo' (sfsc), and returns its combo-hash. <br>
    /// The combo-hash returned by this fn must be provided as the first-stroke when defining two-stroke-combos. <br>
    /// Note that sfscs are active only while some-modkey is held, and therefore only a combo with some modkey can be a valid sfsc
    pub fn register_combo_sticky_first_stroke (&self, cg: impl Into<CG>) -> ComboHash {
        let cg = cg.into();
        let fsc = Combo::gen_fsc_hash(cg.clone());
        self.setup_af_sticky_first_stroke (cg, fsc);
        fsc
    }
    /// Co-Registers a possible first-stroke combo as an alternate for another sticky first-stroke with the combo-hash supplied
    pub fn co_register_combo_sticky_first_stroke (&self, cg: impl Into<CG>, fsc:ComboHash) {
        self.setup_af_sticky_first_stroke (cg.into(), fsc);
    }
    fn setup_af_sticky_first_stroke (&self, cg:CG, fsc:ComboHash) {
        let ks = cg.ks.clone();
        let af = Arc::new ( move || {
            // we'll check some mk-down for safety, as any recorded fsc only clears on all-modkeys-released ..
            // (fscs are required to have some mod-key in them and are active until all modkeys are released)
            if ks.mod_keys.some_mk_down() {
                ks.sticky_first_stroke.store(fsc);
                jiggle_cursor(1)
            }
        } );
        self._add_combo (cg, ag().af(af), true);
    }


    /// Registers a combo as a possible 'latching-first-stroke-combo' (lfsc), and returns its combo-hash. <br>
    /// The combo-hash returned by this fn must be provided as the first-stroke when defining two-stroke-combos. <br>
    /// Note that lfscs remain active upon triggering until clear-latching-first-stroke is triggered
    pub fn register_combo_latching_first_stroke (&self, cg: impl Into<CG>) -> ComboHash {
        let cg = cg.into();
        let fsc = Combo::gen_fsc_hash(cg.clone());
        self.setup_af_latching_first_stroke (cg,fsc);
        fsc
    }
    /// Co-Registers a possible first-stroke combo as an alternate for another latching first-stroke with the combo-hash supplied
    pub fn co_register_combo_latching_first_stroke (&self, cg: impl Into<CG>, fsc:ComboHash) {
        self.setup_af_latching_first_stroke (cg.into(), fsc);
    }
    fn setup_af_latching_first_stroke (&self, cg:CG, fsc:ComboHash) {
        let ks = cg.ks.clone();
        let af = Arc::new ( move || {
            ks.latching_first_stroke.store(fsc);
            jiggle_cursor(2);
        } );
        self._add_combo (cg, ag().af(af), true);
    }


    /// Registers a combo to CLEAR any active latching-first-stroke-combo
    pub fn register_combo_clear_latching_first_stroke (&self, cg: impl Into<CG>) {
        let cg = cg.into();
        let ks = cg.ks.clone();
        let af = Arc::new ( move || {
            ks.latching_first_stroke.clear();
            jiggle_cursor(3);
        } );
        self._add_combo (cg, ag().af(af), true);
    }



    fn add_to_wildcards_map (&self, c:Combo) {
        let mut wcm = self.wildcard_combos.borrow_mut();
        let pair = (c, c.strip_wildcards());
        let wcmk = WcCombosMapKey::new (c.bmk, c.first_stroke);
        if let Some(cs) = wcm.get_mut(&wcmk) {
            if !cs.contains(&pair) { cs.push(pair) }
        } else {
            wcm.insert (wcmk, vec![pair]);
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
        if let BindingsMapKey::key_ev_t (key, ..) = c.bmk {
            self.add_to_handled_keys_set (key);
        }
        // and finally, we can add the combo to our combos map
        let mut cm = self.combos_map.borrow_mut();
        //self.combos_map.write().unwrap() .insert (c, cv);
        if let Some(cvs) = cm.get_mut(&c) {
            // note that we allow multiple conditional or mult non-conditional combos to trigger ..
            // .. but if any conditional combo triggers, then non-conditional combos for that are ignored
            cvs.push(cv);
            cvs.sort_by_cached_key (|cv| (cv.cond.is_none(), cv.stamp));
            // ^^ we want to sort such that conditionals are up top .. (hence the is_none supplied)
        } else {
            cm.insert (c, vec![cv]);
        }
    }





    pub fn debug_print_combos_map (&self) {
        let cm = self.clone();
        thread::spawn ( move || {
            println! ("\nCombo entries and their combo-values counts \n# (first-stroke-combos, combos-total, conditional-combos)");
            cm.combos_map .borrow() .iter() .map ( |(c,cvs)| {
                let conds = cvs.iter().filter(|cv| cv.cond.is_some()).count();
                let fscs = cvs.iter().filter(|cv| cv.is_fsc).count();
                //note : same combos might be both tscs and cond .. i.e adding the ones above can be > cvs.len()
                format! ("fsc: {:?}, tot: {:?}, cond: {:?}   {:?}", fscs, cvs.len(), conds, c)
            } ) .sorted() .for_each (|s| println!("{}",s));
            println! ("nTot = {:?}", cm.combos_map.borrow().len());

            println! ("combo counts by underlying bindings-map-key:");
            cm.combos_map .borrow() .keys() .map(|c| c.bmk) .counts() .iter()
                .map (|(bmk,n)| format!("  {:3}  {:?}", n, bmk)) .sorted() .for_each (|s| println!("{}",s));

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
            println! ("## total combo-map-keys count: {:?}", cm.combos_map.borrow().keys() .map (|c| c.bmk) .unique() .count());
            println! ("## two-stroke combos count: {:?}", cm.combos_map.borrow().keys() .filter (|c| !c.first_stroke.is_empty()) .count());
            println! ("## combo-map-keys with wildcards: {:?}", cm.wildcard_combos.borrow().len());

            let fscs_count = cm.combos_map .borrow() .values() .filter (|cvs| cvs.iter().any (|cv| cv.is_fsc)) .count();
            println! ("## first-stroke registrations: {:?}", fscs_count);

            let combos_w_mult_non_cond_cvs = cm .combos_map .borrow() .iter() .map ( |(c,cvs)| {
                (*c, cvs.iter() .filter (|cv| cv.cond.is_none() && !cv.is_fsc) .count())
            } ) .filter (|(_,n)| *n > 1) .sorted_by_key (|(_,n)| *n) .collect_vec();
            println! ("## combos with multiple non-cond combo value entries each: {:?}", combos_w_mult_non_cond_cvs.len());
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
                        if let Some(bs) = ks.mouse.get_btn_state(btn) {
                            bs.active.set(); bs.btn.press();
                    } } ) )
                }
                MouseBtnEv_T::BtnUp   => {
                    Some ( Arc::new ( move || {
                        if let Some(bs) = ks.mouse.get_btn_state(btn) {
                            if bs.active.is_set() { bs.active.clear(); bs.btn.release(); }
                    } } ) )
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
    // Note that combo-values for each combo-key are prior sorted by .. first conditionals, then non-cond by timestamp
    //  .. this ensures determinism, and since the non-conditional AFs are at the end, allows us to only run those if no conditions matched.
    //
    // Condition Matching (cond) rules :
    // - we execute AFs for all matching coditional-combos OR all matching non-conditional combos
    // - however, if any conditional combo triggers, then any remaining non-conditional combos are ignored
    // (This allows for ergonomic declaration and use of base-case actions and special conditional-case actions).
    //
    // Note that this is repeated for each category of first-stroke-combo (fsc) [sticky, latched, no-fsc], and with and w/o wildcards ..
    // However, if any fsc-stage executed either a direct-match or wildcard-match, then the rest of the fsc stages are ignored
    //
    fn process_combo_afs (&self, cvs:&Vec<ComboValue>, ev:&Event, ks:&KrustyState) -> bool {
        let mut cond_matched = false;
        let mut combo_execd = false;
        for cv in cvs {
            if let Some(cond) = cv.cond.as_ref() {
                // all conditional combos that are satisfied can be run
                if cond(ks,ev) {
                    cond_matched = true;
                    combo_execd = true;
                    self.exec_combo_value (cv, ev);
                }
            } else if !cond_matched {
                // all non-conditional combos can also be run, but only if no conditional combos (which sort above them) were satisfied
                combo_execd = true;
                self.exec_combo_value (cv, ev);
            }
        }
        combo_execd
    }

    // Exact Combo Matching : we try directly looking up a combo and executing it
    fn try_proc_combo_afs (&self, combo:&Combo, ev:&Event, ks:&KrustyState) -> bool {
        //let pcm = self.combos_map.borrow();
        // ^^ the borrow would be fine too, but there's really no need for any guarding as we dont do any writes at runtime ..
        // .. hence we might as well directly read from the map and avoid the (minor) atomic borrow-check overhead
        let pcm  = unsafe { & *self.combos_map.as_ptr() };
        let mut combo_execd = false;
        if let Some(cvs) = pcm.get(combo) {
            combo_execd = self.process_combo_afs (cvs, ev, ks);
        }
        combo_execd
    }

    // Wild-Card Combo Matching :
    // - we first check the wildcard-combos map to get wildcard combos (if any) for this particular combo-maps-key
    //   .. this keeps it efficient for most typical use-cases (which have no wildcards)
    // - the wildcards, and the base bits are bit-packed, so a simple bit-and with the wildcards and cur-combo should match the base combo bits
    // - actual combos used as keys in combo-maps are stripped of wildcards (mask set to FFs)
    // - the actual wildcarded combos are stored in the wildcard_combos table, with the WcCombosMapKey as key (i.e no bit-fields)
    // - so for wc proc, we check cur wc-map-key in wc-table, if found, we search through the wc combos under that wcmk for wc-match w cur combo
    // - then if we found a cur-combo matching wc-combo, we use its wc-stripped version to lookup the actual combos_map for the combo-values!
    //
    fn try_proc_wildcard_combo_afs (&self, wcmk:WcCombosMapKey, combo:&Combo, ev:&Event, ks:&KrustyState) -> bool {
        let cwm = unsafe { & *self.wildcard_combos.as_ptr() };
        let mut combo_execd = false;
        if let Some(cs) = cwm.get(&wcmk) {    // get list of wildcard combos (if any) for this particular combo-maps-key
            cs .iter() .filter (|(c,_wcsc)| c.check_wildcard_eqv (combo)) .for_each (|(_c,wcsc)| {
                // found a match in wc-combos table, now gotta lookup into actual combo table w its wc-stripped version as key
                // (the wc-stripped-match != cur-combo below is because then we'd have already found/execd it earlier w/o wc-matching)
                if *wcsc != *combo {
                    combo_execd = self.try_proc_combo_afs (wcsc, ev, ks)
                }
            } );
        }
        combo_execd
    }

    // Combo matching rules (w/ or w/o wilcards) :
    // - First we try to directly match the combo into the combos-map table
    // - Next we'll try to match wildcard combos (for the same fsc state .. i.e [sticky, latched, no-fsc])
    // - (Note that under any fsc category, wildcard-combos can run even after direct-match combos have been matched and ran)
    //
    fn try_proc_fsc_combo (&self, combo:&Combo, ev:&Event, fsc:ComboHash, ks:&KrustyState) -> bool {

        let combo = Combo::gen_fsc_combo (combo, fsc);
        let combo_execd = self.try_proc_combo_afs (&combo, ev, ks);

        let wcmk = WcCombosMapKey::new (combo.bmk, fsc);
        let wc_combo_execd = self.try_proc_wildcard_combo_afs (wcmk, &combo, ev, ks);

        combo_execd || wc_combo_execd
    }

    /// combos (and fallback) action handler for current key-event, based on current modes/mod-key states
    pub fn combo_maps_handle_input (&self, bmk:BindingsMapKey, ev:&Event) {
        //println! ("combo-map-key: {:?}", bmk);
        // we'll assume that by the time we're here, callbacks for modifier-keys and mode-keys have already updated their flags
        // note also, that from binding setup, we shouldnt get modifier keys or caps sent here for processing

        let ks = &KrustyState::instance();
        let combo = Combo::gen_cur_combo (bmk, ks);

        //println! ("{:?}",combo);

        // Combo-processing order w respect to first-stroke-combos [sticky-fsc, latching-fsc, no-fsc] :
        // - We first try to match sticky-fscs (if a sticky-fsc cur active) .. (separately for direct-match, and wild-card match)
        // - If still no match, we'll check with latching-fscs if one active .. (again direct and wildcarded)
        // - Else, we'll finally search for non-fsc (i.e. normal) combos
        // (Note that this order means that sfsc combos override lfsc combos, and there can be no layering of sfsc on lfsc etc)
        // (At each of these stages, if we already found and executed a combo, we return as we dont want to fallback to lower categories)

        if !ks.sticky_first_stroke.is_empty() && self.try_proc_fsc_combo (&combo, ev, ks.sticky_first_stroke.get(), ks) { return }

        if !ks.latching_first_stroke.is_empty() && self.try_proc_fsc_combo (&combo, ev, ks.latching_first_stroke.get(), ks) { return }

        if self.try_proc_fsc_combo (&combo, ev, ComboHash::default(), ks) { return }


        // - finally if no lookups (w/ and w/o wildcards) found anything to run, we'll try fallback action gen and processing
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
