#![ allow (non_snake_case, non_upper_case_globals, unused_doc_comments) ]


use std::{ time::Duration, thread, sync::{Arc}, sync::atomic::Ordering};
use once_cell::sync::OnceCell;

use krustyboard::{*, utils::*, key_utils::*, KbdKey::*, MouseButton::*, ModKey::*, ModeState_T::*, WinGroups_E::*};





/// handling for any 'special' keys that need to be bound/handled directly (like for mouse btns) rather than via combo-maps
pub fn setup_direct_binding_keys (_k:&Krusty) {

    // currently, we dont need to do it for anything .. (was more essential when mod-tracking / combo-maps mechanisms were more limited)

    // HOWEVER, there might be cases where it might be simpler to just do direct bindings than to populate all the combo maps etc etc
    // ALSO, for things that require actual action in key-up callbacks, we'd do it direclty here because key-up actions arent specifically
    // .. registered in maps and so combo-maps composed callbacks can only either set key-up to do nothing, or at most clear mode flags

    // NOTE that since combo processing is optional and happens after direct kbd binding cbs, they can still be used despite direct bindings here

    // We used to do unstick-all here, but we have since added support for combo defs with wildcards, and that covers this usecase as well

}


/*
General Principles On Combo Mapping Allocation
    - Caps, if no other contention, will be Ctrl ..  Ralt will be Shift .. Lalt can stay Alt
    - Mode-Key behavior caveats [Q,1,2,3,4,E,D,F,R] :
        - sadly due to the 2wsx hardware issue,  while qks2 is down, qks[1/3/4] WILL NOT layer on it!
        - the mode-keys themselves when with caps/modkey have repeat auto suppressed, and w/ caps they have fallbacks auto suppressed
        - when not w caps/modkey, mode-key consumption is auto marked in combo declarations (will suppress repeats unless otherwise specified)
    - Qks [q,1,2,3,4] keys :
        - qks1 (Numrow_1) can be Ctrl eqv when caps is otherwise used up .. combo fallbacks will auto-gen that if nothing defined
        - qks2, qks3, qks4 are usually for app/ide/context specific setups ..
        - 1/2/3 also overload for vol/brightness w Win/Alt .. F1/F2/F3 for media ctrl w Win
    - Layer-2 mode-state Keys :
        - [E,D,F,R] are for L2 [sel, del, word-fast, 2x-fast] .. and in general E for edit/sel etc, F for faster navs
    - Layer-2 Keys :
        - w/ caps, [J, K, I, Comma, H, L, U, M] will generally be nav keys (left/right, up/down, home/end, pg-up/pg-dn]
        - these will also be primary allocations for those keys, and when w caps etc their own behavior will be secondary
        - (Note that there's nothing internally special about these .. unlike qks or mode keys whose states get tracked)
    - for some keys w multiple allocations, Q can bring up secondary allocations .. (e.g for wheel-to-arrows)
    - mouse wheels should mostly align with these broad strokes .. more details in the mouse-wheel section
    - where applicable, v-wheel should map to up/down nav and h-wheel should map to left/right nav
    - braces should map to braces nav, slash/backslash and/or +/- to tree expand/contract etc etc
 */




// we'll define some common combo-conds that can be reused later

#[allow (dead_code)] fn c_true()  -> ComboCond { Arc::new ( move |_,_| { true  } ) }
#[allow (dead_code)] fn c_false() -> ComboCond { Arc::new ( move |_,_| { false } ) }

#[allow (dead_code)] fn c_flag   (flag:Flag) -> ComboCond { Arc::new ( move |_,_| { flag.is_set() } ) }
#[allow (dead_code)] fn c_flag_n (flag:Flag) -> ComboCond { Arc::new ( move |_,_| { flag.is_clear() } ) }

#[allow (dead_code)] fn af_set_flag   (flag:Flag) -> AF { Arc::new ( move || flag.set() ) }
#[allow (dead_code)] fn af_clear_flag (flag:Flag) -> AF { Arc::new ( move || flag.clear() ) }


fn check_switche_fgnd (wel:&WinEventsListener) -> bool {
    wel.fgnd_info.read().unwrap().exe == "Switche.exe"
}
fn check_alt_tab_fgnd (wel:&WinEventsListener) -> bool {
    wel.fgnd_info.read() .is_ok_and ( |fi| {
        fi.class == "XamlExplorerHostIslandWindow" || fi.class == "MultitaskingViewFrame"
} ) }
fn check_intellij_fgnd (wel:&WinEventsListener) -> bool {
    wel.fgnd_info.read().unwrap().exe == "idea64.exe"
}
fn _check_chrome_fgnd (wel:&WinEventsListener) -> bool {
    wel.fgnd_info.read().unwrap().exe == "chrome.exe"
}
fn check_browser_fgnd (wel:&WinEventsListener) -> bool {
    wel.fgnd_info.read() .is_ok_and (|fi| { fi.exe == "chrome.exe" || fi.exe == "msedge.exe" } )
}

/// the idea here is to clone the listener Arc once during cond-creation, to avoid calling instance() repeatedly during runtime
fn win_evs_cond <WFN> (wel:&WinEventsListener, wfn:WFN) -> ComboCond
    where WFN : Fn(&WinEventsListener) -> bool + Send + Sync + 'static
{
    let wel = wel.clone();
    Arc::new ( move |_,_| { wfn(&wel) } )
}
#[allow (dead_code)] fn intellij_fgnd (k:&Krusty)  -> ComboCond { win_evs_cond (&k.wel, check_intellij_fgnd ) }
#[allow (dead_code)] fn browser_fgnd  (k:&Krusty)  -> ComboCond { win_evs_cond (&k.wel, check_browser_fgnd ) }
#[allow (dead_code)] fn switche_fgnd  (k:&Krusty)  -> ComboCond { win_evs_cond (&k.wel, check_switche_fgnd ) }
#[allow (dead_code)] fn alt_tab_fgnd  (k:&Krusty)  -> ComboCond { win_evs_cond (&k.wel, check_alt_tab_fgnd ) }

#[allow (dead_code)] fn intellij_not_fgnd (k:&Krusty)  -> ComboCond { win_evs_cond (&k.wel, |wel| !check_intellij_fgnd (wel) ) }
#[allow (dead_code)] fn browser_not_fgnd  (k:&Krusty)  -> ComboCond { win_evs_cond (&k.wel, |wel| !check_browser_fgnd (wel) ) }
#[allow (dead_code)] fn switche_not_fgnd  (k:&Krusty)  -> ComboCond { win_evs_cond (&k.wel, |wel| !check_switche_fgnd (wel) ) }
#[allow (dead_code)] fn alt_tab_not_fgnd  (k:&Krusty)  -> ComboCond { win_evs_cond (&k.wel, |wel| !check_alt_tab_fgnd (wel) ) }





// we'll also define some fns for brightness/media etc control to be reused by kbd/mouse combos etc
// note that in these, although we're using win-combos, we dont have to wrap in win-action guards as win is Modkey_Doubled

fn gen_af_incr_brightness (step:i32) -> AF {
    Arc::new ( move || { let _ = incr_brightness(step); } )
}

// skips work by alt-ctrl-volUp (needs to guard win-inactive since its on win-combo)
fn media_skips_action (n_skips:u32, ks:&KrustyState, fwd_not_bkwd:bool) -> AF {
    let action_key = if fwd_not_bkwd {VolumeUp} else {VolumeDown};
    //ks.mod_keys.lwin.inactive_action ( ks.mod_keys.lalt.active_action ( ks.mod_keys.lctrl.active_action (
    ks.mod_keys.lalt.active_action ( ks.mod_keys.lctrl.active_action (
        Arc::new ( move || { (0 .. n_skips) .for_each (|_| { action_key.press_release() }) } )
) ) }

// media next/prev work via alt-shift-vol-up/dn as configured in musicbee etc
fn media_next_action (ks:&KrustyState, next_not_prev:bool)  -> AF {
    let media_next_af = {
        if next_not_prev { ag().k(VolumeUp  ).m(lalt ).m(lshift).gen_af() }
        else             { ag().k(VolumeDown).m(lalt ).m(lshift).gen_af() }
    };
    let media_next_skips_af = media_skips_action (2, ks, true);
    Arc::new ( move || {
        media_next_af();
        let mnsaf = media_next_skips_af.clone();  // clone again to move into spawned thread (spawned since combos run in single queued side-thread)
        thread::spawn ( move || { thread::sleep(Duration::from_millis(2000));  mnsaf(); } );
} ) }





/// caps-d wheel to directly **_ SWITCH WINDOWS _** through cur window-list snapshot (via switche)
// (and instead of adding a flag in krusty itself to track while we're in this mode, we'll just define a flag here)
fn get_switche_snap_switch_flag () -> Flag {
    // alt-tab snapshot switch mode flag .. we'll check this to take a snapshot everytime we start
    static SWITCHE_SNAPSHOT_FLAG : OnceCell<Flag> = OnceCell::new();
    SWITCHE_SNAPSHOT_FLAG .get_or_init (Flag::default) .clone()
}
fn gen_af_switche_snap_switch (dir_down:bool, flag:Flag) -> AF {
    let refresh_af = ag().k(F15).m(alt).m(shift).gen_af();
    let nav_key = if dir_down { F16 } else { F17 };
    let nav_af = ag().k(nav_key).m(alt).m(shift).gen_af();
    Arc::new ( move || {
        if flag.is_clear() {  flag.set(); refresh_af(); }
        thread::sleep (Duration::from_millis(15));   // to give time for the win-enum snap to be taken
        nav_af();
    } )
}




fn setup_default_keys  (k:&Krusty) {

    // we'll setup most keys via key-combo action maps that we'll compose into relevant callbacks after all mapping is registered
    // HOWEVER, there are some keys (incl those that look for shift/ctrl) that will be set directly at the end after all the action-map setups

    // in addition, we'll want to bind MOST keys so default actions for things like ralt or caps combos are generated for them even if we
    // >  dont have any special combos to setup for them .. to keep combos tables light, we'll register everything there first

    // NOTE that if we wanted special-keys-setups (which we dont currently for any key), we should do that after all binding is done so as to
    // >  overwrite what default bindings would otherwise be generated .. (we currently invoke that at the end, even though its not yet needed)

    // NOTE that fallback defaults are : ralt-as-shift, caps-as-ctrl, caps-ralt/shift/alt/win as ctrl-shift/shift/alt/win

    let char_keys  = "qwertasdfgzxcvb`123456yuiop[]\\hjkl;\'nm,./7890-=" .chars() .filter_map(Key::from_char);
    let fnum_keys  = (u64::from(F1) .. u64::from(F24)) .map(Key::from);
    let nav_keys   = [Left, Right, Up, Down, PageUp, PageDown, Home, End];
    let spcl_keys  = [Backspace, Delete, Space, Tab, Enter, Escape, Insert, Apps];
    //let media_keys = [BrowserBack, BrowserForward, BrowserRefresh, VolumeMute, VolumeDown, VolumeUp,
    //                  MediaNextTrack, MediaPrevTrack, MediaStop, MediaPlayPause];
    //let mouse_keys = [MouseLeftBtn, MouseRightBtn, MouseMiddleBtn, MouseX1Btn, MouseX1Btn];

    char_keys .chain (fnum_keys) .chain (nav_keys) .chain (spcl_keys) .for_each ( |key| {
        k.cm .add_to_handled_keys_set (key);
    } );
    // ^^ we can ofc put combos for these later in code .. all these do is register for default binding if no combo gets mapped!

}



fn setup_unstick_all  (k:&Krusty) {
    // we want to set up a combo to unstick-all in case we get into weird states due to other hooks stealing/suppressing key events etc
    // lets do dbl-caps (Insert) for reset .. (Insert because End is on Fn key F12, Insert is direct key on this pc)
    // note that since we want the combo to be active even in presence of 'stuck' combo keys etc, we want to define that w global wildcards
    let ks = k.ks.clone(); let clear = Arc::new (move || ks.unstick_all());
    k.cm .add_combo ( cg().k(Insert).m(caps_dbl).wcma().wcsa(),  ag().af(clear) );

    // could prob add something simple to quit too? .. and thatd be easier coz expectation is usage while nothing-stuck?

    /// debug printout of cur state
    let ks = k.ks.clone(); let print_ks = Arc::new (move || println!("{:#?}",ks));
    k.cm .add_combo ( cg().k(F10).no_rpt().m(caps_dbl),  ag().af (print_ks.clone()) );     // caps-dbl-F10 -> debug-printout_ks
    k.cm .add_combo ( cg().k(F10).no_rpt().m(lalt_dbl),  ag().af (print_ks.clone()) );     // lalt-dbl-F10 -> debug-printout_ks
    // and of the combo-maps table itself
    let cm = k.cm.clone(); let print_cm = Arc::new (move || cm.debug_print_combos_map());
    k.cm .add_combo ( cg().k(F9).no_rpt().m(caps_dbl),  ag().af (print_cm) );    // caps-dbl-F9 -> debug-printout_cm
}

fn setup_latching_first_stroke_clear (k:&Krusty) {
    // we'll setup a number of combos to clear active latching fsc .. can cut it down later once we find out which ones see usage
    // .. caps-dbl-F12, caps-dbl-Esc, alt-dbl-Esc, caps-dbl-e-o
    k.cm .register_combo_clear_latching_first_stroke ( cg().k(F12   ).no_rpt().m(caps_dbl) );
    k.cm .register_combo_clear_latching_first_stroke ( cg().k(Escape).no_rpt().m(caps_dbl) );
    k.cm .register_combo_clear_latching_first_stroke ( cg().k(Escape).no_rpt().m(lalt_dbl) );
    k.cm .register_combo_clear_latching_first_stroke ( cg().k(O     ).no_rpt().m(caps_dbl).s(msE) );
}



fn setup_mode_keys (k:&Krusty) {

    fn register_mode_key (k:&Krusty, key:Key, ms_t:ModeState_T) {

        // first we'll do the registration, then we can try and set any auxillary combos here too
        // note that mode-keys down flag will track its physical state, but combo trigger on mode-state requires caps to be down too
        k.ks.mode_states .register_mode_key (key, ms_t);

        // we'll also include this in handled keys (in case it isnt already)
        k.cm.add_to_handled_keys_set (key);

        // since pressing mode-keys sets their flags first, we want to map their own presses w their own flags back to base-action
        // (note the mode-state-kdn_no-consume gets auto added for mode-key triggered combos .. so key-repeats are enabled for base action)
        k.cm .add_combo ( cg().k(key).s(ms_t),  ag().k(key).mkg_nw() );
        // ^^ the modkey-guard-no-wrap is specified to make it explicit, but isnt strictly necessary here as there are no mod-keys when this triggers

        // to avoid stragglers, we'll set the mode-keys pressed w caps to disable their repeat until release (ie. even after caps is released!)
        // note that the following works because any mode-state specified in combo-gen is auto marked for consumption (unless do .msk_nc())
        //k.cm .add_combo ( cg().k(key).m(caps).s(ms_t),  ag().af(no_action()) );
        // ^^ no longer necessary as we disable modkey repeat by default in bindings when w caps
        // (note that repeats are suppressed, but first presses would still come in, but mode-keys w caps dont get any fallback proc either)

        // and we could do the same for alt/win etc, but we'd rather leave those open and they can be done later when/if such combos are set
        // (this allows mod-key combos for mode-keys, and we can disable it only for specific cases (e.g. qks1 during vol ctrl etc)
        //k.cm .add_combo ( cg().k(key).m(lalt).s(ms_t),  ag().af(no_action()).mkg_w() );
        //k.cm .add_combo ( cg().k(key).m(lwin).s(ms_t),  ag().af(no_action()).mkg_w() );

        // note that we want to disable mode-keys across most mod-key combos when caps down ..
        // .. and thats painful to do via combos-maps, so we're now instead just disabling them in runtime fallbacks
        // further, in fallback, we'll also layer base action w mod-keys for these mode-trigger-keys when qks1 down!

        // however, we could at least restore caps-alt-<mode-key> to the expected ctrl-alt by default .. can ofc override these later
        //k.cm .add_combo ( cg().k(key).m(caps).s(ms_t).m(lalt),  ag().k(key).m(ctrl).m(alt) );
        // ^^ naah, some of these we need to be silent and modify other combos (e.g. qks1), so lets do them individually later

        // aight, so at the very least we can open back up the expected caps-as-ctrl behavior for mode-trigger keys on caps-dbl
        //k.cm .add_combo ( cg().k(key).m(caps_dbl),  ag().k(key).m(ctrl) );
        // ^^ naah, even during caps-dbl we'd rather use the mode keys as mode keys, esp since we're so used to that behavior
    }

    // setup keys for layer-2 caret nav msE/msD/msF/msR mode states (typically for l2 sel/del/word/fast nav)
    // note: registering as mode key will set all w/caps actions silent in fallback, along w layering mod-key combos w qks1
    // however, they are all in fallback only, so that behavior will be overridden by any combo registrations!
    // (and just for reminder, in theory we can assign other keys to these modes here w/o much issue)
    register_mode_key ( k, E, msE );
    register_mode_key ( k, D, msD );
    register_mode_key ( k, F, msF );
    register_mode_key ( k, R, msR );

    // setup the key for l4 shortcuts mode, but the mechanism is same as for the caret modes
    register_mode_key ( k, Q,        qks  );
    register_mode_key ( k, Numrow_1, qks1 );
    register_mode_key ( k, Numrow_2, qks2 );
    register_mode_key ( k, Numrow_3, qks3 );
    register_mode_key ( k, Numrow_4, qks4 );


    // we want to overlay some additional combos on some of these w Alt (others that modify other combos should remain silent)
    //k.cm .add_combo ( cg().k(E).m(caps).m(lalt),   ag().k(E).m(ctrl).m(alt) );
    //k.cm .add_combo ( cg().k(D).m(caps).m(lalt),   ag().k(D).m(ctrl).m(alt) );
    //k.cm .add_combo ( cg().k(F).m(caps).m(lalt),   ag().k(F).m(ctrl).m(alt) );
    //k.cm .add_combo ( cg().k(R).m(caps).m(lalt),   ag().k(R).m(ctrl).m(alt) );
    // ^^ naah, we'd rather keep these silent for valuable caps-lalt-ms<?>-<key> combos

    // since F is in caret mode, we'll remap some of the other combos to replace ctr-f etc
    k.cm .add_combo ( cg().k(F).no_rpt().m(lalt),          ag().k(F).m(ctrl) );     // alt-f --> ctrl-f
    k.cm .add_combo ( cg().k(F).no_rpt().m(caps).m(lalt),  ag().k(F).m(lalt) );     // caps-lalt-f --> alt-f, though it goes against typical mode-key usage

    // e in caret mode, so we'll put our left-handed-enter on alt-e instead .. (note that there are also caps-space-* combos for *-enter)
    k.cm .add_combo ( cg().k(E).m(lalt),             ag().k(Enter) );    // alt-e   --> Enter
    k.cm .add_combo ( cg().k(E).m(lalt).s(msE_dbl),  ag().k(Enter) );    // alt-e-e --> Enter


    //k.cm .add_combo ( cg().k(E).msk_nc().m(caps).m(lalt),   ag().k(Enter).m(ctrl) );    // caps-alt-e --> ctrl-Enter
    // ^^ nah we want to keep caps-lalt-E-<key> combos .. (plus we have other decent ctrl-Enter options)

}



fn setup_caps_as_shift_mappings  (k:&Krusty) {
    // basically nums or kbd-right symbols not otherwise involved in l2
    // (these are 'caps-atypical', as typically caps-<key> will do ctrl-<key> via fallback)
    let cas = "567890-=[]\\;\'/.";   // note that we setup 1,2,3,4 as qks keys earlier
    cas .chars() .for_each ( |c| {
        Key::from_char(c) .into_iter() .for_each ( |key|
            k.cm .add_combo ( cg().k(key).m(caps),  ag().k(key).m(lshift) )
        )
    } );
}




fn disable_win_num_combos (k:&Krusty) {
    // win-number combos annoyingly activate/minimize items from taskbar etc .. we'll disable those
    // NOTE that these could be removed now that we've made win into TMK_dbl ..
    // .. but if we set win-combo single-press fallback to win-combo, these will still be useful, so we'll let them be!,
    // .. note ofc that everything disabled like this will be accessible on win-dbl press combos!
    //[Numrow_1, Numrow_2, Numrow_3, Numrow_4, Numrow_5, Numrow_6, Numrow_7, Numrow_8, Numrow_9, Numrow_0] .iter().for_each ( |&key| {
    [Numrow_5, Numrow_6, Numrow_7, Numrow_8, Numrow_9, Numrow_0] .iter().for_each ( |&key| {
        k.cm .add_combo ( cg().k(key).m(lwin),           ag().af(no_action()) );
        k.cm .add_combo ( cg().k(key).m(lwin).m(caps),   ag().af(no_action()) );
    } );
    // win-1,2,3 are separately setup for vol  plus caps-win-1,2,3,4 are used for win-grps
    // .. that leaves just the win-4, which we'll set it here
    k.cm .add_combo ( cg().k(Numrow_4).m(lwin),   ag().af(no_action()) );

    // we'll disable win-d too, as I never use that show/hide desktop and it's disruptive
    k.cm .add_combo ( cg().k(D).m(lwin),  ag().af(no_action()) );
}



fn setup_caps_dbl_combos (k:&Krusty) {
    // we'll setup some keys on caps double tap first, esp those that modify global-ish behavior

    // dbl-caps T to toggle capslock
    k.cm .add_combo ( cg().k(T).m(caps_dbl),   ag().k(CapsLock) );

    // we'll set dbl-caps-win-S/C/A/W as tmp shift/ctrl/alt/win lock (useful for doing mouse horiz scroll on say moon-reader etc)
    fn gen_af_ensure_mk (mk:UnifModKey) -> AF { Arc::new ( move || {
        mk.ensure_active(); mk.mngd_active.clear();
    } ) }
    k.cm .add_combo ( cg().k(S).m(caps_dbl).m(lwin),   ag().af ( gen_af_ensure_mk (k.ks.mod_keys.lshift.clone()) ) );
    k.cm .add_combo ( cg().k(C).m(caps_dbl).m(lwin),   ag().af ( gen_af_ensure_mk (k.ks.mod_keys.lctrl.clone()) ) );
    k.cm .add_combo ( cg().k(A).m(caps_dbl).m(lwin),   ag().af ( gen_af_ensure_mk (k.ks.mod_keys.lalt.clone()) ) );
    //k.cm .add_combo ( cg().k(W).m(caps_dbl).m(lalt),   ag().af ( gen_af_ensure_mk (k.ks.mod_keys.lwin.clone()) ) );
    // ^^ win is now dbled modkey (and so not full-managed), and so ensure-active doesnt make sense for it


    /// **_ modkey-wrapping OUTPUT SWAPS_**
    // we'll setup a helper to remap some key such that it works with any modkey [alt/ctrl/shift/win]
    fn gen_full_mk_key_swap_af (k:&Krusty, key:Key) -> AF {
        // we could in theory, just register these as all the 16 combinations of the [ctrl,alt,shift,win] combos ..
        // .. but if wanted to use the l/r/generic triplets, those 16 would expand out to 128 combos for each fn call !!
        // .. so we'd rather just setup wildcarded combos and build layered afs for mod-keys (like how fallback works)
        let ks = k.ks.clone();
        Arc::new ( move || {
            let mut af = base_action(key);
            if ks.mod_keys.some_ctrl_down()  { af = ks.mod_keys.lctrl .active_action(af) }
            if ks.mod_keys.some_alt_down()   { af = ks.mod_keys.lalt  .active_action(af) }
            if ks.mod_keys.some_shift_down() { af = ks.mod_keys.lshift.active_action(af) }
            if ks.mod_keys.some_win_down()   { af = ks.mod_keys.lwin  .active_action(af) }
            af();
        } )
    }
    fn setup_caps_dbl_ms_key_swap (k:&Krusty, ms:ModeState_T, k1:Key, k2:Key) {
        k.cm .add_combo ( cg().k(k1).s(ms).m(caps_dbl).wcma(),  ag().af (gen_full_mk_key_swap_af (k, k2)) );
    }

    // we'll setup Fn<1-12> to output Fn[13-24] so we can use them to program in IDE
    [ (F1, F23), (F2, F24), (F3, F13), (F4,  F14), (F5,  F15), (F6,  F16),
      (F7, F17), (F8, F18), (F9, F19), (F10, F20), (F11, F21), (F12, F22),
    ] .iter() .for_each ( |(k1,k2)| setup_caps_dbl_ms_key_swap (k, qks, *k1, *k2) );

    // we'll do the same for the Numrow_<?> keys to generate Numpad_<?> keys so those too can be generated to configure in IDEs etc
    [ (Numrow_1, Numpad_1), (Numrow_2, Numpad_2), (Numrow_3, Numpad_3), (Numrow_4, Numpad_4), (Numrow_5, Numpad_5), (Numrow_6, Numpad_6),
      (Numrow_7, Numpad_7), (Numrow_8, Numpad_8), (Numrow_9, Numpad_9), (Numrow_0, Numpad_0), (Minus, Numpad_Minus), (Equal, Numpad_Add),
    ] .iter() .for_each ( |(k1,k2)| setup_caps_dbl_ms_key_swap (k, qks, *k1, *k2) );

    // finally, we'll allow outputting the 9 unspecified keys in the virtual-keycodes mapping (this is 'extended' with mode-state-E)
    [ (Numrow_1, OtherKey(0x97)), (Numrow_2, OtherKey(0x98)), (Numrow_3, OtherKey(0x99)),
      (Numrow_4, OtherKey(0x9A)), (Numrow_5, OtherKey(0x9B)), (Numrow_6, OtherKey(0x9C)),
      (Numrow_7, OtherKey(0x9D)), (Numrow_8, OtherKey(0x9E)), (Numrow_9, OtherKey(0x9F)),
    ] .iter() .for_each ( |(k1,k2)| setup_caps_dbl_ms_key_swap (k, msE, *k1, *k2) );


    // we'll also reuse the swap fn above for repmapping the arrow keys
    // windows itself seems to treat shift-arrow keys as special (cf Numpad-arrow keys), injecting its own logic of shift-up/dn in
    // >  the stream when it sees left/right/up/down w shift held down .. and that ofc screws up with our own handling for that
    // so instead, we'll swap those (even w/o shift) with the ext-left/right/up/down etc versions
    // NOTE that while these differ in scan-code, they are the same in vk-code
    // (so the difference be seen in ahk-log, where they are named Numpad-<?>, but they can look identical in browser/IDE etc)
    // for more ref: https://www.win.tue.nl/~aeb/linux/kbd/scancodes-1.html
    fn setup_ext_key_swap (k:&Krusty, key:Key, ext_key:Key) {
        // first we'll set up the direct full-mk swaps themselves ..
        // (wont need wcsa() etc here coz there's no default/fallback when in combo w mode-states, only w modkeys )
        k.cm .add_combo ( cg().k(key).wcma(),                     ag().af (gen_full_mk_key_swap_af(k, ext_key)) );
        k.cm .add_combo ( cg().k(key).s(qks).m(caps_dbl).wcma(),  ag().af (gen_full_mk_key_swap_af(k, key)) );
        // ^^ these caps-dbl alternatives can be used to generate the orig versions (again, for setup in IDEs etc)
    }
    [ (Left, ExtLeft),   (Right, ExtRight), (Up,     ExtUp),    (Down,     ExtDown),
      (Home, ExtHome),   (End,   ExtEnd),   (PageUp, ExtPgUp),  (PageDown, ExtPgDn),
    ] .iter() .for_each ( |&(key, ext_key)| setup_ext_key_swap (k, key, ext_key) );

}




fn setup_mouse_left_btn (k:&Krusty) {

    /// for caps-lbtn we'll enable **_ caps-as-ctrl _** (for drags etc) via mngd_ctrl_state .. (but not other caps-mod-combos as ctrl-mod-combos)
    // (note that plain clicks will work ok via fallback, though mod-click fallback will only have mod-wrap arouund press not press-rel)
    fn gen_af_caps_mngd_lbtn (ks:KrustyState) -> AF { Arc::new ( move || {
        ks.mod_keys.lctrl.ensure_active();
        // ^^ this will leave ctrl active (managed), ctrl will clear when caps comes up
        ks.mouse.lbtn.active.set(); LeftButton.press();
        // ^^ we only want to press after ctrl has been made active
    } ) }
    k.cm .add_combo ( cg().mbtn(LeftButton).m(caps),  ag().af (gen_af_caps_mngd_lbtn(k.ks.clone())) );

    /// for **_ mbtn release _**, we'll specify full wildcards (modkeys, modes) to avoid missing btn releases regardless of mode-states
    fn gen_af_lbtn_release (ks:KrustyState) -> AF { Arc::new ( move || {
        if ks.mouse.lbtn.active.is_set() { ks.mouse.lbtn.active.clear(); LeftButton.release() }
    } ) }
    k.cm .add_combo ( cg().mbtn(LeftButton).rel().wcma().wcsa(), ag().af ( gen_af_lbtn_release (k.ks.clone()) ) );


    /// for win-lbtn and win-caps-lbtn, we want to **_ capture win-snap-dat _** for window drag/resizing
    fn gen_af_win_snap_dat (ks:KrustyState, wgo:Option<WinGroups_E>) -> AF { Arc::new ( move || {
        ks.capture_pointer_win_snap_dat(wgo);
        win_set_fgnd (ks.win_snap_dat.read().unwrap().hwnd);
    } ) }
    // win-drag does drag with snap .. caps-win does resize .. and adding shift disables snap for both
    // .. so we'll use wildcards to set any of these to trigger taking a win-snap-dat for subsequent use
    k.cm .add_combo ( cg().mbtn(LeftButton).m(lwin) .wcm(caps).wcm(shift),
                      ag().af (gen_af_win_snap_dat (k.ks.clone(), None) ) );


    /// for **_ win-groups _** ..
    // .. caps-lwin-qks<?> + lbtn-dbl-click on window is add that window to the corresponding group
    fn setup_win_grp_action (k:&Krusty, s:ModeState_T, wg:WinGroups_E) {
        let ks = k.ks.clone();
        let add_af = Arc::new ( move || {
            let hwnd = win_get_hwnd_from_pointer();
            ks.win_groups.add_to_group (wg, hwnd);
            jiggle_window(hwnd);
        } );
        k.cm .add_combo ( cg().mbtn(LeftButton).m(lwin).m(caps).s(s) .c(c_flag(k.ks.mouse.lbtn.dbl_tap.clone())),  ag().af(add_af) );
        // also, on single-click we should capture dat to allow group window drag
        let wsd_af = gen_af_win_snap_dat (k.ks.clone(), Some(wg));
        k.cm .add_combo ( cg().mbtn(LeftButton).m(lwin).m(caps).s(s),           ag().af(wsd_af.clone()) );
        k.cm .add_combo ( cg().mbtn(LeftButton).m(lwin).m(caps).s(s).m(shift),  ag().af(wsd_af) );
    }
    setup_win_grp_action (k, qks1, wg1);
    setup_win_grp_action (k, qks2, wg2);
    setup_win_grp_action (k, qks3, wg3);
    setup_win_grp_action (k, qks4, wg4);



    // for win-lbtn-dbl we want to maximize the pointed window
    fn gen_af_win_tog_max (ks:KrustyState) -> AF { Arc::new ( move || {
        win_toggle_maximize (ks.win_snap_dat.read().unwrap().hwnd)
    } ) }
    k.cm .add_combo ( cg().mbtn(LeftButton).m(lwin) .c(c_flag(k.ks.mouse.lbtn.dbl_tap.clone())),
                      ag().af (gen_af_win_tog_max(k.ks.clone())) );


    // for caps-alt-click, we want to bring up find usages window in IDE (for word under cursor)
    // --> (directly assigned in IDE to ctrl-alt-click, which caps-alt-click sends via default fallback)

    // for caps-d-click in IDE, we want to bring up diff for cur file (via ctrl-alt-shift-d configd there)
    k.cm .add_combo ( cg().mbtn(LeftButton).m(caps).s(msD),  ag().k(D).m(ctrl).m(alt).m(shift) );

    // caps-e-e-click in IDE, we want it to add additional carets .. (via Alt-Shift-Click configd in IDE)
    k.cm .add_combo ( cg().mbtn(LeftButton).m(caps).s(msE_dbl),   ag().mbtn(LeftButton).m(alt).m(shift) );

    // but just caps-e-click should be regular click .. (esp while we're doing tab scroll on caps-e etc)
    k.cm .add_combo ( cg().mbtn(LeftButton).m(caps).s(msE),   ag().mbtn(LeftButton) );

    // for caps-f-click, we want to bring up find/usages-window in IDE (via alt-u configd in IDE)
    fn gen_af_ide_find_usages () -> AF {
        let af = ag().k(U).m(alt).gen_af();
        // before we call the hotkey, we'll send a click to have the caret set up at the right place
        Arc::new ( move || { LeftButton.press_release(); af(); } )
    }
    k.cm .add_combo ( cg().mbtn(LeftButton).m(caps).s(msF),  ag().af(gen_af_ide_find_usages()) );


}




fn setup_mouse_right_btn (k:&Krusty) {

    // win-caps-qks? + rbtn-click is used to remove the pointed window from the corresponding win-group
    fn gen_af_win_grp_remove (wg:WinGroups_E, ks:KrustyState) -> AF { Arc::new ( move || {
        let hwnd = win_get_hwnd_from_pointer();
        ks.win_groups.remove_from_group (wg, hwnd);
        jiggle_window(hwnd);
    } ) }
    k.cm .add_combo ( cg().mbtn(RightButton).m(lwin).m(caps).s(qks1),  ag().af (gen_af_win_grp_remove (wg1, k.ks.clone()) ) );
    k.cm .add_combo ( cg().mbtn(RightButton).m(lwin).m(caps).s(qks2),  ag().af (gen_af_win_grp_remove (wg2, k.ks.clone()) ) );
    k.cm .add_combo ( cg().mbtn(RightButton).m(lwin).m(caps).s(qks3),  ag().af (gen_af_win_grp_remove (wg3, k.ks.clone()) ) );
    k.cm .add_combo ( cg().mbtn(RightButton).m(lwin).m(caps).s(qks4),  ag().af (gen_af_win_grp_remove (wg4, k.ks.clone()) ) );

    // for release, we'll again setup global wildcard combo
    fn gen_af_rbtn_release (ks:KrustyState) -> AF { Arc::new ( move || {
        let (mut should_release, mut should_mask) = (true, false);   // for safety, default should be to release w/o masking
        if ks.in_right_btn_scroll_state.is_set() {
            // ^^ upon switche rbtn scroll, switche sends early rbtn-rel, so when we get this real one, we might be rbtn-inactive
            // .. so catching this separately here, lets us pass this through for switche (for cases its hook is behind us)
            // (and lower down, for normal cases w rbtn inactive (coz maybe we/krusty suppressed it), we suppress the release)
            ks.in_right_btn_scroll_state.clear();
            // and the default release w/o masking will work out ok here
        } else if ks.mouse.rbtn.active.is_clear() {
            // if its not even active, we dont need to send a release (presumably we blocked the press going out)
            should_release = false
        } else if ks.mouse.rbtn.consumed.is_set() {
            // else if it is active, we'll mask if its marked consumed
            should_mask = true
        }
        if should_release {
            if should_mask { mouse_rbtn_release_masked() }
            else { RightButton.release() }
        }
        ks.mouse.rbtn.consumed.clear(); ks.mouse.rbtn.active.clear();
    } ) }
    k.cm .add_combo ( cg().mbtn(RightButton).rel().wcma().wcsa(),  ag().af (gen_af_rbtn_release (k.ks.clone())) );

}




fn setup_middle_and_xbtn_combos (k:&Krusty) {
    // we want to set side btns to serve as middle btns too ..
    // (note x-btns (on mx mouse) seem to report nothing on press, and dn/up on rel .. (and nothing if held down for too long))
    // (note also that normal no-mod no-caps btn-click will work via fallback)

    fn setup_middle_btn_eqv_combos (k:&Krusty, btn:MouseButton) {
        // base action rerouting for eqv btns (middle/x1/x2) to middle-btn
        fn gen_xbtn_base_press_af (ks:KrustyState) -> AF { Arc::new ( move || {
            ks.mouse.mbtn.active.set(); MiddleButton.press();
        } ) }
        k.cm .add_combo ( cg().mbtn(btn),  ag().af (gen_xbtn_base_press_af (k.ks.clone())) );

        // for release, we'll specify full wildcards (modkeys, modes), to avoid missing btn releases regardless of mode-states
        fn gen_xbtn_base_rel_af (ks:KrustyState, mbs:MouseBtnState) -> AF { Arc::new ( move || {
            if ks.mouse.mbtn.active.is_set() { ks.mouse.mbtn.active.clear(); MiddleButton.release(); }
            if mbs.active.is_set() { mbs.active.clear(); mbs.btn.release(); }
        } ) }
        let x_btn_base_rel_af = gen_xbtn_base_rel_af (k.ks.clone(), k.ks.mouse.get_btn_state(btn).unwrap());
        k.cm .add_combo ( cg().mbtn(btn).rel().wcma().wcsa(),  ag().af (x_btn_base_rel_af) );

    }
    setup_middle_btn_eqv_combos (k, MiddleButton);
    setup_middle_btn_eqv_combos (k, X1Button);
    setup_middle_btn_eqv_combos (k, X2Button);

    /// and after that we'll do non-common configs specific to x1/x2/mid btns

    // win-x2 as window close
    k.cm .add_combo ( cg().mbtn(X2Button).m(lwin),  ag().af ( Arc::new ( || win_close(win_get_hwnd_from_pointer()) ) ) );
    // win-caps-x2 as ctrl-w for tab-close
    k.cm .add_combo ( cg().mbtn(X2Button).m(lwin).m(caps),  ag().k(W).m(ctrl) );

    // alt-x2 to nav bkwd in IDE via alt-left,
    k.cm .add_combo ( cg().mbtn(X2Button).m(lalt),          ag().k(ExtLeft).m(lalt) );
    // caps-alt-x2 to nav fwd in ide via alt-right
    k.cm .add_combo ( cg().mbtn(X2Button).m(lalt).m(caps),  ag().k(ExtRight).m(lalt) );

    // during caps-e-wheel scroll, we'll set x2 btn to close windows via ctrl-w
    k.cm .add_combo ( cg().mbtn(X2Button).m(caps).s(msE),  ag().k(W).m(ctrl) );



    // caps-x2 to search highlighted in chrome (via macro like sets of steps w available chrome hotkeys)
    fn chrome_search_highlighted () {
        shift_press_release(F10);
        ExtDown.press_release(); ExtDown.press_release(); ExtDown.press_release();
        Enter.press_release();
    }
    k.cm .add_combo ( cg().mbtn(X2Button).m(caps),  ag().af (action(chrome_search_highlighted)) );

}



// helper fn to set both frwd/bkwd CG mappings with a parameterized CG gen fn
fn setup_frwd_bkwd_whl <CGF, ICG, P, AGPF, IAG> (k:&Krusty, cgFn:CGF, bkwd_p:P, frwd_p:P, agFn:AGPF) where
    ICG : Into<CG>,  IAG : Into<AG>,
    CGF  : Fn(ComboGen<ComboGenSt_Wheel>) -> ICG,
    AGPF : Fn(ActionGen, P) -> IAG,
{
    k.cm .add_combo ( cgFn (cg().whl().bkwd()),  agFn (ag(), bkwd_p) );
    k.cm .add_combo ( cgFn (cg().whl().frwd()),  agFn (ag(), frwd_p) );
}

fn setup_vert_wheel (k:&Krusty) {

    // re the wheel-delta (which is among few things of runtime use in event-data), we have ev-data available in event-proc and conditionals ..
    // .. but we have avoided having AFs taking that as arg (mostly coz AF composition would then suck, and AFs hard to use stand-alone)
    // as such, one way around is to just set conditionals that check that upon trigger .. which the following fn lets us do
    // (alternately, we could use stored last wheel-delta in mouse-wheel struct, which should be fine as long as queue clearance is fast-enough)
    // UPDATE: we now have wheel direction added into binding key and event itself, but letting the below stand as its still valid too
    //
    // #[allow(dead_code)]
    // fn c_wheel_dir (is_down:bool) -> ComboCond { Arc::new ( move |_,e| {
    //     if let EventDat::wheel_event {delta, ..} = e.dat { is_down == (delta < 0) } else { false }
    // } ) }
    //fn c_wheel_dir_down () -> ComboCond { c_wheel_dir(true ) }
    //fn c_wheel_dir_up ()   -> ComboCond { c_wheel_dir(false) }


    /* General mouse wheel setup guidelines
        - wheel         -->  wheel       .. (via fallback)
        - caps/ctrl-wh  -->  ctrl wheel  .. (plus switche and ctrl-tab overloads)
        - caps-x2-wh    -->  horiz-wheel

        - alt-wh        -->  brightness  .. (plus switche and alt-tab overloads)
        - alt-1-wh      -->  fine-mode brightness
        - win-wh        -->  volume
        - caps-win-3-wh -->  media skip fwd/bkwd

        - caps-d-wh     -->  nav windows (via switche snapshots)
        - caps-r-wh     -->  fast scrolls

        - caps-3-wh     -->  IDE last-loc nav
        - caps-3-e-wh   -->  IDE last-edit-loc nav

        - caps-win-w  based sticky two-stroke-combos  .. (in separate tscs block)
            - caps-wh       -->  move window left/right
            - caps-d-wh     -->  move window left/right
            - caps-e-wh     -->  move window up/down
            - caps-f-wh     -->  snap window left/right
            - caps-f-d-wh   -->  snap window left/right
            - caps-f-e-wh   -->  snap window up/down
            - caps-r-wh     -->  resize window width
            - caps-r-d-wh   -->  resize window width
            - caps-r-e-wh   -->  resize window height

        - caps-win-d based sticky two-stroke-combos  .. (in separate tscs block)
            - caps-wh  -->  nav desktops

        - caps-e-t based sticky two-stroke-combos .. (in separate tscs block)
            - caps-wh  -->  nav tabs (in ide, npp, chrome)


        (now a whole pile to gen up/dn, left/right arrows w layered mods)
        (note that qks1 combos are special here as for ergo reasons, they can be pressed w/o Q)
        - caps-q-wh      -->  up/dn
        - caps-q-A-wh    -->  alt up/dn
        - caps-q-e-wh    -->  shift up/dn
        - caps-q-A-e-wh  -->  alt-shift up/dn
        - caps-1-wh      -->  ctrl up/dn
        - caps-1-A-wh    -->  ctrl-alt up/dn
        - caps-1-e-wh    -->  ctrl-shift up/dn
        - caps-1-A-e-wh  -->  ctrl-alt-shift up/dn

        - caps-qq-wh     -->  left/right
        - caps-qq-A-wh   -->  alt l/r
        - caps-qq-e-wh   -->  shift l/r
        - caps-qq-A-e-wh -->  alt-shift l/r
        - caps-11-wh     -->  ctrl l/r
        - caps-11-A-wh   -->  ctrl-alt l/r
        - caps-11-e-wh   -->  ctrl-shift l/r
        - caps-11-A-e-wh -->  ctrl-alt-shift l/r
     */

    fn gen_af_base_wheel (dir_is_down:bool, ks:KrustyState) -> AF {
        // we want to mark when we enter switche right-btn-scroll .. but otherwise, we just send regular wheel scrolls
        let af_wheel_scroll   = if dir_is_down { ag().whl().bkwd().gen_af() } else { ag().whl().frwd().gen_af() };
        Arc::new ( move || {
            if ks.mouse.rbtn.down.is_set() { ks.in_right_btn_scroll_state.set() }
            af_wheel_scroll()
            // ^^ we'll send out the scroll even during rbtn-scrll, despite switche hooks directly listening to it ..
            // .. because in cases when our hooks are ahead of switche hooks, it wouldn't otherwise get there
        } )
    }
    setup_frwd_bkwd_whl ( k,  |wg| wg,   true, false,   |ag,p| ag.af (gen_af_base_wheel (p, k.ks.clone()) ) );


    fn gen_af_caps_wheel (dir_is_down:bool, ks:KrustyState) -> AF {
        // for switche, we send shift-up/dn which will do in-block-only-wrap instead in recents/grouped instead of across the groups
        // else during ctrl-tab, we'll do ctrl-up/dn, mostly for IDE as it doesnt seem to respond to wheel during ctrl-tab
        // else for general caps-wheel, we send out managed-ctrl-wheels (managed to avoid having ctrl dn/up be interspersed)
        let af_ctrl_wheel   = if dir_is_down { ag().whl().bkwd().m(ctrl ).gen_af() } else { ag().whl().frwd().m(ctrl ).gen_af() };
        let af_switche_fgnd = if dir_is_down { ag().k(ExtDown  ).m(shift).gen_af() } else { ag().k(ExtUp    ).m(shift).gen_af() };
        let wel = WinEventsListener::instance();
        Arc::new ( move || {
            if check_switche_fgnd (&wel) {
                af_switche_fgnd()
            } else {
                ks.mod_keys.lctrl.ensure_active();
                af_ctrl_wheel()
            }
        } )
    }
    // we'll also do this for actual ctrl-wheel so the behavior is consistent ('ctrl' expands out to both lctrl and rctrl)
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps),          true, false,   |ag,p| ag.af ( gen_af_caps_wheel (p, k.ks.clone()) ) );
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(ctrl),          true, false,   |ag,p| ag.af ( gen_af_caps_wheel (p, k.ks.clone()) ) );
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).m(ctrl),  true, false,   |ag,p| ag.af ( gen_af_caps_wheel (p, k.ks.clone()) ) );

    // caps-dbl wheel can simply translate to horiz-wheel
    k.cm .add_combo ( cg().whl().bkwd().m(caps_dbl),   ag().hwhl().frwd() );
    k.cm .add_combo ( cg().whl().frwd().m(caps_dbl),   ag().hwhl().bkwd() );


    fn gen_af_alt_wheel (dir_is_down:bool) -> AF {
        // general alt-wheel will do brightness control, and we'll separately support alt-tab and swtiche nav
        let af_brightness   = if dir_is_down { gen_af_incr_brightness(-4) } else { gen_af_incr_brightness(4) };
        let af_alt_tab_fgnd = if dir_is_down { ag().k(ExtRight).mkg_nw().gen_af() } else { ag().k(ExtLeft).mkg_nw().gen_af() };
        let af_switche_fgnd = if dir_is_down { ag().k(ExtDown ).mkg_nw().gen_af() } else { ag().k(ExtUp  ).mkg_nw().gen_af() };
        let wel = WinEventsListener::instance();
        Arc::new ( move || {
            if check_switche_fgnd (&wel) { af_switche_fgnd() }
            else if check_alt_tab_fgnd (&wel) { af_alt_tab_fgnd() }
            else { af_brightness() }
        } )
    }
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(lalt),   true, false,   |ag,p| ag.af (gen_af_alt_wheel (p)) );

    // and for alt-wheel w qks1 (i.e. alt+1+wheel), we do finer brightness adjustments
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(lalt).s(qks1),    -1, 1,   |ag,p| ag.af (gen_af_incr_brightness (p)) );


    fn gen_af_caps_alt_wheel (dir_is_down:bool) -> AF {
        // general caps-alt-wheel we'll do up/down nav, and we'll support regular alt-tab tab
        // and for swtiche, we'll do in-block-only nav (via alt-shift-up/dn arrows)
        let af_normal       = if dir_is_down { ag().k(ExtDown).m(alt).gen_af()    } else { ag().k(ExtUp).m(alt).gen_af() };
        let af_alt_tab_fgnd = if dir_is_down { ag().k(ExtRight).mkg_nw().gen_af() } else { ag().k(ExtLeft).mkg_nw().gen_af() };
        let af_switche_fgnd = if dir_is_down { ag().k(ExtDown).m(shift).mkg_nw().gen_af() } else { ag().k(ExtUp).m(shift).mkg_nw().gen_af() };
        let wel = WinEventsListener::instance();
        Arc::new ( move || {
            if check_switche_fgnd (&wel) { af_switche_fgnd() }
            else if check_alt_tab_fgnd (&wel) { af_alt_tab_fgnd() }
            else { af_normal() }
        } )
    }
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).m(lalt),   true, false,   |ag,p| ag.af (gen_af_caps_alt_wheel (p)) );


    /// setups for **_ Arrow-Up/Down nav _** (in addn to some portions above)
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks),                  ExtDown, ExtUp,   |ag,key| ag.k(key) );
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks).m(lalt),          ExtDown, ExtUp,   |ag,key| ag.k(key).m(alt) );
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks).s(msE),           ExtDown, ExtUp,   |ag,key| ag.k(key).m(shift) );
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks).m(lalt).s(msE),   ExtDown, ExtUp,   |ag,key| ag.k(key).m(alt).m(shift) );

    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks1),                  ExtDown, ExtUp,   |ag,key| ag.k(key).m(ctrl) );
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks1).m(lalt),          ExtDown, ExtUp,   |ag,key| ag.k(key).m(ctrl).m(alt) );
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks1).s(msE),           ExtDown, ExtUp,   |ag,key| ag.k(key).m(ctrl).m(shift) );
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks1).m(lalt).s(msE),   ExtDown, ExtUp,   |ag,key| ag.k(key).m(ctrl).m(alt).m(shift) );


    /// setups for **_ LEFT-RIGHT nav _** (reminscent of h-wheel, useful to nav sidebar trees etc)
    // we'll set Left/Right up on sensible _dbl taps on the mod-keys we use for Up/Down nav combos
    // (we're trying to avoid wildcards on wheel itself, and would rather do this)
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks_dbl),                  ExtRight, ExtLeft,   |ag,key| ag.k(key) );
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks_dbl).m(lalt),          ExtRight, ExtLeft,   |ag,key| ag.k(key).m(alt) );
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks_dbl).s(msE),           ExtRight, ExtLeft,   |ag,key| ag.k(key).m(shift) );
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks_dbl).m(lalt).s(msE),   ExtRight, ExtLeft,   |ag,key| ag.k(key).m(alt).m(shift) );

    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks1_dbl),                 ExtRight, ExtLeft,   |ag,key| ag.k(key).m(ctrl) );
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks1_dbl).m(lalt),         ExtRight, ExtLeft,   |ag,key| ag.k(key).m(ctrl).m(alt) );
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks1_dbl).s(msE),          ExtRight, ExtLeft,   |ag,key| ag.k(key).m(ctrl).m(shift) );
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks1_dbl).m(lalt).s(msE),  ExtRight, ExtLeft,   |ag,key| ag.k(key).m(ctrl).m(alt).m(shift) );



    /// for win-wheel, we'll do **_ VOLUME control _**
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(lwin),    VolumeDown, VolumeUp,   |ag,key| ag.k(key).mkg_nw() );

    /// caps-win-3 wheel .. **_ media FWD-BKWD SKIP _** .. (cf win-3 for vol, win-f3 skip-fwd)
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(lwin).m(caps).s(qks3),    true, false,   |ag,p| ag.af (media_skips_action (1, &k.ks, p)) );

    /// caps-d-wheel, we'll **_ navigate across WINDOWS _** (via switche snapshots)
    let ssf = get_switche_snap_switch_flag();
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(msD),    true, false,   |ag,p| ag.af (gen_af_switche_snap_switch (p, ssf.clone())) );

    // and once we're done w the switching, we clear the flag so we'll check and refresh the snap next time we start
    k.cm .add_combo (cg().k(D).rel()         .c(c_flag(ssf.clone())), ag().af (af_clear_flag(ssf.clone())) );
    k.cm .add_combo (cg().k(D).rel().m(caps) .c(c_flag(ssf.clone())), ag().af (af_clear_flag(ssf.clone())) );


    /// caps-f (i.e word mode) wheel, we'll set as **_ nav through SEARCH (F3, Shift-F3) _**
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(msF),    no_mk, shift,   |ag,p| ag.k(F3).m(p) );

    /// we'll let caps-R-wheel do **_ FASTER SCROLL _**
    fn gen_af_fast_scroll (ks:KrustyState) -> AF { Arc::new ( move || {
        ks.mouse.vwheel.wheel.scroll (3 * ks.mouse.vwheel.last_delta.load (Ordering::Relaxed));
    } ) }
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(msR),    (), (),   |ag,_| ag.af (gen_af_fast_scroll (k.ks.clone())) );


    /// caps-qks3-wheel .. we'll use for **_ IDE LAST LOCATION NAV _** .. (via Alt Left/Right)
    //  (in theory, we have easy combos for alt-l/r, but this make it tie in better w the edit locs below)
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks3),    ExtRight, ExtLeft,   |ag,key| ag.k(key).m(alt) );

    /// and with mode-state-E, we'll do **_ IDE LAST EDIT LOCATION NAV _** .. (via Alt-Shift-Left/Right)
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks3).s(msE),    ExtRight, ExtLeft,   |ag,key| ag.k(key).m(alt).m(shift) );

}




fn setup_horiz_wheel (_k:&Krusty) {
    // (note that simple horiz-scroll will work as is w passthrough)
    // general h-wheel note : we do get h-wheel from mouse w x2-btn-wheel, but its still not ergo ..
    // .. so ideally we really wouldnt rely on this, and just have some setup overloaded in kbd w v-wheel setup
}




fn setup_back_quote (k:&Krusty) {
    // make normal backquote be Delete, caps can do back-tick, and shift or ralt do its tilde
    k.cm .add_combo ( cg().k(Backquote),          ag().k(ExtDelete) );
    k.cm .add_combo ( cg().k(Backquote).m(caps),  ag().k(Backquote) );
    //k.cm .add_combo ( cg().k(Backquote).m(lalt),    ag().k(Backquote) );
    //k.cm .add_combo ( cg().k(Backquote).m(shift),   ag().k(Backquote).m(shift) );
    //k.cm .add_combo ( cg().k(Backquote).m(ralt),    ag().k(Backquote).m(shift) );
    // ^^ not strictly necessary as cb composition now defaults to this, but also useful to see here for reference

    // for alt-backquote, we'll set that up to give ctrl-tab as more ergo alternative, and tying in w alt-tab
    k.cm .add_combo ( cg().k(Backquote).m(lalt),  ag().k(Tab).m(lctrl) );
    // and for now, we'll do the same for alt-1, kinda tying in with out alt-f1 window switching
    //k.cm .add_combo ( cg().k(Numrow_1).m(lalt),   ag().k(Tab).m(lctrl) );
    // ^^ naah .. thats cur used in the way complex fast/slow modification of vol/bright w/ win/alt 2/3 combos or mouse scrolls

}




fn setup_space_key (k:&Krusty) {

    // we wanted a bunch of Enter options on space .. (mostly coz Space is ergo, Enter is not)
    k.cm .add_combo ( cg().k(Space).m(ralt),          ag().k(Enter) );                  // ralt-space       -> Enter
    k.cm .add_combo ( cg().k(Space).m(caps).s(msF),   ag().k(Enter) );                  // caps-f-space     -> Enter

    k.cm .add_combo ( cg().k(Space).m(lalt).c(switche_not_fgnd(k)),  ag().k(Enter) );          // lalt-space  -> Enter ..  (excl switche)
    k.cm .add_combo ( cg().k(Space).m(lalt_dbl),                     ag().k(Space).m(alt) );   // dbl-lalt-space -> alt-space (orig action)

    k.cm .add_combo ( cg().k(Space).m(caps).m(lalt),  ag().k(Enter).m(lalt) );          // caps-lalt-space  -> alt-enter

    k.cm .add_combo ( cg().k(Space).m(caps).s(qks),         ag().k(Enter).m(ctrl) );    // caps-q-space     -> ctrl-enter
    k.cm .add_combo ( cg().k(Space).m(caps).s(msF_dbl),     ag().k(Enter).m(ctrl) );    // caps-ff-space    -> ctrl-enter
    k.cm .add_combo ( cg().k(Space).m(caps).s(msE).s(msF),  ag().k(Enter).m(ctrl) );    // caps-e-f-space   -> ctrl-enter


    // beyond that .. caps-space as ctrl-space, caps-lalt-space as alt-enter for intellij
    // and some actual Space outputs
    k.cm .add_combo ( cg().k(Space).m(caps).s(qks1),  ag().k(Space).m(ctrl).m(shift) ); // qks1-space       -> ctrl_shift_space for IDE
    k.cm .add_combo ( cg().k(Space).m(caps).s(qks2),  ag().k(Space).m(ctrl).m(shift) ); // qks1-space       -> ctrl_shift_space for IDE
    //k.cm .add_combo ( cg().k(Space),                  cg().k(Space) );                // space            -> space
    //k.cm .add_combo ( cg().k(Space).m(caps),          cg().k(Space).m(ctrl) );        // caps-space       -> ctrl-space
    // ^^ not necessary as cb compositions default to this, but also useful to see here for reference

    // and some final extras
    k.cm .add_combo ( cg().k(Space).m(caps).m(ralt),  ag().k(Escape) );                 // caps-ralt-space  -> Escape
}




fn setup_escape_key (k:&Krusty) {
    // Escape is just escape, but we want it to do press-release immediately (so switche is faster)
    k.cm .add_combo ( cg().k(Escape),          ag().k(Escape) );
    k.cm .add_combo ( cg().k(Escape).m(caps),  ag().k(Escape) );

    // use the apps key to send shift-escape ..
    k.cm .add_combo ( cg().k(Apps),            ag().k(Escape).m(shift) );

    // for alt-escape, we want to override the default send-to-back behavior, as it has issues detailed in notes
    // .. if switche not in fgnd, we'll switch to next window in switche and send cur to back
    //  .. and if switche is fgnd, it'll directly process the Esc itself (will auto-hide upon fgnd lost if so configd)
    // note: any alt release (eg for bare Esc) will disrupt alt-tab, and any alt-esc variation will trigger windows, hence a dedicated hotkey
    let switche_next_af = ag().k(F16).m(alt).m(ctrl).gen_af();
    let alt_esc_action = Arc::new ( move || {
        let hwnd_to_back = WinEventsListener::instance().fgnd_info.read().unwrap().hwnd;   // cache before switche changes fgnd
        switche_next_af();
        win_send_to_back (hwnd_to_back);
    } );
    //k.cm .add_combo    ( cg().k(Escape).m(lalt).c(switche_fgnd()),      ag().k(F18).m(alt).m(ctrl) );   // switche alt-esc
    // ^^ disabled since swi now does auto-hide-on-fgnd-lost, and that mostly does the dismiss/esc anyway .. so natural alt-esc is fine
    //k.cm .add_combo    ( cg().k(Escape).m(lalt).c(switche_fgnd()),      ag().k(Escape) );               // switche alt-esc
    //k.cm .add_combo    ( cg().k(Escape).m(lalt).c(switche_fgnd()),      ag().af (no_action()) );        // switche alt-esc
    k.cm .add_combo ( cg().k(Escape).m(lalt).c(switche_not_fgnd(k)),  ag().af (alt_esc_action) );


    // we have win-mouse window drag/resize .. we'd like to cancel any in-progress action via escape
    fn gen_cancel_win_mouse_action (key:Key, ks:&KrustyState) -> AF {
        let ks = ks.clone();
        Arc::new ( move || {
            if ks.mouse.lbtn.down.is_set() {
                ks.mouse.lbtn.consumed.set();
                handle_pointer_action_cancel (&ks);
            } else {
                //press_release(key)
                // ^^ hmm, instead of win-esc being just esc otherwise, we'll use it as window minimize-and-back
                if key==Escape { win_fgnd_min_and_back() } else { key.press_release() }
            }
        } )
    }
    // we'll allow Escape to cancel in-progress win-drag-to-move/resize operations
    k.cm .add_combo ( cg().k(Escape).m(lwin),          ag().af (gen_cancel_win_mouse_action (Escape, &k.ks)) );
    k.cm .add_combo ( cg().k(Escape).m(lwin).m(caps),  ag().af (gen_cancel_win_mouse_action (Escape, &k.ks)) );
    // and since Esc is hard to press w caps-win, we'll let Q do the same too
    k.cm .add_combo ( cg().k(Q).m(lwin).m(caps),       ag().af (gen_cancel_win_mouse_action (Q, &k.ks)) );

    // in the same vein, we'll let win-q to everything-search (to match other win-a/s etc)
    k.cm .add_combo ( cg().k(Q).no_rpt().m(lwin),               ag().k(Q).m(win).m(alt) );
    k.cm .add_combo ( cg().k(Q).no_rpt().m(lwin).m(shift),      ag().k(Q).m(win).m(alt).m(shift) );
    // ^^ note that alt-q is set in 'everything' as global invocation hotkey, and alt-ctrl-q as new search window hotkey

}




fn setup_win_key_combos (k:&Krusty) {

    // the OS listens to win-L press at lowest levels for lockscreen (just like ctrl-alt-del) .. (though we do hear it) ..
    // .. and after that, we wont hear anything (incl the win release), and so our win state can get out of sync ..
    // .. hence we'll add a listener for win-L and clear out our win state
    fn gen_lwin_l_rel_af (ks:KrustyState) -> AF { Arc::new ( move || {
        ks.mod_keys.lwin.down.clear(); ks.mod_keys.lwin.dbl_tap.clear(); ks.mod_keys.lwin.consumed.clear();
        ks.mod_keys.lwin.active.clear(); ks.mod_keys.lwin.mngd_active.clear();
        // we'll also clear caps state in case we had done caps-win-l .. (the other modkey combos dont trigger win-lock)
        ks.mod_keys.caps.down.clear(); ks.mod_keys.caps.dbl_tap.clear();
    } ) }
    k.cm .add_combo ( cg().k(L).m(lwin),          ag().af (gen_lwin_l_rel_af (k.ks.clone())) );
    k.cm .add_combo ( cg().k(L).m(lwin).m(caps),  ag().af (gen_lwin_l_rel_af (k.ks.clone())) );

    // win-m by default minimized all windows .. we just want to disable it .. (note that win-d still does show-desktop)
    k.cm .add_combo ( cg().k(M).m(lwin),  ag().af(no_action()) );

    // win-f can toggle window full-screen .. (the OS default feedback-hub will stay on double-win-f)
    k.cm .add_combo ( cg().k(F).no_rpt().m(lwin),  ag().k(F11) );

    // win-e should bring up whatever we configured for file-explorer alternative
    k.cm .add_combo ( cg().k(E).no_rpt().m(lwin),  ag().af(action(start_alt_file_explorer)) );

    // since msE is often used in first-stroke-combos etc, we'll ensure held win-ee etc dont spam piles of explorer windows
    k.cm .add_combo ( cg().k(E).no_rpt().m(lwin    ).s(msE_dbl),  ag().af(no_action()) );
    k.cm .add_combo ( cg().k(E).no_rpt().m(lwin_dbl).s(msE    ),  ag().af(no_action()).mkg_w() );
    k.cm .add_combo ( cg().k(E).no_rpt().m(lwin_dbl).s(msE_dbl),  ag().af(no_action()).mkg_w() );

    // win-i should start irfanview
    k.cm .add_combo ( cg().k(I).no_rpt().m(lwin),  ag().af(action(start_irfanview)) );

    // win-n should start chrome-incognitoa
    k.cm .add_combo ( cg().k(N).no_rpt().m(lwin),  ag().af(action(start_chrome_incognito)) );

    // win-caps-b for bard .. hah we'll see
    k.cm .add_combo ( cg().k(B).no_rpt().m(lwin).m(caps),  ag().af(action(start_chrome_bard)) );

    // win-v can bring up vlc .. note that this will override native win-c for win clipboard (can get that win dbl-win-v)
    k.cm .add_combo ( cg().k(V).no_rpt().m(lwin),  ag().af(action(start_vlc)) );

    // we'll set win-s to quickly bringup the windows start menu via ctrl-esc shortcut (what double win press also does)
    k.cm .add_combo ( cg().k(S).m(lwin),  ag().k(Escape).m(lctrl) );

    // and win-ctrl-s to actually bring up the windows settings (via default win-i)
    k.cm .add_combo ( cg().k(S).m(caps).m(lwin),  ag().k(I).m(lwin) );

    // and win-a to bring up the launchy popup (which is win-a in practice, but for us would only be dbl-win-a otherwise)
    // (we've assigned that to win-ctrl-shift-a to not interfere with win-a doing windows-action center by default)
    //k.cm .add_combo ( cg().k(A).m(lwin),  ag().k(A).m(lwin).m(lctrl).m(lshift) );
    // ^^ seems to not work w elev switche fgnd .. presumably some global blockage of ctrl-hotkeys
    k.cm .add_combo ( cg().k(A).m(lwin),  ag().k(A).m(win).m(shift) );

    // we'll setup win-w for closing windows (via alt-f4)
    k.cm .add_combo ( cg().k(W).no_rpt().m(lwin),  ag().k(F4).m(lalt) );

    // we'll also setup a shortcut to pull up our taskbar shortcuts folder ...
    // (by focusing on tray btn first, then nav to our toolbar)
    //fn taskbar_focus_yak_tools_bar (k:&Krusty) -
    let _cb_focus_yak_tools_bar : AF = {
        let hk_tray_focus = ag().k(B).m(lwin).gen_af();
        let af_shift_tab  = ag().k(Tab).m(lshift).gen_af();
        let af_ext_down   = ag().k(ExtDown).gen_af();
        Arc::new ( move || {
            let (hk_tray_focus, af_shift_tab, af_ext_down) = (hk_tray_focus.clone(), af_shift_tab.clone(), af_ext_down.clone());
            thread::spawn ( move ||  {
                fn sleep() { thread::sleep(Duration::from_millis(100)) }   // win masked-release is delayed, so we wanna spread these out
                hk_tray_focus(); sleep(); af_shift_tab(); sleep(); af_shift_tab(); sleep(); af_ext_down(); sleep(); af_ext_down();
            } );
        } )
    };
    //k.cm .add_combo ( cg().k(Numrow_1).m(lwin), ag().af(cb_focus_yak_tools_bar) );



    /// we'll also add in some caps-win combos here that go together w these stand-alone win combos

    // this is counterpart to starting chrome incognito .. w/ caps will set that to open non-incognito
    k.cm .add_combo ( cg().k(N).no_rpt().m(caps).m(lwin),  ag().af (action (start_chrome)) );

    // caps-win-c being used to launch winmerge diff from last two clipboard entries
    k.cm .add_combo ( cg().k(C).no_rpt().m(caps).m(lwin),  ag().af (action (start_winmerge_clipboard)) );

    // gaah we'll just throw in iDEA diff for drag-drop diffing (just coz winmerge doesnt do dark mode)
    //k.cm .add_combo  ( k.ks, cg().k(C).m(lwin),  k.ks.cg_af (Arc::new (|| start_idea_diff() )));
    // ^^ cant do from here, turns out idea diff from cmd line can ONLY be opened with two files pointed, unlike empty from Idea shortcut!

}




fn setup_caps_2wsx_combos (k:&Krusty) {
    // .. initially noticed with caps-shift-w, which should have auto given ctrl-shift-w (close all tabs) .. but nothing comes out
    // .. it turns out (on this kbd) caps-shift-[F2, 2, w, s, x] dont produce any key event at the hook at all .. maybe from the driver itself
    // funnily enough, there's a bunch of complaints about specifically those keys for dell/hp laptops .. looks like hardware
    //    appears to be a common kbd pcb layout issue .. heres from 2007: (https://www.joachim-breitner.de/blog/250-Shift-Caps-2)
    // sooo .. to makeup, we'll set those on caps_dbl instead
    fn map_caps_dbl_as_ctrl_shift (k:&Krusty, key:Key) {
        k.cm .add_combo ( cg().k(key).m(caps_dbl),           ag().k(key).m(ctrl).m(shift) );
        k.cm .add_combo ( cg().k(key).m(caps_dbl).m(shift),  ag().k(key).m(ctrl).m(shift) );
    }
    [Numrow_2, W, S, X] .iter().for_each (|&key| map_caps_dbl_as_ctrl_shift (k, key));
    // ^^ note that caps-dbl F2 is used for latching-fscs, so excluded from the list above

    // we wanted to add support for caps-e-w (as ctrl-w) during tabs scroll with caps-e-wheel
    //k.cm .add_combo ( cg().k(W).m(caps).s(msE),  ag().k(W).m(ctrl) );
    // ^^ wont work .. another one of those caps-f2/2/w/s/x hardware-level issues (entire row is down when caps-w down)
    // otoh, doing a caps-e-w then release/re-press E does give out a caps-w .. so oh well
    // either way, we'll setup Q to do that at least
    //k.cm .add_combo ( cg().k(Q).m(caps).s(msE),  ag().k(W).m(ctrl) );
    // ^^ naah, we'd rather keep that to layer with other modkeys, modes etc
    // .. instead, elsewhere in mouse code, we've added caps-e-x2 for ctrl-w

}




fn setup_brightness_vol_media (k:&Krusty) {

    // in cur laptop, Fn-F6/F7 do brightness, but at +10 incrs .. set them to do small incrs with alt combos
    fn gen_incr_brightness (incr:i32) -> AF { Arc::new ( move || { let _ = incr_brightness(incr); } ) }
    k.cm .add_combo ( cg().k(F6).m(lalt),  ag().af (gen_incr_brightness(-1)) );
    k.cm .add_combo ( cg().k(F7).m(lalt),  ag().af (gen_incr_brightness( 1)) );

    // we'll use win-2/3 as vol down/up .. and alt-2/3 for brightness down/up
    // .. and we'll set these to have a 'fine-mode' when qks-1 key is held with alt
    // (note that mode keys (e.g. qks) in combos get auto marked for consumption (so no repeats), specifying 'msc_nc' disables that)
    // this one is complicated as we're trying to make the mode-keys themselves do the action (generally not advisable) ..
    // .. plus we want them to continue working when repeatedly tapped triggering accidental dbl_tap .. hence the _dbl below
    // (note that q/1/2/3/4 are qks* keys, so their caps combos (and repeats) are already suppressed at binding level)
    // (re disabling ms_dbl below, those would already have no fallback, but we'll including them to disable low-level repeats too)
    [qks1, qks2, qks3, qks1_dbl, qks2_dbl, qks3_dbl] .iter().for_each ( |&ms| {
        k.cm .add_combo ( cg().k(Numrow_1).m(lalt).s(ms),  ag().af (no_action()) );
        k.cm .add_combo ( cg().k(Numrow_1).m(lwin).s(ms),  ag().af (no_action()) );
    } );
    fn setup_fine_mode_ms_key (k:&Krusty, ms:&ModeState, mk:ModKey, fine_ms_t:ModeState_T, af:AF) {
        if let Some(key) = ms.key() {
            k.cm .add_combo ( cg().k(key).m(mk).s(ms.ms_t    ).s(fine_ms_t),  ag().af (af.clone()) );
            k.cm .add_combo ( cg().k(key).m(mk).s(ms.ms_dbl_t).s(fine_ms_t),  ag().af (af.clone()) );
        }
    }
    // alt-2 is brightness down, alt-3 is brightness up .. (fine mode when 1 is held)
    setup_fine_mode_ms_key (k, &k.ks.mode_states.qks2, lalt, no_ms, gen_incr_brightness(-4));
    setup_fine_mode_ms_key (k, &k.ks.mode_states.qks3, lalt, no_ms, gen_incr_brightness( 4));
    setup_fine_mode_ms_key (k, &k.ks.mode_states.qks2, lalt, qks1,  gen_incr_brightness(-1));
    setup_fine_mode_ms_key (k, &k.ks.mode_states.qks3, lalt, qks1,  gen_incr_brightness( 1));

    // win-2 is vol down, win-3 is vol up .. (fine mode does nothing different for volume)
    setup_fine_mode_ms_key (k, &k.ks.mode_states.qks2, lwin, no_ms, ag().k(VolumeDown).gen_af());
    setup_fine_mode_ms_key (k, &k.ks.mode_states.qks3, lwin, no_ms, ag().k(VolumeUp  ).gen_af());
    setup_fine_mode_ms_key (k, &k.ks.mode_states.qks2, lwin, qks1,  ag().k(VolumeDown).gen_af());
    setup_fine_mode_ms_key (k, &k.ks.mode_states.qks3, lwin, qks1,  ag().k(VolumeUp  ).gen_af());

    // win-f1 play/pause, caps-f1 toggle mute, base-case: switche-invoke, ralt for actual F1
    // (Note that there also a bunch of F1 and F2 combos in switche sections)

    k.cm .add_combo ( cg().k(F1).no_rpt().m(caps),  ag().k(VolumeMute) );
    //k.cm .add_combo ( cg().k(F1).m(lwin),  ag().k(MediaPlayPause) );
    // ^^ media keys seems to get captured by elev apps in fgnd (e.g. switche) and not pass to musicbee .. so we'll setup alts
    k.cm .add_combo ( cg().k(F1).m(lwin), ag().k(VolumeUp).m(lctrl).m(lshift) );  // gotta match w music-bee/winamp settings

    // and keeping w the theme, set caps-win-F1 (key with vol-mute printed on it) to toggle microphone mute
    //k.cm .add_combo ( cg().k(F1).m(caps).m(lwin),  ag().af (Arc::new (|| {mic_mute_toggle(); open_mic_cpl();})) );
    k.cm .add_combo ( cg().k(F1).m(caps).m(lwin),  ag().af (action (mic_mute_toggle)) );


    // want win-f2 for next with some initial skip .. we'll use caps-win-f2 for prev, so we'll set it up for both
    // note that our mechanism for wrapping mod-key-state restoring guards operates via AFs, hence setting those up (instead of fns)

    // win-f2 for next with some initial skip
    k.cm .add_combo ( cg().k(F2).m(lwin),           ag().af (media_next_action (&k.ks, true )) );
    k.cm .add_combo ( cg().k(F2).m(lwin).m(caps),   ag().af (media_next_action (&k.ks, false)) );
    k.cm .add_combo ( cg().k(F2).m(lwin).m(shift),  ag().af (media_next_action (&k.ks, false)) );

    // win-f3 for skip forward a bit (w/ caps for rewind)
    k.cm .add_combo ( cg().k(F3).m(lwin),           ag().af (media_skips_action (1, &k.ks, true )) );
    k.cm .add_combo ( cg().k(F3).m(lwin).m(caps),   ag().af (media_skips_action (2, &k.ks, false)) );
    k.cm .add_combo ( cg().k(F3).m(lwin).m(shift),  ag().af (media_skips_action (2, &k.ks, false)) );

    // gaah, for track trawling, even that is being annoying to press, wanted to set up right hand alternative too
    k.cm .add_combo ( cg().k(Down ) .m(caps_dbl),  ag().af (media_next_action (&k.ks, true )) );
    k.cm .add_combo ( cg().k(Up   ) .m(caps_dbl),  ag().af (media_next_action (&k.ks, false)) );
    k.cm .add_combo ( cg().k(Right) .m(caps_dbl),  ag().af (media_skips_action (1, &k.ks, true)) );
    k.cm .add_combo ( cg().k(Left ) .m(caps_dbl),  ag().af (media_skips_action (1, &k.ks, false)) );


    // we'll also set these on a latching fsc on F1 for sustained sessions of track trawling w arrow keys
    let fsc = k.cm.register_combo_latching_first_stroke ( cg().k(F1).m(caps_dbl) );
    k.cm .add_combo ( cg().k(Down ).fsc(fsc),  ag().af (media_next_action (&k.ks, true )) );
    k.cm .add_combo ( cg().k(Up   ).fsc(fsc),  ag().af (media_next_action (&k.ks, false)) );
    k.cm .add_combo ( cg().k(Right).fsc(fsc),  ag().af (media_skips_action (1, &k.ks, true)) );
    k.cm .add_combo ( cg().k(Left ).fsc(fsc),  ag().af (media_skips_action (1, &k.ks, false)) );



}




fn setup_l2 (k:&Krusty) {
    /* l2-setup config summary:
     - only j/k for left/right get f-for-word-nav mode speedup (native word nav by sending ctrl)
     - those and i/comma for up/down get r-for-double-speed nav mode (2x nav) .. i/comma get that for f too
     - h/l/u/m for home/end/pgup/pgdown get no speedup modes
     - e/d do sel/del modes, and those can be freely combined with the f/r word/fast nav modes
     - in del mode, left/home/up/pgup get ExtBackspace, right/end/down/pgdn get ExtDelete
     - in del mode, left/right do direct bksp/del, but others get select then bksp/del
     - and in general, for l2 alternate outputs, qks1 will layer ctrl, and msE will layer shift (lalt will still do alt)
     */

    /// action-fn generator type (e.g. to specify for various sel/del/word/fast modes for various l2-keys)
    type AFG = fn(Key) -> AF ;

    fn setup_l2_key (k:&Krusty, key:Key, l2k:Key, dk:Key, wafg:AFG, fafg:AFG, del_via_sel:bool) {

        // register nav actions for normal-nav, word-nav, and fast-nav modes
        k.cm .add_combo ( cg().k(key).m(caps),         ag().k(l2k) );
        k.cm .add_combo ( cg().k(key).m(caps).s(msF),  ag().af (wafg(l2k)) );
        k.cm .add_combo ( cg().k(key).m(caps).s(msR),  ag().af (fafg(l2k)) );

        // selection actions are via wrapping those with shift press-release
        k.cm .add_combo ( cg().k(key).m(caps).s(msE),         ag().k(l2k).m(shift) );
        k.cm .add_combo ( cg().k(key).m(caps).s(msE).s(msF),  ag().af (wafg(l2k)) .m(shift) );
        k.cm .add_combo ( cg().k(key).m(caps).s(msE).s(msR),  ag().af (fafg(l2k)) .m(shift) );

        // delete actions are dependent on whether the delete can be done directly or has to be done via selection then delete
        fn del_sel_afg (del_key:Key, nav_af:AF) -> AF {
            Arc::new ( move || {
                LShift.press(); nav_af(); LShift.release(); // dont need guards for shift here.. this is deep into multi key L2
                //press_release(del_key);
                thread::spawn ( move || { thread::sleep (Duration::from_millis(20)); del_key.press_release(); } );
        } ) }
        let (da, dwa, dfa) = if del_via_sel {
            // if deleting via selection, we wrap the del-sel action around the normal nav actions
            ( del_sel_afg(dk,base_action(l2k)), del_sel_afg(dk,wafg(l2k)), del_sel_afg(dk,fafg(l2k)) )
        } else { // and for direct deletes, we perform the nav-eqv action but with the specified delete-key
            ( base_action(dk), ctrl_action(dk), fast_action(dk) )
        };
        k.cm .add_combo ( cg().k(key).m(caps).s(msD),         ag().af(da ) );
        k.cm .add_combo ( cg().k(key).m(caps).s(msD).s(msF),  ag().af(dwa) );
        k.cm .add_combo ( cg().k(key).m(caps).s(msD).s(msR),  ag().af(dfa) );

        // additionally, we'll overlay qks1 -> ctrl, and msE -> shift on l2 keys (for ergonomics while holding caps down)
        // (shift is on msE instead of qks2, not just coz E is for sel elsewhere, but also 2wsx issues prevent qks1+qks2 layering)
        //k.cm .add_combo ( cg().k(key).m(caps).s(msE),                ag().k(l2k).m(shift) );   // covered above
        k.cm .add_combo ( cg().k(key).m(caps).m(lalt),                 ag().k(l2k).m(alt) );
        k.cm .add_combo ( cg().k(key).m(caps).s(qks1),                 ag().k(l2k).m(ctrl) );
        k.cm .add_combo ( cg().k(key).m(caps).s(qks1).m(lalt),         ag().k(l2k).m(ctrl).m(alt) );
        k.cm .add_combo ( cg().k(key).m(caps).m(lalt).s(msE),          ag().k(l2k).m(alt).m(shift) );
        k.cm .add_combo ( cg().k(key).m(caps).s(qks1).s(msE),          ag().k(l2k).m(ctrl).m(shift) );
        k.cm .add_combo ( cg().k(key).m(caps).s(qks1).m(lalt).s(msE),  ag().k(l2k).m(ctrl).m(alt).m(shift) );

        // finally we'll layer caps-as-ctrl on caps-dbl for these for easy access
        k.cm .add_combo ( cg().k(key).m(caps_dbl),          ag().k(key).m(ctrl) );
        k.cm .add_combo ( cg().k(key).m(caps_dbl).m(lalt),  ag().k(key).m(ctrl).m(alt) );

    }

    // filling out l2/l3 actions
    setup_l2_key ( k,  J,     ExtLeft,   Backspace,   ctrl_action,   fast_action,   false );
    setup_l2_key ( k,  K,     ExtRight,  ExtDelete,   ctrl_action,   fast_action,   false );
    setup_l2_key ( k,  I,     ExtUp,     Backspace,   fast_action,   fast_action,   true  );
    setup_l2_key ( k,  Comma, ExtDown,   ExtDelete,   fast_action,   fast_action,   true  );
    setup_l2_key ( k,  H,     ExtHome,   Backspace,   base_action,   base_action,   true  );
    setup_l2_key ( k,  L,     ExtEnd,    ExtDelete,   base_action,   base_action,   true  );
    setup_l2_key ( k,  U,     ExtPgUp,   Backspace,   base_action,   base_action,   true  );
    setup_l2_key ( k,  M,     ExtPgDn,   ExtDelete,   base_action,   base_action,   true  );

    // and finally to round out the l2 keys, we'll add some non-nav word actions on Space key in (msE/msD/msF)-modes
    // (word selection action is a composite of move-to-word-end then select-to-word-beginning)
    let wsa = Arc::new ( || {
        LCtrl.press(); ExtRight.press_release(); shift_press_release(ExtLeft); LCtrl.release();
    } );
    k.cm .add_combo ( cg().k(Space).m(caps).s(msE),  ag().af(wsa) );

    // (note that there are also a bunch of uses of Space key on msE/msF/msR for Enter etc .. they're up by space key mappings)

    // we can further add cut/copy/paste actions on the sel mode (caps-e-x/c/v)
    fn gen_wxcv_af (key:Key) -> AF { Arc::new ( move || {
        LCtrl.press(); ExtRight.press_release(); shift_press_release(ExtLeft); key.press_release(); LCtrl.release();
    } ) }
    k.cm .add_combo ( cg().k(X).m(caps).s(msE),  ag().af (gen_wxcv_af(X)) );
    k.cm .add_combo ( cg().k(C).m(caps).s(msE),  ag().af (gen_wxcv_af(C)) );
    k.cm .add_combo ( cg().k(V).m(caps).s(msE),  ag().af (gen_wxcv_af(V)) );

}




fn setup_qks_combos (k:&Krusty) {
    // note: there are 5 quick-keys modes (qks, qks1, qks2, qks3, qks4) on keys (q, 1, 2, 3, 4) respectively! .. all are pretty ergonomic!

    // Note that there are further caps-qks2 combos specific to IDE in the section for IDE hotkeys

    // ditto quick-paste combos
    let ditto_quick_paste__first   =  ag().k(Numrow_9).m(alt).m(ctrl).m(shift);
    let ditto_quick_paste__second  =  ag().k(Numrow_0).m(alt).m(ctrl).m(shift);
    let ditto_quick_paste__third   =  ag().k(Minus   ).m(alt).m(ctrl).m(shift);
    let ditto_quick_paste__fourth  =  ag().k(Equal   ).m(alt).m(ctrl).m(shift);

    /// qks3 shortcuts to **_ paste nth ditto clip _** (configd via alt-ctrl-shift-<key> in ditto)
    k.cm .add_combo ( cg().k(Numrow_9).m(caps).s(qks3), ditto_quick_paste__first  );
    k.cm .add_combo ( cg().k(Numrow_0).m(caps).s(qks3), ditto_quick_paste__second );
    k.cm .add_combo ( cg().k(Minus   ).m(caps).s(qks3), ditto_quick_paste__third  );
    k.cm .add_combo ( cg().k(Equal   ).m(caps).s(qks3), ditto_quick_paste__fourth );

}




fn setup_misc_standalone_combos (k:&Krusty) {
    // just using this as staging pile for anything else we want to add

    // we'll set caps-alt-p to bring up process explorer (via ctrl-shift-esc)
    k.cm .add_combo ( cg().k(P).m(caps).m(lalt),  ag().k(Escape).m(lctrl).m(lshift) );

    // chrome/browser specific combos
    // caps-alt-t --> ctrl-shift-a (tabs search popup)
    k.cm .add_combo ( cg().k(T).m(lalt).m(caps).c(browser_fgnd(k)),  ag().k(A).m(ctrl).m(shift) );

}




fn setup_window_action_tscs (k:&Krusty) {
    use RectEdgeSide::*;
    // fsc:  caps-win-w   or  caps-alt-w  .. latching
    //  - j/k/i/comma .. caps-only -> move .. w/ f -> snap .. w/ r -> resize ..
    //  - whl fwd/bkwd .. caps-only OR w/ d -> left/right .. w/ e -> up/dn .. w f/fd/fe -> snap .. r/rd/re -> resize
    //  - toggles: u -> vertmax .. m -> max .. n -> min .. t -> always-on-top .. b -> border/titlebar

    let fsc = k.cm .register_combo_sticky_first_stroke ( cg().k(W).m(caps).m(lwin) );      // caps-win-w  as fsc

    k.cm .co_register_combo_sticky_first_stroke ( cg().k(W).m(caps).m(lalt), fsc );        // caps-alt-w  as fsc too!!g

    fn ksi() -> KrustyState { KrustyState::instance() }

    // - j/k/i/comma .. caps-only -> move .. w/ F -> snap .. w/ R -> resize ..
    let setup_win_move_key = |key:Key, dx:i32, dy:i32, side_t:RectEdgeSide| {
        k.cm .add_combo ( cg().k(key).m(caps).fsc(fsc).s(msF),   ag().af (Arc::new (move || snap_closest_edge_side (&ksi(), side_t) )) );
        k.cm .add_combo ( cg().k(key).m(caps).fsc(fsc),          ag().af (Arc::new (move || win_fgnd_move_rel (dx * 40, dy * 40) )) );
        k.cm .add_combo ( cg().k(key).m(caps).fsc(fsc).s(msR),   ag().af (Arc::new (move || win_fgnd_stretch (dx * 20, dy * 20) )) );
        // and fine steps
        k.cm .add_combo ( cg().k(key).m(caps).fsc(fsc).s(qks1).s(msF),   ag().af (Arc::new (move || snap_closest_edge_side (&ksi(), side_t) )) );
        k.cm .add_combo ( cg().k(key).m(caps).fsc(fsc).s(qks1),          ag().af (Arc::new (move || win_fgnd_move_rel (dx * 4, dy * 4) )) );
        k.cm .add_combo ( cg().k(key).m(caps).fsc(fsc).s(qks1).s(msR),   ag().af (Arc::new (move || win_fgnd_stretch (dx * 2, dy * 2) )) );
    };
    setup_win_move_key ( J,     -1,  0,  Left  );
    setup_win_move_key ( K,      1,  0,  Right );
    setup_win_move_key ( I,      0, -1,  Top   );
    setup_win_move_key ( Comma,  0,  1,  Bottom);

    // - wheel fwd/bkwd .. caps-only OR w/ D -> left/right .. w/ E -> up/dn .. w F/FD/FE -> snap .. r/rd/re -> resize

    setup_frwd_bkwd_whl (k, |wg| wg.m(caps).fsc(fsc).s(msF),         Right, Left,  |ag,p| ag.af (Arc::new (move || snap_closest_edge_side (&ksi(),p) )) );
    setup_frwd_bkwd_whl (k, |wg| wg.m(caps).fsc(fsc).s(msF).s(msD),  Right, Left,  |ag,p| ag.af (Arc::new (move || snap_closest_edge_side (&ksi(),p) )) );
    setup_frwd_bkwd_whl (k, |wg| wg.m(caps).fsc(fsc).s(msF).s(msE),  Bottom, Top,  |ag,p| ag.af (Arc::new (move || snap_closest_edge_side (&ksi(),p) )) );

    setup_frwd_bkwd_whl (k, |wg| wg.m(caps).fsc(fsc),         1, -1,  |ag,p| ag.af (Arc::new (move || win_fgnd_move_rel (p * 20, 0) )) );
    setup_frwd_bkwd_whl (k, |wg| wg.m(caps).fsc(fsc).s(msD),  1, -1,  |ag,p| ag.af (Arc::new (move || win_fgnd_move_rel (p * 20, 0) )) );
    setup_frwd_bkwd_whl (k, |wg| wg.m(caps).fsc(fsc).s(msE),  1, -1,  |ag,p| ag.af (Arc::new (move || win_fgnd_move_rel (0, p * 20) )) );

    setup_frwd_bkwd_whl (k, |wg| wg.m(caps).fsc(fsc).s(msR),         1, -1,  |ag,p| ag.af (Arc::new (move || win_fgnd_stretch (p * 10, 0) )) );
    setup_frwd_bkwd_whl (k, |wg| wg.m(caps).fsc(fsc).s(msR).s(msD),  1, -1,  |ag,p| ag.af (Arc::new (move || win_fgnd_stretch (p * 10, 0) )) );
    setup_frwd_bkwd_whl (k, |wg| wg.m(caps).fsc(fsc).s(msR).s(msE),  1, -1,  |ag,p| ag.af (Arc::new (move || win_fgnd_stretch (0, p * 10) )) );


    // toggles: u -> vertmax .. m -> max .. n -> min .. t -> always-on-top .. b -> border/titlebar
    k.cm .add_combo ( cg().k(U).m(caps).fsc(fsc),   ag().af (action (win_fgnd_toggle_vertmax)) );
    k.cm .add_combo ( cg().k(M).m(caps).fsc(fsc),   ag().af (action (win_fgnd_toggle_max)) );
    k.cm .add_combo ( cg().k(N).m(caps).fsc(fsc),   ag().af (action (win_fgnd_min_and_back)) );
    k.cm .add_combo ( cg().k(T).m(caps).fsc(fsc),   ag().af (action (win_fgnd_toggle_always_on_top)) );
    k.cm .add_combo ( cg().k(B).m(caps).fsc(fsc),   ag().af (action (win_fgnd_toggle_titlebar)) );

    // regardless, since max/vertmax toggles are pretty frequent, we'll for now retain their direct combos too
    k.cm .add_combo ( cg().k(U).m(caps).m(lwin),  ag().af (action (win_fgnd_toggle_vertmax)) );
    k.cm .add_combo ( cg().k(M).m(caps).m(lwin),  ag().af (action (win_fgnd_toggle_max)) );

}



fn setup_switch_desktop_tscs (k:&Krusty) {
    // caps-win-d as fsc for desktop moves .. w jk arrow keys, as well as wheel

    let fsc = k.cm .register_combo_sticky_first_stroke ( cg().k(D).m(caps).m(lwin) );

    k.cm .add_combo ( cg().k(J    ).m(caps).fsc(fsc),   ag().k(ExtLeft ).m(win).m(ctrl) );
    k.cm .add_combo ( cg().k(K    ).m(caps).fsc(fsc),   ag().k(ExtRight).m(win).m(ctrl) );
    k.cm .add_combo ( cg().k(Left ).m(caps).fsc(fsc),   ag().k(ExtLeft ).m(win).m(ctrl) );
    k.cm .add_combo ( cg().k(Right).m(caps).fsc(fsc),   ag().k(ExtRight).m(win).m(ctrl) );

    // and for wheel
    k.cm .add_combo ( cg().whl().frwd().m(caps).fsc(fsc),   ag().k(ExtLeft ).m(win).m(ctrl) );
    k.cm .add_combo ( cg().whl().bkwd().m(caps).fsc(fsc),   ag().k(ExtRight).m(win).m(ctrl) );

}



fn setup_tab_nav_tscs (k:&Krusty) {
    // fsc : caps-e-t .. sticky
    let fsc = k.cm .register_combo_sticky_first_stroke ( cg().k(T).m(caps).s(msE) );

    // (note that these have been kept uniform between IDE, chrome, npp etc)
    let tab_nav_right = ag().k(PageDown).m(ctrl);
    let tab_nav_left  = ag().k(PageUp  ).m(ctrl);

    k.cm .add_combo ( cg().k(K).m(caps).fsc(fsc),  tab_nav_right.clone() );
    k.cm .add_combo ( cg().k(J).m(caps).fsc(fsc),  tab_nav_left.clone() );

    k.cm .add_combo ( cg().whl().bkwd().m(caps).fsc(fsc),  tab_nav_right.clone() );
    k.cm .add_combo ( cg().whl().frwd().m(caps).fsc(fsc),  tab_nav_left.clone() );

    // regardless, we'll also add the caps-e-wheel, as usage patterns sometimes seem to prefer that
    k.cm .add_combo ( cg().whl().bkwd().m(caps).s(msE),  tab_nav_right );
    k.cm .add_combo ( cg().whl().frwd().m(caps).s(msE),  tab_nav_left );

}



fn setup_arrow_wheel_tscs (k:&Krusty) {
    // fsc : caps-caps-q-q .. latching
    let fsc = k.cm .register_combo_latching_first_stroke ( cg().k(Q).s(qks_dbl).m(caps_dbl) );
    // turn regular wheel scroll into arrow-nav
    k.cm .add_combo ( cg().whl().bkwd().fsc(fsc),  ag().k(ExtDown) );
    k.cm .add_combo ( cg().whl().frwd().fsc(fsc),  ag().k(ExtUp) );
    // and caps-dbl wheel into regular wheel
    k.cm .add_combo ( cg().whl().bkwd().fsc(fsc).m(caps_dbl),  ag().whl().bkwd() );
    k.cm .add_combo ( cg().whl().frwd().fsc(fsc).m(caps_dbl),  ag().whl().bkwd() );

}



fn setup_kbd_pointer_tscs (k:&Krusty) {
    // fsc : caps-e-e-m .. sticky
    let fsc = k.cm .register_combo_sticky_first_stroke ( cg().k(M).m(caps).s(msE_dbl) );
    let v : i32 = 20;

    // first the cardinal mouse directions .. the msE wc simply to avoid inadvertent actions (e.g when adding cursors)
    k.cm .add_combo ( cg().k(I     ).m(caps).fsc(fsc).wcs(msE),  ag().pointer() .move_rel ( 0, -v) );
    k.cm .add_combo ( cg().k(Comma ).m(caps).fsc(fsc).wcs(msE),  ag().pointer() .move_rel ( 0,  v) );
    k.cm .add_combo ( cg().k(J     ).m(caps).fsc(fsc).wcs(msE),  ag().pointer() .move_rel (-v,  0) );
    k.cm .add_combo ( cg().k(K     ).m(caps).fsc(fsc).wcs(msE),  ag().pointer() .move_rel ( v,  0) );
    // then diagonal directions
    k.cm .add_combo ( cg().k(U     ).m(caps).fsc(fsc),  ag().pointer() .move_rel (-v, -v) );
    k.cm .add_combo ( cg().k(M     ).m(caps).fsc(fsc),  ag().pointer() .move_rel (-v,  v) );
    k.cm .add_combo ( cg().k(O     ).m(caps).fsc(fsc),  ag().pointer() .move_rel ( v, -v) );
    k.cm .add_combo ( cg().k(Period).m(caps).fsc(fsc),  ag().pointer() .move_rel ( v,  v) );

    // we'll add in click and right click
    k.cm .add_combo ( cg().k(Space ).m(caps).fsc(fsc),         ag().mbtn(LeftButton ) );
    k.cm .add_combo ( cg().k(Space ).m(caps).fsc(fsc).s(msR),  ag().mbtn(RightButton) );

    // and finally, the alt-shift-click in IDE to add extra cursors that we wanted
    k.cm .add_combo ( cg().k(Space ).m(caps).fsc(fsc).s(msE),  ag().mbtn(LeftButton ).m(alt).m(shift) );

}



fn setup_ctrl_tab_tscs (k:&Krusty) {
    // caps-as-ctrl for caps-tab switching (and shift/ralt combos will work out naturally in fallbacks)
    // note that caps-as-ctrl is default in fallbacks anyway, but IDE doesnt like the ctrl being pressed/rel for every tab press ..
    // .. so instead, we keep the ctrl active throughout the caps-tabbing, hence the need for the defs below
    // note also that there's also separate tab-nav two-stroke combos .. this is specifically for ctrl-tab nav

    // fscs : caps-tab or ctrl-tab or caps-ctrl-tab
    let fsc = k.cm .register_combo_sticky_first_stroke ( cg().k(Tab).m(caps) );
    k.cm.co_register_combo_sticky_first_stroke ( cg().k(Tab).m(ctrl), fsc );
    k.cm.co_register_combo_sticky_first_stroke ( cg().k(Tab).m(ctrl).m(caps), fsc );

    // in addition to just registering the fsc action, we also want the Tab to actually send itself out
    let ks = k.ks.clone();
    let af_caps_tab : AF = Arc::new (move || { ks.mod_keys.lctrl.ensure_active(); Tab.press_release(); } );
    k.cm .add_combo ( cg().k(Tab).m(caps),           ag().af (af_caps_tab.clone()) );
    k.cm .add_combo ( cg().k(Tab).m(caps).m(ctrl),   ag().af (af_caps_tab) );
    k.cm .add_combo ( cg().k(Tab).m(ctrl),           ag().k(Tab).m(ctrl) );

    // note that shift/ralt will work on these as-is .. as the fallbacks dont care about our fscs!
    // and now we can set about the rest of behavior under that fsc ..

    // we'll set up the wheel for caps-tab (and ctrl-tab)
    setup_frwd_bkwd_whl ( k, |wg| wg.fsc(fsc).m(caps),           ExtDown, ExtUp,   |ag,key| ag.k(key).m(ctrl) );
    setup_frwd_bkwd_whl ( k, |wg| wg.fsc(fsc).m(ctrl),           ExtDown, ExtUp,   |ag,key| ag.k(key).m(ctrl) );
    setup_frwd_bkwd_whl ( k, |wg| wg.fsc(fsc).m(ctrl).m(caps),   ExtDown, ExtUp,   |ag,key| ag.k(key).m(ctrl) );

    // and since we've had to keep ctrl forced-active for IDE, now it doesnt like our l2 arrow navs, so we'll send them mkg-wrap-disabled
    [ (J, ExtLeft), (K, ExtRight), (I, ExtUp), (Comma, ExtDown), (U, ExtPgUp), (M, ExtPgDn) ] .into_iter() .for_each ( |(ko,kn)| {
        k.cm .add_combo ( cg().k(ko).fsc(fsc).m(caps),           ag().k(kn).mkg_nw() );
        k.cm .add_combo ( cg().k(ko).fsc(fsc).m(ctrl),           ag().k(kn).mkg_nw() );
        k.cm .add_combo ( cg().k(ko).fsc(fsc).m(ctrl).m(caps),   ag().k(kn).mkg_nw() );
    } );

    /// Now specifally for the IDE tab-switcher popup, to escape out of it ..
    // .. sending esc while ctrl-active triggers win-start-menu
    // .. so we use the trick of pressing space first to defocus from the list, then releasing ctrl to exit out of it
    let ct_esc = {
        let ks = k.ks.clone();
        Arc::new ( move || {
            Space.press_release();
            ks.mod_keys.lctrl.ensure_inactive();
        } )
    };
    k.cm .add_combo ( cg().k(Escape).fsc(fsc).m(caps),    ag().af(ct_esc.clone()) );

    // and another easier version to go along with our caps-e-o as esc elsewhere
    k.cm .add_combo ( cg().k(O).fsc(fsc).m(caps).s(msE),  ag().af(ct_esc.clone()) );


    /// again for the IDE switcher popup, we wanted to add a quick switch from tab-switcher to searchable one
    // (we'll do it by escaping it first (via space then ctrl rel like above), then invoking the searchable switcher)
    let ide_pers_switcher = {
        let ks = k.ks.clone();
        Arc::new ( move || {
            ct_esc();   // first we escape out of it as above
            let ks = ks.clone();
            // we'll want to give a tiny delay so IDE has time to process focus changes appropriately
            thread::spawn ( move || {
                thread::sleep (Duration::from_millis(10));
                // then do actual ctrl-e to bring up the persistent-switcher (as configd in IDE)
                ks.mod_keys.lctrl.active_on_key(E)()
            } );
        } )
    };
    //k.cm .add_combo ( cg().k(Space).m(caps).fsc(fsc),  ag().af(ide_pers_switcher) );
    k.cm .add_combo ( cg().k(Space).m(caps).fsc(fsc) .c(intellij_fgnd(k)),  ag().af(ide_pers_switcher) );

}



fn setup_ide_diff_nav_tscs (k:&Krusty) {

    // diff nav mode on dbl-caps-e-d .. (latching mode usually on caps-dbl, e->edit, d->diff)
    let fsc = k.cm .register_combo_latching_first_stroke ( cg().k(D).m(caps_dbl).s(msE) );

    // ^^ we've put this in latching fsc .. and made even regular wheel (w/o caps) do diff nav ..
    // .. (so we'll have to clear out the latched-fsc (via caps-dbl-End etc) before the wheel reverts to normal!)

    // next/prev diff --> wheel, caps-wheel
    setup_frwd_bkwd_whl ( k, |wg| wg.fsc(fsc),                 ExtDown,  ExtUp,    |ag,key| ag.k(key).m(ctrl).m(alt) );
    setup_frwd_bkwd_whl ( k, |wg| wg.fsc(fsc).m(caps),         ExtDown,  ExtUp,    |ag,key| ag.k(key).m(ctrl).m(alt) );
    // next/prev file --> caps-f-wheel
    setup_frwd_bkwd_whl ( k, |wg| wg.fsc(fsc).m(caps).s(msF),  ExtRight, ExtLeft,  |ag,key| ag.k(key).m(ctrl).m(alt).m(shift) );
    // accept l/r  --> caps-e-wheel
    setup_frwd_bkwd_whl ( k, |wg| wg.fsc(fsc).m(caps).s(msE),  ExtRight, ExtLeft,  |ag,key| ag.k(key).m(ctrl).m(alt) );

    // and during this time, we'll put regular wheel on caps-dbl-wheel .. (which otherwise would do horiz-wheel)
    k.cm .add_combo ( cg().whl().frwd().fsc(fsc).m(caps_dbl),   ag().whl().frwd() );
    k.cm .add_combo ( cg().whl().bkwd().fsc(fsc).m(caps_dbl),   ag().whl().bkwd() );

    // next/prev diff
    k.cm .add_combo ( cg().k(Comma).fsc(fsc).m(caps),   ag().k(ExtDown ).m(ctrl).m(alt) );
    k.cm .add_combo ( cg().k(I    ).fsc(fsc).m(caps),   ag().k(ExtUp   ).m(ctrl).m(alt) );
    // and for next/prev file
    k.cm .add_combo ( cg().k(J).fsc(fsc).m(caps),   ag().k(ExtLeft ).m(ctrl).m(alt).m(shift) );
    k.cm .add_combo ( cg().k(K).fsc(fsc).m(caps),   ag().k(ExtRight).m(ctrl).m(alt).m(shift) );
    // and accept left/right
    k.cm .add_combo ( cg().k(J).fsc(fsc).m(caps).s(msE),   ag().k(ExtLeft ).m(ctrl).m(alt) );
    k.cm .add_combo ( cg().k(K).fsc(fsc).m(caps).s(msE),   ag().k(ExtRight).m(ctrl).m(alt) );

    // and for actual arrow-keys as well .. next/prev
    k.cm .add_combo ( cg().k(Down ).fsc(fsc),  ag().k(ExtDown ).m(ctrl).m(alt) );
    k.cm .add_combo ( cg().k(Up   ).fsc(fsc),  ag().k(ExtUp   ).m(ctrl).m(alt) );
    // next/prev file
    k.cm .add_combo ( cg().k(Left ).fsc(fsc),  ag().k(ExtLeft ).m(ctrl).m(alt).m(shift) );
    k.cm .add_combo ( cg().k(Right).fsc(fsc),  ag().k(ExtRight).m(ctrl).m(alt).m(shift) );
    // and for accept left/right
    k.cm .add_combo ( cg().k(Left ).fsc(fsc).m(caps),  ag().k(ExtLeft ).m(ctrl).m(alt) );
    k.cm .add_combo ( cg().k(Right).fsc(fsc).m(caps),  ag().k(ExtRight).m(ctrl).m(alt) );

}




fn setup_win_groups (k:&Krusty) {
    // we'll assign the Numrow_[1/2/3/4] (overloaded with qks1/qks2/qks3/qks4) for win-grp activations
    // and when those are held down (hence why assigned to qks keys), we'll have T/W etc do actions on those groups
    // (Note that there are also mouse lbtn-dbl-click and rbtn-click combos (defined in mouse sections above) for add/remove to wingroups)
    fn set_win_grp_af_combos <F> (k:&Krusty, key:Option<Key>, f:F)
        where F : Fn (&KrustyState, WinGroups_E) + Clone + Send + Sync + 'static
    {
        fn wgs (wg: WinGroups_E) -> ModeState_T {
            match wg { wg1 => qks1,  wg2 => qks2,  wg3 => qks3,  wg4 => qks4 }
        }
        fn wgk (wg: WinGroups_E) -> Key {
            match wg { wg1 => Numrow_1,  wg2 => Numrow_2,  wg3 => Numrow_3,  wg4 => Numrow_4 }
        }
        let gen_af = |wg:WinGroups_E, f:&F| {
            let ks = k.ks.clone(); let f = f.clone();
            Arc::new ( move || f (&ks, wg) )
        };
        k.cm .add_combo ( cg().k( key.unwrap_or_else(|| wgk(wg1)) ).s(wgs(wg1)).m(caps).m(lwin),  ag().af (gen_af (wg1, &f)) );
        k.cm .add_combo ( cg().k( key.unwrap_or_else(|| wgk(wg2)) ).s(wgs(wg2)).m(caps).m(lwin),  ag().af (gen_af (wg2, &f)) );
        k.cm .add_combo ( cg().k( key.unwrap_or_else(|| wgk(wg3)) ).s(wgs(wg3)).m(caps).m(lwin),  ag().af (gen_af (wg3, &f)) );
        k.cm .add_combo ( cg().k( key.unwrap_or_else(|| wgk(wg4)) ).s(wgs(wg4)).m(caps).m(lwin),  ag().af (gen_af (wg4, &f)) );
    }
    // finally we can now set up actions (which will be set up for each of the three win-groups)
    set_win_grp_af_combos ( k, None,    |ks,wg| ks.win_groups.toggle_grp_activation(wg) );
    set_win_grp_af_combos ( k, Some(T), |ks,wg| ks.win_groups.toggle_grp_always_on_top(wg) );
    set_win_grp_af_combos ( k, Some(W), |ks,wg| ks.win_groups.close_grp_windows(wg) );

}




fn setup_switche_combos (k:&Krusty) {
    // (Note that there also a bunch of these in mouse/wheel sections)

    let switche_invoke                   =  ag().k(F15).m(alt).m(ctrl);
    let switche_direct__z_top            =  ag().k(F16).m(alt).m(ctrl);
    let switche_direct__z_second         =  ag().k(F17).m(alt).m(ctrl);
    let switche_direct__z_third          =  ag().k(F18).m(alt).m(ctrl);

    let switche_direct__claude           =  ag().k(F19).m(alt).m(ctrl);
    let switche_direct__tabs_outliner    =  ag().k(F20).m(alt).m(ctrl);
    let switche_direct__notepadpp        =  ag().k(F21).m(alt).m(ctrl);
    let switche_direct__ide              =  ag().k(F22).m(alt).m(ctrl);
    let switche_direct__music            =  ag().k(F23).m(alt).m(ctrl);
    let switche_direct__browser          =  ag().k(F24).m(alt).m(ctrl);
    let switche_direct__kbd_evs_printer  =  ag().k(F24).m(alt).m(shift);


    k.cm .add_combo ( cg().k(F1),          switche_invoke  );
    k.cm .add_combo ( cg().k(F1).m(ralt),  ag().k(F1) );
    // ^^ this allows actual F1 use (if we disable F1 in swi configs)

    k.cm .add_combo ( cg().k(F1).m(lalt),      switche_direct__z_top     .clone() );
    k.cm .add_combo ( cg().k(F1).m(lalt_dbl),  switche_direct__z_second  .clone() );
    k.cm .add_combo ( cg().k(F2).m(lalt_dbl),  switche_direct__z_third   .clone() );


    // we'll set Alt-F2 to bring chrome tabs-outliner (via switche) to keep w the theme of Alt-F<n> keys for task switching
    k.cm .add_combo ( cg().k(F2).m(lalt),      switche_direct__tabs_outliner.clone() );

    // we'll put app-specific direct-switch on lalt-qks1 combos
    k.cm .add_combo ( cg().k(L).m(lalt).s(qks1),  switche_direct__z_top           );   // L -> last-active
    k.cm .add_combo ( cg().k(B).m(lalt).s(qks1),  switche_direct__browser         );   // B -> first browser window
    k.cm .add_combo ( cg().k(M).m(lalt).s(qks1),  switche_direct__music           );   // M -> winamp (music)
    k.cm .add_combo ( cg().k(I).m(lalt).s(qks1),  switche_direct__ide             );   // I -> first IDEA window
    k.cm .add_combo ( cg().k(N).m(lalt).s(qks1),  switche_direct__notepadpp       );   // N -> Notepad++
    k.cm .add_combo ( cg().k(O).m(lalt).s(qks1),  switche_direct__tabs_outliner   );   // O -> TabsOutliner (chrome)
    k.cm .add_combo ( cg().k(C).m(lalt).s(qks1),  switche_direct__claude          );   // C -> Claude (chrome)
    k.cm .add_combo ( cg().k(K).m(lalt).s(qks1),  switche_direct__kbd_evs_printer );   // K -> kbd-events-printer (chrome)


    /// switche tweak for **_ ALT-TAB _**
    // in general, alt-tab is direclty listened to by switche, so we no longer drive it from here ..
    // however, we'll handle the case for caps-alt-tab which we'll set to do switche within-block nav (shift up/dn)
    k.cm .add_combo ( cg().k(Tab  ).m(caps).m(alt)  .c(switche_fgnd(k)),  ag().k(ExtUp).m(alt).m(shift) );
    // and for completeness, might as well set that conditional override for generic l2 nav too
    k.cm .add_combo ( cg().k(I    ).m(caps).m(alt)  .c(switche_fgnd(k)),  ag().k(ExtUp  ).m(alt).m(shift) );
    k.cm .add_combo ( cg().k(Comma).m(caps).m(alt)  .c(switche_fgnd(k)),  ag().k(ExtDown).m(alt).m(shift) );
    k.cm .add_combo ( cg().k(U    ).m(caps).m(alt)  .c(switche_fgnd(k)),  ag().k(ExtPgUp).m(alt).m(shift) );
    k.cm .add_combo ( cg().k(M    ).m(caps).m(alt)  .c(switche_fgnd(k)),  ag().k(ExtPgDn).m(alt).m(shift) );

}




fn setup_IDE_combos (k:&Krusty) {
    // helper fn for sequential actions
    fn compose_seq_actions (af_a:AF, af_b:AF) -> AF {
        Arc::new ( move || { af_a(); af_b(); } )
    }
    /// Setting up two-stroke-combos for IDE use .. (not krusty two-stroke combos .. IDEs internal two-stroke combos)
    // sadly though, intellij doesnt not suppress default action on the second-stroke-hotkey even when it follows the first-stroke ..
    // .. meaning, its only useful if neither the first-stroke, nor the second-stroke are ever used as direct hotkeys ..
    // .. this ofc, greatly limits the usefulness and the possibilities-space .. but with some careful allocations could be made useful
    // so .. for our use, we'll try and define some restrictions for ourselves to make this usable
    // .. we'll use .. alt-F13-F17 (5 fn keys) as first strokes, and alt-F18-F24 (7 Fn keys) as second strokes
    // .. that'll give us 5*7=35 combinations, which should be more than plenty .. (neither of these should be used as direct hotkeys)
    // .. (plus other unused hotkey blocks could be anything with Numpad-[0-9], Alt-[0-9] etc etc)
    fn ide_two_stroke_combo (s1k:Key, s2k:Key) -> AF {
        let s1c = ag().k(s1k).m(lalt).gen_af();
        let s2c = ag().k(s2k).m(lalt).gen_af();
        Arc::new ( move || { s1c(); s2c(); } )
    }

    // some generated AFs for IDE cmds use

    let goto_ref_usage  =  ag().k(ExtDown).m(alt).m(ctrl);
    let goto_impl_decl  =  ag().k(ExtUp  ).m(alt).m(ctrl);

    let popup_bookmarks_viewer  =  ag().k(F11).m(shift);
    let caret_bookmark_toggle   =  ag().k(F11).m(ctrl).m(shift);
    let bookmark_next           =  ag().k(ExtDown).m(alt).m(ctrl).m(shift);
    let bookmark_prev           =  ag().k(ExtUp  ).m(alt).m(ctrl).m(shift);

    let collapse_nav_tree  =  ag().k(Slash    ).m(ctrl).m(alt).m(shift);
    let expand_nav_tree    =  ag().k(Backslash).m(ctrl).m(alt).m(shift);

    let caret_to_block_start  =  ag().k(LBracket).m(alt);
    let caret_to_block_end    =  ag().k(RBracket).m(alt);
    let sel_to_block_start    =  ag().k(LBracket).m(alt).m(shift);
    let sel_to_block_end      =  ag().k(RBracket).m(alt).m(shift);

    let caret_to_matching_brace  =  ag().k(P).m(ctrl).m(shift);
    // unfortunately, there's no support in IDE for selecting while moving to matching brace .. :(

    let expand_selection  =  ag().k(ExtUp  ).m(alt).m(shift);
    let shrink_selection  =  ag().k(ExtDown).m(alt).m(shift);

    let toggle_column_mode   =  ag().k(C).m(alt).m(shift);
    let extend_caret_above   =  ag().af (ide_two_stroke_combo (F13, F19));
    let extend_caret_below   =  ag().af (ide_two_stroke_combo (F13, F20));

    let duplicate_line  =  ag().k(L    ).m(alt).m(ctrl);
    let move_line_up    =  ag().k(I    ).m(alt).m(ctrl);
    let move_line_dn    =  ag().k(Comma).m(alt).m(ctrl);
    let move_stmt_up    =  ag().k(I    ).m(alt).m(ctrl).m(shift);
    let move_stmt_dn    =  ag().k(Comma).m(alt).m(ctrl).m(shift);

    let show_file_git_diff   =  ag().k(D).m(ctrl).m(alt).m(shift);
    let toggle_diff_preview  =  ag().af (ide_two_stroke_combo (F13, F18));

    //let tab_nav_left  = ag().k(PageUp  ).m(ctrl);
    //let tab_nav_right = ag().k(PageDown).m(ctrl);
    // these are kept uniform between IDE, chrome, npp etc .. so these are covered by tab-nav-tscs


    k.cm .add_combo ( cg().k(G).m(caps).s(msE),   show_file_git_diff );
    k.cm .add_combo ( cg().k(P).m(caps).s(qks3),  toggle_diff_preview );

    k.cm .add_combo ( cg().k(Comma).m(caps).m(lalt).s(msR),  goto_ref_usage );
    k.cm .add_combo ( cg().k(I    ).m(caps).m(lalt).s(msR),  goto_impl_decl );
    // ^^ note that these two can have very similar results, e.g for fn usage etc etc

    // .. note that there's natural caps-alt-<l2> that does nav among last caret locations (via alt-left/right)

    k.cm .add_combo ( cg().k(K).m(caps).s(qks2),  popup_bookmarks_viewer );
    k.cm .add_combo ( cg().k(U).m(caps).s(qks2),  caret_bookmark_toggle  );

    k.cm .add_combo ( cg().k(I    ).m(caps).s(qks2),  bookmark_prev );
    k.cm .add_combo ( cg().k(Comma).m(caps).s(qks2),  bookmark_next );

    k.cm .add_combo ( cg().k(Backslash).m(caps).s(msF),  expand_nav_tree.clone() );
    k.cm .add_combo ( cg().k(Numrow_8 ).m(caps).s(msF),  expand_nav_tree );
    k.cm .add_combo ( cg().k(Slash    ).m(caps).s(msF),  collapse_nav_tree );

    k.cm .add_combo ( cg().k(LBracket).m(caps).s(msF),  caret_to_matching_brace.clone() );
    k.cm .add_combo ( cg().k(RBracket).m(caps).s(msF),  caret_to_matching_brace.clone() );

    k.cm .add_combo ( cg().k(LBracket).m(lalt),         caret_to_block_start );
    k.cm .add_combo ( cg().k(RBracket).m(lalt),         caret_to_block_end );
    k.cm .add_combo ( cg().k(LBracket).m(caps).s(msE),  sel_to_block_start );
    k.cm .add_combo ( cg().k(RBracket).m(caps).s(msE),  sel_to_block_end   );

    k.cm .add_combo ( cg().k(Equal).m(caps).s(msE),  expand_selection );
    k.cm .add_combo ( cg().k(Minus).m(caps).s(msE),  shrink_selection );

    k.cm .add_combo ( cg().k(N).m(caps).s(qks3),  duplicate_line.clone() );
    k.cm .add_combo ( cg().k(N).m(caps).s(msE ),  duplicate_line.clone() );

    k.cm .add_combo ( cg().k(I    ).m(caps).s(qks3),         move_line_up );
    k.cm .add_combo ( cg().k(Comma).m(caps).s(qks3),         move_line_dn );
    k.cm .add_combo ( cg().k(I    ).m(caps).s(qks3).s(msR),  move_stmt_up );
    k.cm .add_combo ( cg().k(Comma).m(caps).s(qks3).s(msR),  move_stmt_dn );

    k.cm .add_combo ( cg().k(Numrow_9).m(caps).s(msE),  toggle_column_mode );

    k.cm .add_combo ( cg().k(I    ).m(caps).s(msE_dbl),  extend_caret_above );
    k.cm .add_combo ( cg().k(Comma).m(caps).s(msE_dbl),  extend_caret_below );

    k.cm .add_combo ( cg().k(O).m(caps).s(msE),      ag().k(Escape) );    // multi-caret escape sugar
    k.cm .add_combo ( cg().k(O).m(caps).s(msE_dbl),  ag().k(Escape) );    // multi-caret escape sugar

    k.cm .add_combo ( cg().k(Numrow_8).m(caps).s(msE),  ag().k(Insert) );         // insert-mode toggle
    k.cm .add_combo ( cg().k(Slash   ).m(caps).s(msE),  ag().k(Slash).m(ctrl) );  // block-comment





    // w github copilot, we set ctrl-right (via caps-f-k) picks up the next word, which works nicely w regular l2 ..
    // however, caps-l (for end) doesnt have ctrl-end for caps-f-l (as that would do things like pgup/pgdn typically) ..
    // so instead, we'll layer that on lalt instead (and use alt-end for activation instead)
    //k.cm .add_combo ( cg().k(K).m(caps).m(lalt).c(intellij_fgnd(k)),  ag().k(End).m(alt) );
    // ^^ nah, that conflicts w the natural l2 alt-left, alt-right, which we also make use of in IDE already for last loc nav
    // at which point, we might as well use at least the alt-layered L for taking the whole multiline suggestion (via ctrl-alt-end)
    //k.cm .add_combo ( cg().k(L).m(caps).m(lalt).c(intellij_fgnd(k)),  ag().k(End).m(alt).m(ctrl) );
    //k.cm .add_combo ( cg().k(L).m(caps).m(lalt),  ag().k(Tab).mkg_nw() ); // no guard as alt is down, but w caps, so itll be inactive
    //
    // instead, we'll just use alt-k for next-line and alt-l for full multiline suggestion (along w tab too)


    // some setups for IDE interpreter use etc

    let line_sel          =  ag().k(ExtHome).m(alt).m(shift);
    let line_repl_send    =  ag().k(ExtEnd ).m(alt).m(shift);
    let caret_line_start  =  ag().k(ExtHome);
    let caret_sel_start   =  ag().k(ExtLeft);

    let sel_page       =  ag().k(A).m(ctrl);
    let sel_send       =  ag().k(End).m(alt).m(shift);
    let esc_send       =  ag().k(Escape);
    let sel_format     =  ag().k(F).m(ctrl).m(shift);
    let clear_console  =  ag().k(Minus).m(alt).m(ctrl).m(shift);

    // we'll define an action wrapper to clear console, e.g. right after sending submission (console needs focus for clear to work)
    let console_clearing_af = |af:AF| -> AF {
        let cc = clear_console.clone().gen_af();
        Arc::new ( move || {
            af();
            let cc = cc.clone();
            thread::spawn ( move || { thread::sleep (Duration::from_millis(2000)); cc(); } );
        } )
    };

    let line_to_repl : AF = {
        let (a, b, c, d) = (line_sel.gen_af(), line_repl_send.gen_af(), caret_sel_start.gen_af(), caret_line_start.gen_af());
        Arc::new ( move || { a(); b(); c(); d(); } )
    };

    // and we'll add a full page send to repl action too .. is useful for things like lcqs
    let page_to_repl : AF = {
        let (a,b,c) = ( sel_page.clone().gen_af(), sel_send.clone().gen_af(), esc_send.clone().gen_af() );
        Arc::new ( move || { a(); b(); c(); } )
    };

    let format_page : AF = {
        let (a,b,c) = ( sel_page.clone().gen_af(), sel_format.gen_af(), esc_send.clone().gen_af() );
        Arc::new ( move || {
            a(); b();
            let c = c.clone();
            thread::spawn ( move || { thread::sleep(Duration::from_millis(100)); c() } );
        } )  // ^^ format needs some bit of time before we esc selection
    };

    k.cm .add_combo ( cg() .k(F2) .c(intellij_fgnd(k)),                   ag().af (line_to_repl) );
    k.cm .add_combo ( cg() .k(F2) .c(intellij_fgnd(k)) .m(caps).m(ralt),  ag().af (page_to_repl.clone()) );
    k.cm .add_combo ( cg() .k(I ) .c(intellij_fgnd(k)) .m(caps).s(qks ),  ag().af (page_to_repl.clone()) );
    k.cm .add_combo ( cg() .k(F ) .c(intellij_fgnd(k)) .m(caps).s(qks ),  ag().af (format_page) );

    // and caps-F2 will simply send selection to repl as is (w/o selecting full line etc)
    k.cm .add_combo ( cg() .k(F2) .c(intellij_fgnd(k)) .m(caps),  sel_send.clone() );


    // caps-q-h to directly open search in the find window (rather than in the search overlay)
    let search_replace  = ag().k(H).m(alt);
    let ide_search_in_new_window = compose_seq_actions (
        search_replace.gen_af(),
        ag().k(Enter).m(ctrl).gen_af()
    );
    k.cm .add_combo ( cg().k(H).m(caps).s(qks),   ag().af (ide_search_in_new_window) );


    // IDE lcq helpers .. we'll put these on two stroke .. s1: Alt-F14 .. s2: Alt-[F24-F21]
    let lc_tool   = ide_two_stroke_combo (F14, F24);   // bring up lc tool window
    let lc_run    = ide_two_stroke_combo (F14, F23);   // locally run lc solution
    let lc_submit = ide_two_stroke_combo (F14, F22);   // submit file to lc
    let lc_tests  = ide_two_stroke_combo (F14, F21);   // get additional lc test-cases

    k.cm .add_combo ( cg().k(L).s(qks).m(caps),  ag().af (lc_tool) );    // caps-q-l .. lc_tool
    k.cm .add_combo ( cg().k(M).s(qks).m(caps),  ag().af (lc_tests) );   // caps-q-m .. more lc test-cases

    k.cm .add_combo ( cg().k(Period).m(lalt),         ag().af (console_clearing_af (lc_run)) );     // alt-period .. lc-run
    k.cm .add_combo ( cg().k(Period).s(qks).m(caps),  ag().af (console_clearing_af (lc_submit)) );  // caps-q-period .. lc-submit


    // note there's also some fancy ide-tab-switcher tweaks in ctrl-tab first-stroke-combo block


    /// **_ IDE FLOAT TOOLS TOGGLE / CLEAR _**
    // this one is a hack around intellij not giving a shortcut action to hide floating tool windows
    fn ide_float_tools_toggle() {
        // note that for the enum query for this, we've disabled filtering out hidden windows
        thread::spawn ( move || {
            let hwnds = win_get_ide_dialog_hwnds();
            if hwnds .iter() .any (|hwnd| check_window_visible (*hwnd)) {
                // found at least one visible popup hwnd, so we'll hide everything
                hwnds .iter() .for_each (|hwnd| win_hide (*hwnd));
            } else {
                // no visible windows, so we'll try and toggle them back (if any)
                hwnds .iter() .for_each (|hwnd| win_show_no_activate (*hwnd));
            }
        } );
    }
    fn ide_float_tools_clear() {
        thread::spawn ( move || {
            win_get_ide_dialog_hwnds() .into_iter() .for_each ( |hwnd| { win_close (hwnd) } );
        } );
    }
    k.cm .add_combo ( cg().k(Numrow_0).m(lalt)        .c(intellij_fgnd(k)),  ag().af (Arc::new (ide_float_tools_toggle)) );
    k.cm .add_combo ( cg().k(Numrow_0).m(lalt).m(caps).c(intellij_fgnd(k)),  ag().af (Arc::new (ide_float_tools_clear )) );

}



fn setup_one_note_combos (k:&Krusty) {
    // note that these ofc rely on the setup of note-note quick-access toolbar ..
    // .. where .. select-mode is pos-3 in toolbar, finger-draw 4, eraser-stroke 5, eraser-point 6, pens 7
    // further, it has its own logic that keeps toggling or reverting back pens and erasers etc .. so the most robust seems to be ..
    // - for pen, pick sel-mode, then write-mode .. seems to always revert to pen (even from eraser before)
    // - for eraser, just pick eraser

    let sel_mode  = ag().k(Numrow_3).m(alt).gen_af();
    let draw_mode = ag().k(Numrow_4).m(alt).gen_af();
    let eraser    = ag().k(Numrow_5).m(alt).gen_af();

    fn threaded (af:AF) -> AF { Arc::new ( move || {
        let af = af.clone();
        thread::spawn ( move || af() );
    } ) }
    fn s (ms:usize) {
        thread::sleep (Duration::from_millis (ms as _))
    }
    fn one_note_fgnd (k:&Krusty) -> ComboCond {
        win_evs_cond ( &k.wel, |wel| wel.fgnd_info.read().unwrap().exe == "ONENOTE.EXE" )
    }
    let pen = {
        let (sel_mode, draw_mode) = (sel_mode.clone(), draw_mode.clone());
        Arc::new ( move || { sel_mode(); s(20); draw_mode(); } )
    };
    k.cm .add_combo ( cg().k(E).m(caps).s(qks).s(msE_dbl) .c(one_note_fgnd(k)),   ag().af (eraser) );
    k.cm .add_combo ( cg().k(D).m(caps).s(qks).s(msD_dbl) .c(one_note_fgnd(k)),   ag().af (threaded (pen.clone())) );

}



fn setup_gaming_combos (k:&Krusty) {

    // we'll put some actions on pointed windows on some latching-first-stroke combos
    fn s (ms:u64) { thread::sleep (Duration::from_millis(ms)); }
    fn gen_pointed_v2 (x:i32, y:i32, key:Option<Key>) -> AF {
        Arc::new ( move || {
            thread::spawn ( move || {
                MousePointer::move_abs(x,y);
                LeftButton.press_release();
                key .iter().for_each (|key| { s(20); key.press_release(); s(5); key.press_release(); } );
            } );
        } )
    }
    let (xo, xd, y) = (500, 1000, 2250);
    let pointed_3 = { Arc::new ( move || {
        let ko = Some(Escape);
        let (a,b,c,d) = (gen_pointed_v2(xo,y,ko), gen_pointed_v2(xo+xd,y,ko), gen_pointed_v2(xo+2*xd,y,ko), gen_pointed_v2(xo+3*xd,y,ko));
        thread::spawn ( move || { a(); s(50); b(); s(50); c(); s(50); d(); } );
    } ) };
    fn pc() -> ComboCond { Arc::new ( move |_,_| {
        let fi = WinEventsListener::instance(); let fi = fi.fgnd_info.read().unwrap();
        fi.exe == "chrome.exe" && fi.title.contains("Random")
    } ) }

    let fsc = k.cm .register_combo_latching_first_stroke ( cg().k(F2).m(caps_dbl) );

    k.cm .add_combo ( cg().k(Left  ).fsc(fsc).c(pc()),   ag().af (gen_pointed_v2 ( xo +     xd, y, Some(Escape))) );
    k.cm .add_combo ( cg().k(Right ).fsc(fsc).c(pc()),   ag().af (gen_pointed_v2 ( xo + 3 * xd, y, Some(Escape))) );
    k.cm .add_combo ( cg().k(Down  ).fsc(fsc).c(pc()),   ag().af (gen_pointed_v2 ( xo + 2 * xd, y, Some(Escape))) );
    //k.cm .add_combo ( cg().k(Slash ).fsc(fsc).c(pc()), ag().af (gen_pointed_v2 ( xo         , y, Some(Escape))) );
    k.cm .add_combo ( cg().k(Up    ).fsc(fsc).c(pc()),           ag().af ( pointed_3 ) );
    k.cm .add_combo ( cg().k(Left  ).fsc(fsc).c(pc()).m(caps),   ag().af (gen_pointed_v2 ( xo +     xd, y, None)) );
    k.cm .add_combo ( cg().k(Right ).fsc(fsc).c(pc()).m(caps),   ag().af (gen_pointed_v2 ( xo + 3 * xd, y, None)) );
    k.cm .add_combo ( cg().k(Down  ).fsc(fsc).c(pc()).m(caps),   ag().af (gen_pointed_v2 ( xo + 2 * xd, y, None)) );
    k.cm .add_combo ( cg().k(Slash ).fsc(fsc).c(pc()).m(caps),   ag().af (gen_pointed_v2 ( xo         , y, None)) );

    fn og_clear() -> AF { Arc::new ( || { thread::spawn ( || { for i in 0 .. 4 {
        MousePointer::move_abs (550 + 950*i, 790); s(30);
        LeftButton.press_release(); s(20);
    } } ); } ) }
    k.cm .add_combo ( cg().k(Up).fsc(fsc).c(pc()).m(caps),  ag().af (og_clear()) );

    fn og_setup () -> AF { Arc::new ( || { thread::spawn ( || {
        // assume four sized windows are up, move them to right loc, start em up, deblur,
        let _xd = 950;
        for _i in (0 .. 4).rev() {
            //MousePointer::move_abs (240, 200);         // home
            MousePointer::move_abs (600, 600); s(50);    // video
            LeftButton.press_release(); s(800);          //
            MousePointer::move_abs (80, 300); s(50);     // unblur
            LeftButton.press_release(); s(800); LeftButton.press_release(); s(50); s(20);
            //win_fgnd_move_to (_xd*_i, 0, 940, 2400); s(500);
            snap_closest_edge_side (&KrustyState::instance(), RectEdgeSide::Left ); s(10);
            snap_closest_edge_side (&KrustyState::instance(), RectEdgeSide::Right); s(10);
        }
    } ); } ) }
    fn og_teardown() -> AF { Arc::new ( || { thread::spawn ( || { for i in 0 .. 4 {
        MousePointer::move_abs (950*i + 240, 200); s(50);       // home
        LeftButton.press_release(); s(500);
        ctrl_press_release(W); s(50);
    } } ); } ) }
    k.cm .add_combo ( cg().k(Insert).fsc(fsc).m(caps),  ag().af (og_setup()) );
    k.cm .add_combo ( cg().k(Delete).fsc(fsc).m(caps),  ag().af (og_teardown()) );

}






/// setup for the entire krusty-board application, incl setting up key/btn bindings and combos
pub fn setup_krusty_board () {

    let k = Krusty::new();

    // setup all the mod-keys .. (can override this with own setup if desired)
    k.ks.mod_keys.setup_tracking(&k);

    // mouse setup incl lbtn/rbtn/mbtn/x1btn/x2btn and the scroll wheels
    k.ks.mouse.setup_mouse(&k);



    /*  PROCESS_STARTING UAC INHERITANCE REMINDER
        - remember that if running krusty as admin (as we often want to do), processes created here will be admin too !!
        - (now there's wrinkles like if there's already chrome open, new stuff still opens under that w its UAC as is)
        - (^^ although that means opening chrome window from here when none are present are not good idea (will be admin))
        -
        - that said, there do seem to be complex ways to avoid that, but for now seems a bit too onerous to impl
            - https://devblogs.microsoft.com/oldnewthing/20190425-00/?p=102443
            - https://stackoverflow.com/questions/1173630/how-do-you-de-elevate-privileges-for-a-child-process/2785337#2785337

        - for now, for most cases, running as 'explorer.exe <app-to-start.exe> works ok .. supposedly its an explorer bug .. meh
     */



    // we're gonna put basically all keys to have at least default combo handling (others just pass through)
    setup_default_keys(&k);

    // caps-dbl-Insert --> unstick all
    setup_unstick_all(&k);

    // [caps-dbl-F12, caps-dbl-Esc, alt-dbl-Esc, caps-dbl-e-o] --> clear-latching-first-stroke
    setup_latching_first_stroke_clear(&k);

    // [E,D,F,R,Q,1,2,3] as mode-keys
    setup_mode_keys(&k);


    setup_caps_as_shift_mappings(&k);

    disable_win_num_combos(&k);

    setup_caps_dbl_combos(&k);


    // [J,K,I,Comma,U,M,H,L] as l2 keys that are modified under caps, mode-keys etc
    setup_l2(&k);


    // mouse, wheel combos .. (though there are others in tsc or app-specific sections too)

    setup_mouse_left_btn(&k);

    setup_mouse_right_btn(&k);

    setup_middle_and_xbtn_combos(&k);

    setup_vert_wheel(&k);

    setup_horiz_wheel(&k);


    // some specific keys and combo-patterns

    setup_back_quote(&k);

    setup_space_key(&k);

    setup_escape_key(&k);


    setup_win_key_combos(&k);

    setup_caps_2wsx_combos(&k);

    setup_brightness_vol_media(&k);


    setup_qks_combos(&k);

    setup_misc_standalone_combos(&k);

    setup_win_groups(&k);


    // two-stroke combos (tsc) thematically grouped under separate sticky-first-strokes (sfsc)

    setup_window_action_tscs(&k);

    setup_switch_desktop_tscs(&k);

    setup_tab_nav_tscs(&k);

    setup_kbd_pointer_tscs(&k);

    setup_ctrl_tab_tscs(&k);


    // and some two-stroke combos (tsc) with latching first-stroke (lfsc)

    setup_arrow_wheel_tscs(&k);

    setup_ide_diff_nav_tscs(&k);


    // gaming and app specific combo setups (many with sticky/latching fscs)

    setup_switche_combos(&k);

    setup_IDE_combos(&k);

    setup_one_note_combos(&k);

    setup_gaming_combos(&k);




    /// some inlined tests for reminder
    // // - check that wildcard combos trigger even when other directly matching combos have been defined
    // k.cm .add_combo ( cg().k(F9).m(caps).s(qks3),  ag().k(F19).m(ctrl) );
    // k.cm .add_combo ( cg().k(F9).m(caps).wcsa(),   ag().k(F19).m(alt) );
    // // - check that multiple conditional or non-conditional combos can run, but if cond matches, then non-cond will be ignored
    // k.cm .add_combo ( cg().k(F9).m(caps).c(Arc::new(|_,_| true)),   ag().k(F19).m(alt).m(ctrl) );
    // k.cm .add_combo ( cg().k(F9).m(caps).c(Arc::new(|_,_| true)),   ag().k(F19).m(alt).m(shift) );
    // k.cm .add_combo ( cg().k(F10).m(caps).c(Arc::new(|_,_| false)),   ag().k(F19).m(shift).m(ctrl) );
    // k.cm .add_combo ( cg().k(F10).m(caps),   ag().k(F19).m(shift).m(ctrl) );
    // k.cm .add_combo ( cg().k(F10).m(caps),   ag().k(F20).m(shift).m(ctrl) );
    // k.cm .add_combo ( cg().k(F10).m(caps).wcsa(),   ag().k(F20).m(shift).m(alt) );




    /// **_ Reminders for availble new key combinations landscape _**
    // we could set up specific r-alt combos (for F<?> keys etc other than the default ralt-as-shift)

    // could also setup caps-ralt combos (for non l2/caret keys), which can be separate from caps-lalt combos!
    // (these will be two hand combos, so not particularly preferable)

    // also fyi re free combos: caps-win-<non-l3>, win-<num>, caps-alt-<num>, caps-ralt<non-l2>
    // .. even for caps-lalt-<?> defaulting to ctr-alt-<?> most are still free (other than l2, caret, e, f, w, space, f2)
    // .. and almost all F<num> combos with caps, caps-win, caps-lalt, ralt, caps-ralt, even w just lalt

    // and ofc, there's always lots of hotkeys available in qks2, qks3, qks4 etc (and ofc also qks, qks1, though those are used more generically)
    // (and further, all those can be combined .. eg. qks-msF-combos etc .. incl w mouse-btns, wheel etc)

    // and ofc, now there are a pile of availble options with mode-state _dbl combos, in combination w other [mk, mk_dbl, ms, ms_dbl] keys!

    // and there's a huge nested pile available under two-stroke-combos .. both the modkey-sticky, and latching types!




    /// **_ END OF USER COMBO SETUPS _**

    // finally we can start binding key maps .. first the specialized handling for mode-state trigger keys
    k.ks.mode_states.bind_mode_keys_actions(&k);

    //k.cm.debug_print_combos_map();
    k.cm.info_print_simult_active_combos_check();



    // and we'll put any direct special key setups after all this
    // .. which is for safety in case anything above accidentally included those, although ofc we dont want to rely on that!
    setup_direct_binding_keys (&k);


    // and we'll give a lil indicator for when we restart etc
    jiggle_cursor(3);

    // note: the handle_input_events to start the whole shebang should be being called from main, like via the start_krusty_board fn
    //start_krusty_board();

}




pub fn main () {

    // setup everything to for the krusty keyboard configuration
    setup_krusty_board();

    // we'll first start the windows-events listener
    WinEventsListener::instance().setup_win_event_hooks();

    // then start handling inputs
    InputProcessor::instance().begin_input_processing();

    // and finally start the system tray monitor event-loop .. (which will NOT return)
    start_system_tray_monitor();

}
