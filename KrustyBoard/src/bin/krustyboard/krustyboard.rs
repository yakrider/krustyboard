#![ allow (non_camel_case_types, non_snake_case, non_upper_case_globals, unused_doc_comments) ]


mod qbar_grid;
// ^^ The actual set-up of the action-grid for the quick-bar is separated into its own file

use std::{time::Duration, thread, sync::{Arc}, sync::atomic::Ordering};

use krustyboard::{*, utils::*, key_utils::*, KbdKey::*, MouseButton::*, ModKey::*, ModeState_T::*, WinGroups_E::*};





/// handling for any 'special' keys that need to be bound/handled directly (like for mouse btns) rather than via combo-maps
pub fn setup_direct_binding_keys (_k:KR) {

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




/// These are static pre-defined combo-hashes that we'll as shared truth of fsc combo-hashes. <br><br>
/// Note that fsc ComboHashes are simply unique ids, and dont really neeed to be actually hashed from Combos. <br>
/// As such, we dont HAVE to use these (as otherwise registering a FSC trigger will generate a ComboHash for it). <br>
/// However, if we'd like to refer to a FSC in different places, its better just added here (and co-registered with this combo-hash).<br>
/// (plus, that makes printed out combo-hashes in debug-out etc traceable to actual FSC definitions) <br>
/// This enum therefore contains MOST latching FSCs, and many sticky ones as well.
enum FSC {

    /// Fsc for a shared caps-sticky mode which we'll use (as the first-stroke-mode) to trigger various latching-modes <br>
    /// caps-q-w (rolling only due to 2wsx) <br>
    /// Note that since this is on caps, any combos (for some latching-mode trigger etc) specifying this fsc should also specify caps)
    LatchInit = 0x77700001,

    /// Fsc for rbn-x2-switching mode .. <br><br>
    /// Note that we're using this in a very un-orthodox manner, in that we dont intend to actually define combos w this.
    /// Instead, we'll use this in setting appropriate combo-conditionss. <br>
    /// Similarly, we dont expect this to have registered activation/clearing triggers or actions.
    /// Instead, we'll also activate/clear this fsc ourselves at appropriate points in the rbtn ctrl-tab/alt-tab choreography! <br><br>
    /// This allows for sharing mode-specific combos between fsc and non-fsc states, gated by combo-conds checking for this fsc.
    X2_Wheel,

    /// Quickbar fsc requires a quickbar-pre fsc
    QuickBarPre,
    QuickBar,

    // various switche invocations (plus there's non-fsc alt-tab etc)
    SwitcheCaps,
    SwitcheBlind,
    SwitcheDirect,
    //SwitcheRbtn,
    //SwitcheAltTab,

    // ctrl-tab setups
    TabsCtrl,
    TabsDirect,

    // other more regular fscs

    MediaVol,
    Brightness,

    WindowActions,
    DesktopSwitch,
    KbdPointer,

    WheelArrows,
    WheelDiff,

    GamingOG,

}

impl FSC {
    pub fn ch(self) -> ComboHash {
        // we'll make the combo-hash easier to recognized when printed in info/debug printouts
        ComboHash::wrapped (0x7777777700000000 + self as i32 as u64)
    }
}




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
fn win_evs_cond <WFN> (wfn:WFN) -> ComboCond
    where WFN : Fn(&WinEventsListener) -> bool + Send + Sync + 'static
{
    let wel = WinEventsListener::instance();   // get a 'static ref that can be moved to spawned thread
    Arc::new ( move |_,_| { wfn(wel) } )
}
#[allow (dead_code)] fn intellij_fgnd()  -> ComboCond { win_evs_cond ( check_intellij_fgnd ) }
#[allow (dead_code)] fn browser_fgnd()   -> ComboCond { win_evs_cond ( check_browser_fgnd ) }
#[allow (dead_code)] fn switche_fgnd()   -> ComboCond { win_evs_cond ( check_switche_fgnd ) }
#[allow (dead_code)] fn alt_tab_fgnd()   -> ComboCond { win_evs_cond ( check_alt_tab_fgnd ) }

#[allow (dead_code)] fn intellij_not_fgnd()  -> ComboCond { win_evs_cond ( |wel| !check_intellij_fgnd (wel) ) }
#[allow (dead_code)] fn browser_not_fgnd()   -> ComboCond { win_evs_cond ( |wel| !check_browser_fgnd (wel) ) }
#[allow (dead_code)] fn switche_not_fgnd()   -> ComboCond { win_evs_cond ( |wel| !check_switche_fgnd (wel) ) }
#[allow (dead_code)] fn alt_tab_not_fgnd()   -> ComboCond { win_evs_cond ( |wel| !check_alt_tab_fgnd (wel) ) }




// we'll also define some fns for brightness/media etc control to be reused by kbd/mouse combos etc
// note that in these, although we're using win-combos, we dont have to wrap in win-action guards as win is Modkey_Doubled

fn gen_af_incr_brightness (step:i32) -> AF {
    Arc::new ( move || { let _ = incr_brightness(step); } )
}

// skips work by alt-ctrl-volUp (needs to guard win-inactive since its on win-combo)
fn media_skips_action (n_skips:u32, ks:KSR, fwd_not_bkwd:bool) -> AF {
    let action_key = if fwd_not_bkwd {VolumeUp} else {VolumeDown};
    //ks.mod_keys.lwin.inactive_action ( ks.mod_keys.lalt.active_action ( ks.mod_keys.lctrl.active_action (
    ks.mod_keys.lalt.active_action ( ks.mod_keys.lctrl.active_action (
        Arc::new ( move || { (0 .. n_skips) .for_each (|_| { action_key.press_release() }) } )
    ) )
}

// media next/prev work via alt-shift-vol-up/dn as configured in musicbee etc
fn media_next_action (ks:KSR, next_not_prev:bool)  -> AF {
    let media_next_af = {
        if next_not_prev { ag().k(VolumeUp  ).m(lalt ).m(lshift).gen_af() }
        else             { ag().k(VolumeDown).m(lalt ).m(lshift).gen_af() }
    };
    let media_next_skips_af = media_skips_action (2, ks, true);
    Arc::new ( move || {
        media_next_af();
        let mnsaf = media_next_skips_af.clone();  // clone again to move into spawned thread (spawned since combos run in single queued side-thread)
        thread::spawn ( move || { thread::sleep(Duration::from_millis(2000));  mnsaf(); } );
    } )
}


// chrome bookmarklets activation macros e.g for darken/brighen
// NOTE : no longer using these .. worked but was too slow .. instead ..
// .. installed Shortkeys extension, defined hotkeys, just drive from here
pub fn bookmarklet_af (cmd: &'static str) -> AF {
    static pre : &str = "@bookmarks ";
    static lag : u64 = 30;
    // ^^ spacing between sending indiv chars
    let f6_af = ag().k(L).m(lctrl).gen_af();
    Arc::new ( move || {
        let f6_af = f6_af.clone();
        thread::spawn ( move || {
            // first we gotta get to the address bar
            f6_af();
            thread::sleep (Duration::from_millis (100));
            // then we send the text
            KeySequence (pre) .lag_send(lag);
            thread::sleep (Duration::from_millis (100));
            // send the actual darken/ligthen bookmark name
            KeySequence (cmd) .lag_send(lag);
            thread::sleep (Duration::from_millis (300));
            // then select the second option
            ExtDown.press_release();
            thread::sleep (Duration::from_millis (100));
            // and finally exec the selection
            Enter.press_release();
        } );
    } )
}
// darken/brighten the whole page
pub fn pg_darken_af (wheel_bkwd:bool) -> AF {
    static darken  : &str = "darker";
    static lighten : &str = "undark";
    let cmd = if wheel_bkwd { darken } else { lighten };
    bookmarklet_af (cmd)
}
// darken/ligthen only the images
pub fn im_darken_af (wheel_bkwd:bool) -> AF {
    static darken  : &str = "im-dark";
    static lighten : &str = "im-br";
    let cmd = if wheel_bkwd { darken } else { lighten };
    bookmarklet_af (cmd)
}






fn setup_default_keys  (k:KR) {

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
    let ext_keys   = [ExtLeft, ExtRight, ExtUp, ExtDown, ExtPgUp, ExtPgDn, ExtHome, ExtEnd, ExtInsert, ExtDelete];
    let spcl_keys  = [Backspace, Delete, Space, Tab, Enter, Escape, Insert, Apps];
    //let media_keys = [BrowserBack, BrowserForward, BrowserRefresh, VolumeMute, VolumeDown, VolumeUp,
    //                  MediaNextTrack, MediaPrevTrack, MediaStop, MediaPlayPause];
    //let mouse_keys = [MouseLeftBtn, MouseRightBtn, MouseMiddleBtn, MouseX1Btn, MouseX1Btn];

    char_keys .chain (fnum_keys) .chain (nav_keys) .chain (ext_keys) .chain (spcl_keys) .for_each ( |key| {
        k.cm .add_to_handled_keys_set (key);
    } );
    // ^^ we can ofc put combos for these later in code .. all these do is register for default binding if no combo gets mapped!

}



fn setup_unstick_all  (k:KR) {
    // we want to set up a combo to unstick-all in case we get into weird states due to other hooks stealing/suppressing key events etc
    // note that since we want the combo to be active even in presence of 'stuck' combo keys etc, we want to define that w global wildcards
    // caps-caps-Insert -> unstick-all
    let clear = Arc::new (move || k.ks.unstick_all());
    k.cm .add_combo ( cg().k(Insert).m(caps_dbl).wcma().wcsa(),  ag().af(clear) );      // caps-caps-Insert -> unstick-all

    // caps-caps-F12 -> suspend  .. (F12 typically has 'End' as oem Fn overload, next to Insert)
    k.cm .add_combo ( cg().k(F12).m(caps_dbl),  ag().af (Arc::new (move || k.ks.suspend_krusty())) );     // caps-caps-F12/End --> Suspend

    // sadly, there's no easy way to do 'resume' once we stop listening to kbd inputs .. (and would have to use tray-menu)
    // (if really wanted, could try and use hotkeys-manager from tauri and set that one at that level .. meh)

    /// debug printout of cur state
    //let print_ks = Arc::new (move || { thread::spawn (move || println!("{:#?}",ks) ); } );
    //k.cm .add_combo ( cg().k(F10).no_rpt().m(caps_dbl),  ag().af (print_ks.clone()) );     // caps-dbl-F10 -> debug-printout_ks
    //k.cm .add_combo ( cg().k(F10).no_rpt().m(lalt_dbl),  ag().af (print_ks.clone()) );     // lalt-dbl-F10 -> debug-printout_ks
    // ^^ cant do this anymore, as after moving to &'static, we've put cyclic references inside!

    // and of the combo-maps table itself
    let cm = k.cm;   // we can move the cm reference to spawned threads as its 'static
    let print_cm = Arc::new (move || cm.debug_print_combos_map());
    k.cm .add_combo ( cg().k(F9).no_rpt().m(caps_dbl),  ag().af (print_cm) );    // caps-dbl-F9 -> debug-printout_cm
}





/// This will associate kbd-keys triggers to internal mode-states. <br>
/// Note that some facilities will only work after this is performed. <br>
/// (e.g. auto-add of mode-states into mode-state associated key combos)
fn setup_mode_keys (k:KR) {

    fn register_mode_key (k:KR, key:Key, ms_t:ModeState_T) {

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




/// this sets-up/returns the fsc for a shared caps-sticky mode which we'll use (as the first-stroke-mode) to trigger various latching-modes <br>
/// caps-q-w (rolling only due to 2wsx) <br>
/// Note that since this is on caps, any combos (for some latching-mode trigger etc) specifying this fsc should also specify caps)
fn setup_latch_init_sfsc (k:KR) {
    // we'll setup a latch-init latch (sticky) just to make latch-triggers easier to remember
    // (.. can ofc still use whatever combo for any latch trigger .. we're just trying out this convention)

    // latch-init -> caps-q-w .. (sticky) .. must be pressed rolling (2wsx limitation) .. (cf caps-q for many sticky modes)
    let fsc = FSC::LatchInit.ch();
    k.cm .register_combo_sticky_first_stroke ( fsc,  cg() .k(W).no_rpt() .m(caps).s(qks) );

    // note that we've added and removed various co-registrations here in the past, incl caps-caps-Q, caps-caps-L etc ..
    // .. (and can add more if feel the need) .. but for now, we seem to exclusively end up using the caps-qw-<?> above

}




fn setup_latching_first_stroke_clear (k:KR) {
    /// clear active latching fscs
    k.cm .register_combo_clear_latching_first_stroke ( cg().k(O).no_rpt() .m(caps_dbl) );            // caps-caps-o
    k.cm .register_combo_clear_latching_first_stroke ( cg().k(Q).no_rpt() .m(caps_dbl) );            // caps-caps-q

    k.cm .register_combo_clear_latching_first_stroke ( cg().k(O).no_rpt() .m(caps).s(qks) );         // caps-q-o
    k.cm .register_combo_clear_latching_first_stroke ( cg().k(Q).no_rpt() .m(caps).s(qks_dbl) );     // caps-q-q

    k.cm .register_combo_clear_latching_first_stroke ( cg().k(O).no_rpt() .m(caps) .fsc(FSC::LatchInit.ch()) );     // caps-qw-o
    k.cm .register_combo_clear_latching_first_stroke ( cg().k(Q).no_rpt() .m(caps) .fsc(FSC::LatchInit.ch()) );     // caps-qw-q
}




fn setup_caps_as_shift_mappings  (k:KR) {
    // basically nums or kbd-right symbols not otherwise involved in l2
    // (these are 'caps-atypical', as typically caps-<key> will do ctrl-<key> via fallback)
    let cas = "567890-=[]\\;\'/.";   // note that we setup 1,2,3,4 as qks keys earlier
    cas .chars() .for_each ( |c| {
        if let Some(key) = Key::from_char(c) {
            k.cm .add_combo ( cg().k(key).m(caps),  ag().k(key).m(lshift) )
        }
    } );

    // now for at least some of these, we want to enable caps-q for ctrl (e.g. ctrl +/-)
    for key in [Minus, Equal, Slash] {
        k.cm .add_combo ( cg().k(key).m(caps).s(qks1),  ag().k(key).m(ctrl) )
    }
}




fn disable_win_num_combos (k:KR) {
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
    // (note that for mode-state keys, we want to disable the _dbl too, as those trigger and fallback separately)
    k.cm .add_combo ( cg().k(Numrow_4).m(lwin).s(qks4),       ag().af(no_action()) );
    k.cm .add_combo ( cg().k(Numrow_4).m(lwin).s(qks4_dbl),   ag().af(no_action()) );
}




fn setup_caps_dbl_combos (k:KR) {
    // we'll setup some keys on caps double tap first, esp those that modify global-ish behavior
    // (there are ofc other mode/app/tsc specific caps-dbl combos defined elsewhere too)

    // dbl-caps T to toggle capslock
    k.cm .add_combo ( cg().k(T).m(caps_dbl),   ag().k(CapsLock) );

    // we'll set dbl-caps-win-S/C/A/W as tmp shift/ctrl/alt/win lock (useful for doing mouse horiz scroll on say moon-reader etc)
    fn gen_af_ensure_mk (mk: &'static UnifModKey) -> AF { Arc::new ( move || {
        mk.ensure_active(); mk.mngd_active.clear();
    } ) }
    k.cm .add_combo ( cg().k(S).m(caps_dbl).m(lwin),   ag().af ( gen_af_ensure_mk (k.ks.mod_keys.lshift ) ) );
    k.cm .add_combo ( cg().k(C).m(caps_dbl).m(lwin),   ag().af ( gen_af_ensure_mk (k.ks.mod_keys.lctrl  ) ) );
    k.cm .add_combo ( cg().k(A).m(caps_dbl).m(lwin),   ag().af ( gen_af_ensure_mk (k.ks.mod_keys.lalt   ) ) );
    //k.cm .add_combo ( cg().k(W).m(caps_dbl).m(lalt),   ag().af ( gen_af_ensure_mk (k.ks.mod_keys.lwin ) ) );
    // ^^ win is now dbled modkey (and so not full-managed), and so ensure-active doesnt make sense for it


    /// **_ modkey-wrapping OUTPUT SWAPS_**
    // we'll setup a helper to remap some key such that it works with any modkey [alt/ctrl/shift/win]
    fn gen_full_mk_key_swap_af (k:KR, key:Key) -> AF {
        // we could in theory, just register these as all the 16 combinations of the [ctrl,alt,shift,win] combos ..
        // .. but if wanted to use the l/r/generic triplets, those 16 would expand out to 128 combos for each fn call !!
        // .. so we'd rather just setup wildcarded combos and build layered afs for mod-keys (like how fallback works)
        Arc::new ( move || {
            let mut af = base_action(key);
            if k.ks.mod_keys.some_ctrl_down()  { af = k.ks.mod_keys.lctrl .active_action(af) }
            if k.ks.mod_keys.some_alt_down()   { af = k.ks.mod_keys.lalt  .active_action(af) }
            if k.ks.mod_keys.some_shift_down() { af = k.ks.mod_keys.lshift.active_action(af) }
            if k.ks.mod_keys.some_win_down()   { af = k.ks.mod_keys.lwin  .active_action(af) }
            af();
        } )
    }
    fn setup_caps_dbl_ms_key_swap (k:KR, ms:ModeState_T, k1:Key, k2:Key) {
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
    fn setup_ext_key_swap (k:KR, key:Key, ext_key:Key) {
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




fn setup_l2 (k:KR) {
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

    fn setup_l2_key (k:KR, key:Key, l2k:Key, dk:Key, wafg:AFG, fafg:AFG, del_via_sel:bool) {

        // register nav actions for normal-nav, word-nav, and fast-nav modes
        k.cm .add_combo ( cg().k(key).m(caps),         ag().k(l2k) );
        k.cm .add_combo ( cg().k(key).m(caps).s(msF),  ag().af (wafg(l2k)) );
        k.cm .add_combo ( cg().k(key).m(caps).s(msR),  ag().af (fafg(l2k)) );

        // selection actions are via wrapping those with shift press-release
        k.cm .add_combo ( cg().k(key).m(caps).s(msE),         ag().k(l2k).m(shift) );
        k.cm .add_combo ( cg().k(key).m(caps).s(msE).s(msF),  ag().af (wafg(l2k)) .m(shift) );

        //k.cm .add_combo ( cg().k(key).m(caps).s(msE).s(msR),  ag().af (fafg(l2k)) .m(shift) );
        // ^^ we'd rather keep this for other stuff than this 2x selection which basically never gets used

        // delete actions are dependent on whether the delete can be done directly or has to be done via selection then delete
        fn del_sel_afg (del_key:Key, nav_af:AF) -> AF {
            Arc::new ( move || {
                LShift.press(); nav_af(); LShift.release(); // dont need guards for shift here.. this is deep into multi key L2
                //press_release(del_key);
                thread::spawn ( move || { thread::sleep (Duration::from_millis(20)); del_key.press_release(); } );
        } ) }
        let (da, dwa, _dfa) = if del_via_sel {
            // if deleting via selection, we wrap the del-sel action around the normal nav actions
            ( del_sel_afg(dk,base_action(l2k)),  del_sel_afg(dk,wafg(l2k)),  del_sel_afg(dk,fafg(l2k)) )
        } else {
            // and for direct deletes, we perform the nav-eqv action but with the specified delete-key
            ( base_action(dk),  ctrl_action(dk),  fast_action(dk) )
        };
        k.cm .add_combo ( cg().k(key).m(caps).s(msD),         ag().af(da ) );
        k.cm .add_combo ( cg().k(key).m(caps).s(msD).s(msF),  ag().af(dwa) );

        //k.cm .add_combo ( cg().k(key).m(caps).s(msD).s(msR),  ag().af(dfa) );
        // ^^ again, the 2x delete is pointless, we'd rather free it up for other uses

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
        //k.cm .add_combo ( cg().k(key).m(caps_dbl),          ag().k(key).m(ctrl) );
        //k.cm .add_combo ( cg().k(key).m(caps_dbl).m(lalt),  ag().k(key).m(ctrl).m(alt) );
        // ^^ nah we'd rather keeep the caps-dbl for sticky/latching mode triggers etc
        // .. besides, can simply ofc do actual ctrl and alt

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




fn setup_mouse_left_btn (k:KR) {

    /// Regular lbtn clicks would mostly work via fallback .. but we still want to capture win-snap-dats
    // (the early capture is mostly for robustness on moves where the pointer moves out of clicked hwnd before win-key pressed etc)
    fn gen_af_base_lbtn (ks:KSR) -> AF { Arc::new ( move || {
        ks.mouse.lbtn.active.set(); LeftButton.press();
        let xy = ks.mouse.lbtn.down_xy.load();
        let hwnd = win_get_hwnd_from_point(xy);
        ks.capture_win_snap_dat (xy, hwnd, None);
    } ) }
    k.cm .add_combo ( cg().mbtn(LeftButton),  ag().af (gen_af_base_lbtn(k.ks)) );

    /// for caps-lbtn we'll enable **_ caps-as-ctrl _** (for drags etc) via mngd_ctrl_state .. (but not other caps-mod-combos as ctrl-mod-combos)
    fn gen_af_caps_mngd_lbtn (ks:KSR) -> AF { Arc::new ( move || {
        ks.mod_keys.lctrl.ensure_active();
        // ^^ this will leave ctrl active (managed), ctrl will clear when caps comes up
        ks.mouse.lbtn.active.set(); LeftButton.press();
        // ^^ we only want to press after ctrl has been made active
    } ) }
    k.cm .add_combo ( cg().mbtn(LeftButton).m(caps),  ag().af (gen_af_caps_mngd_lbtn(k.ks)) );

    /// and to also provide ctrl-drag on caps press after drag-start, we'll put a combo directly on caps-down too!
    let cc : ComboCond = Arc::new (|ks,_ev| ks.mouse.lbtn.down.is_set() && ks.mod_keys.lwin.down.is_clear());
    // ^^ the lwin conditional is to exclude the window-move/resize w lwin-lbtn-drag
    let af = Arc::new (move || k.ks.mod_keys.lctrl.ensure_active() );
    k.cm .add_combo ( cg().k(CapsLock) .c(cc),  ag().af(af) );


    /// for **_ mbtn release _**, we'll specify full wildcards (modkeys, modes) to avoid missing btn releases regardless of mode-states
    fn gen_af_lbtn_release (ks:KSR) -> AF { Arc::new ( move || {
        if ks.mouse.lbtn.active.is_set() { ks.mouse.lbtn.active.clear(); LeftButton.release() }
    } ) }
    k.cm .add_combo ( cg().mbtn(LeftButton).rel().wcma().wcsa(), ag().af ( gen_af_lbtn_release (k.ks) ) );


    /// for win-lbtn and win-caps-lbtn, we want to **_ capture win-snap-dat _** for window drag/resizing
    fn gen_af_win_snap_dat (ks:KSR, wgo:Option<WinGroups_E>) -> AF { Arc::new ( move || {
        let xy = ks.mouse.lbtn.down_xy.load();
        let hwnd = win_get_hwnd_from_point (xy);
        ks.capture_win_snap_dat (xy, hwnd, wgo);
        win_set_fgnd (ks.win_snap_dat.read().unwrap().hwnd);
    } ) }
    // win-drag does drag with snap .. caps-win does resize .. and adding shift disables snap for both
    // .. so we'll use wildcards to set any of these to trigger taking a win-snap-dat for subsequent use
    k.cm .add_combo ( cg().mbtn(LeftButton).m(lwin) .wcm(caps).wcm(shift),
                      ag().af (gen_af_win_snap_dat (k.ks, None) ) );


    /// for **_ win-groups _** ..
    // .. caps-lwin-qks<?> + lbtn-dbl-click on window is add that window to the corresponding group
    fn setup_win_grp_action (k:KR, s:ModeState_T, wg:WinGroups_E) {
        let add_af = Arc::new ( move || {
            let hwnd = win_get_hwnd_from_pointer();
            k.ks.win_groups.add_to_group (wg, hwnd);
            jiggle_window(hwnd);
        } );
        let cc : ComboCond = Arc::new (move |ks,_| ks.mouse.lbtn.dbl_tap.is_set());
        k.cm .add_combo ( cg().mbtn(LeftButton).m(lwin).m(caps).s(s) .c(cc),  ag().af(add_af) );
        // also, on single-click we should capture dat to allow group window drag
        let wsd_af = gen_af_win_snap_dat (k.ks, Some(wg));
        k.cm .add_combo ( cg().mbtn(LeftButton).m(lwin).m(caps).s(s),           ag().af(wsd_af.clone()) );
        k.cm .add_combo ( cg().mbtn(LeftButton).m(lwin).m(caps).s(s).m(shift),  ag().af(wsd_af) );
    }
    setup_win_grp_action (k, qks1, wg1);
    setup_win_grp_action (k, qks2, wg2);
    setup_win_grp_action (k, qks3, wg3);
    setup_win_grp_action (k, qks4, wg4);



    // for win-lbtn-dbl we want to maximize the pointed window
    fn gen_af_win_tog_max (ks:KSR) -> AF { Arc::new ( move || {
        win_toggle_maximize (ks.win_snap_dat.read().unwrap().hwnd)
    } ) }
    let cc : ComboCond = Arc::new (move |ks,_| ks.mouse.lbtn.dbl_tap.is_set());
    k.cm .add_combo ( cg().mbtn(LeftButton).m(lwin) .c(cc),  ag().af (gen_af_win_tog_max(k.ks)) );


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




fn setup_mouse_right_btn (k:KR) {

    // we're doing a rel-delayed-rbtn scheme .. we'll postpone rbtn-press till release (to avoid ctx menu on rbtn-scroll swi invocation)
    // (otherwise if letting sw handle rbtn-scroll natively, we would've been fine with just the default fallback for press)
    let sw_snap_af = ag().k(F15).m(alt).m(shift).gen_af();
    let rbtn_press_af = Arc::new ( move || {
        // if we've already got switche up, or are trying to do tab-switching, we'd rather clear pending than set it!
        if check_switche_fgnd(k.wel) || k.ks.mod_keys.caps.down.is_set() {
            k.ks.mouse.rbtn.pending.clear();
            return
        }
        // otherwise, we wont send out rbtn-click, but mark it pending for later
        k.ks.mouse.rbtn.pending.set();
        // and to make rbtn-scroll switching faster, we'll also trigger a switche snapshot for preloading/warm-up
        sw_snap_af();
    } );
    k.cm .add_combo ( cg().mbtn(RightButton),           ag().af (rbtn_press_af.clone()) );
    k.cm .add_combo ( cg().mbtn(RightButton) .m(caps),  ag().af (rbtn_press_af) );


    // rbtn-rel handling .. w support for swi .. (whether letting sw do native, or us driving it from here)
    fn gen_af_rbtn_release (ks:KSR) -> AF { Arc::new ( move || {
        if ks.in_right_btn_scroll_state.is_set() {
            ks.in_right_btn_scroll_state.clear();
            ks.mod_keys.lalt.ensure_inactive();
            // ^^ for the new kr-driven rbtn-scroll, we want to make sure btn-rel sends alt-rel to activate the switching

            ks.mod_keys.lctrl.ensure_inactive();
            // ^^ we'll also do the same for caps-rbtn-scroll for ctrl-tab

            if ks.mouse.rbtn.active.is_set() { RightButton.release() }
            // ^^ this rel is only to suport sw-native rbtn-scroll .. for kr-driven, this will never happen anyway, so we'll let it be
            // ^^ and it's here coz upon sw-native rbtn scroll, switche sends early rbtn-rel, so when we get this real one, we might be rbtn-inactive
            // .. so catching this separately here lets us pass this through for switche (for cases its hook is behind us)
            // (and lower down, for normal cases w rbtn inactive (coz maybe we suppressed it), we'll suppress the release)
        }
        else if ks.mouse.rbtn.pending.is_set() {
            RightButton.press_release();
        }
        else if ks.mouse.rbtn.active.is_set() {
            if ks.mouse.rbtn.consumed.is_set() { mouse_rbtn_release_masked() }
            else { RightButton.release() }
            // ^^ if it is active, we'll mask if its marked consumed ..
            // (this is no longer used since switche started doing mouse-btn handling, but could be useful for other consumption cases)
        }
        else { } // if its not even active, we dont need to send a release (presumably we blocked the press going out)

        // either way, we can now clear rbtn flags
        ks.mouse.rbtn.consumed.clear(); ks.mouse.rbtn.active.clear(); ks.mouse.rbtn.pending.clear();

        // finally, we'll also clear the x2-rbtn-wheel ctrl-tab fsc if active
        if ks.sticky_first_stroke.check_match (FSC::X2_Wheel.ch()) {
            ks.clear_cur_sticky_fsc()
        }
    } ) }
    // we'll setup global wildcard combo (so rbtn never gets stuck)
    k.cm .add_combo ( cg().mbtn(RightButton).rel().wcma().wcsa(),  ag().af (gen_af_rbtn_release (k.ks)) );



    // win-caps-qks? + rbtn-click is used to remove the pointed window from the corresponding win-group
    fn gen_af_win_grp_remove (wg:WinGroups_E, ks:KSR) -> AF { Arc::new ( move || {
        let hwnd = win_get_hwnd_from_pointer();
        ks.win_groups.remove_from_group (wg, hwnd);
        jiggle_window(hwnd);
    } ) }
    k.cm .add_combo ( cg().mbtn(RightButton).m(lwin).m(caps).s(qks1),  ag().af (gen_af_win_grp_remove (wg1, k.ks) ) );
    k.cm .add_combo ( cg().mbtn(RightButton).m(lwin).m(caps).s(qks2),  ag().af (gen_af_win_grp_remove (wg2, k.ks) ) );
    k.cm .add_combo ( cg().mbtn(RightButton).m(lwin).m(caps).s(qks3),  ag().af (gen_af_win_grp_remove (wg3, k.ks) ) );
    k.cm .add_combo ( cg().mbtn(RightButton).m(lwin).m(caps).s(qks4),  ag().af (gen_af_win_grp_remove (wg4, k.ks) ) );


    // caps-q2-rbtn to search highlighted in chrome (via macro like sets of steps w available chrome hotkeys)
    fn chrome_search_highlighted () {
        LeftButton.press_release(); LeftButton.press_release(); LeftButton.press_release();
        thread::sleep (Duration::from_millis(100));
        shift_press_release(F10);
        thread::sleep (Duration::from_millis(100));
        ExtDown.press_release(); ExtDown.press_release(); ExtDown.press_release();
        Enter.press_release();
    }
    k.cm .add_combo ( cg().mbtn(RightButton).m(caps).s(qks2),  ag().af (spawned_action (chrome_search_highlighted)) );

}




fn setup_middle_and_xbtn_combos (k:KR) {
    // we want to set side btns to serve as middle btns too ..
    // Note that for the setup below, we assume the mouse firmware has been set to send both press and rel events for x2 buttons
    // (esp as the default for e.g. Logitech MX mice is to already delay the press to rel, and use down-wheel for horiz-wheel)

    fn setup_middle_btn_eqv_combos (k:KR, mbs : &'static MouseBtnState) {
        // base action rerouting for eqv btns (middle/x1/x2) to middle-btn

        // first the press action
        fn gen_xbtn_base_press_af (k:KR) -> AF {
            Arc::new ( move || {
                // if we're actively doing alt-tab, we completely ignore this
                if check_switche_fgnd(k.wel) {
                    return
                }
                // else we want to use rbtn held x2 for tab-switching ..
                // .. but for easier ergo (since x2 btn is stiff), we want to enter a sticky fsc mode (so we wont have to keep it held down)
                // .. (we intend for this sticky to be cleared upon rbtn release, and not x2 release!)
                if k.ks.mouse.rbtn.down.is_set() {
                    k.ks.activate_sticky_fsc (FSC::X2_Wheel.ch());
                    return
                }
                // otherwise, we still wont send out a press now, but we'll mark it pending for later
                k.ks.mouse.mbtn.pending.set();
            } )
        }
        k.cm .add_combo ( cg().mbtn(mbs.btn),  ag().af (gen_xbtn_base_press_af (k)) );

        // and for release, we'll specify full wildcards (modkeys, modes), to avoid missing btn releases regardless of mode-states
        fn gen_xbtn_base_rel_af (k:KR, mbs: &'static MouseBtnState) -> AF {
            // lets define a common fn first
            fn handle_mbtn_eqv_rel (k:KR, mbs: &MouseBtnState) {
                if check_switche_fgnd(k.wel) {
                    // if we're actively doing alt-tab, we shouldnt butt-in
                } else if k.ks.in_right_btn_scroll_state.is_set() {
                    k.ks.in_right_btn_scroll_state.clear();
                    k.ks.mouse.vwheel.spin_invalidated.set();
                    k.ks.mod_keys.lctrl.ensure_inactive();
                } else if mbs.pending.is_set() {
                    mbs.btn.press_release();
                } else if mbs.active.is_set() {
                    mbs.btn.release();
                }
                mbs.pending.clear(); mbs.active.clear(); mbs.consumed.clear();
            }
            Arc::new ( move || {
                // we'll handle the typical middle-btn mapped states first
                handle_mbtn_eqv_rel (k, k.ks.mouse.mbtn);
                // but if we had the actual specific btn pending/active, we'll handle those as well
                if mbs.btn != MiddleButton {
                    handle_mbtn_eqv_rel (k, mbs);
                }
            } )
        }
        k.cm .add_combo ( cg().mbtn(mbs.btn).rel().wcma().wcsa(),  ag().af (gen_xbtn_base_rel_af (k, mbs)) );

    }
    setup_middle_btn_eqv_combos (k, k.ks.mouse.mbtn);
    setup_middle_btn_eqv_combos (k, k.ks.mouse.x1btn);
    setup_middle_btn_eqv_combos (k, k.ks.mouse.x2btn);

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
    k.cm .add_combo ( cg().mbtn(X2Button).m(caps),         ag().k(W).m(ctrl) );
    k.cm .add_combo ( cg().mbtn(X2Button).m(caps).s(msE),  ag().k(W).m(ctrl) );


}



// helper fn to set both frwd/bkwd CG mappings with a parameterized CG gen fn
fn setup_frwd_bkwd_whl <CGF,ICG,P,AGPF,IAG> (k:KR, cgFn:CGF, bkwd_p:P, frwd_p:P, agFn:AGPF)
    where ICG : Into<CG>,  IAG : Into<AG>,
          CGF  : Fn(ComboGen<ComboGenSt_Wheel>) -> ICG,
          AGPF : Fn(ActionGen, P) -> IAG,
{
    k.cm .add_combo ( cgFn (cg().whl().bkwd()),  agFn (ag(), bkwd_p) );
    k.cm .add_combo ( cgFn (cg().whl().frwd()),  agFn (ag(), frwd_p) );
}

fn setup_vert_wheel (k:KR) {

    // re touchpad events .. note that there is no touchpad specific hook interception ..
    // .. but if the OS translates touchpad events to mouse events, we'll receive them here as mouse-move or mouse-wheel events
    // further, some apps like chrome seem to directly listen to touch events, and only pass up pointer-move as mouse events ..
    // .. so not even wheel events will be heard from two-finger scrolls within chrome windows (let alone pinch zoom, gestures etc)
    // else in general, pinch zoom seems to generate (at ll-hook), a Lctrl-down upon zoom-start, then wheel-evs, then lctrl-up upon finger-up

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
        - caps/ctrl-wh  -->  ctrl wheel  .. (plus switche and ctrl-tab overloads set-up elsewhere)
        - caps-x2-wh    -->  horiz-wheel

        - alt-wh        -->  brightness  .. (plus switche and alt-tab overloads set-up elsewhere)
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

    fn gen_af_base_wheel (dir_is_down:bool, ks:KSR) -> AF {
        // we want to mark when we enter switche right-btn-scroll .. but otherwise, we just send regular wheel scrolls
        let af_wheel_scroll   = if dir_is_down {
            ag().whl().bkwd().mkg_nw().gen_af()
        } else {
            ag().whl().frwd().mkg_nw().gen_af()
        };
        Arc::new ( move || {
            if ks.mouse.rbtn.down.is_set() { ks.in_right_btn_scroll_state.set() }
            af_wheel_scroll()
            // ^^ we'll send out the scroll even during rbtn-scrll, despite switche hooks directly listening to it ..
            // .. because in cases when our hooks are ahead of switche hooks, it wouldn't otherwise get there
        } )
    }
    setup_frwd_bkwd_whl ( k,  |wg| wg,   true, false,   |ag,p| ag.af (gen_af_base_wheel (p, k.ks) ) );
    // ^^ this is useful for supporting switche-native rbtn-scroll ..
    // .. however, for krusty-driven rbtn-scrl (like now), the default fallback would have been fine too ..
    // .. but, we'll keep it as its useful if we ever want to revert (e.g for sw testing etc)
    // .. (and its harmless coz for krusty-driven setup, w rbtn-dn the separate conditional combo will trigger w precedence)


    fn gen_af_caps_wheel (dir_is_down:bool, k:KR) -> AF {
        // for general caps-wheel, we send out managed-ctrl-wheels (managed to avoid having ctrl dn/up be interspersed)
        let af_ctrl_wheel = {
            if dir_is_down { ag().whl().bkwd().m(ctrl).gen_af() }
            else           { ag().whl().frwd().m(ctrl).gen_af() }
        };
        Arc::new ( move || {
            k.ks.mod_keys.lctrl.ensure_active();
            af_ctrl_wheel()
        } )
    }
    // we'll also do this for actual ctrl-wheel so the behavior is consistent ('ctrl' expands out to both lctrl and rctrl)
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps),          true, false,   |ag,p| ag.af ( gen_af_caps_wheel (p,k) ) );
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(ctrl),          true, false,   |ag,p| ag.af ( gen_af_caps_wheel (p,k) ) );
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).m(ctrl),  true, false,   |ag,p| ag.af ( gen_af_caps_wheel (p,k) ) );


    // caps-dbl wheel can simply translate to horiz-wheel
    k.cm .add_combo ( cg().whl().bkwd().m(caps_dbl),   ag().hwhl().frwd() );
    k.cm .add_combo ( cg().whl().frwd().m(caps_dbl),   ag().hwhl().bkwd() );


    // alt-wheel will do brightness control .. (and we'll separately support alt-tab and swtiche nav)
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(lalt),   -4, 4,   |ag,p| ag.af (gen_af_incr_brightness (p)) );

    // qks1-alt-wheel (i.e. alt+1+wheel) .. we'll do finer brightness adjustments
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(lalt).s(qks1),   -1, 1,   |ag,p| ag.af (gen_af_incr_brightness (p)) );


    // caps-alt-wheel .. we'll do up/down nav .. (and switche etc is set-up separately)
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).m(lalt),   ExtDown, ExtUp,   |ag,p| ag.k(p).m(lalt) );

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

    /// win-1 wheel .. **_ media next/prev _** ..
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(lwin).s(qks1),   true, false,   |ag,p| ag.af (media_next_action (k.ks, p)) );

    /// and win-1-1 wheel .. **_ media skip-fwd-bkwd _** ..
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(lwin).s(qks1_dbl),   true, false,   |ag,p| ag.af (media_skips_action (1, k.ks, p)) );

    /// caps-d-d-wheel, we'll **_ navigate across WINDOWS _** (via switche snapshots)
    // ^^ moved to switche section

    /// caps-f (i.e word mode) wheel, we'll set as **_ nav through SEARCH (F3, Shift-F3) _**
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(msF),    no_mk, shift,   |ag,p| ag.k(F3).m(p) );

    /// we'll let caps-R-wheel do **_ FASTER SCROLL _**
    fn gen_af_fast_scroll (ks:KSR) -> AF { Arc::new ( move || {
        ks.mouse.vwheel.wheel.scroll (3 * ks.mouse.vwheel.last_delta.load (Ordering::Relaxed));
    } ) }
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(msR),    (), (),   |ag,_| ag.af (gen_af_fast_scroll (k.ks)) );


    /// caps-qks3-wheel .. we'll use for **_ IDE LAST LOCATION NAV _** .. (via Alt Left/Right)
    //  (in theory, we have easy combos for alt-l/r, but this make it tie in better w the edit locs below)
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks3),    ExtRight, ExtLeft,   |ag,key| ag.k(key).m(alt) );

    /// and with mode-state-E, we'll do **_ IDE LAST EDIT LOCATION NAV _** .. (via Alt-Shift-Left/Right)
    setup_frwd_bkwd_whl ( k,  |wg| wg.m(caps).s(qks3).s(msE),    ExtRight, ExtLeft,   |ag,key| ag.k(key).m(alt).m(shift) );

}




fn setup_horiz_wheel (_k:KR) {
    // (note that simple horiz-scroll will work as is w passthrough)
    // general h-wheel note : we do get h-wheel from mouse w x2-btn-wheel, but its still not ergo ..
    // .. so ideally we really wouldnt rely on this, and just have some setup overloaded in kbd w v-wheel setup
}




fn setup_win_key_combos (k:KR) {

    // the OS listens to win-L press at lowest levels for lockscreen (just like ctrl-alt-del) .. (though we do hear it) ..
    // .. and after that, we wont hear anything (incl the win release), and so our win state can get out of sync ..
    // .. hence we'll add a listener for win-L and clear out our win state
    fn gen_lwin_l_rel_af (ks:KSR) -> AF { Arc::new ( move || {
        ks.mod_keys.lwin.down.clear(); ks.mod_keys.lwin.dbl_tap.clear(); ks.mod_keys.lwin.consumed.clear();
        ks.mod_keys.lwin.active.clear(); ks.mod_keys.lwin.mngd_active.clear();
        // we'll also clear caps state in case we had done caps-win-l .. (the other modkey combos dont trigger win-lock)
        ks.mod_keys.caps.down.clear(); ks.mod_keys.caps.dbl_tap.clear();
    } ) }
    k.cm .add_combo ( cg().k(L).m(lwin),          ag().af (gen_lwin_l_rel_af (k.ks)) );
    k.cm .add_combo ( cg().k(L).m(lwin).m(caps),  ag().af (gen_lwin_l_rel_af (k.ks)) );

    // win-m by default minimized all windows .. we just want to disable it .. (note that win-d still does show-desktop)
    k.cm .add_combo ( cg().k(M).m(lwin),  ag().af(no_action()) );

    // win-f can toggle window full-screen .. (the OS default feedback-hub will stay on double-win-f)
    // (note that to avoid accidental trigger, since this is _dbl tracked mode-key, we'll add _dbl too)
    k.cm .add_combo ( cg().k(F).no_rpt().m(lwin).s(msF),      ag().k(F11) );
    k.cm .add_combo ( cg().k(F).no_rpt().m(lwin).s(msF_dbl),  ag().k(F11) );

    // win-e should bring up whatever we configured for file-explorer alternative
    k.cm .add_combo ( cg().k(E).no_rpt().m(lwin).s(msE),      ag().af(action(start_alt_file_explorer)) );

    // since msE is often used in first-stroke-combos etc, we'll ensure held win-ee etc dont spam piles of explorer windows
    k.cm .add_combo ( cg().k(E).no_rpt().m(lwin    ).s(msE_dbl),  ag().af(no_action()) );
    k.cm .add_combo ( cg().k(E).no_rpt().m(lwin_dbl).s(msE    ),  ag().af(no_action()).mkg_w() );
    k.cm .add_combo ( cg().k(E).no_rpt().m(lwin_dbl).s(msE_dbl),  ag().af(no_action()).mkg_w() );

    // we'll disable win-d too, as we never use that show/hide desktop and it's disruptive
    k.cm .add_combo ( cg().k(D).m(lwin).s(msD),      ag().af(no_action()) );
    k.cm .add_combo ( cg().k(D).m(lwin).s(msD_dbl),  ag().af(no_action()) );

    // win-i should start irfanview
    k.cm .add_combo ( cg().k(I).no_rpt().m(lwin),  ag().af(action(start_irfanview)) );

    // win-n should start chrome-incognito
    k.cm .add_combo ( cg().k(N).no_rpt().m(lwin),  ag().af(action(start_chrome_incognito)) );

    // win-caps-b for bard .. hah we'll see
    k.cm .add_combo ( cg().k(B).no_rpt().m(lwin).m(caps),  ag().af (action_p1 (start_chrome_app, "nohacooabmgpjcdeajcfjgkpfibiffjf")) );
    // and win-caps-c for claude
    k.cm .add_combo ( cg().k(C).no_rpt().m(lwin).m(caps),  ag().af (action_p1 (start_chrome_app, "fmpnliohjhemenmnlpbfagaolkdacoja")) );

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
    //fn taskbar_focus_yak_tools_bar (k:KR) -
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

    // we'll let win-q do everything-search
    // (alt-q is set in 'everything' as global invocation hotkey, and alt-ctrl-q as new search window hotkey)
    k.cm .add_combo ( cg().k(Q).no_rpt().m(lwin),               ag().k(Q).m(win).m(alt) );
    k.cm .add_combo ( cg().k(Q).no_rpt().m(lwin).m(shift),      ag().k(Q).m(win).m(alt).m(shift) );
    // and since this is mode-key again, we'll disable the _dbl
    k.cm .add_combo ( cg().k(Q).no_rpt().m(lwin).s(qks_dbl),    ag().af(no_action()) );

    // this is counterpart to starting chrome incognito .. w/ caps will set that to open non-incognito
    k.cm .add_combo ( cg().k(N).no_rpt().m(caps).m(lwin),  ag().af (action (start_chrome)) );

    // caps-win-c being used to launch winmerge diff from last two clipboard entries
    //k.cm .add_combo ( cg().k(C).no_rpt().m(caps).m(lwin),  ag().af (action (start_winmerge_clipboard)) );
    // ^^ instead set to start claude further up

    // gaah we'll just throw in iDEA diff for drag-drop diffing (just coz winmerge doesnt do dark mode)
    //k.cm .add_combo  ( k.ks, cg().k(C).m(lwin),  k.ks.cg_af (Arc::new (|| start_idea_diff() )));
    // ^^ cant do from here, turns out idea diff from cmd line can ONLY be opened with two files pointed, unlike empty from Idea shortcut!


}



fn setup_brightness_vol_media (k:KR) {

    // (note that there are also a bunch of wheel combo setups for most of these in the wheel section)

    // in cur laptop, Fn-F6/F7 do brightness, but at +10 incrs .. set them to do small incrs with alt combos
    fn gen_incr_brightness (incr:i32) -> AF { Arc::new ( move || { let _ = incr_brightness(incr); } ) }
    k.cm .add_combo ( cg().k(F6).m(lalt),  ag().af (gen_incr_brightness(-1)) );
    k.cm .add_combo ( cg().k(F7).m(lalt),  ag().af (gen_incr_brightness( 1)) );

    // we'll also add these under a brightness fsc .. caps-q-b .. sticky
    let fsc = FSC::Brightness.ch();
    k.cm .register_combo_sticky_first_stroke ( fsc,  cg().k(B).no_rpt().m(caps).s(qks) );
    k.cm .add_combo ( cg().k(I    ).m(caps).fsc(fsc),  ag().af (gen_incr_brightness( 1)) );
    k.cm .add_combo ( cg().k(Comma).m(caps).fsc(fsc),  ag().af (gen_incr_brightness(-1)) );

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
    fn setup_fine_mode_ms_key (k:KR, ms:&'static ModeState, mk:ModKey, fine_ms_t:ModeState_T, af:AF) {
        if let Some(key) = ms.key() {
            k.cm .add_combo ( cg().k(key).m(mk).s(ms.ms_t    ).s(fine_ms_t),  ag().af (af.clone()) );
            k.cm .add_combo ( cg().k(key).m(mk).s(ms.ms_dbl_t).s(fine_ms_t),  ag().af (af.clone()) );
        }
    }
    // alt-2 is brightness down, alt-3 is brightness up .. (fine mode when 1 is held)
    setup_fine_mode_ms_key (k, k.ks.mode_states.qks2, lalt, no_ms, gen_incr_brightness(-4));
    setup_fine_mode_ms_key (k, k.ks.mode_states.qks3, lalt, no_ms, gen_incr_brightness( 4));
    setup_fine_mode_ms_key (k, k.ks.mode_states.qks2, lalt, qks1,  gen_incr_brightness(-1));
    setup_fine_mode_ms_key (k, k.ks.mode_states.qks3, lalt, qks1,  gen_incr_brightness( 1));

    // win-2 is vol down, win-3 is vol up .. (fine mode does nothing different for volume)
    setup_fine_mode_ms_key (k, k.ks.mode_states.qks2, lwin, no_ms, ag().k(VolumeDown).gen_af());
    setup_fine_mode_ms_key (k, k.ks.mode_states.qks3, lwin, no_ms, ag().k(VolumeUp  ).gen_af());
    setup_fine_mode_ms_key (k, k.ks.mode_states.qks2, lwin, qks1,  ag().k(VolumeDown).gen_af());
    setup_fine_mode_ms_key (k, k.ks.mode_states.qks3, lwin, qks1,  ag().k(VolumeUp  ).gen_af());

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
    k.cm .add_combo ( cg().k(F2).m(lwin),           ag().af (media_next_action (k.ks, true )) );
    k.cm .add_combo ( cg().k(F2).m(lwin).m(caps),   ag().af (media_next_action (k.ks, false)) );
    k.cm .add_combo ( cg().k(F2).m(lwin).m(shift),  ag().af (media_next_action (k.ks, false)) );

    // win-f3 for skip forward a bit (w/ caps for rewind)
    k.cm .add_combo ( cg().k(F3).m(lwin),           ag().af (media_skips_action (1, k.ks, true )) );
    k.cm .add_combo ( cg().k(F3).m(lwin).m(caps),   ag().af (media_skips_action (2, k.ks, false)) );
    k.cm .add_combo ( cg().k(F3).m(lwin).m(shift),  ag().af (media_skips_action (2, k.ks, false)) );

    // gaah, for track trawling, even that is being annoying to press, wanted to set up right hand alternative too
    k.cm .add_combo ( cg().k(Down ) .m(caps_dbl),  ag().af (media_next_action (k.ks, true )) );
    k.cm .add_combo ( cg().k(Up   ) .m(caps_dbl),  ag().af (media_next_action (k.ks, false)) );
    k.cm .add_combo ( cg().k(Right) .m(caps_dbl),  ag().af (media_skips_action (1, k.ks, true)) );
    k.cm .add_combo ( cg().k(Left ) .m(caps_dbl),  ag().af (media_skips_action (1, k.ks, false)) );


    // we'll also setup a media/vol fsc : caps-q-m .. sticky
    let fsc = FSC::MediaVol.ch();
    k.cm .register_combo_sticky_first_stroke ( fsc,  cg().k(M).no_rpt().m(caps).s(qks) );

    k.cm .add_combo ( cg().k(I    ).m(caps).wcs(qks).fsc(fsc),  ag().k(VolumeUp  ) );    // vol up
    k.cm .add_combo ( cg().k(Comma).m(caps).wcs(qks).fsc(fsc),  ag().k(VolumeDown) );    // vol down
    k.cm .add_combo ( cg().k(M    ).m(caps).wcs(qks).fsc(fsc),  ag().k(VolumeMute) );    // mute

    k.cm .add_combo ( cg().k(J).m(caps).wcs(qks).fsc(fsc),  ag().af (media_next_action (k.ks, false)) );         // prev
    k.cm .add_combo ( cg().k(K).m(caps).wcs(qks).fsc(fsc),  ag().af (media_next_action (k.ks, true )) );         // next
    k.cm .add_combo ( cg().k(H).m(caps).wcs(qks).fsc(fsc),  ag().af (media_skips_action (1, k.ks, false)) );     // skip bkwd
    k.cm .add_combo ( cg().k(L).m(caps).wcs(qks).fsc(fsc),  ag().af (media_skips_action (1, k.ks, true )) );     // skip fwd


    // we'll also set these on a latching fsc (caps-qw)-m for sustained sessions of track trawling w arrow keys
    // fsc : caps-qw-M -> media trolling .. (latching .. but same as the sticky fsc combohash as the sticky one above)
    k.cm.register_combo_latching_first_stroke ( fsc,  cg() .k(M).no_rpt() .m(caps) .fsc (FSC::LatchInit.ch()) );
    k.cm .add_combo ( cg().k(Down ).fsc(fsc),  ag().af (media_next_action (k.ks, true )) );
    k.cm .add_combo ( cg().k(Up   ).fsc(fsc),  ag().af (media_next_action (k.ks, false)) );
    k.cm .add_combo ( cg().k(Right).fsc(fsc),  ag().af (media_skips_action (1, k.ks, true)) );
    k.cm .add_combo ( cg().k(Left ).fsc(fsc),  ag().af (media_skips_action (1, k.ks, false)) );

}




fn setup_win_groups (k:KR) {
    // we'll assign the Numrow_[1/2/3/4] (overloaded with qks1/qks2/qks3/qks4) for win-grp activations
    // and when those are held down (hence why assigned to qks keys), we'll have T/W etc do actions on those groups
    // (Note that there are also mouse lbtn-dbl-click and rbtn-click combos (defined in mouse sections above) for add/remove to wingroups)
    fn set_win_grp_af_combos <F> (k:KR, key:Option<Key>, f:F)
        where F : Fn (&KrustyState, WinGroups_E) + Clone + Send + Sync + 'static
    {
        fn wgs (wg: WinGroups_E) -> ModeState_T {
            match wg { wg1 => qks1,  wg2 => qks2,  wg3 => qks3,  wg4 => qks4 }
        }
        fn wgk (key:Option<Key>, wg: WinGroups_E) -> Key {
            key .unwrap_or ( match wg { wg1 => Numrow_1,  wg2 => Numrow_2,  wg3 => Numrow_3,  wg4 => Numrow_4 } )
        }
        let gen_af = |wg:WinGroups_E, f:&F| {
            let f = f.clone();
            Arc::new ( move || f (k.ks, wg) )
        };
        k.cm .add_combo ( cg().k(wgk(key,wg1)) .s(wgs(wg1)) .m(caps).m(lwin),  ag().af (gen_af (wg1, &f)) );
        k.cm .add_combo ( cg().k(wgk(key,wg2)) .s(wgs(wg2)) .m(caps).m(lwin),  ag().af (gen_af (wg2, &f)) );
        k.cm .add_combo ( cg().k(wgk(key,wg3)) .s(wgs(wg3)) .m(caps).m(lwin),  ag().af (gen_af (wg3, &f)) );
        k.cm .add_combo ( cg().k(wgk(key,wg4)) .s(wgs(wg4)) .m(caps).m(lwin),  ag().af (gen_af (wg4, &f)) );
    }
    // finally we can now set up actions (which will be set up for each of the three win-groups)
    set_win_grp_af_combos ( k, None,    |ks,wg| ks.win_groups.toggle_grp_activation(wg) );
    set_win_grp_af_combos ( k, Some(T), |ks,wg| ks.win_groups.toggle_grp_always_on_top(wg) );
    set_win_grp_af_combos ( k, Some(W), |ks,wg| ks.win_groups.close_grp_windows(wg) );


    // we have win-mouse window drag/resize .. we'd like to cancel any in-progress action via escape
    fn gen_cancel_win_mouse_action (key:Key, ks:KSR) -> AF {
        Arc::new ( move || {
            if ks.mouse.lbtn.down.is_set() {
                ks.mouse.lbtn.consumed.set();
                handle_pointer_action_cancel (ks);
            } else {
                //press_release(key)
                // ^^ hmm, instead of win-esc being just esc otherwise, we'll use it as window minimize-and-back
                if key==Escape { win_fgnd_min_and_back() } else { key.press_release() }
            }
        } )
    }
    // we'll allow Escape to cancel in-progress win-drag-to-move/resize operations
    k.cm .add_combo ( cg().k(Escape).m(lwin),          ag().af (gen_cancel_win_mouse_action (Escape, k.ks)) );
    k.cm .add_combo ( cg().k(Escape).m(lwin).m(caps),  ag().af (gen_cancel_win_mouse_action (Escape, k.ks)) );
    // and since Esc is hard to press w caps-win, we'll let Q do the same too
    k.cm .add_combo ( cg().k(Q).m(lwin).m(caps),       ag().af (gen_cancel_win_mouse_action (Q, k.ks)) );

}




fn setup_caps_2wsx_combos (_k:KR) {
    // .. initially noticed with caps-shift-w, which should have auto given ctrl-shift-w (close all tabs) .. but nothing comes out
    // .. it turns out (on this kbd) caps-shift-[F2, 2, w, s, x] dont produce any key event at the hook at all .. maybe from the driver itself
    // funnily enough, there's a bunch of complaints about specifically those keys for dell/hp laptops .. looks like hardware
    //    appears to be a common kbd pcb layout issue .. heres from 2007: (https://www.joachim-breitner.de/blog/250-Shift-Caps-2)

    // turns out its a well known issue called key-rollover (kro) .. fancier n-kro and 6-kro keyboards are apparently avaiable
    // .. sadly enough, they'd have to be external, no way to replace a laptop kbd to something with higher-kro


    // sooo .. to makeup, we'll set those on caps_dbl instead
    #[allow (dead_code)]
    fn map_caps_dbl_as_ctrl_shift (k:KR, key:Key) {
        k.cm .add_combo ( cg().k(key).m(caps_dbl),           ag().k(key).m(ctrl).m(shift) );
        k.cm .add_combo ( cg().k(key).m(caps_dbl).m(shift),  ag().k(key).m(ctrl).m(shift) );
    }
    //[Numrow_2, W, S, X] .iter().for_each (|&key| map_caps_dbl_as_ctrl_shift (k, key));
    // ^^ note that caps-dbl F2 is used for latching-fscs, so excluded from the list above
    // ^^ meh, never gets used, wont even be remembered .. S is also now switching etc

    // we wanted to add support for caps-e-w (as ctrl-w) during tabs scroll with caps-e-wheel
    //k.cm .add_combo ( cg().k(W).m(caps).s(msE),  ag().k(W).m(ctrl) );
    // ^^ wont work .. another one of those caps-f2/2/w/s/x hardware-level issues (entire row is down when caps-w down)
    // otoh, doing a caps-e-w then release/re-press E does give out a caps-w .. so oh well
    // either way, we'll setup Q to do that at least
    //k.cm .add_combo ( cg().k(Q).m(caps).s(msE),  ag().k(W).m(ctrl) );
    // ^^ naah, we'd rather keep that to layer with other modkeys, modes etc
    // .. instead, elsewhere in mouse code, we've added caps-e-x2 for ctrl-w

}



fn setup_space_key (k:KR) {

    // we wanted a bunch of Enter options on space .. (mostly coz Space is ergo, Enter is not)
    k.cm .add_combo ( cg().k(Space).m(ralt),          ag().k(Enter) );                  // ralt-space       -> Enter
    k.cm .add_combo ( cg().k(Space).m(caps).s(msF),   ag().k(Enter) );                  // caps-f-space     -> Enter

    k.cm .add_combo ( cg().k(Space).m(lalt)         .c(switche_not_fgnd()),   ag().k(Enter) );          // lalt-space  -> Enter ..  (excl switche)
    k.cm .add_combo ( cg().k(Space).m(caps).m(lalt) .c(switche_not_fgnd()),   ag().k(Enter).m(lalt) );  // caps-lalt-space  -> alt-enter

    k.cm .add_combo ( cg().k(Space).m(lalt_dbl),   ag().k(Space).m(alt) );   // dbl-lalt-space -> alt-space (orig action)

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



fn setup_misc_standalone_combos (k:KR) {
    // just using this as staging pile for anything else we want to add


    /// Ctrl-S .. caps-s is now window-switching hotkey, and the actual ctrl-s is not ergo to use ..
    /// .. so we'll put additional ctrl-s on Alt-s, caps-q-s, caps-f-s, and caps-caps-s for now (we'll see what gets used)
    k.cm .add_combo ( cg().k(S).m(lalt),         ag().k(S).m(ctrl) );
    k.cm .add_combo ( cg().k(S).m(caps_dbl),     ag().k(S).m(ctrl) );
    k.cm .add_combo ( cg().k(S).m(caps).s(qks),  ag().k(S).m(ctrl) );
    k.cm .add_combo ( cg().k(S).m(caps).s(msF),  ag().k(S).m(ctrl) );   // (must be rolling due to 2wsx)


    /// Escape setups .. (there are also more Esc configs in switche-specific and win-grps sections)
    // .. Escape is just escape, but we want it to do press-release immediately (so switche is faster)
    k.cm .add_combo ( cg().k(Escape),          ag().k(Escape) );
    k.cm .add_combo ( cg().k(Escape).m(caps),  ag().k(Escape) );

    // .. and use the apps key to send shift-escape ..
    k.cm .add_combo ( cg().k(Apps),   ag().k(Escape).m(shift) );



    /// BackQuote setups
    // .. make normal backquote be Delete, caps can do back-tick, and shift or ralt do its tilde
    k.cm .add_combo ( cg().k(Backquote),          ag().k(ExtDelete) );
    k.cm .add_combo ( cg().k(Backquote).m(caps),  ag().k(Backquote) );
    //k.cm .add_combo ( cg().k(Backquote).m(lalt),    ag().k(Backquote) );
    //k.cm .add_combo ( cg().k(Backquote).m(shift),   ag().k(Backquote).m(shift) );
    //k.cm .add_combo ( cg().k(Backquote).m(ralt),    ag().k(Backquote).m(shift) );
    // ^^ not strictly necessary as cb composition now defaults to this, but also useful to see here for reference

    // .. alt-backquote, we'll set that up to give ctrl-tab as more ergo alternative, and tying in w alt-tab
    k.cm .add_combo ( cg().k(Backquote).m(lalt),  ag().k(Tab).m(lctrl) );


    // we'll set caps-alt-p to bring up process explorer (via ctrl-shift-esc)
    k.cm .add_combo ( cg().k(P).m(caps).m(lalt),  ag().k(Escape).m(lctrl).m(lshift) );

    // chrome/browser specific combos
    // caps-alt-t --> ctrl-shift-a (tabs search popup)
    k.cm .add_combo ( cg().k(T).m(lalt).m(caps).c(browser_fgnd()),  ag().k(A).m(ctrl).m(shift) );


    // quick shortcut to reset system cursors .. mostly useful while impl/testing it
    k.cm .add_combo ( cg().k(C).m(caps_dbl).m(lalt),  ag().af (action (|| Cursors::instance().apply_sys())) );

}




fn setup_switche_alt_tab (k:KR) {
    // in general, alt-tab is direclty listened to by switche, so we woudlnt have to drive it from here ..
    // ^^ no longer true as we wanted many more overloads, so now we drive switche alt-tab from krusty again

    // either way, we'll override F1 so we can use it via ralt-F1 (if we disable F1 in swi-configs)
    k.cm .add_combo ( cg().k(F1),          ag().k(F15).m(alt).m(ctrl)  );
    k.cm .add_combo ( cg().k(F1).m(ralt),  ag().k(F1) );

    // now we can setup the actual nav ..
    // .. and since we might have alt-active and want to avoid alt-release, we'll override to have no-alt, mkg-no-wrap
    // .. and with actual lalt pressed, we'll switch from doing block-lim to non-block-lim (i.e. w/ and w/o shift)
    [ (I, ExtUp), (Comma, ExtDown), (U, ExtPgUp), (M, ExtPgDn), (J, ExtLeft), (K, ExtRight),
    ] .iter().for_each ( |&(k1,k2)| {
        k.cm .add_combo ( cg() .k(k1) .m(lalt)          .c(switche_fgnd()),   ag().k(k2).mkg_nw() );
        k.cm .add_combo ( cg() .k(k1) .m(caps)          .c(switche_fgnd()),   ag().k(k2).mkg_nw() );
        k.cm .add_combo ( cg() .k(k1) .m(caps) .m(lalt) .c(switche_fgnd()),   ag().k(k2).m(shift).mkg_nw() );
    } );
    // for the physical arrow keys though, we can make caps alone switch to non-block-lim
    [ (Up, ExtUp), (Down, ExtDown), (Left, ExtLeft), (Right, ExtRight),
    ] .iter().for_each ( |&(k1,k2)| {
        k.cm .add_combo ( cg() .k(k1) .m(lalt)          .c(switche_fgnd()),   ag().k(k2).mkg_nw() );
        k.cm .add_combo ( cg() .k(k1) .m(caps)          .c(switche_fgnd()),   ag().k(k2).m(shift).mkg_nw() );
        k.cm .add_combo ( cg() .k(k1) .m(caps) .m(lalt) .c(switche_fgnd()),   ag().k(k2).m(shift).mkg_nw() );
    } );
    // plus for pg-up/down, we want to support in-block on tap, and full-list on dbl-tap setup
    [ (U,ExtPgUp), (M,ExtPgDn) ] .iter().for_each ( |&(k1,k2)| {
        k.cm .add_combo ( cg() .k(k1) .dbl() .m(caps) .c(switche_fgnd()) .no_rpt(),  ag().k(k2).m(shift).mkg_nw() );
        k.cm .add_combo ( cg() .k(k1) .dbl() .m(lalt) .c(switche_fgnd()) .no_rpt(),  ag().k(k2).m(shift).mkg_nw() );
    } );

    // we'll also set caps-o/q as Esc eqv .. (to match what we do in fsc-mode invocations)
    k.cm .add_combo ( cg() .k(O) .m(lalt) .c(switche_fgnd()),   ag().k(Escape) );     // alt-o  -> Escape
    k.cm .add_combo ( cg() .k(O) .m(caps) .c(switche_fgnd()),   ag().k(Escape) );     // caps-o -> Escape
    k.cm .add_combo ( cg() .k(Q) .m(caps) .c(switche_fgnd()),   ag().k(Escape) );     // caps-q -> Escape


    // we'll separately setup wheel behavior when switche is in fgnd (other conditionals for rbtn-wheel, x2-wheel etc are elsewhere)
    let wel = k.wel;
    let cc : ComboCond = Arc::new ( |ks,_ev| {
        check_switche_fgnd(wel) && !ks.mouse.rbtn.down.is_set() && !ks.sticky_first_stroke.check_match (FSC::X2_Wheel.ch())
    } );

    // regular or alt- wheel can send shift-up/down for non-block-lim nav .. this makes wheel work outside switche window
    setup_frwd_bkwd_whl ( k, |wg| wg.c(cc.clone()),            ExtDown, ExtUp,   |ag,p| ag.k(p).m(shift).mkg_nw() );
    setup_frwd_bkwd_whl ( k, |wg| wg.c(cc.clone()) .m(lalt),   ExtDown, ExtUp,   |ag,p| ag.k(p).m(shift).mkg_nw() );

    // caps/ctrl wheel should do block-lim nav (which is default and doesnt need shift)
    setup_frwd_bkwd_whl ( k, |wg| wg.c(cc.clone()) .m(ctrl),           ExtDown, ExtUp,   |ag,p| ag.k(p).mkg_nw() );
    setup_frwd_bkwd_whl ( k, |wg| wg.c(cc.clone()) .m(caps),           ExtDown, ExtUp,   |ag,p| ag.k(p).mkg_nw() );
    setup_frwd_bkwd_whl ( k, |wg| wg.c(cc.clone()) .m(caps).m(lalt),   ExtDown, ExtUp,   |ag,p| ag.k(p).mkg_nw() );


    // while at it, lets also add some minimal wheel support for when actual native alt-tab is active
    let cc : ComboCond = Arc::new ( |_ks,_ev| check_alt_tab_fgnd(wel) );
    //k.cm .add_combo ( cg().whl().bkwd(),  ag().k(Tab) )
    setup_frwd_bkwd_whl ( k, |wg| wg.c(cc.clone()),           ExtRight, ExtLeft,  |ag,p| ag.k(p).mkg_nw() );
    setup_frwd_bkwd_whl ( k, |wg| wg.c(cc.clone()) .m(lalt),  ExtRight, ExtLeft,  |ag,p| ag.k(p).mkg_nw() );


    // for alt-escape, we want to override the default send-to-back behavior, as it has issues detailed in notes ..
    // .. basically that send-to-back doesnt produce events switche hears, so switche would get out-of-sync
    // .. and also, just the send-to-back leaves focus lost if the next-in-line was minimized, which is also awkward
    //
    // so for alt-esc we'll send special switche cmd that will activate the next non-minimized window .. (and also send cur to back)
    // .. if switche is fgnd, it'll directly process the Esc itself (to hide its window) .. so nothing to do
    // .. if switche NOT fgnd, we'll switch to next window in switche and send cur to back
    let switche_next_non_minimized_af = ag().k(F20).m(alt).m(shift).gen_af();
    let alt_esc_action = Arc::new ( move || {
        let hwnd_to_back = wel.fgnd_info.read().unwrap().hwnd;   // cache before switche changes fgnd
        switche_next_non_minimized_af();
        win_send_to_back (hwnd_to_back);
    } );
    k.cm .add_combo ( cg().k(Escape).m(lalt).c(switche_not_fgnd()),   ag().af (alt_esc_action) );

}



fn setup_switch_windows_w_caps_sfsc (k:KR) {

    // sfsc : caps-s .. we'll use as alt-tab alternative via switche
    // caps-s -> invoke .. then with caps held :
    // w/s -> up/dn, e/d -> block-pg-up/dn, ee/dd -> pg-up/dn
    // caps-rel -> do-switch, space -> unarm, o/q/Esc -> escape

    let fsc = FSC::SwitcheCaps.ch();
    k.cm .register_combo_sticky_first_stroke ( fsc,  cg().k(S).m(caps) );   // caps-s trigger


    // we'll setup invocation on the fsc trigger itself (similar to how alt-tab and ctrl-tab work)
    let alt_tab_af = Arc::new (move || { k.ks.mod_keys.lalt.ensure_active(); Tab.press_release(); } );
    let act_af = Arc::new ( move || {
        // F1 would be easier for invocation, but won't turn on the armed-indicator
        // .. so we'll simulate actual alt-tab itself .. (with its various complications)
        // if we're already active (e.g after disarming caps-release activation) we'll need to Esc out of it (avoids unintended release)
        if check_switche_fgnd(k.wel) { Escape.press_release() }
        // now if we had ctrl active, e.g. caps-[clicked/drag/scroll], we'll need to wrap alt-tab with ctrl release and re-press afterwards
        if k.ks.mod_keys.lctrl.active.is_set() {
            LCtrl.release(); alt_tab_af(); LCtrl.press();
        } else { alt_tab_af() }
    } );
    k.cm .add_combo ( cg().k(S).m(caps),  ag().af(act_af) );

    // and we'll setup fsc clearing action upon caps-release to actually switch to the selected window
    let fsc_clear_af = Arc::new ( move || k.ks.mod_keys.lalt.ensure_inactive() );
    k.cm .register_af_sticky_first_stroke_cleared (fsc, fsc_clear_af.clone());

    // we'll also setup Space to unarm the key-release-activation trigger .. (similar to switche alt-tab)
    // and instead of just sending Space (to layer w active-Alt), we'll send Ctrl-Space to avoid win32 ui titlebar ctxt menu
    let unarm_af = Arc::new ( move || {
        LCtrl.press(); Space.press_release(); LCtrl.release();
    } );
    k.cm .add_combo ( cg().k(Space).m(caps).fsc(fsc),  ag().af(unarm_af) );

    // for escape options, actual Esc, and generic caps-o would still work, but we want those to exit the fsc too ..
    // (and calling the fsc clear will then clear fsc flags, swap cursor, and call the registered fsc-clearing callback)
    let esc_af = Arc::new ( move || { Escape.press_release(); k.ks.clear_cur_sticky_fsc(); } );
    k.cm .add_combo ( cg() .k(Escape) .m(caps) .fsc(fsc) .c(switche_fgnd()),  ag().af (esc_af.clone()) );
    k.cm .add_combo ( cg() .k(O     ) .m(caps) .fsc(fsc) .c(switche_fgnd()),  ag().af (esc_af.clone()) );
    k.cm .add_combo ( cg() .k(Q     ) .m(caps) .fsc(fsc) .c(switche_fgnd()),  ag().af (esc_af.clone()) );

    // because caps-tab usually triggers ctrl-tab fsc (replacing this fsc), we'll override it while under this fsc
    k.cm .add_combo ( cg() .k(Tab) .m(caps).fsc(fsc),  ag().k(Tab).mkg_nw() );

    // now we can setup the actual nav .. (the ones defined in alt-tab section will continue to work, but we want E/D/S/W additionally)
    // .. and again, regular nav we'll do block-lim, and w/ alt we'll do non-block-lim (which needs shift wrapping)
    [ (W, ExtUp), (S, ExtDown), (E, ExtPgUp), (D, ExtPgDn) ] .iter().for_each ( |&(k1,k2)| {
        k.cm .add_combo ( cg() .k(k1) .m(caps)          .fsc(fsc),   ag().k(k2).mkg_nw() );
        k.cm .add_combo ( cg() .k(k1) .m(caps) .m(lalt) .fsc(fsc),   ag().k(k2).m(shift).mkg_nw() );
    } );
    // for the physical arrow keys, since we need caps-held here, we'll override them to do block-lim (i.e no shift)
    // (the rest can work as normal from general alt-tab setup)
    [ (Up, ExtUp), (Down, ExtDown), (Left, ExtLeft), (Right, ExtRight),
    ] .iter().for_each ( |&(k1,k2)| {
        k.cm .add_combo ( cg() .k(k1) .m(caps) .fsc(fsc),   ag().k(k2).mkg_nw() );
    } );
    // plus, for pg-up/down, we want to support in-block on tap (done above), and full-list on dbl-tap
    [ (E, ExtPgUp), (D, ExtPgDn) ] .iter().for_each ( |&(k1,k2)| {
        k.cm .add_combo ( cg() .k(k1) .dbl() .m(caps) .fsc(fsc) .no_rpt(),   ag().k(k2).m(shift).mkg_nw() );
    } );


    // wheel scrolls would mostly continue to work identically between this and regular switche alt-tab ..
    // however, for regular switche, we do caps-wheel as block lim .. but here caps will always be down
    // .. so we'll map caps-wheel for this fsc to regular wheel .. (and alt-caps will continue to do block-lim as regularly)
    setup_frwd_bkwd_whl ( k,  |wg| wg .m(caps) .fsc(fsc),   ExtDown, ExtUp,   |ag,p|  ag.k(p).m(shift).mkg_nw() );

}




fn setup_switch_windows_rbtn_scroll (k:KR) {
    // This is for the alternative krusty driven rbtn switching mechanism where we've set rbtn to release-activation-only
    // (as opposed to letting rbtn remain natural and letting switche handle rbtn-wheel-invocation natively)
    // Avoiding the initial rbtn-press like this completely eliminates the annoyance of context menu popping up (when in desktop or explorer)
    // .. and the cost of this is basically disabling rbtn-drag etc .. which we almost never use in any app etc anyway

    // note that it is impld w up/dn keys below after invocation instead of just Tabs or wheel fallbacks because ..
    // .. when sw isnt doing mouse hook, it can only listen mouse events in its window .. (and we'd like rbtn scrolls to work anywhere)
    // further, any injected alt-shift-tab wont work because for alt-tab switche uses kbd-hook and queries phys shift-state directly !!
    // .. (hence why we're doing the invocation first and then going to list top when the invocation itself is wheel-forward)

    // re the complex combo-condition gating this below .. refer to comments in the rbtn ctrl-tab sections

    let cc : ComboCond = Arc::new ( |ks,_ev| {
        // rbtn-wheel is for window-switching, but we want to leave x2-rbtn-wheel for tab switching!
        // (note that caps during rbtn-scroll is handled separately below)
        ks.mouse.rbtn.down.is_set() &&
            ( check_switche_fgnd (k.wel) ||
                ( !ks.mouse.x2btn.down.is_set() &&
                    !ks.sticky_first_stroke.check_match (FSC::X2_Wheel.ch()) &&
                    !ks.sticky_first_stroke.check_match (FSC::QuickBar.ch())
            ) )
    } );
    fn gen_rbtn_wheel_af (dir_is_bkwd:bool, k:KR) -> AF {
        let whl_key = if dir_is_bkwd { ExtDown } else { ExtUp };
        Arc::new ( move || {
            k.ks.mouse.rbtn.pending.clear();
            if k.ks.in_right_btn_scroll_state.is_set() && check_switche_fgnd(k.wel) {
                // this is special case where we're already actively in rbtn-scroll w switche fgnd
                // .. so we want to just nav up/dn .. (but regular up/dn do block-lim, so need shift to do no-block-lim nav)
                LShift.press(); whl_key.press_release(); LShift.release();
                return
            }
            // everything else is first rbtn-scroll, we have to send alt-tab (which will also set it armed), but shift wont work!
            k.ks.in_right_btn_scroll_state.set();
            k.ks.mod_keys.lalt.ensure_active();
            // and here, we dont want any caps-rel during alt-tab to send out alt-rel ..
            // so we'll clear the managed-active flag that the ensure_active above sets
            k.ks.mod_keys.lalt.mngd_active.clear();
            // now whether sw was fgnd or not, we want to arm it, so we'll have to send an alt-tab (and shift-tab wont work)
            Tab.press_release();
            // now if dir was down, Tab is enough, we're done
            if dir_is_bkwd {
                return
            }
            // else we'll have to workaround..and we'll need some delay for the alt-tab above to be processed and get switche in fgnd
            thread::spawn ( move || {
                thread::sleep (Duration::from_millis(100));
                // ^^ for robustness, looks like we need a delay at least around 100ms
                // and then we simply move two-steps back, as at first invocation, we'd want to be one-back from current (i.e at bottom)
                // .. (and if because this is delayed, another fast wheel-up managed to get through before fgnd .. meh its still harmless)
                if check_switche_fgnd(k.wel) {
                    // we'll guard for sw fgnd so we dont send spurious keys to random window if sw is not yet fgnd
                    LShift.press();  ExtUp.press_release(); ExtUp.press_release(); LShift.release();
                }
            } );
        } )
    }
    setup_frwd_bkwd_whl ( k, |wg| wg .c(cc.clone()),   true, false,   |ag,p| ag.af (gen_rbtn_wheel_af(p,k)) );


    // for caps-rbtn-scrolls .. only if rbtn-alt-tab already active .. (coz from afresh, we use that for ctrl-tab)
    let cc : ComboCond = Arc::new (|ks,_ev| ks.mouse.rbtn.down.is_set() && check_switche_fgnd(k.wel));

    // and for these we simply want regular switche nav (block-limited)
    setup_frwd_bkwd_whl ( k, |wg| wg .m(caps) .c(cc.clone()),   ExtDown, ExtUp,   |ag,p| ag.k(p).mkg_nw() );

    // but for alt-rbtn-scroll, we want to do what alt-tab does, so non-block-lim nav
    setup_frwd_bkwd_whl ( k, |wg| wg .m(lalt) .c(cc.clone()),   ExtDown, ExtUp,   |ag,p| ag.k(p).m(shift).mkg_nw() );


    // now on rbtn release, we'll want to send out the alt-release if need be
    // .. and thats just added to the general rbtn release section instead of here
    // (where we also postpone regular rbtn-press from going out till it's released)

}




fn setup_switch_windows_blind_sfsc (k:KR) {

    // sfsc : caps-d-w ..  we'll **_ navigate across windows _** (via switche snapshots w/o switche popup)
    // note that unlike for the non-blind version, we dont get visual feedback of which dir it switching

    let fsc = FSC::SwitcheBlind.ch();
    k.cm .register_combo_sticky_first_stroke ( fsc,  cg().k(W).no_rpt().m(caps).s(msD) );

    // the nav-keys should be .. refresh:F15,  next:F16,  prev:F17,  top:F18,  bottom:F19  (w/ alt-shift)
    let nav_ag = |nav_key:Key| ag().k(nav_key).m(alt).m(shift);

    // we'll make the fsc trigger itself do the first switch, similar to how alt-tab works
    let init_af : AF = {
        let (refresh_af, fwd_af) = (nav_ag(F15).gen_af(), nav_ag(F16).gen_af());
        Arc::new ( move || {
            let (refresh_af, fwd_af) = (refresh_af.clone(), fwd_af.clone());
            thread::spawn ( move || {
                refresh_af();
                thread::sleep (Duration::from_millis(15));  // to give time for the win-enum snap to be taken
                fwd_af();
            } );
        } )
    };
    k.cm .add_combo ( cg().k(W).m(caps).s(msD).no_rpt(),   ag().af (init_af) );

    // D/W should nav fwd/bkwd through the snapshot stack ..
    // W -> prev in z-stack .. (might look reverse from invocation but preserving up/down direction was more important)
    k.cm .add_combo ( cg().k(W).m(caps).fsc(fsc).no_rpt(),  nav_ag(F17) );

    // D -> next in z-stack .. and since its a mode-key that cares about _dbl, we'll set that too
    k.cm .add_combo ( cg().k(D).m(caps).s(msD    ).fsc(fsc).no_rpt(),  nav_ag(F16) );
    k.cm .add_combo ( cg().k(D).m(caps).s(msD_dbl).fsc(fsc).no_rpt(),  nav_ag(F16) );

    // and similar using keyboard keys too .. (using l2 keys as arrows as expected)
    [ (K,F16), (Comma,F16), (J,F17), (I,F17), (U,F18), (M,F19) ] .iter().for_each ( |&(k1,k2)| {
        k.cm .add_combo ( cg().k(k1).m(caps).fsc(fsc).no_rpt(),  nav_ag(k2) );
    } );

    // and for the wheels
    k.cm .add_combo ( cg().whl().bkwd().m(caps).fsc(fsc), nav_ag(F16) );
    k.cm .add_combo ( cg().whl().frwd().m(caps).fsc(fsc), nav_ag(F17) );

    // we'll enable caps-o to send windows to back while doing that
    k.cm .add_combo ( cg().k(O).m(caps).fsc(fsc),  ag().af (action(win_fgnd_min_and_back)) );

}



fn setup_switch_windows_direct_sfsc (k:KR) {
    // (Note that there also a bunch of these in mouse/wheel sections)

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


    k.cm .add_combo ( cg().k(F1).m(lalt),      switche_direct__z_top     .clone() );
    k.cm .add_combo ( cg().k(F1).m(lalt_dbl),  switche_direct__z_second  .clone() );
    k.cm .add_combo ( cg().k(F2).m(lalt_dbl),  switche_direct__z_third   .clone() );

    // we'll set Alt-F2 to bring chrome tabs-outliner (via switche) to keep w the theme of Alt-F<n> keys for task switching
    k.cm .add_combo ( cg().k(F2).m(lalt),      switche_direct__tabs_outliner.clone() );


    // we'll put app-specific direct-switch on lalt-qks1 combos, and on caps-d-s sticky fsc
    let fsc = FSC::SwitcheDirect.ch();
    k.cm .register_combo_sticky_first_stroke ( fsc,  cg().k(S).m(caps).s(msD) );

    // and we'll allow wheel snapshot-switch on this, so lets refresh the snapshot on trigger (via sw Alt-Shift-F15)
    k.cm .add_combo ( cg().k(S).m(caps).s(msD).no_rpt(),   ag().k(F15).m(alt).m(shift) );

    // and enable wheel to do snapshot-switch (as in the actual snap-switch sfsc)
    k.cm .add_combo ( cg().whl().bkwd().m(caps).fsc(fsc),   ag().k(F16).m(alt).m(shift) );
    k.cm .add_combo ( cg().whl().frwd().m(caps).fsc(fsc),   ag().k(F17).m(alt).m(shift) );


    let setup_direct_switch = move |key:Key, ag: &ActionGen<ActionGenSt_Key>| {
        k.cm .add_combo ( cg().k(key).m(lalt).s(qks1),   ag.clone() );
        k.cm .add_combo ( cg().k(key).m(caps).fsc(fsc),  ag.clone() );
    };
    setup_direct_switch ( Space,  & switche_direct__z_top           );   // Space -> last-active
    setup_direct_switch ( L,      & switche_direct__z_top           );   // L -> last-active
    setup_direct_switch ( B,      & switche_direct__browser         );   // B -> first browser window
    setup_direct_switch ( M,      & switche_direct__music           );   // M -> winamp (music)
    setup_direct_switch ( I,      & switche_direct__ide             );   // I -> first IDEA window
    setup_direct_switch ( N,      & switche_direct__notepadpp       );   // N -> Notepad++
    setup_direct_switch ( T,      & switche_direct__tabs_outliner   );   // O -> TabsOutliner (chrome)
    setup_direct_switch ( C,      & switche_direct__claude          );   // C -> Claude (chrome)
    setup_direct_switch ( K,      & switche_direct__kbd_evs_printer );   // K -> kbd-events-printer (chrome)

    // we'll enable caps-o / caps-q to send windows to back while doing that too
    k.cm .add_combo ( cg().k(O).m(caps).fsc(fsc),    ag().af (action(win_fgnd_min_and_back)) );
    k.cm .add_combo ( cg().k(Q).m(caps).fsc(fsc),    ag().af (action(win_fgnd_min_and_back)) );

}




fn setup_switch_desktop_sfsc (k:KR) {
    // caps-win-d as fsc for desktop moves .. w jk arrow keys, as well as wheel

    let fsc = FSC::DesktopSwitch.ch();
    k.cm .register_combo_sticky_first_stroke ( fsc,  cg().k(D).no_rpt().m(caps).m(lwin) );

    k.cm .add_combo ( cg().k(J    ).m(caps).fsc(fsc),   ag().k(ExtLeft ).m(win).m(ctrl) );
    k.cm .add_combo ( cg().k(K    ).m(caps).fsc(fsc),   ag().k(ExtRight).m(win).m(ctrl) );
    k.cm .add_combo ( cg().k(Left ).m(caps).fsc(fsc),   ag().k(ExtLeft ).m(win).m(ctrl) );
    k.cm .add_combo ( cg().k(Right).m(caps).fsc(fsc),   ag().k(ExtRight).m(win).m(ctrl) );

    // and for wheel
    k.cm .add_combo ( cg().whl().frwd().m(caps).fsc(fsc),   ag().k(ExtLeft ).m(win).m(ctrl) );
    k.cm .add_combo ( cg().whl().bkwd().m(caps).fsc(fsc),   ag().k(ExtRight).m(win).m(ctrl) );

}



fn setup_tab_nav_sfsc (k:KR) {
    // fsc : caps-e-w .. sticky
    // note that e-w is 2wsx and only works w/ rolling press (must release e before releasing w)
    let fsc = FSC::TabsDirect.ch();
    k.cm .register_combo_sticky_first_stroke ( fsc,  cg().k(W).no_rpt().m(caps).s(msE) );

    // (note that these have been kept uniform between IDE, chrome, npp etc)
    let tab_nav_right = ag().k(PageDown).m(ctrl);
    let tab_nav_left  = ag().k(PageUp  ).m(ctrl);

    //k.cm .add_combo ( cg().k(W).m(caps).s(msE),  tab_nav_right.clone() );

    k.cm .add_combo ( cg().k(W).m(caps).fsc(fsc),  tab_nav_left.clone() );
    k.cm .add_combo ( cg().k(E).m(caps).fsc(fsc),  tab_nav_right.clone() );

    k.cm .add_combo ( cg().k(E).m(caps).s(msE_dbl).fsc(fsc),  tab_nav_right.clone() );

    k.cm .add_combo ( cg().k(K).m(caps).fsc(fsc),  tab_nav_right.clone() );
    k.cm .add_combo ( cg().k(J).m(caps).fsc(fsc),  tab_nav_left.clone() );

    k.cm .add_combo ( cg().k(Right).m(caps).fsc(fsc),  tab_nav_right.clone() );
    k.cm .add_combo ( cg().k(Left ).m(caps).fsc(fsc),  tab_nav_left.clone() );

    k.cm .add_combo ( cg().whl().bkwd().m(caps).fsc(fsc),  tab_nav_right.clone() );
    k.cm .add_combo ( cg().whl().frwd().m(caps).fsc(fsc),  tab_nav_left.clone() );

    // regardless, we'll also add the caps-e-wheel, as usage patterns sometimes seem to prefer that
    k.cm .add_combo ( cg().whl().bkwd().m(caps).s(msE),  tab_nav_right );
    k.cm .add_combo ( cg().whl().frwd().m(caps).s(msE),  tab_nav_left );

}



fn setup_ctrl_tab_sfsc (k:KR) {
    // caps-as-ctrl for caps-tab switching (and shift/ralt combos will work out naturally in fallbacks)
    // note that caps-as-ctrl is default in fallbacks anyway, but IDE doesnt like the ctrl being pressed/rel for every tab press ..
    // .. so instead, we keep the ctrl active throughout the caps-tabbing, hence the need for the defs below
    // note also that there's also separate tab-nav two-stroke combos .. this is specifically for ctrl-tab nav

    // fscs : caps-tab or ctrl-tab or caps-ctrl-tab
    let fsc = FSC::TabsCtrl.ch();
    k.cm .register_combo_sticky_first_stroke ( fsc,  cg().k(Tab).m(caps) );
    k.cm .register_combo_sticky_first_stroke ( fsc,  cg().k(Tab).m(ctrl) );
    k.cm .register_combo_sticky_first_stroke ( fsc,  cg().k(Tab).m(ctrl).m(caps) );

    // in addition to just registering the fsc action, we also want the Tab to actually send itself out
    let af_caps_tab : AF = Arc::new (move || { k.ks.mod_keys.lctrl.ensure_active(); Tab.press_release(); } );
    k.cm .add_combo ( cg().k(Tab).m(caps),           ag().af (af_caps_tab.clone()) );
    k.cm .add_combo ( cg().k(Tab).m(caps).m(ctrl),   ag().af (af_caps_tab) );
    k.cm .add_combo ( cg().k(Tab).m(ctrl),           ag().k(Tab).m(ctrl) );

    // note that shift/ralt will work on these as-is .. as the fallbacks dont care about our fscs!
    // and caps-release activation will also auto-work as caps release will send ctrl release as normal!
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

    /// Now specifically for the IDE tab-switcher popup, to escape out of it ..
    // .. sending esc while ctrl-active triggers win-start-menu
    // .. so we use the trick of pressing space first to defocus from the list, then releasing ctrl to exit out of it
    let ct_esc = {
        Arc::new ( move || {
            Space.press_release();
            k.ks.mod_keys.lctrl.ensure_inactive();
        } )
    };
    k.cm .add_combo ( cg().k(Escape).fsc(fsc).m(caps),    ag().af(ct_esc.clone()) );

    // and another easier version to go along with our caps-e-o as esc elsewhere
    k.cm .add_combo ( cg().k(O).fsc(fsc).m(caps).s(msE),  ag().af(ct_esc.clone()) );


    /// again for the IDE switcher popup, we wanted to add a quick switch from tab-switcher to searchable one
    // (we'll do it by escaping it first (via space then ctrl rel like above), then invoking the searchable switcher)
    let ide_persistent_switcher = {
        Arc::new ( move || {
            ct_esc();   // first we escape out of it as above
            // we'll want to give a tiny delay so IDE has time to process focus changes appropriately
            thread::spawn ( move || {
                thread::sleep (Duration::from_millis(10));
                // then do actual ctrl-e to bring up the persistent-switcher (as configd in IDE)
                k.ks.mod_keys.lctrl.active_on_key(E)()
            } );
        } )
    };
    //k.cm .add_combo ( cg().k(Space).m(caps).fsc(fsc),  ag().af(ide_persistent_switcher) );
    k.cm .add_combo ( cg().k(Space).m(caps).fsc(fsc) .c(intellij_fgnd()),  ag().af(ide_persistent_switcher) );

}



fn setup_caps_rbtn_mbtn_ctrl_tab (k:KR) {
    // Given the setup for rbtn-scroll window switching, we'll piggyback on that flag for tab-switching with caps-rbtn-scroll
    // and for this, we'll enable the same behavior whether caps is held or x2 is held

    // re the complex combo-cond gating this below .. we've setup x2-rbtn-wheel to do ctrl-tab (cf rbtn-wheel for alt-tab) ..
    // .. but the x2 btn was found to be too stiff, so additionally, we wanted a fsc mode to be activated upon first x2-press w rbtn-down
    // .. and since the co-ordination between these states is complex, we're just adding the fscs options into the same combo-cond
    // .. (instead of the typical expectation of defining combos w the fscs specified in them)

    let wel = k.wel;
    let cc : ComboCond = Arc::new ( |ks,_ev| {
        // we need rbtn down but dont want to overlay ctrl-tab if we're already doing alt-tab
        ks.mouse.rbtn.down.is_set()
            && !check_switche_fgnd (wel)
            && ( ks.mod_keys.caps.down.is_set()
                || ks.mouse.x2btn.down.is_set()
                || ks.sticky_first_stroke.check_match (FSC::X2_Wheel.ch()))
    } );

    fn gen_rbtn_wheel_af (dir_is_bkwd:bool, k:KR) -> AF {
        Arc::new ( move || {
            k.ks.mouse.rbtn.pending.clear(); k.ks.mouse.mbtn.pending.clear(); k.ks.mouse.x2btn.pending.clear();
            k.ks.activate_sticky_fsc (FSC::X2_Wheel.ch());    // <- this will clear upon rbtn or caps release
            k.ks.in_right_btn_scroll_state.set();
            k.ks.mod_keys.lctrl.ensure_active();
            if !dir_is_bkwd {
                Shift.press(); Tab.press_release(); Shift.release();
            } else { Tab.press_release() }
        } )
    }
    k.cm .add_combo ( cg().whl().frwd()          .c(cc.clone()),  ag().af (gen_rbtn_wheel_af (false, k)) );
    k.cm .add_combo ( cg().whl().bkwd()          .c(cc.clone()),  ag().af (gen_rbtn_wheel_af (true,  k)) );
    k.cm .add_combo ( cg().whl().frwd() .m(caps) .c(cc.clone()),  ag().af (gen_rbtn_wheel_af (false, k)) );
    k.cm .add_combo ( cg().whl().bkwd() .m(caps) .c(cc.clone()),  ag().af (gen_rbtn_wheel_af (true,  k)) );

}





fn setup_window_action_sfsc (k:KR) {
    use RectEdgeSide::*;
    // fsc:  caps-win-w   or  caps-alt-w  .. sticky
    //  - j/k/i/comma .. caps-only -> move .. w/ f -> snap .. w/ r -> resize ..
    //  - whl fwd/bkwd .. caps-only OR w/ d -> left/right .. w/ e -> up/dn .. w f/fd/fe -> snap .. r/rd/re -> resize
    //  - toggles: u -> vertmax .. m -> max .. n -> min .. t -> always-on-top .. b -> border/titlebar

    let fsc = FSC::WindowActions.ch();
    k.cm .register_combo_sticky_first_stroke ( fsc,  cg().k(W).no_rpt().m(caps).m(lwin) );      // caps-win-w  as fsc

    k.cm .register_combo_sticky_first_stroke ( fsc,  cg().k(W).no_rpt().m(caps).m(lalt) );      // caps-alt-w  as fsc too

    // - j/k/i/comma .. caps-only -> move .. w/ F -> snap .. w/ R -> resize ..
    let setup_win_move_key = |key:Key, dx:i32, dy:i32, side_t:RectEdgeSide| {
        k.cm .add_combo ( cg().k(key).m(caps).fsc(fsc).s(msF),   ag().af (Arc::new (move || snap_closest_edge_side (k.ks, side_t) )) );
        k.cm .add_combo ( cg().k(key).m(caps).fsc(fsc),          ag().af (Arc::new (move || win_fgnd_move_rel (dx * 40, dy * 40) )) );
        k.cm .add_combo ( cg().k(key).m(caps).fsc(fsc).s(msR),   ag().af (Arc::new (move || win_fgnd_stretch (dx * 20, dy * 20) )) );
        // and fine steps
        k.cm .add_combo ( cg().k(key).m(caps).fsc(fsc).s(qks1).s(msF),   ag().af (Arc::new (move || snap_closest_edge_side (k.ks, side_t) )) );
        k.cm .add_combo ( cg().k(key).m(caps).fsc(fsc).s(qks1),          ag().af (Arc::new (move || win_fgnd_move_rel (dx * 4, dy * 4) )) );
        k.cm .add_combo ( cg().k(key).m(caps).fsc(fsc).s(qks1).s(msR),   ag().af (Arc::new (move || win_fgnd_stretch (dx * 2, dy * 2) )) );
    };
    setup_win_move_key ( J,     -1,  0,  Left  );
    setup_win_move_key ( K,      1,  0,  Right );
    setup_win_move_key ( I,      0, -1,  Top   );
    setup_win_move_key ( Comma,  0,  1,  Bottom);

    // - wheel fwd/bkwd .. caps-only OR w/ D -> left/right .. w/ E -> up/dn .. w F/FD/FE -> snap .. r/rd/re -> resize

    setup_frwd_bkwd_whl (k, |wg| wg.m(caps).fsc(fsc).s(msF),         Right, Left,  |ag,p| ag.af (Arc::new (move || snap_closest_edge_side (k.ks,p) )) );
    setup_frwd_bkwd_whl (k, |wg| wg.m(caps).fsc(fsc).s(msF).s(msD),  Right, Left,  |ag,p| ag.af (Arc::new (move || snap_closest_edge_side (k.ks,p) )) );
    setup_frwd_bkwd_whl (k, |wg| wg.m(caps).fsc(fsc).s(msF).s(msE),  Bottom, Top,  |ag,p| ag.af (Arc::new (move || snap_closest_edge_side (k.ks,p) )) );

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




fn setup_kbd_pointer_sfsc (k:KR) {

    // fsc : caps-e-e-P .. sticky
    let fsc = FSC::KbdPointer.ch();

    k.cm .register_combo_sticky_first_stroke ( fsc,  cg().k(P).no_rpt().m(caps).s(msE_dbl) );
    // ^^ this is actually the most ergonomic to use for quick editing, esp to put multi-caret .. cf the standard caps-q-p, or caps-qw-p triggers

    // fsc : caps-q-P .. sticky
    // we've been registering independent stickies under caps-q-<?> for easier recall, so might as well add that
    k.cm .register_combo_sticky_first_stroke ( fsc,  cg() .k(P).no_rpt() .m(caps).s(qks) );

    // fsc : caps-qw-P .. latching
    // we'll also co-register it under common latch-init fsc for latching mode on the same fsc!
    k.cm .register_combo_latching_first_stroke ( fsc,  cg() .k(P).no_rpt() .m(caps) .fsc(FSC::LatchInit.ch()) );

    let v : i32 = 30;

    // first the cardinal mouse directions .. the msE wc simply to avoid inadvertent actions (e.g when adding cursors)
    k.cm .add_combo ( cg().k(I     ).m(caps).fsc(fsc).wcs(msE),  ag().pointer() .move_rel ( 0, -v) );
    k.cm .add_combo ( cg().k(Comma ).m(caps).fsc(fsc).wcs(msE),  ag().pointer() .move_rel ( 0,  v) );
    k.cm .add_combo ( cg().k(J     ).m(caps).fsc(fsc).wcs(msE),  ag().pointer() .move_rel (-v,  0) );
    k.cm .add_combo ( cg().k(K     ).m(caps).fsc(fsc).wcs(msE),  ag().pointer() .move_rel ( v,  0) );
    // then diagonal directions
    k.cm .add_combo ( cg().k(O     ).m(caps).fsc(fsc),  ag().pointer() .move_rel ( v, -v) );
    k.cm .add_combo ( cg().k(Period).m(caps).fsc(fsc),  ag().pointer() .move_rel ( v,  v) );
    k.cm .add_combo ( cg().k(U     ).m(caps).fsc(fsc),  ag().pointer() .move_rel (-v, -v) );
    k.cm .add_combo ( cg().k(M     ).m(caps).fsc(fsc),  ag().pointer() .move_rel (-v,  v) );
    // more ergonomic/natural version for the U and M keys
    k.cm .add_combo ( cg().k(Y     ).m(caps).fsc(fsc),  ag().pointer() .move_rel (-v, -v) );
    k.cm .add_combo ( cg().k(N     ).m(caps).fsc(fsc),  ag().pointer() .move_rel (-v,  v) );

    // we'll add in click and right click
    k.cm .add_combo ( cg().k(Space ).m(caps).fsc(fsc),         ag().mbtn(LeftButton ) );
    k.cm .add_combo ( cg().k(Space ).m(caps).fsc(fsc).s(msR),  ag().mbtn(RightButton) );

    // and finally, the alt-shift-click in IDE to add extra cursors that we wanted
    k.cm .add_combo ( cg().k(Space ).m(caps).fsc(fsc).s(msE),  ag().mbtn(LeftButton ).m(alt).m(shift) );

    // and for actual arrows we'll add direct remaps, esp useful when using latching mode for click etc
    k.cm .add_combo ( cg().k(Up   ).fsc(fsc),   ag().pointer() .move_rel ( 0, -v) );
    k.cm .add_combo ( cg().k(Down ).fsc(fsc),   ag().pointer() .move_rel ( 0,  v) );
    k.cm .add_combo ( cg().k(Left ).fsc(fsc),   ag().pointer() .move_rel (-v,  0) );
    k.cm .add_combo ( cg().k(Right).fsc(fsc),   ag().pointer() .move_rel ( v,  0) );

    k.cm .add_combo ( cg().k(Enter).fsc(fsc),   ag().mbtn(LeftButton) );
}



fn setup_wheel_arrows_lfsc (k:KR) {
    // fsc : caps-qw-A --> wheel to arrows mode (latching)
    let fsc = FSC::WheelArrows.ch();
    k.cm .register_combo_latching_first_stroke ( fsc,  cg() .k(A).no_rpt() .m(caps) .fsc(FSC::LatchInit.ch()) );

    // turn regular wheel scroll into arrow-nav
    k.cm .add_combo ( cg().whl().bkwd().fsc(fsc),  ag().k(ExtDown) );
    k.cm .add_combo ( cg().whl().frwd().fsc(fsc),  ag().k(ExtUp) );

    // and caps-dbl wheel into regular wheel
    k.cm .add_combo ( cg().whl().bkwd().fsc(fsc).m(caps_dbl),  ag().whl().bkwd() );
    k.cm .add_combo ( cg().whl().frwd().fsc(fsc).m(caps_dbl),  ag().whl().frwd() );

}



fn setup_ide_diff_nav_lfsc (k:KR) {

    // fsc : caps-qw-D -> diff nav mode .. (latching)
    let fsc = FSC::WheelDiff.ch();
    k.cm .register_combo_latching_first_stroke ( fsc,  cg() .k(D).no_rpt() .m(caps) .fsc(FSC::LatchInit.ch()) );

    // ^^ we've put this in latching fsc .. and made even regular wheel (w/o caps) do diff nav ..
    // .. (so we'll have to clear out the latched-fsc (e.g via caps-qq etc) before the wheel reverts to normal!)

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
    // and while doing this, might as well support nav too
    k.cm .add_combo ( cg().k(Comma).fsc(fsc).m(caps).s(msE),   ag().k(ExtDown ).m(ctrl).m(alt) );
    k.cm .add_combo ( cg().k(I    ).fsc(fsc).m(caps).s(msE),   ag().k(ExtUp   ).m(ctrl).m(alt) );


    // and for actual arrow-keys as well .. next/prev
    k.cm .add_combo ( cg().k(Down ).fsc(fsc),  ag().k(ExtDown ).m(ctrl).m(alt) );
    k.cm .add_combo ( cg().k(Up   ).fsc(fsc),  ag().k(ExtUp   ).m(ctrl).m(alt) );
    // next/prev file
    k.cm .add_combo ( cg().k(Left ).fsc(fsc),  ag().k(ExtLeft ).m(ctrl).m(alt).m(shift) );
    k.cm .add_combo ( cg().k(Right).fsc(fsc),  ag().k(ExtRight).m(ctrl).m(alt).m(shift) );

    // and for accept left/right
    k.cm .add_combo ( cg().k(Left ).fsc(fsc).m(caps),  ag().k(ExtLeft ).m(ctrl).m(alt) );
    k.cm .add_combo ( cg().k(Right).fsc(fsc).m(caps),  ag().k(ExtRight).m(ctrl).m(alt) );
    // and while doing this, might as well support nav too
    k.cm .add_combo ( cg().k(Down ).fsc(fsc).m(caps),  ag().k(ExtDown ).m(ctrl).m(alt) );
    k.cm .add_combo ( cg().k(Up   ).fsc(fsc).m(caps),  ag().k(ExtUp   ).m(ctrl).m(alt) );

}




fn setup_IDE_combos (k:KR) {
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

    let popup_quick_nav_bar  = ag().k(Backquote).m(alt).m(shift);
    let tools_tabs_dropdown  = ag().k(ExtDown).m(alt);

    let collapse_nav_tree  =  ag().k(Slash    ).m(ctrl).m(alt).m(shift);
    let expand_nav_tree    =  ag().k(Backslash).m(ctrl).m(alt).m(shift);

    let caret_to_block_start  =  ag().k(LBracket).m(alt);
    let caret_to_block_end    =  ag().k(RBracket).m(alt);
    let sel_to_block_start    =  ag().k(LBracket).m(alt).m(shift);
    let sel_to_block_end      =  ag().k(RBracket).m(alt).m(shift);

    let caret_to_matching_brace  =  ag().k(P).m(ctrl).m(shift);
    // there no sel eqv for this in IDE, but the sel-to-block-start/end

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

    let _nav_loc_back       =  ag().k(ExtLeft ).m(alt);
    let _nav_loc_frwd       =  ag().k(ExtRight).m(alt);
    let _nav_edit_loc_back  =  ag().k(ExtLeft ).m(alt).m(shift);
    let _nav_edit_loc_frwd  =  ag().k(ExtRight).m(alt).m(shift);
    // ^^ these IDE location nav combos auto work based on generic l2 setups .. (just here for reference)
    // caps-alt-J/K    ->  alt-[left, right]
    // caps-alt-E-J/K  ->  alt-shift-[left, right]

    let _open_in_other_view_pane =  ag().k(O).m(ctrl).m(shift);
    let _popup_recent_locations  =  ag().k(L).m(alt);
    // ^^ these can be used directly ofc .. mostly here for reference or additional hotkeys


    k.cm .add_combo ( cg().k(G).m(caps).s(msE),   show_file_git_diff );
    k.cm .add_combo ( cg().k(P).m(caps).s(qks3),  toggle_diff_preview );

    k.cm .add_combo ( cg().k(Comma).m(caps).s(msR_dbl),  goto_ref_usage );
    k.cm .add_combo ( cg().k(I    ).m(caps).s(msR_dbl),  goto_impl_decl );
    // ^^ note that these two can have very similar results, e.g for fn usage etc etc

    // .. note that there's natural caps-alt-<l2> that does nav among last caret locations (via alt-left/right)

    k.cm .add_combo ( cg().k(K).m(caps).s(qks2),  popup_bookmarks_viewer );
    k.cm .add_combo ( cg().k(U).m(caps).s(qks2),  caret_bookmark_toggle  );

    k.cm .add_combo ( cg().k(N).m(caps).s(msF),  popup_quick_nav_bar );
    k.cm .add_combo ( cg().k(O).m(caps).s(msF),  _open_in_other_view_pane );

    k.cm .add_combo ( cg().k(I    ).m(caps).s(qks2),  bookmark_prev );
    k.cm .add_combo ( cg().k(Comma).m(caps).s(qks2),  bookmark_next );

    k.cm .add_combo ( cg().k(Comma).m(caps).s(qks),  tools_tabs_dropdown );
    // ^^ normal caps-alt-comma will also naturally give alt-down

    k.cm .add_combo ( cg().k(Backslash).m(caps).s(msF),  expand_nav_tree.clone() );
    k.cm .add_combo ( cg().k(Numrow_8 ).m(caps).s(msF),  expand_nav_tree );
    k.cm .add_combo ( cg().k(Slash    ).m(caps).s(msF),  collapse_nav_tree );

    k.cm .add_combo ( cg().k(LBracket).m(caps).s(msF),  caret_to_matching_brace.clone() );
    k.cm .add_combo ( cg().k(RBracket).m(caps).s(msF),  caret_to_matching_brace.clone() );

    k.cm .add_combo ( cg().k(LBracket).m(caps).s(msF_dbl),  caret_to_block_start );
    k.cm .add_combo ( cg().k(RBracket).m(caps).s(msF_dbl),  caret_to_block_end );

    k.cm .add_combo ( cg().k(LBracket).m(caps).s(msE),  sel_to_block_start );
    k.cm .add_combo ( cg().k(RBracket).m(caps).s(msE),  sel_to_block_end   );

    k.cm .add_combo ( cg().k(Equal).m(caps).s(msE),  expand_selection );
    k.cm .add_combo ( cg().k(Minus).m(caps).s(msE),  shrink_selection );

    k.cm .add_combo ( cg().k(N).m(caps).s(qks3),        duplicate_line.clone() );
    k.cm .add_combo ( cg().k(N).m(caps).s(msE),         duplicate_line.clone() );
    k.cm .add_combo ( cg().k(N).m(caps).s(msE).s(msR),  duplicate_line.clone() );

    k.cm .add_combo ( cg().k(I    ).m(caps).s(qks3),  move_line_up.clone() );
    k.cm .add_combo ( cg().k(Comma).m(caps).s(qks3),  move_line_dn.clone() );

    k.cm .add_combo ( cg().k(I    ).m(caps).s(msE).s(msR),  move_line_up );
    k.cm .add_combo ( cg().k(Comma).m(caps).s(msE).s(msR),  move_line_dn );

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
    //k.cm .add_combo ( cg().k(K).m(caps).m(lalt).c(intellij_fgnd()),  ag().k(End).m(alt) );
    // ^^ nah, that conflicts w the natural l2 alt-left, alt-right, which we also make use of in IDE already for last loc nav
    // at which point, we might as well use at least the alt-layered L for taking the whole multiline suggestion (via ctrl-alt-end)
    //k.cm .add_combo ( cg().k(L).m(caps).m(lalt).c(intellij_fgnd()),  ag().k(End).m(alt).m(ctrl) );
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

    k.cm .add_combo ( cg() .k(F2) .c(intellij_fgnd()),                   ag().af (line_to_repl) );
    k.cm .add_combo ( cg() .k(F2) .c(intellij_fgnd()) .m(caps).m(ralt),  ag().af (page_to_repl.clone()) );
    k.cm .add_combo ( cg() .k(I ) .c(intellij_fgnd()) .m(caps).s(qks ),  ag().af (page_to_repl) );
    k.cm .add_combo ( cg() .k(F ) .c(intellij_fgnd()) .m(caps).s(qks ),  ag().af (format_page) );

    // and caps-F2 will simply send selection to repl as is (w/o selecting full line etc)
    k.cm .add_combo ( cg() .k(F2) .c(intellij_fgnd()) .m(caps),  sel_send.clone() );


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

    k.cm .add_combo ( cg().k(L       ).s(qks).m(caps),  ag().af (lc_tool) );    // caps-q-l .. lc_tool
    k.cm .add_combo ( cg().k(Numrow_8).s(qks).m(caps),  ag().af (lc_tests) );   // caps-q-8 .. more lc test-cases

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
    k.cm .add_combo ( cg().k(Numrow_0).m(lalt)        .c(intellij_fgnd()),  ag().af (Arc::new (ide_float_tools_toggle)) );
    k.cm .add_combo ( cg().k(Numrow_0).m(lalt).m(caps).c(intellij_fgnd()),  ag().af (Arc::new (ide_float_tools_clear )) );

}



fn setup_ditto_combos (k:KR) {

    // ditto alt invocation .. C-d-v .. (Alt-v ofc will work too)
    k.cm .add_combo ( cg().k(V).m(caps).s(msD),  ag().k(V).m(alt) );

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



fn setup_one_note_combos (k:KR) {
    // note that these ofc rely on the setup of note-note quick-access toolbar ..
    // .. where .. select-mode is pos-3 in toolbar, finger-draw 4, eraser-stroke 5, eraser-point 6, pens 7
    // further, it has its own logic that keeps toggling or reverting back pens and erasers etc .. so the most robust seems to be ..
    // - for pen, pick sel-mode, then write-mode .. seems to always revert to pen (even from eraser before)
    // - for eraser, just pick eraser

    // caps-q-dd -> draw .. caps-q-ee -> eraser  .. (no fscs)

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
    fn one_note_fgnd() -> ComboCond {
        win_evs_cond ( |wel| wel.fgnd_info.read().unwrap().exe == "ONENOTE.EXE" )
    }
    let pen = {
        let (sel_mode, draw_mode) = (sel_mode.clone(), draw_mode.clone());
        Arc::new ( move || { sel_mode(); s(20); draw_mode(); } )
    };
    k.cm .add_combo ( cg().k(E).m(caps).s(qks).s(msE_dbl) .c(one_note_fgnd()),   ag().af (eraser) );
    k.cm .add_combo ( cg().k(D).m(caps).s(qks).s(msD_dbl) .c(one_note_fgnd()),   ag().af (threaded (pen.clone())) );

}



fn setup_gaming_combos (k:KR) {

    // we'll put some actions on pointed windows on some latching-first-stroke combos
    fn s (ms:u64) { thread::sleep (Duration::from_millis(ms)); }
    fn gen_pointed_v2 (x:i32, y:i32, key:Option<Key>) -> AF {
        Arc::new ( move || {
            thread::spawn ( move || {
                MousePointer::move_abs(x,y);
                LeftButton.press_release();
                if let Some(key) = key { s(20); key.press_release(); s(5); key.press_release(); }
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

    // fsc : caps-qw-C -> og macro mode .. (latching)
    let fsc = FSC::GamingOG.ch();
    k.cm .register_combo_latching_first_stroke ( fsc,  cg() .k(C).no_rpt() .m(caps) .fsc(FSC::LatchInit.ch()) );

    k.cm .add_combo ( cg().k(Left  ).fsc(fsc).c(pc()),   ag().af (gen_pointed_v2 ( xo +     xd, y, Some(Escape))) );
    k.cm .add_combo ( cg().k(Right ).fsc(fsc).c(pc()),   ag().af (gen_pointed_v2 ( xo + 3 * xd, y, Some(Escape))) );
    k.cm .add_combo ( cg().k(Down  ).fsc(fsc).c(pc()),   ag().af (gen_pointed_v2 ( xo + 2 * xd, y, Some(Escape))) );
    //k.cm .add_combo ( cg().k(Slash ).fsc(fsc).c(pc()), ag().af (gen_pointed_v2 ( xo         , y, Some(Escape))) );
    k.cm .add_combo ( cg().k(Up    ).fsc(fsc).c(pc()),           ag().af ( pointed_3 ) );
    k.cm .add_combo ( cg().k(Left  ).fsc(fsc).c(pc()).m(caps),   ag().af (gen_pointed_v2 ( xo +     xd, y, None)) );
    k.cm .add_combo ( cg().k(Right ).fsc(fsc).c(pc()).m(caps),   ag().af (gen_pointed_v2 ( xo + 3 * xd, y, None)) );
    k.cm .add_combo ( cg().k(Down  ).fsc(fsc).c(pc()).m(caps),   ag().af (gen_pointed_v2 ( xo + 2 * xd, y, None)) );
    k.cm .add_combo ( cg().k(Slash ).fsc(fsc).c(pc()).m(caps),   ag().af (gen_pointed_v2 ( xo         , y, None)) );

    fn og_clear() -> AF { Arc::new ( || { thread::spawn ( || {
        for i in 0 .. 4 {
            MousePointer::move_abs (550 + 950*i, 790); s(30);
            LeftButton.press_release(); s(20);
        }
    } ); } ) }
    k.cm .add_combo ( cg().k(Up).fsc(fsc).c(pc()).m(caps),  ag().af (og_clear()) );

    let og_setup = Arc::new ( move || { thread::spawn ( move || {
        // assume four sized windows are up, move them to right loc, start em up, deblur,
        let _xd = 950;
        for _i in (0 .. 4).rev() {
            //MousePointer::move_abs (240, 200);         // home
            MousePointer::move_abs (600, 600); s(50);    // video
            LeftButton.press_release(); s(800);          //
            MousePointer::move_abs (80, 300); s(50);     // unblur
            LeftButton.press_release(); s(800); LeftButton.press_release(); s(50); s(20);
            //win_fgnd_move_to (_xd*_i, 0, 940, 2400); s(500);
            snap_closest_edge_side (k.ks, RectEdgeSide::Left ); s(10);
            snap_closest_edge_side (k.ks, RectEdgeSide::Right); s(10);
        }
    } ); } );

    let og_teardown = Arc::new ( || {  thread::spawn ( || {
        for i in 0 .. 4 {
            MousePointer::move_abs (950*i + 240, 180); s(50);       // home
            LeftButton.press_release(); s(500);
            ctrl_press_release(W); s(50);
        }
    } ); } );
    k.cm .add_combo ( cg().k(Insert).fsc(fsc).m(caps),  ag().af (og_setup) );
    k.cm .add_combo ( cg().k(Delete).fsc(fsc).m(caps),  ag().af (og_teardown) );

}




fn setup_quick_bar (k:KR) {

    // we want the invocation to be on seq .. lbtn-dn -> rbtn-dn -> lbtn-up ..
    // so we'll put a fsc on lbtn-dn when rbtn-dn .. (unless rbtn-switche etc active)
    // then we'll put another fsc on the lbtn-up while rbtn-dn and in the above fsc

    let (fsc_pre, fsc) = (FSC::QuickBarPre.ch(), FSC::QuickBar.ch());

    // first we'll set up the first-step (lbtn-down)
    let pre_cond : ComboCond = Arc::new ( |ks,_ev| {
        ks.mouse.lbtn.down.is_set() &&
            // hmm !check_switche_fgnd(wel) &&
            !ks.sticky_first_stroke.check_match(FSC::X2_Wheel.ch())
    } );
    let pre_cond = || pre_cond.clone();
    k.cm .register_combo_sticky_first_stroke ( fsc_pre,  cg().mbtn(RightButton) .c(pre_cond()) );


    // next, we can setup the second step (lbtn-dn -> rbtn-dn)
    let cond : ComboCond = Arc::new ( |ks,_ev| {
        ks.mouse.rbtn.down.is_set() && !ks.sticky_first_stroke.check_match(FSC::X2_Wheel.ch())
    } );
    let cond = || cond.clone();
    k.cm .register_combo_sticky_first_stroke ( fsc,  cg().mbtn(LeftButton).rel() .c(cond()) .fsc(fsc_pre) );


    // and we'll set the same trigger to also bring up the quick-bar
    let trigger_af = Arc::new ( move || {
        // since this rel will override the normal lbtn-rel, we'll do any mouse cleanup right here
        if k.ks.mouse.lbtn.active.is_set() {
            k.ks.mouse.lbtn.active.clear();
            LeftButton.release()
        }
        // then just popup the bar itself .. (and open it w/o the persist flag)
        if !k.qbar.is_visible() {
            k.qbar.show(false);
        }
        else if k.qbar.is_persistent() {
            // if it was already visible and persisting, we'll re-open at the new location, but w/o persist flag
            k.qbar.hide(true);   // force hide
            k.qbar.show(false);  // re-open/move but w/o persist flag (ofc can click on drag-spot to make it persist)
        }
    } );
    //k.cm .add_combo ( cg().mbtn(LeftButton).rel() .c(cond()) .fsc(fsc),  ag().af (trigger_af) );
    k.cm .add_combo ( cg().mbtn(LeftButton).rel() .c(cond()) .fsc(fsc_pre),  ag().af (trigger_af) );


    // and we'll setup kbd only combo to toggle quick-bar too .. leaves it persistent, mostly useful for testing
    // caps-caps-A --> bring up Action Grid
    k.cm.add_combo ( cg().k(A).m(caps_dbl),     ag().af (Arc::new (move || k.qbar.toggle())) );
    k.cm.add_combo ( cg().k(O).m(caps).s(msD),  ag().af (Arc::new (move || k.qbar.toggle())) );
    // ^^ we could have tried to have this also enter the fsc state .. (with a bit of manual toggle management here)
    // However, we've decided NOT to do that, as not doing so means the quick-bar persists over [caps/mod]-rel etc clearing fscs
    // Further, since we only care about wheel actions in this fsc, and w/o alt/ctrl, they work decently from regular fallback
    // (note that a few combos esp w rbtn, alt-tab etc wont work w qbar up .. but fail mostly harmlessly)


    // for cleanup, we'll setup the rbtn-rel to hide the quick-bar and clear out of the state and related flags etc
    let exit_af = Arc::new ( move || {
        k.ks.clear_cur_sticky_fsc();
        //k.qbar.hide(false);
        // ^^we'll let the fsc cleared event af do the hiding .. (which will respect the persist flag too)
        // the rest below are mostly just for safety .. clearing out what rbtn rel usually might have
        k.ks.mod_keys.lalt.ensure_inactive();
        k.ks.mod_keys.lctrl.ensure_inactive();
        k.ks.in_right_btn_scroll_state.clear();
        if k.ks.mouse.rbtn.active.is_set() { RightButton.release() }
        k.ks.mouse.rbtn.active.clear(); k.ks.mouse.rbtn.pending.clear();
    } );
    k.cm .add_combo ( cg().mbtn(RightButton).rel() .fsc(fsc),  ag().af (exit_af) );


    // and if we get kicked out of this state by anything else, we still want to hide the quick-bar
    let fsc_clear_af = Arc::new ( move || {
        k.qbar.hide(false);     // the bool param is the forced flag
        // ^^ (closing w/o force flag means it wont close if persist flag set, e.g by kbd invocation)
    } );
    k.cm.register_af_sticky_first_stroke_cleared (fsc, fsc_clear_af);


    // if there's a x2 click during this mode, we'll make it persistent .. i.e can release rbtn without qb closing
    // (the next rbtn-rel whether inside/outside will close it .. so will any caps/mod-rel etc that clears the fsc)
    let persist_af = Arc::new ( move || {
        k.qbar.show(true);              // the bool param is the persist flag
        k.ks.clear_cur_sticky_fsc();  // after that we can clear out fsc
    } );
    k.cm .add_combo ( cg().mbtn(X2Button) .fsc(fsc),  ag().af (persist_af) );


    // finally, we'll setup the wheels for this fsc by default here to just re-broadcast the wheels ..
    // (so that the bar can receive it and directly trigger actions from its ActionGrid itself)
    // (and explicitly adding this coz dont want the wheels to fall-through to some non-fsc mapping etc)
    k.cm .add_combo ( cg().whl().bkwd().fsc(fsc),  ag().whl().bkwd().mkg_nw() );
    k.cm .add_combo ( cg().whl().frwd().fsc(fsc),  ag().whl().frwd().mkg_nw() );

    // ugh, and to support our hacky way to drag qb (given doing drag while qb has focus is laggy) ..
    // .. we'll have to manage drag-state ourselves upon lbtn release
    let c_drag : ComboCond = Arc::new ( |_,_| k.qbar.is_drag_active() );
    let end_drag = Arc::new (move || {
        k.qbar.set_dragging(false);
        if k.ks.mouse.lbtn.active.is_set() { k.ks.mouse.lbtn.active.clear(); LeftButton.release() }
    } );
    k.cm .add_combo ( cg().mbtn(LeftButton).rel() .c(c_drag),  ag().af (end_drag) );

    // and now lets populate the qbar with our action-grid
    k.qbar .set_grid_provider_builder ( Box::new (qbar_grid::grid_provider_builder) );

    // and finally, we'll also set-up an action to update qbar on fgnd change events (pushed by win-events listener)
    // (note that combos dont make sense there, so just direct bindings .. meaning gotta be combined to one if need be))
    let fgnd_af = Arc::new ( move || k.qbar.handle_fgnd_change() );
    InputProcessor::instance().input_bindings.bind_internal_event (InternalEvent_T::Fgnd_Changed, EvCbEntry {
        ev_proc_ds: EvProc_Ds::new (EvProp_D::EvProp_Stop, ComboProc_D::ComboProc_Disable),
        cb : EvCbFn_T::EvCbFn_Queued ( Arc::new ( move |_| fgnd_af() ) ),
    } );

}






/// setup for the entire krusty-board application, incl setting up key/btn bindings and combos
pub fn setup_krusty_board (k:KR) {

    //let k = Krusty::new();

    // setup all the mod-keys .. (can override this with own setup if desired)
    k.ks.mod_keys.setup_tracking(k);

    // mouse setup incl lbtn/rbtn/mbtn/x1btn/x2btn and the scroll wheels
    k.ks.mouse.setup_mouse(k);



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
    setup_default_keys (k);

    // caps-dbl-Insert --> unstick all
    setup_unstick_all (k);

    // [E,D,F,R,Q,1,2,3] as mode-keys
    setup_mode_keys (k);

    // caps-q-w (rolling only due to 2wsx) --> first-step before common latch-fsc declarations
    setup_latch_init_sfsc (k);

    // [caps-dbl-F12, caps-dbl-Esc, alt-dbl-Esc, caps-dbl-e-o] --> clear-latching-first-stroke
    setup_latching_first_stroke_clear (k);


    setup_caps_as_shift_mappings (k);

    disable_win_num_combos (k);

    setup_caps_dbl_combos (k);


    // [J,K,I,Comma,U,M,H,L] as l2 keys that are modified under caps, mode-keys etc
    setup_l2 (k);


    // mouse, wheel combos .. (though there are others in tsc or app-specific sections too)

    setup_mouse_left_btn (k);

    setup_mouse_right_btn (k);

    setup_middle_and_xbtn_combos (k);

    setup_vert_wheel (k);

    setup_horiz_wheel (k);


    // some specific keys and combo-patterns

    setup_win_key_combos (k);

    setup_brightness_vol_media (k);

    setup_win_groups (k);

    setup_caps_2wsx_combos (k);

    setup_space_key (k);

    setup_misc_standalone_combos (k);


    // setups for switching windows via various mechanisms (incl via switche)

    setup_switche_alt_tab (k);

    setup_switch_windows_w_caps_sfsc (k);

    setup_switch_windows_rbtn_scroll (k);

    setup_switch_windows_blind_sfsc (k);

    setup_switch_windows_direct_sfsc (k);


    // and for switching desktop
    setup_switch_desktop_sfsc (k);


    // and various ways for switching tabs
    setup_tab_nav_sfsc (k);

    setup_ctrl_tab_sfsc (k);

    setup_caps_rbtn_mbtn_ctrl_tab (k);


    // and some two-stroke combos with sticky or latching first-strokes (sfsc/lfsc)
    // (note that there are also other latch setups in sections for media, gaming etc)

    setup_window_action_sfsc (k);

    setup_kbd_pointer_sfsc (k);

    setup_wheel_arrows_lfsc (k);

    setup_ide_diff_nav_lfsc (k);


    // gaming and app specific combo setups (many with sticky/latching fscs)

    setup_IDE_combos (k);

    setup_ditto_combos (k);

    setup_one_note_combos (k);

    setup_gaming_combos (k);


    // and finally we have the quick-bar with its ui
    setup_quick_bar (k);




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
    k.ks.mode_states.bind_mode_keys_actions(k);

    //k.cm.debug_print_combos_map();
    k.cm.info_print_simult_active_combos_check();



    // and we'll put any direct special key setups after all this
    // .. which is for safety in case anything above accidentally included those, although ofc we dont want to rely on that!
    setup_direct_binding_keys (k);

    // and we'll give a lil indicator for when we restart etc
    blip_cursor(3);

}




pub fn main () {

    // setup the whole krusty keyboard configuration
    let k = Krusty::instance();

    // we'll enable cursor color swaps for visual indication
    k.cursors.set_swaps_enabled(true);

    setup_krusty_board(k);

    // start up the quick_bar ui if configured
    k.qbar.start();

    // start the windows-events listener
    k.wel.setup_win_event_hooks();

    // then start handling inputs
    k.iproc.begin_input_processing();

    // and finally start the system tray monitor event-loop .. (which will NOT return)
    start_system_tray_monitor();

}
