

use std::{ time, thread, sync::Arc, };
use windows::core::{HSTRING, PCWSTR};
use windows::Win32::UI::WindowsAndMessaging::{FindWindowW, GetForegroundWindow, SetForegroundWindow};


use crate::{*, KbdKey::*, key_utils::*, ModKey::*, ModeState_T::*};





/// handling for any 'special' keys that need to be bound/handled directly (like for mouse btns) rather than via combo-maps
pub mod special_keys_setups {

    use crate::Krusty;

    pub fn setup_direct_binding_keys (_k:&Krusty) {

        // currently, we dont need to do it for anything .. (was more essential when mod-tracking / combo-maps mechanisms were more limited)

        // HOWEVER, there might be cases where it might be simpler to just do direct bindings than to populate all the combo maps etc etc
        // ALSO, for things that require actual action in key-up callbacks, we'd do it direclty here because key-dn actions arent registered
        // > specifically in maps and so combo-maps composed callbacks can only either set key-up to do nothing, or at most clear mode flags

        // IMPORTANT : note that whatever we put here, we will NOT want to include them in ANY action maps (or overwrite w these at the end) !!

    }

}





/// setup for the entire krusty-board application, incl setting up key/btn bindings and combos
pub fn setup_krusty_board () {

    // lets import various windows/process/brightness util fns
    use crate::utils::{windows_utils::*, process_utils::*, /*brightness_utils::*,*/};
    // note .. module krusty_mouse has a minimal wrapper around brightness control, we'll use that instead


    let k = Krusty::new();

    // setup all the mod-keys .. (can override this with own setup if desired)
    k.ks.mod_keys.setup_tracking(&k);

    // mouse setup incl lbtn/rbtn/mbtn/x1btn/x2btn and the scroll wheels
    k.ks.mouse.setup_mouse(&k);



    // we'll setup most keys via key-combo action maps that we'll compose into relevant callbacks after all mapping is registered
    // HOWEVER, there are some keys (incl those that look for shift/ctrl) that will be set directly at the end after all the action-map setups

    // in addition, we'll want to bind MOST keys so default actions for things like ralt or caps combos are generated for them even if we
    // >  dont have any special combos to setup for them .. to keep combos tables light, we'll register everything there first

    // NOTE that if we wanted special-keys-setups (which we dont currently for any key), we should do that after all binding is done so as to
    // >  overwrite what default bindings would otherwise be generated .. (we currently invoke that at the end, even though its not yet needed)

    // NOTE that fallback defaults are : ralt-as-shift, caps-as-ctrl, caps-ralt/shift/alt/win as ctrl-shift/shift/alt/win

    let char_keys: Vec<Key> = "qwertasdfgzxcvb`123456yuiop[]\\hjkl;\'nm,./7890-=" .chars() .map (|c| Key::from_char(c)) .flatten() .collect();
    let fnum_keys: Vec<Key> = (u64::from(F1) .. u64::from(F24)) .map (|v| Key::from(v)) .collect();
    let nav_keys   = vec![Left, Right, Up, Down, PageUp, PageDown, Home, End];
    let spcl_keys  = vec![Backspace, Delete, Space, Tab, Enter, Escape, Insert, Apps];
    //let media_keys = vec![BrowserBack, BrowserForward, BrowserRefresh, VolumeMute, VolumeDown, VolumeUp,
    //                      MediaNextTrack, MediaPrevTrack, MediaStop, MediaPlayPause];

    vec![char_keys, fnum_keys, nav_keys, spcl_keys] .concat() .into_iter() .for_each ( |key| {
        k.cm .register_default_binding_key (key);
    } );
    // ^^ we can ofc put combos for these later in code .. all these do is register for default binding if no combo gets mapped!



    // we'll start with some caps-atypical caps-as-shift mappings, basically nums or kbd-right symbols not otherwise involved in l2
    "1234567890-=[]\\;\'/." .chars() .for_each ( |c| {
        Key::from_char(c) .into_iter() .for_each ( |key|
            k.cm .add_combo ( k.cg(key).m(caps),  k.ag(key).m(lshift) )
        )
    } );



    // lets also disable the win-number combos as they annoyingly activate/minimize items from taskbar etc
    "1234567890" .chars() .map (|c| Key::from_char(c)) .flatten() .for_each ( |key| {
        k.cm .add_combo_af ( k.cg(key).m(lwin),                   k.ag_af(no_action()) );
        k.cm .add_combo_af ( k.cg(key).m(lwin).m(caps),           k.ag_af(no_action()) );
        k.cm .add_combo_af ( k.cg(key).m(lwin).m(lalt),           k.ag_af(no_action()) );
        k.cm .add_combo_af ( k.cg(key).m(lwin).m(ralt),           k.ag_af(no_action()) );
        k.cm .add_combo_af ( k.cg(key).m(lwin).m(caps).m(lalt),   k.ag_af(no_action()) );
        k.cm .add_combo_af ( k.cg(key).m(lwin).m(caps).m(ralt),   k.ag_af(no_action()) );
        k.cm .add_combo_af ( k.cg(key).m(lwin).m(caps).m(shift),  k.ag_af(no_action()) );
    } ); // some of ^^ these will get overwritten by specific win-combos added later .. which is fine
    // note that at least for now, we're choosing to ignore caps-shift, caps-ctrl etc combos, though ofc could impl if need arises

    // we'll disable win-d too, as I never use that show/hide desktop and it's disruptive
    k.cm .add_combo_af ( k.cg(D).m(lwin),  k.ag_af(no_action()) );



    // we'll set up a combo (caps-alt-Insert) to unstick everything in case we get into weird states due to other hooks stealing/suppressing key events etc
    // note since the whole point is to invoke it at stuck times, we'd want to overload that also for alt/ctr/shift etc and their combinations!
    // .. otoh, they can all be unstuck by just press/releasing them, incl mouse btns, so for now we'll ignore that, can update as necessary
    // further, combos incl all mode states and some other flags .. for a combo to trigger those would have to match too ..
    // soo .. we'll let these simple combo be, but it will mostly only be useful to clear mouse-btn states .. or single mod-key sticking
    let ks = k.ks.clone(); let clear = Arc::new (move || ks.unstick_all());
    k.cm .add_combo_af ( k.cg(Insert).m(caps).m(lalt),          k.ag_af (clear.clone()) );
    k.cm .add_combo_af ( k.cg(Insert).m(caps).m(lalt).m(ctrl),  k.ag_af (clear.clone()) );
    k.cm .add_combo_af ( k.cg(Insert).m(caps).m(lalt).m(win ),  k.ag_af (clear.clone()) );



    fn register_mode_key (k:&Krusty, key:Key, ms_t:ModeState_T) {
        // first we'll do the registration, then we can try and set any auxillary combos here too
        // note that mode-keys down flag will track its physical state, but combo trigger on mode-state requires caps to be down too
        k.ks.mode_states .register_mode_key (key, ms_t);

        // since pressing mode-keys sets their flags first, we want to map their own presses w their own flags back to base-action
        // (note the mode-state-kdn_no-consume (msk_nc) specified below so we're not disabling key-repeats for base action)
        k.cm .add_combo ( k.cg(key).s(ms_t).msk_nc(),  k.ag(key).mkg_nw() );
        // ^^ the modkey-guard-no-wrap is specified to make it explicit, but isnt strictly necessary here as there are no mod-keys when this triggers

        // to avoid stragglers, we'll set the mode-keys pressed w caps to disable their repeat until release (ie. even after caps is released!)
        // note that the following works because any mode-state specified in combo-gen is auto marked for consumption (unless do .msk_nc())
        k.cm .add_combo_af ( k.cg(key).m(caps).s(ms_t),  k.ag_af(no_action()).mkg_w() );

        // and we could do the same for alt/win etc, but we'd rather leave those open and they can be done later when/if such combos are set
        // (this allows mod-key combos for mode-keys, and we can disable it only for specific cases like qks1 which modifies other mode-state-keys)
        //k.cm .add_combo_af ( k.cg(key).m(lalt).s(ms_t),  k.ag_af(no_action()).mkg_w() );
        //k.cm .add_combo_af ( k.cg(key).m(lwin).s(ms_t),  k.ag_af(no_action()).mkg_w() );

        // note that we want to disable mode-keys across most mod-key combos when caps down ..
        // .. and thats painful to do via combos-maps, so we're now instead just disabling them in runtime fallbacks
        // further, in fallback, we'll also layer base action w mod-keys for these mode-trigger-keys when qks1 down!

        // however, we could at least restore caps-alt-<mode-key> to the expected ctrl-alt by default .. can ofc override these later
        //k.cm .add_combo ( k.cg(key).m(caps).s(ms_t).m(lalt),  k.ag(key).m(ctrl).m(alt) );
        // ^^ naah, some of these we need to be silent and modify other combos (e.g. qks1), so lets do them individually later
    }

    // setup keys for layer-2 caret nav sel/del/fast modes
    // note: registering as mode key will set all w/caps actions silent in fallback, along w layering mod-key combos w qks1
    // however, they are all in fallback only, so that behavior will be overridden by any combo registrations!
    register_mode_key ( &k, E, sel  );
    register_mode_key ( &k, D, del  );
    register_mode_key ( &k, F, word );
    register_mode_key ( &k, R, fast );

    // setup the key for l4 shortcuts mode, but the mechanism is same as for the caret modes
    register_mode_key ( &k, Q,        qks  );
    register_mode_key ( &k, Numrow_1, qks1 );
    register_mode_key ( &k, Numrow_2, qks2 );
    register_mode_key ( &k, Numrow_3, qks3 );



    // we want to overlay some additional combos on some of these w Alt (other that modify other combos should remain silent)
    k.cm .add_combo ( k.cg(E).m(caps).s(sel ).m(lalt),   k.ag(E).m(ctrl).m(alt) );
    k.cm .add_combo ( k.cg(D).m(caps).s(del ).m(lalt),   k.ag(D).m(ctrl).m(alt) );
    //k.cm .add_combo ( k.cg(F).m(caps).s(word).m(lalt),   k.ag(F).m(ctrl).m(alt) );   // overriden below
    k.cm .add_combo ( k.cg(R).m(caps).s(fast).m(lalt),   k.ag(R).m(ctrl).m(alt) );

    // since F is in caret mode, we'll remap some of the other combos to replace ctr-f etc
    k.cm .add_combo ( k.cg(F).m(lalt),                  k.ag(F).m(ctrl) );     // alt-f to ctrl-f
    k.cm .add_combo ( k.cg(F).m(caps).s(word).m(lalt),  k.ag(F).m(lalt) );     // caps-lalt-f to alt-f, though it goes against typical mode-key usage

    // e in caret mode, so we'll put our left-handed-enter on alt-e instead .. (note that there are also caps-space-* combos for *-enter)
    k.cm .add_combo ( k.cg(E).m(lalt),  k.ag(Enter) );      // alt-e -> enter






    // setup backquote .. make normal case be Delete, caps or alt do back-tick, and shift or ralt do its tilde
    k.cm .add_combo ( k.cg(Backquote),          k.ag(ExtDelete) );
    k.cm .add_combo ( k.cg(Backquote).m(caps),  k.ag(Backquote) );
    k.cm .add_combo ( k.cg(Backquote).m(lalt),  k.ag(Backquote) );
    //k.cm .add_combo ( k.cg(Backquote).m(shift),   k.ag(Backquote).m(shift) );
    //k.cm .add_combo ( k.cg(Backquote).m(ralt),    k.ag(Backquote).m(shift) );
    // ^^ not strictly necessary as cb composition now defaults to this, but also useful to see here for reference


    // setup tab .. caps-as-ctrl for caps-tab switching, incl for ctrl-shift-tab .. also ralt-tab for shift-tab
    let ks = k.ks.clone();
    let cb : AF = Arc::new (move || {
        if ks.mod_keys.caps.down.check() || ks.mod_keys.some_ctrl_down() {
            if ks.mod_keys.caps.down.check() {
                ks.in_managed_ctrl_down_state.set();
                ks.mod_keys.lctrl.ensure_active();  // this enables caps-as-ctrl for caps-tab switching
                // ^^ we're not gonna release ctrl immediately, but keep track and release when caps is released
            }
            ks.in_ctrl_tab_scroll_state.set();
            press_release(Tab)
        }
    } );
    // lets add support for this in caps tab, incl for various flags that might be active (since they are now in combo bits)
    k.cm .add_combo_af ( k.cg(Tab).m(caps),                                   k.ag_af(cb.clone()) );
    k.cm .add_combo_af ( k.cg(Tab).m(caps).s(mngd_ctrl_dn),                   k.ag_af(cb.clone()) );
    k.cm .add_combo_af ( k.cg(Tab).m(caps).s(mngd_ctrl_dn).s(ctrl_tab_scrl),  k.ag_af(cb.clone()) );

    k.cm .add_combo_af ( k.cg(Tab).m(ctrl),                                    k.ag_af(cb.clone()) );
    k.cm .add_combo_af ( k.cg(Tab).m(ctrl).s(mngd_ctrl_dn ),                   k.ag_af(cb.clone()) );
    k.cm .add_combo_af ( k.cg(Tab).m(ctrl).s(ctrl_tab_scrl),                   k.ag_af(cb.clone()) );
    k.cm .add_combo_af ( k.cg(Tab).m(ctrl).s(mngd_ctrl_dn ).s(ctrl_tab_scrl),  k.ag_af(cb.clone()) );

    k.cm .add_combo_af ( k.cg(Tab).m(caps).m(shift),                                   k.ag_af(cb.clone()).m(shift) );
    k.cm .add_combo_af ( k.cg(Tab).m(caps).m(shift).s(mngd_ctrl_dn),                   k.ag_af(cb.clone()).m(shift) );
    k.cm .add_combo_af ( k.cg(Tab).m(caps).m(shift).s(mngd_ctrl_dn).s(ctrl_tab_scrl),  k.ag_af(cb.clone()).m(shift) );

    // lets explicitly support arrow keys during ctrl tab (coz fallback will send full ctrl-action as it doesnt check act/inact guards)
    k.cm .add_combo ( k.cg(Left ).m(caps).s(mngd_ctrl_dn),  k.ag(Left ).mkg_nw() );
    k.cm .add_combo ( k.cg(Right).m(caps).s(mngd_ctrl_dn),  k.ag(Right).mkg_nw() );
    k.cm .add_combo ( k.cg(Up   ).m(caps).s(mngd_ctrl_dn),  k.ag(Up   ).mkg_nw() );
    k.cm .add_combo ( k.cg(Down ).m(caps).s(mngd_ctrl_dn),  k.ag(Down ).mkg_nw() );

    k.cm .add_combo ( k.cg(Left ).m(caps).s(mngd_ctrl_dn).s(ctrl_tab_scrl),  k.ag(Left ).mkg_nw() );
    k.cm .add_combo ( k.cg(Right).m(caps).s(mngd_ctrl_dn).s(ctrl_tab_scrl),  k.ag(Right).mkg_nw() );
    k.cm .add_combo ( k.cg(Up   ).m(caps).s(mngd_ctrl_dn).s(ctrl_tab_scrl),  k.ag(Up   ).mkg_nw() );
    k.cm .add_combo ( k.cg(Down ).m(caps).s(mngd_ctrl_dn).s(ctrl_tab_scrl),  k.ag(Down ).mkg_nw() );

    // and finally for ralt-as-shift support for caps-tabbing too
    k.cm .add_combo_af ( k.cg(Tab).m(caps).m(ralt),                                   k.ag_af(cb.clone()).m(shift) );
    k.cm .add_combo_af ( k.cg(Tab).m(caps).m(ralt).s(mngd_ctrl_dn),                   k.ag_af(cb.clone()).m(shift) );
    k.cm .add_combo_af ( k.cg(Tab).m(caps).m(ralt).s(mngd_ctrl_dn).s(ctrl_tab_scrl),  k.ag_af(cb.clone()).m(shift) );



    // aight, and this is prob excessive, but specifically for IntelliJ, wanted to add a quick switch from tab-switcher to searchable one
    // (we'll do it by escaping it first (via space then ctrl rel), then invoking the searchable switcher)
    fn gen_ide_switcher_switch_af (k:&Krusty, key:Key) -> AF {
        let ks = k.ks.clone();
        Arc::new ( move || {
            if get_fgnd_win_exe().filter(|s| s == "idea64.exe").is_some() {
                ks.mod_keys.lctrl.ensure_inactive(); ks.in_managed_ctrl_down_state.clear(); ks.in_ctrl_tab_scroll_state.clear();
                // space defocuses from list so we wont actually switch tabs when we release the ctrl
                press_release(Space);
                // then do actual ctrl-e to bring up the persistent-switcher (ctrl-e is default shortcut in IDE for that)
                ks.mod_keys.lctrl.active_on_key(E)()
            } else { press_release(key) }
        } )
    }
    k.cm .add_combo_af ( k.cg(Space).m(caps).s(mngd_ctrl_dn),  k.ag_af(gen_ide_switcher_switch_af(&k, Space)) );




    // setup space key .. ralt-space as enter, caps-space as ctrl-space, caps-lalt-space as alt-enter for intellij
    k.cm .add_combo ( k.cg(Space).m(ralt),          k.ag(Enter) );                  // ralt-space -> enter
    k.cm .add_combo ( k.cg(Space).m(caps).m(lalt),  k.ag(Enter).m(lalt) );          // caps-lalt-space -> alt-enter
    k.cm .add_combo ( k.cg(Space).m(caps).m(ralt),  k.ag(Escape) );                 // caps-ralt-space -> Escape
    k.cm .add_combo ( k.cg(Space).m(caps).s(qks1),  k.ag(Space).m(ctrl).m(shift) ); // qks1-space -> ctrl_shift_space for IDE
    k.cm .add_combo ( k.cg(Space).m(caps).s(qks2),  k.ag(Space).m(ctrl).m(shift) ); // ^^ ditto for qks2
    //k.cm .add_combo ( k.cg(Space).m(caps),          k.cg(Space).m(ctrl) );   // caps-space -> ctrl-enter
    // ^^ not strictly necessary as cb composition now defaults to this, but also useful to see here for reference



    // win-m by default minimized all windows .. we just want to disable it
    k.cm .add_combo_af ( k.cg(M).m(lwin),  k.ag_af(no_action()) );

    // win-i should start irfanview
    k.cm .add_combo_af ( k.cg(I).m(lwin),  k.ag_af(action(start_irfanview)) );

    // win-n should start chrome-incognito
    k.cm .add_combo_af ( k.cg(N).m(lwin),  k.ag_af(action(start_chrome_incognito)) );

    // we'll setup win-C to quickly bring up chrome Tabs-Outliner via switche Ctrl-Alt-F20 hotkey
    k.cm .add_combo ( k.cg(C).m(lwin),  k.ag(F20).m(alt).m(lctrl) );      // win-c -> ctrl-F20 .. switche tabs-outliner


    // in cur laptop, Fn-F6/F7 do brightness, but at +10 incrs .. set them to do small incrs with alt combos
    k.cm .add_combo_af ( k.cg(F6).m(lalt),  k.ag_af (Arc::new (|| incr_brightness(-1))) );
    k.cm .add_combo_af ( k.cg(F7).m(lalt),  k.ag_af (Arc::new (|| incr_brightness(1))) );

    // actually, since we use win-1/2/3 as vol mute/down/up, might as well also set alt-1/2/3 for brightness zero/down/up
    // .. and we'll set these to have a 'fast-mode' when qks-1 key is held with alt
    // (note that numrow 1/2/3 with caps are qks* keys, so they cant be used with any caps combos as those would be silent!)
    // (also note that e.g. by explicitly specifying qks-x for Numrow_x, it will be marked for consumption etc (i.e. suppresses key events until released))
    k.cm .add_combo_af ( k.cg(Numrow_1).m(lalt).s(qks1),  k.ag_af (no_action()) );
    k.cm .add_combo_af ( k.cg(Numrow_1).m(lalt).s(qks2),  k.ag_af (no_action()) );
    k.cm .add_combo_af ( k.cg(Numrow_1).m(lalt).s(qks3),  k.ag_af (no_action()) );
    k.cm .add_combo_af ( k.cg(Numrow_2).m(lalt),          k.ag_af (Arc::new (|| incr_brightness(-1))) );
    k.cm .add_combo_af ( k.cg(Numrow_3).m(lalt),          k.ag_af (Arc::new (|| incr_brightness( 1))) );
    k.cm .add_combo_af ( k.cg(Numrow_2).m(lalt).s(qks1),  k.ag_af (Arc::new (|| incr_brightness(-5))) );
    k.cm .add_combo_af ( k.cg(Numrow_3).m(lalt).s(qks1),  k.ag_af (Arc::new (|| incr_brightness( 5))) );

    // win-2 is vol down, win-3 is vol up, win-1 can do fast-mode
    k.cm .add_combo_af ( k.cg(Numrow_1).m(lwin).s(qks1),  k.ag_af (no_action()) );
    k.cm .add_combo_af ( k.cg(Numrow_1).m(lwin).s(qks2),  k.ag_af (no_action()) );
    k.cm .add_combo_af ( k.cg(Numrow_1).m(lwin).s(qks3),  k.ag_af (no_action()) );
    k.cm .add_combo_af ( k.cg(Numrow_2).m(lwin),          k.ag_af (Arc::new (|| incr_volume(-1))) );
    k.cm .add_combo_af ( k.cg(Numrow_3).m(lwin),          k.ag_af (Arc::new (|| incr_volume( 1))) );
    k.cm .add_combo_af ( k.cg(Numrow_2).m(lwin).s(qks1),  k.ag_af (Arc::new (|| incr_volume(-2))) );
    k.cm .add_combo_af ( k.cg(Numrow_3).m(lwin).s(qks1),  k.ag_af (Arc::new (|| incr_volume( 2))) );


    // win-f1 play/pause, caps-f1 toggle mute, base-case: switche-invoke alt-F1: switche silent-switch, ralt for actual F1
    k.cm .add_combo ( k.cg(F1),          k.ag(F16) );                   // switche next
    //k.cm .add_combo ( k.cg(F1).m(shift), k.ag(F17) );                 // switche prev
    k.cm .add_combo ( k.cg(F1).m(shift), k.ag(F16).m(shift) );          // switche prev (passthrough shift-F16 instead of F17 is more efficient)
    k.cm .add_combo ( k.cg(F1).m(lalt),  k.ag(F19).m(alt).m(ctrl) );    // switche no-popup next switch
    k.cm .add_combo ( k.cg(F1).m(ralt),  k.ag(F1) );
    k.cm .add_combo ( k.cg(F1).m(caps),  k.ag(VolumeMute) );
    k.cm .add_combo ( k.cg(F1).m(lwin),  k.ag(MediaPlayPause) );
    // and keeping w the theme, set caps-win-F1 (key with vol-mute printed on it) to toggle microphone mute
    k.cm .add_combo_af ( k.cg(F1).m(caps).m(lwin),  k.ag_af (Arc::new (|| {mic_mute_toggle(); open_mic_cpl();})) );
    // we'll set Alt-F2 to bring chrome tabs-outliner (via switche) to keep w the theme of Alt-F<n> keys for task switching
    k.cm .add_combo ( k.cg(F2).m(lalt),  k.ag(F20).m(alt).m(ctrl) );     // switche no-popup tabs-outliner switch

    // we'll overload regular F2 with special IDE scala-console action if IDEA window is foreground
    fn gen_line_to_repl_action (k:&Krusty) -> AF {
        // note that its better not to specify no-modkey-guard-wrap below in case the action moves from F2 to some combo with a mod-key
        let line_sel         = k.ag(Home).m(alt).m(shift).gen_af();  // IDE alt-shift-home to sel line
        let line_repl_send   = k.ag(End ).m(alt).m(shift).gen_af();  // IDE alt-shift-end to send sel to repl
        let caret_sel_start  = k.ag(ExtLeft).gen_af();               // unselect the line (caret to line beginning)
        let caret_line_start = k.ag(ExtHome).gen_af();               // caret to beginning of first word in line
        Arc::new ( move || { line_sel(); line_repl_send(); caret_sel_start(); caret_line_start() } )
    }
    fn gen_f2_action(k:&Krusty) -> AF {
        let af = gen_line_to_repl_action(k);
        Arc::new ( move || {
            if get_fgnd_win_exe().filter(|s| s == "idea64.exe").is_some() { af() }
            else { press_release(F2) }
        } )
    }
    k.cm .add_combo_af ( k.cg(F2),  k.ag_af(gen_f2_action(&k)) );

    // and caps-F2 will simply send selection to repl as is (w/o selecting full line etc)
    k.cm .add_combo ( k.cg(F2).m(caps),  k.ag(End).m(alt).m(shift) );

    // want win-f2 for next with some initial skip .. we'll use caps-win-f2 for prev, so we'll set it up for both
    // note that our mechanism for wrapping mod-key-state restoring guards operates via AFs, hence setting those up (instead of fns)

    // skips work by alt-ctrl-volUp (needs to guard win-inactive since its on win-combo)
    fn media_skips_action (n_skips:u32, ks:&KrustyState, fwd_not_bkwd:bool) -> AF {
        let action_key = if fwd_not_bkwd {VolumeUp} else {VolumeDown};
        ks.mod_keys.lwin.inactive_action ( ks.mod_keys.lalt.active_action ( ks.mod_keys.lctrl.active_action (
            Arc::new ( move || { (0 .. n_skips) .into_iter() .for_each (|_| { press_release(action_key) }) } )
    ) ) ) }
    // ^^ gives an AF with specified number of skips

    // it uses alt-active media-skips, so we'll need alt_inactive-action, plus guard on win-combo it is on
    let ks = k.ks.clone();
    let media_next_action = k.ks.mod_keys.lwin.inactive_action ( k.ks.mod_keys.lalt.inactive_action ( Arc::new ( move || {
        if !ks.mod_keys.caps.down.check() { press_release(MediaNextTrack) }
        else { press_release(MediaPrevTrack) }
        let ks = ks.clone();  // clone again to move into spawned thread (spawned since combos run in single queued side-thread)
        thread::spawn ( move || { thread::sleep(time::Duration::from_millis(2000));  media_skips_action(3,&ks,true)(); } );
    } ) ) );

    // win-f2 for next with some initial skip
    k.cm .add_combo_af ( k.cg(F2).m(lwin),          k.ag_af(media_next_action.clone()) );
    k.cm .add_combo_af ( k.cg(F2).m(lwin).m(caps),  k.ag_af(media_next_action) );

    // win-f3 for skip forward a bit (w/ caps for rewind)
    k.cm .add_combo_af ( k.cg(F3).m(lwin),           k.ag_af (media_skips_action(1, &k.ks, true)) );
    k.cm .add_combo_af ( k.cg(F3).m(lwin).m(caps),   k.ag_af (media_skips_action(2, &k.ks, false)) );
    k.cm .add_combo_af ( k.cg(F3).m(lwin).m(shift),  k.ag_af (media_skips_action(2, &k.ks, false)) );



    // escape is just escape, but we want it to do press-release immediately (so switche is faster)
    // (incl separately for various flag states that go into combo-bits)
    k.cm .add_combo ( k.cg(Escape),                                          k.ag(Escape) );
    k.cm .add_combo ( k.cg(Escape).m(caps),                                  k.ag(Escape) );
    k.cm .add_combo ( k.cg(Escape).m(caps).s(mngd_ctrl_dn),                  k.ag(Escape) );
    k.cm .add_combo ( k.cg(Escape).m(caps).s(mngd_ctrl_dn).s(ctrl_tab_scrl), k.ag(Escape) );

    // use the apps key to send shift-escape ..
    k.cm .add_combo ( k.cg(Apps),            k.ag(Escape).m(shift) );


    // we have win-mouse window drag/resize .. we'd like to cancel any in-progress action via escape
    fn gen_cancel_win_mouse_action (key:Key, ks:&KrustyState) -> AF {
        let ks = ks.clone();
        Arc::new ( move || {
            if ks.mouse.lbtn.down.is_set() {  //press_release(Z);
                ks.mouse.lbtn.consumed.set();
                handle_pointer_action_cancel (&ks);
            } else { press_release(key) }
        } )
    }
    // we'll allow Escape to cancel in-progress win-drag-to-move/resize operations
    k.cm .add_combo_af ( k.cg(Escape).m(lwin),          k.ag_af (gen_cancel_win_mouse_action (Escape, &k.ks)) );
    k.cm .add_combo_af ( k.cg(Escape).m(lwin).m(caps),  k.ag_af (gen_cancel_win_mouse_action (Escape, &k.ks)) );
    // and since Esc is hard to press w caps-win, we'll let Q do the same too
    k.cm .add_combo_af ( k.cg(Q).m(lwin),                 k.ag_af (gen_cancel_win_mouse_action (Q, &k.ks)) );
    k.cm .add_combo_af ( k.cg(Q).m(lwin).m(caps).s(qks),  k.ag_af (gen_cancel_win_mouse_action (Q, &k.ks)) );




    // caps-lalt-F for full screen ..(but from bulk mapping above, caps-f should still give ctrl-f)
    //k.cm .add_combo (k.cg(F).m(caps).m(lalt), k.cg(F11));
    // ^^ no longer available as we made F a caret mode key, and set caps-alt-F to alt-F instead

    // 'w' should have caps-ctrl mapping, but when w/ alt, send alt-f4 (to close all-tabs, windows etc)
    //k.cm .add_combo ( k.cg(W).m(caps).m(shift), k.cg(F4).m(alt) );
    // ^^ note: initially we wanted this with caps-shift-w, but turns out (at least on my kbd, turns out caps+shift+[F1, 2, w, s, x]
    // >  dont produce any key event at the hook at all .. nothing .. its like the keyboard driver not sending those out
    // funnily enough, there's a bunch of complaints about specifically those keys for dell/hp laptops .. looks like hardware
    // >  appears to be a common kbd pcb layout issue .. heres from 2007: (https://www.joachim-breitner.de/blog/250-Shift-Caps-2)
    // sooo .. to makeup, we'll do alt-caps-w do the alt-f4 business instead
    k.cm .add_combo ( k.cg(W).m(caps).m(lalt),  k.ag(F4).m(lalt) );




    // filling out l2 actions (incl w caps-alt combos)
    /* l2-setup config summary:
     - only j/k for left/right get f-for-word-nav mode speedup (native word nav by sending ctrl)
     - those and i/comma for up/down get r-for-double-speed nav mode (2x nav) .. i/comma get that for f too
     - h/l/u/m for home/end/pgup/pgdown get no speedup modes
     - e/d do sel/del modes, and those can be freely combined with the f/r word/fast nav modes
     - in del mode, left/home/up/pgup get ExtBackspace, right/end/down/pgdn get ExtDelete
     - in del mode, left/right do direct bksp/del, but others get select then bksp/del
     */

    /// action-fn generator type (e.g. to specify for various sel/del/word/fast modes for various l2-keys)
    type AFG = fn(Key) -> AF ;

    fn setup_l2_key (k:&Krusty, key:Key, l2k:Key, dk:Key, wafg:AFG, fafg:AFG, del_via_sel:bool) {
        // register nav actions for normal-nav, word-nav, and fast-nav modes
        k.cm .add_combo_af ( k.cg(key).m(caps),          k.ag_af (base_action(l2k)) );
        k.cm .add_combo_af ( k.cg(key).m(caps).s(word),  k.ag_af (wafg(l2k)) );
        k.cm .add_combo_af ( k.cg(key).m(caps).s(fast),  k.ag_af (fafg(l2k)) );

        // selection actions are via wrapping those with shift press-release
        k.cm .add_combo_af ( k.cg(key).m(caps).s(sel),          k.ag_af (base_action(l2k)) .m(shift) );
        k.cm .add_combo_af ( k.cg(key).m(caps).s(sel).s(word),  k.ag_af (wafg(l2k)) .m(shift) );
        k.cm .add_combo_af ( k.cg(key).m(caps).s(sel).s(fast),  k.ag_af (fafg(l2k)) .m(shift) );

        fn del_sel_afg (del_key:Key, nav_af:AF) -> AF {
            Arc::new ( move || {
                LShift.press(); nav_af(); LShift.release(); // dont need guards for shift here.. this is deep into multi key L2
                press_release(del_key);
        } ) }

        let (da, dwa, dfa) = if del_via_sel {
            // if deleting via sel, we wrap the del-sel action around the normal nav actions
            ( del_sel_afg(dk,base_action(l2k)), del_sel_afg(dk,wafg(l2k)), del_sel_afg(dk,fafg(l2k)) )
        } else { // and for direct deletes, we perform the nav-eqv action but with the specified delete-key
            ( base_action(dk), ctrl_action(dk), fast_action(dk) )
        };

        k.cm .add_combo_af ( k.cg(key).m(caps).s(del),          k.ag_af(da ) );
        k.cm .add_combo_af ( k.cg(key).m(caps).s(del).s(word),  k.ag_af(dwa) );
        k.cm .add_combo_af ( k.cg(key).m(caps).s(del).s(fast),  k.ag_af(dfa) );


        // also add these to l2-key registry that gets used to enable their l2+ fallbacks in combo processor
        //.. the fallbacks will layer extensive l2+ mod-key combos functionality on the l2keys (e.g alt/ctrl etc combos on nav arrows keys)
        //.. in brief, w caps + l2-key, alt, ctrl, shift, and ralt-as-shift, qks1-as-ctrl will layer on the l2k-nav-key!!
        k.cm .register_l2_key (key, l2k);

        // and on lalt w/o caps, we'll layer qks1 as ctrl like it is used elsewhere
        k.cm .add_combo ( k.cg(key).m(lalt).s(qks1),  k.ag(key).m(alt).m(ctrl) );
    }

    // filling out l2/l3 actions
    setup_l2_key ( &k,  J,     ExtLeft,   Backspace,   ctrl_action,   fast_action,   false );
    setup_l2_key ( &k,  K,     ExtRight,  ExtDelete,   ctrl_action,   fast_action,   false );
    setup_l2_key ( &k,  I,     ExtUp,     Backspace,   fast_action,   fast_action,   true  );
    setup_l2_key ( &k,  Comma, ExtDown,   ExtDelete,   fast_action,   fast_action,   true  );
    setup_l2_key ( &k,  H,     ExtHome,   Backspace,   base_action,   base_action,   true  );
    setup_l2_key ( &k,  L,     ExtEnd,    ExtDelete,   base_action,   base_action,   true  );
    setup_l2_key ( &k,  U,     ExtPgUp,   Backspace,   base_action,   base_action,   true  );
    setup_l2_key ( &k,  M,     ExtPgDn,   ExtDelete,   base_action,   base_action,   true  );

    // and finally to round out the l2 keys, we'll add some non-nav word actions on Space key in (sel/del/word)-modes
    // (word selection action is a composite of move-to-word-end then select-to-word-beginning)
    let wsa = Arc::new ( || {
        LCtrl.press(); press_release(ExtRight); shift_press_release(ExtLeft); LCtrl.release();
    } );
    k.cm .add_combo_af ( k.cg(Space).m(caps).s(sel ),           k.ag_af (wsa.clone()) );
    //k.cm .add_combo_af ( k.cg(Space).m(caps).s(word),           k.ag_af (wsa.clone()) );
    k.cm .add_combo_af ( k.cg(Space).m(caps).s(sel ).s(word),   k.ag_af (wsa        ) );

    // (word del action is a composite of move-to-word-end then del-to-word-beginning)
    let wda = Arc::new ( || {
        LCtrl.press(); press_release(ExtRight); shift_press_release(ExtLeft); LCtrl.release();
        press_release(Backspace);
    } );
    k.cm .add_combo_af ( k.cg(Space).m(caps).s(del),  k.ag_af(wda) );

    // (line sel/del actions are composite of move-to-line-end then sel/del-to-line-start)
    let lsa = Arc::new ( || { press_release(ExtEnd); shift_press_release(ExtHome); } );
    let lda = Arc::new ( || { press_release(ExtEnd); shift_press_release(ExtHome); press_release(Backspace); } );
    k.cm .add_combo_af ( k.cg(Space).m(caps).m(lalt).s(sel ),           k.ag_af (lsa.clone()) );
    //k.cm .add_combo_af ( k.cg(Space).m(caps).m(lalt).s(word),           k.ag_af (lsa.clone()) );
    k.cm .add_combo_af ( k.cg(Space).m(caps).m(lalt).s(sel ).s(word),   k.ag_af (lsa) );
    k.cm .add_combo_af ( k.cg(Space).m(caps).m(lalt).s(del),            k.ag_af (lda) );

    // we can further add cut/copy/paste actions on the sel mode (caps-e-x/c/v)
    fn gen_wxcv_af (key:Key) -> AF { Arc::new ( move || {
        LCtrl.press(); press_release(ExtRight); shift_press_release(ExtLeft); press_release(key); LCtrl.release();
    } ) }
    k.cm .add_combo_af ( k.cg(X).m(caps).s(sel),  k.ag_af (gen_wxcv_af(X)) );
    k.cm .add_combo_af ( k.cg(C).m(caps).s(sel),  k.ag_af (gen_wxcv_af(C)) );
    k.cm .add_combo_af ( k.cg(V).m(caps).s(sel),  k.ag_af (gen_wxcv_af(V)) );

    // specifically for IDE, we can also add move actions to nearest matching brace/paren (via ctrl-shift-p)
    k.cm .add_combo ( k.cg(Space).m(caps).s(word),  k.ag(P).m(ctrl).m(shift) );
    // unfortunately, there's no support in IDE for selecting while doing so !! :(

    // and lets support nav to code-block (brace) start/end (and the IDE supports select for this, but its much less useful, oh well)
    //k.cm .add_combo ( k.cg(LBracket).m(lalt),         k.ag(LBracket).m(alt) );
    //k.cm .add_combo ( k.cg(RBracket).m(lalt),         k.ag(RBracket).m(alt) );
    k.cm .add_combo ( k.cg(LBracket).m(caps).s(sel),  k.ag(LBracket).m(alt).m(shift) );
    k.cm .add_combo ( k.cg(RBracket).m(caps).s(sel),  k.ag(RBracket).m(alt).m(shift) );




    // then the caps-win combo (l4?) actions :

    // caps-win-U should vert-max (via shift-win-up) if not already, or else restore window from vert-max
    k.cm .add_combo_af ( k.cg(U).m(caps).m(lwin),  k.ag_af (Arc::new (|| win_fgnd_toggle_vertmax())) );
    // caps-win-m should maximize (via win-m) if not, else restore from max
    k.cm .add_combo_af ( k.cg(M).m(caps).m(lwin),  k.ag_af (Arc::new (|| win_fgnd_toggle_max())) );
    // caps-win-n should minimize (via win-arrrowDown)
    k.cm .add_combo_af ( k.cg(N).m(caps).m(lwin),  k.ag_af (Arc::new (|| win_fgnd_min())) );
    // caps-win-t should toggle always on top for fgnd window
    k.cm .add_combo_af ( k.cg(T).m(caps).m(lwin),  k.ag_af (Arc::new (|| win_fgnd_toggle_always_on_top())) );

    fn setup_win_move_key (k:&Krusty, key:Key, wmfn:fn(i32, i32), dx:i32, dy:i32, m:i32) {
        // we'll setup caps-win combos for regular move/stretch etc, and caps-ctrl or caps-qks1 combos for fineer control
        k.cm .add_combo_af ( k.cg(key).m(caps).m(lwin).m(lctrl),  k.ag_af (Arc::new (move || wmfn (dx, dy) )) );
        k.cm .add_combo_af ( k.cg(key).m(caps).m(lwin).s(qks1),   k.ag_af (Arc::new (move || wmfn (dx, dy) )) );
        k.cm .add_combo_af ( k.cg(key).m(caps).m(lwin),           k.ag_af (Arc::new (move || wmfn (dx*m, dy*m) )) );
    }
    // caps-win-[j,k,i,comma] should  move window [left, right, top, bottom] respectively
    setup_win_move_key (&k, J,     win_fgnd_move_rel, -1, 0, 20 );
    setup_win_move_key (&k, K,     win_fgnd_move_rel, 1, 0, 20 );
    setup_win_move_key (&k, I,     win_fgnd_move_rel, 0, -1, 20 );
    setup_win_move_key (&k, Comma, win_fgnd_move_rel, 0, 1, 20 );

    // caps-win-[h,semicolon,period,o] should stretch window [narrower, wider, shorter, taller] respectively
    setup_win_move_key ( &k, H,         win_fgnd_stretch,  -1,   0, 20 );
    setup_win_move_key ( &k, O,         win_fgnd_stretch,   0,  -1, 20 );
    setup_win_move_key ( &k, Period,    win_fgnd_stretch,   0,   1, 20 );
    //setup_win_move_key ( &k, L,       win_fgnd_stretch,   1,   0, 20 );     // win-L is reserved by windows for lock
    setup_win_move_key ( &k, Semicolon, win_fgnd_stretch,   1,   0, 20 );



    // some additional caps-win combos
    // caps-win-c being used to launch winmerge diff from last two clipboard entries
    k.cm .add_combo_af ( k.cg(C).m(caps).m(lwin),  k.ag_af (Arc::new (|| start_winmerge_clipboard())) );
    // gaah we'll just throw in iDEA diff for drag-drop diffing (just coz winmerge doesnt do dark mode)
    //k.cm .add_combo_af  ( k.ks, k.cg(C).m(lwin),  k.cg_af (Arc::new (|| start_idea_diff() )));
    // ^^ cant do from here, turns out idea diff from cmd line can ONLY be opened with two files pointed, unlike empty from Idea shortcut!





    // then we can add in any l4 quick-keys shortcuts combos we want
    // note: there are 3 quick-keys modes (qks, qks2, qks3) on keys (q, 2, 3) respectively! .. all are pretty ergonomic!
    // note also that during combo gen, the 'caps' mod-key is auto added to caps-based modes, incl these quick-keys

    // we'll add some nav overloading for IDES on qks2 for starters!!
    // and this one to travel along bookmarks in IDE
    k.cm .add_combo ( k.cg(I    ).m(caps).s(qks2),  k.ag(ExtUp  ).m(alt).m(ctrl).m(shift) );
    k.cm .add_combo ( k.cg(Comma).m(caps).s(qks2),  k.ag(ExtDown).m(alt).m(ctrl).m(shift) );
    // and to toggle a bookmark at the current caret location
    k.cm .add_combo ( k.cg(U).m(caps).s(qks2),  k.ag(F11).m(ctrl).m(shift) );
    // and to bring up the bookmarks viewer
    k.cm .add_combo ( k.cg(K).m(caps).s(qks2),  k.ag(F11).m(shift) );
    // this toggles IDE col edit mode via Alt-Shift-C (but can be done w/o moving hands much)
    k.cm .add_combo ( k.cg(C).m(caps).s(qks2),  k.ag(C).m(alt).m(shift) );


    // but for switche-hotkeys, instead of caps, we'll do on lalt, and on qks1
    k.cm .add_combo ( k.cg(L).m(lalt).s(qks1),  k.ag(F19).m(alt).m(ctrl) );   // L switches to last
    k.cm .add_combo ( k.cg(O).m(lalt).s(qks1),  k.ag(F20).m(alt).m(ctrl) );   // O switches to TabsOutliner
    k.cm .add_combo ( k.cg(N).m(lalt).s(qks1),  k.ag(F21).m(alt).m(ctrl) );   // N switches to Notepad++
    k.cm .add_combo ( k.cg(I).m(lalt).s(qks1),  k.ag(F22).m(alt).m(ctrl) );   // I switches to first IDEA window
    k.cm .add_combo ( k.cg(M).m(lalt).s(qks1),  k.ag(F23).m(alt).m(ctrl) );   // M switches to winamp (music)
    k.cm .add_combo ( k.cg(B).m(lalt).s(qks1),  k.ag(F24).m(alt).m(ctrl) );   // B switches to first browser window


    // this one is a hack around intellij not giving a shortcut action to hide floating tool windows
    fn ide_float_tools_clear (ks:&KrustyState) { unsafe {
        let ks = ks.clone();
        thread::spawn ( move || {  //println!("---");
            let mut n_trials = 10;  // how many times do we want to keep finding and escaping matching windows
            while {
                let hwnd = FindWindowW (&HSTRING::from("SunAwtDialog"), PCWSTR::null());
                if hwnd.0 != 0 {
                    SetForegroundWindow(hwnd);
                    let mut n_wait = 10;  // how many times to wait <t>ms for the set fgnd to work
                    while {
                        thread::sleep(time::Duration::from_millis(10)); //print!("{:?}",'.');
                        n_wait -= 1;
                        hwnd != GetForegroundWindow() && n_wait > 0
                    } { }
                    if n_wait > 0 {  //print!("{:?}", '*');
                        ks.mod_keys.lalt.inactive_action(fast_action(Escape))();
                        thread::sleep(time::Duration::from_millis(50));
                    }  //println!();
                }
                n_trials -= 1;
                hwnd.0 != 0  && n_trials > 0    // the while condition
                // todo ^^ (not safe if there are other apps matching windows that dont close on Esc!)
                // todo: prob need to move away from findWindow (as it might keep giving a non-closing window while there are others)
                // .. and to account for possibility of non-closing windows w/o trying forever !
            } { }
        } );
    } }
    let ks = k.ks.clone();
    //let lalt_0_af = Arc::new (move || ide_float_tools_clear(&ks));
    k.cm .add_combo_af ( k.cg(Numrow_0).m(lalt), k.ag_af (Arc::new (move || ide_float_tools_clear(&ks))) );



    // we could also set up specific r-alt combos (for F<?> keys etc other than the default ralt-as-shift)
    // hmm.. cant think of any so far?


    // could also setup caps-ralt combos (for non l2/caret keys), which can be separate from caps-lalt combos!
    // hah .. nothing here yet either huh .. well these are two hand combos, so not preferable anyway


    // if really want/need to, could do completely independent lalt_ralt_<key> combos too (with minimal extra coding)
    // not yet implemented as dont see any need or much utlity for doing so given there are other simpler unused combos available


    // also fyi re free combos: caps-win-<non-l3>, win-<num>, caps-alt-<num>, caps-ralt<non-l2>
    // .. even for caps-lalt-<?> defaulting to ctr-alt-<?> most are still free (other than l2, caret, e, f, w, space, f2)
    // .. and almost all F<num> combos with caps, caps-win, caps-lalt, ralt, caps-ralt, even w just lalt


    // plus, for additional l3+ setup, (e.g. moving windows across monitors), could impl 'mode' in l3 (w/ caps-win) like for l2 (w/ caps-alt)
    // .. or add additional mode keys in l2 (caps-alt) .. although not a lot of free keys there .. maybe q .. could reuse mod keys w/ caps-win though






    // finally we can start binding key maps .. first the specialized handling for mode-trigger keys
    k.ks.mode_states.bind_mode_keys_actions(&k);
    // then setup the combo-processor itself .. (note that modifier key handlers were already set up earlier)
    k.cm.enable_combos_map_events_processor(&k);



    // and we'll put any direct special key setups after all this
    // > which is for safety in case anything above accidentally included those, although ofc we dont want to rely on that!
    special_keys_setups::setup_direct_binding_keys (&k);



    // note: the handle_input_events to start the whole shebang should be being called from main, like via the start_krusty_board fn
    //start_krusty_board();

}



pub fn start_krusty_board () {

    // setup everything to for the krusty keyboard configuration
    setup_krusty_board();

    // then start handling inputs
    InputProcessor::instance().begin_input_processing();

}

