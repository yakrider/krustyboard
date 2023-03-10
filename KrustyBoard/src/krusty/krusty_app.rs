

use std::{ time, thread, sync::Arc, };


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


    // handling for mouse left btn, mostly to allow caps-as-ctrl behavior during drag drops and clicks
    setup_mouse_left_btn_handling (&k);
    // also for mouse right btn, mostly to allow switche scrolling w right-btn-wheel combo
    setup_mouse_right_btn_handling (&k);
    // also setup both Xbutton srcs to act as middle btns (used for link clicks, closing tabs etc)
    setup_mouse_x_btn_1_handling (&k);
    setup_mouse_x_btn_2_handling (&k);

    // setup handling for mouse wheel .. complex overloading over alt-tab, switche, volume, brightness etc !!
    setup_mouse_wheel_handling (&k);



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
        k .default_bind_keys .write().unwrap() .insert (key);
    } );
    // ^^ we can ofc put combos for these later in code .. all these do is register for default binding if no combo gets mapped!



    // we'll start with some caps-atypical caps-as-shift mappings, basically nums or kbd-right symbols not otherwise involved in l2
    "1234567890-=[]\\;\'/." .chars() .for_each ( |c| {
        Key::from_char(c) .into_iter() .for_each ( |key|
            k.cm .add_combo (&k.ks, &k.cg(key).m(caps), &k.cg(key).m(lshift))
        )
    } );



    // lets also disable the win-number combos as they annoyingly activate/minimize items from taskbar etc
    "1234567890" .chars() .map (|c| Key::from_char(c)) .flatten() .for_each ( |key| {
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(lwin),                  no_action() );
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(lwin).m(caps),          no_action() );
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(lwin).m(lalt),          no_action() );
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(lwin).m(ralt),          no_action() );
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(lwin).m(caps).m(lalt),  no_action() );
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(lwin).m(caps).m(ralt),  no_action() );
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(lwin).m(caps).m(shift), no_action() );
    } ); // some of ^^ these will get overwritten by specific win-combos added later .. which is fine
    // note that at least for now, we're choosing to ignore caps-shift, caps-ctrl etc combos, though ofc could impl if need arises

    // we'll disable win-d too, as I never use that show/hide desktop and it's disruptive
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(D).m(lwin), no_action());



    // we'll set up a combo (caps-alt-Insert) to unstick everything in case we get into weird states due to other hooks stealing/suppressing key events etc
    // note since the whole point is to invoke it at stuck times, we'd want to overload that also for alt/ctr/shift etc and their combinations!
    // .. otoh, they can all be unstuck by just press/releasing them, incl mouse btns, so for now we'll ignore that, can update as necessary
    // further, combos incl all mode states and some other flags .. for a combo to trigger those would have to match too ..
    // soo .. we'll let this simple combo be, but it will mostly only be useful to clear mouse-btn states .. not even eqv of press-rel mod-keys
    let ks = k.ks.clone(); let clear = Arc::new (move || ks.unstick_all());
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Insert).m(caps).m(lalt), clear.clone());



    fn register_mode_key (k:&Krusty, key:Key, ms_t:ModeState_T) {
        // first we'll do the registration, then we can try and set any auxillary combos here too
        k.ks.mode_states .register_mode_key (key, ms_t);

        // note that we want to disable mode-keys across most mod-key combos when caps down ..
        // .. and thats painful to do via combos-maps, so we're now instead just disabling them in runtime fallbacks
        // further, in fallback, we'll also layer base action w mod-keys for these mode-trigger-keys when qks1 down!
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



    // f in caret mode, so we'll remap some of the other combos to replace ctr-f etc
    k.cm .add_combo (&k.ks, &k.cg(F).m(lalt),         &k.cg(F).m(ctrl));     // alt-f to ctrl-f
    k.cm .add_combo (&k.ks, &k.cg(F).s(word).m(lalt), &k.cg(F).m(lalt));     // caps-lalt-f to alt-f, though it goes against typical mode-key usage

    // e in caret mode, so we'll put our left-handed-enter on alt-e instead .. (note that there are also caps-space-* combos for *-enter)
    k.cm .add_combo (&k.ks, &k.cg(E).m(lalt),         &k.cg(Enter));             // alt-e -> enter






    // setup backquote .. make normal case be Delete, caps or alt do back-tick, and shift do its tilde
    k.cm .add_combo (&k.ks, &k.cg(Backquote),          &k.cg(ExtDelete));
    k.cm .add_combo (&k.ks, &k.cg(Backquote).m(shift), &k.cg(Backquote).m(shift));
    k.cm .add_combo (&k.ks, &k.cg(Backquote).m(caps),  &k.cg(Backquote));
    k.cm .add_combo (&k.ks, &k.cg(Backquote).m(lalt),  &k.cg(Backquote));
    //k.cm .add_combo (&k.sk, &k.cg(Backquote).m(ralt),  &k.cg(Backquote).m(shift));
    // ^^ not strictly necessary as cb composition now defaults to this, but also useful to see here for reference


    // setup tab .. caps-as-ctrl for caps-tab switching, incl for ctrl-shift-tab .. also ralt-tab for shift-tab
    let ks = k.ks.clone();
    let cb : AF = Arc::new (move || {
        if ks.mod_keys.caps.down.check() {
            ks.in_managed_ctrl_down_state.set();
            ks.mod_keys.lctrl.ensure_active();  // this enables caps-as-ctrl for caps-tab switching
            // ^^ we're not gonna release ctrl immediately, but keep track and release when caps is released
            press_release(Tab)
        }
    } );
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Tab).m(caps), cb.clone());
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Tab).m(caps).m(shift), wrapped_action(LShift, cb.clone()));
    // ^^ this enables the natural ctrl-shift-tab to do backwards tablist nav
    // .. note that we do 'bare' here because caps-tab is in managed ctrl state, and we dont wana get wrapped w ctrl inactive guards here

    // we'll also need to put these in combos wit the managed-ctrl-state already active (as its among combo bits now)
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Tab).m(caps).s(mngd_ctrl_dn), cb.clone());
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Tab).m(caps).m(shift).s(mngd_ctrl_dn), wrapped_action(LShift, cb.clone()));

    // lets add explicit support for arrow keys during ctrl tab (default doesnt check for active/inactive guards)
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Left ).m(caps).s(mngd_ctrl_dn), base_action(Left ));
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Right).m(caps).s(mngd_ctrl_dn), base_action(Right));
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Up   ).m(caps).s(mngd_ctrl_dn), base_action(Up   ));
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Down ).m(caps).s(mngd_ctrl_dn), base_action(Down ));

    // and finally for ralt-as-shift support for caps-tabbing too
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Tab).m(caps).m(ralt),                 wrapped_action(LShift, cb.clone()));
    k.cm .add_bare_af_combo (&k.ks, &k.cg(Tab).m(caps).m(ralt).s(mngd_ctrl_dn), wrapped_action(LShift, cb.clone()));




    // setup space key .. ralt-space as enter, caps-space as ctrl-space, caps-lalt-space as alt-enter for intellij
    k.cm .add_combo (&k.ks, &k.cg(Space).m(ralt),         &k.cg(Enter));             // ralt-space -> enter
    k.cm .add_combo (&k.ks, &k.cg(Space).m(caps).m(lalt), &k.cg(Enter).m(lalt));     // caps-lalt-space -> alt-enter
    k.cm .add_combo (&k.ks, &k.cg(Space).m(caps).m(ralt), &k.cg(Escape));            // caps-ralt-space -> Escape
    //k.cm .add_combo (&k.ks, &k.cg(Space).m(caps),         &k.cg(Space).m(ctrl));   // caps-space -> ctrl-enter
    // ^^ not strictly necessary as cb composition now defaults to this, but also useful to see here for reference



    // win-m by default minimized all windows .. we just want to disable it
    k.cm .add_cnsm_bare_af_combo (&k.ks, k.cg(M).m(lwin), no_action());

    // win-i should start irfanview
    k.cm .add_cnsm_bare_af_combo (&k.ks, k.cg(I).m(lwin), Arc::new (|| start_irfanview()));

    // win-n should start chrome-incognito
    k.cm .add_cnsm_bare_af_combo (&k.ks, k.cg(N).m(lwin), Arc::new (|| start_chrome_incognito()));

    // we'll setup win-C (and caps-alt-C) to quickly bring up chrome Tabs-Outliner via switche Alt-F20 hotkey
    k.cm .add_combo (&k.ks, k.cg(C).m(lwin),         &k.cg(F20).m(lctrl));      // win-c -> ctrl-F20 .. switche tabs-outliner
    k.cm .add_combo (&k.ks, k.cg(C).m(caps).m(lalt), &k.cg(F20).m(lctrl));      // caps-lalt-c -> ctrl-F20 .. one-handed


    // in cur laptop, Fn-F6/F7 do brightness, but at +10 incrs .. set them to do small incrs with alt combos
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(F6).m(lalt), Arc::new (|| incr_brightness(-1)));
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(F7).m(lalt), Arc::new (|| incr_brightness(1)));

    // actually, since we use win-1/2/3 as vol mute/down/up, might as well also set alt-1/2/3 for brightness zero/down/up
    // (note that numrow 1/2/3 with caps are qks* keys, so they cant be used with any caps combos as those would be silent!)
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(Numrow_1).m(lalt),          Arc::new (|| incr_brightness(-100)));
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(Numrow_2).m(lalt),          Arc::new (|| incr_brightness(-1)));
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(Numrow_3).m(lalt),          Arc::new (|| incr_brightness(1)));
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(Numrow_2).m(lalt).m(shift), Arc::new (|| incr_brightness(-5)));
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(Numrow_3).m(lalt).m(shift), Arc::new (|| incr_brightness(5)));

    // win-2 is vol down, win-3 is vol up, win-1 can do mute
    k.cm .add_af_combo (&k.ks, &k.cg(Numrow_1).m(lwin),          &k.cg_af(base_action(VolumeMute)));
    k.cm .add_af_combo (&k.ks, &k.cg(Numrow_2).m(lwin),          &k.cg_af(base_action(VolumeDown)));
    k.cm .add_af_combo (&k.ks, &k.cg(Numrow_3).m(lwin),          &k.cg_af(base_action(VolumeUp)));
    k.cm .add_af_combo (&k.ks, &k.cg(Numrow_2).m(lwin).m(shift), &k.cg_af(fast_action(VolumeDown)));  // double-action
    k.cm .add_af_combo (&k.ks, &k.cg(Numrow_3).m(lwin).m(shift), &k.cg_af(fast_action(VolumeUp)));    // double-action

    // win-f1 play/pause, caps-f1 toggle mute, base-case: switche-invoke alt-F1: switche silent-switch, ralt for actual F1
    k.cm .add_combo (&k.ks, &k.cg(F1),          &k.cg(F16));             // switche next
    //k.cm .add_combo (&k.ks, &k.cg(F1).m(shift), &k.cg(F17));           // switche prev
    k.cm .add_combo (&k.ks, &k.cg(F1).m(shift), &k.cg(F16).m(shift));    // switche prev (passthrough shift-F16 instead of F17 is more efficient)
    k.cm .add_combo (&k.ks, &k.cg(F1).m(lalt),  &k.cg(F19).m(ctrl));     // switche no-popup next switch
    k.cm .add_combo (&k.ks, &k.cg(F1).m(ralt),  &k.cg(F1));
    k.cm .add_combo (&k.ks, &k.cg(F1).m(caps),  &k.cg(VolumeMute));
    k.cm .add_combo (&k.ks, &k.cg(F1).m(lwin),  &k.cg(MediaPlayPause));
    // and keeping w the theme, set caps-win-F1 (key with vol-mute printed on it) to toggle microphone mute
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(F1).m(caps).m(lwin), Arc::new (|| {mic_mute_toggle(); open_mic_cpl();}));
    // we'll set Alt-F2 to bring chrome tabs-outliner (via switche) to keep w the theme of Alt-F<n> keys for task switching
    k.cm .add_combo (&k.ks, &k.cg(F2).m(lalt),  &k.cg(F20).m(ctrl));     // switche no-popup tabs-outliner switch


    // want win-f2 for next with some initial skip .. we'll use caps-win-f2 for prev, so we'll set it up for both
    // note that our mechanism for wrapping mod-key-state restoring guards operates via AFs, hence setting those up (instead of fns)

    // skips work by alt-ctrl-volUp (needs to guard win-inactive since its on win-combo)
    fn media_skips_action (n_skips:u32, ks:&KrustyState) -> AF {
        ks.mod_keys.lwin.inactive_action ( ks.mod_keys.lalt.active_action ( ks.mod_keys.lctrl.active_action ( Arc::new ( move || {
            (0 .. n_skips) .into_iter() .for_each (|_| { press_release(VolumeUp) });
    } ) ) ) ) }
    // ^^ gives an AF with specified number of skips

    // it uses alt-active media-skips, so we'll need alt_inactive-action, plus guard on win-combo it is on
    let ks = k.ks.clone();
    let media_next_action = k.ks.mod_keys.lwin.inactive_action ( k.ks.mod_keys.lalt.inactive_action ( Arc::new ( move || {
        if !ks.mod_keys.caps.down.check() { press_release(MediaNextTrack) }
        else { press_release(MediaPrevTrack) }
        let ks = ks.clone();  // clone again to allow moving into spawned thread closure
        //thread::spawn ( move || { sleep(time::Duration::from_millis(2000));  media_skips_action(3,&ks)(); } );
        // .. note again that we're always spawned out from hook thread, so slower tasks are also ok here
        thread::sleep(time::Duration::from_millis(2000));
        media_skips_action(3,&ks)();
    } ) ) );

    // win-f2 for next with some initial skip
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(F2).m(lwin),         media_next_action.clone());
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(F2).m(lwin).m(caps), media_next_action);

    // win-f3 for skip forward a bit
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(F3).m(lwin), media_skips_action(1, &k.ks));



    // escape is just escape, but we just want it to do press-release immediately (so switche is faster)
    k.cm .add_combo (&k.ks, &k.cg(Escape), &k.cg(Escape));

    // use the apps key to send shift-escape .. or caps-escape too
    k.cm .add_combo (&k.ks, &k.cg(Apps),           &k.cg(Escape).m(shift));
    k.cm .add_combo (&k.ks, &k.cg(Escape).m(caps), &k.cg(Escape).m(shift));




    // caps-lalt-F for full screen ..(but from bulk mapping above, caps-f should still give ctrl-f)
    //k.cm .add_combo (&k.ks, &k.cg(F).m(caps).m(lalt), &k.cg(F11));
    // ^^ no longer available as we made F a caret mode key, and set caps-alt-F to alt-F instead

    // 'w' should have caps-ctrl mapping, but when w/ alt, send alt-f4 (to close all-tabs, windows etc)
    //k.cm .add_combo (&k.ks, &k.cg(W).m(caps).m(shift), &k.cg(F4).m(alt));
    // ^^ note: initially we wanted this with caps-shift-w, but turns out (at least on my kbd, turns out caps+shift+[F1, 2, w, s, x]
    // >  dont produce any key event at the hook at all .. nothing .. its like the keyboard driver not sending those out
    // funnily enough, there's a bunch of complaints about specifically those keys for dell/hp laptops .. looks like hardware
    // >  appears to be a common kbd pcb layout issue .. heres from 2007: (https://www.joachim-breitner.de/blog/250-Shift-Caps-2)
    // sooo .. to makeup, we'll do alt-caps-w do the alt-f4 business instead
    k.cm .add_combo (&k.ks, &k.cg(W).m(caps).m(lalt), &k.cg(F4).m(lalt));




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
        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps),         base_action(l2k));
        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps).s(word), wafg(l2k));
        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps).s(fast), fafg(l2k));

        // selection actions are via wrapping those with shift press-release
        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps).s(sel),         wrapped_action (LShift, base_action(l2k)));
        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps).s(sel).s(word), wrapped_action (LShift, wafg(l2k)));
        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps).s(sel).s(fast), wrapped_action (LShift, fafg(l2k)));

        fn del_sel_afg (del_key:Key, nav_af:AF) -> AF {
            Arc::new ( move || {
                LShift.press(); nav_af(); LShift.release(); // dont need guards for shift here.. this is deep into multi key L2
                press_release(del_key);
        } ) }

        let (dna, dwa, dfa) = if del_via_sel {
            // if deleting via sel, we wrap the del-sel action around the normal nav actions
            ( del_sel_afg(dk,base_action(l2k)), del_sel_afg(dk,wafg(l2k)), del_sel_afg(dk,fafg(l2k)) )
        } else { // and for direct deletes, we perform the nav-eqv action but with the specified delete-key
            ( base_action(dk), ctrl_action(dk), fast_action(dk) )
        };

        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps).s(del),         dna);
        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps).s(del).s(word), dwa);
        k.cm .add_bare_af_combo (&k.ks, k.cg(key).m(caps).s(del).s(fast), dfa);

        // also add these to l2-key registry that gets used to enable their l2+ fallbacks in combo processor
        //.. the fallbacks will layer extensive l2+ mod-key combos functionality on the l2keys (e.g alt/ctrl etc combos on nav arrows keys)
        //.. in brief, w caps + l2-key, alt, ctrl, shift, and ralt-as-shift, qks1-as-ctrl will layer on the l2k-nav-key!!
        k.cm .register_l2_key (key, l2k);
    }


    // idk what layer this even is, but since we're so used to l2 j/k etc nav keys, we'll set them up for various mod-combo eqvs too
    // .. these are useful for various second order nav .. e.g between IDE tabs etc .. no fancy speedups etc here
    // (note ofc that we've filled out the whole set of these for completeness even though realistically we wont use all/most of them)


    // filling out l2/l3 actions
    setup_l2_key ( &k,  J,     ExtLeft,   Backspace,   ctrl_action,   fast_action,   false );
    setup_l2_key ( &k,  K,     ExtRight,  ExtDelete,   ctrl_action,   fast_action,   false );
    setup_l2_key ( &k,  I,     ExtUp,     Backspace,   fast_action,   fast_action,   true  );
    setup_l2_key ( &k,  Comma, ExtDown,   ExtDelete,   fast_action,   fast_action,   true  );
    setup_l2_key ( &k,  H,     ExtHome,   Backspace,   base_action,   base_action,   true  );
    setup_l2_key ( &k,  L,     ExtEnd,    ExtDelete,   base_action,   base_action,   true  );
    setup_l2_key ( &k,  U,     ExtPgUp,   Backspace,   base_action,   base_action,   true  );
    setup_l2_key ( &k,  M,     ExtPgDn,   ExtDelete,   base_action,   base_action,   true  );






    // then the caps-win combo (l4?) actions :

    // caps-win-U should vert-max (via shift-win-up) if not already, or else restore window from vert-max
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(U).m(caps).m(lwin), Arc::new (|| win_fgnd_toggle_vertmax()));
    // caps-win-m should maximize (via win-m) if not, else restore from max
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(M).m(caps).m(lwin), Arc::new (|| win_fgnd_toggle_max()));
    // caps-win-n should minimize (via win-arrrowDown)
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(N).m(caps).m(lwin), Arc::new (|| win_fgnd_min()));

    fn setup_win_move_key (k:&Krusty, key:Key, wmfn:fn(i32, i32), dx:i32, dy:i32, m:i32) {
        // we'll setup caps-win combos for regular move/stretch etc, and caps-ctrl or caps-qks1 combos for fineer control
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(caps).m(lwin).m(lctrl), Arc::new (move || wmfn (dx, dy) ));
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(caps).m(lwin).s(qks1),  Arc::new (move || wmfn (dx, dy) ));
        k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(key).m(caps).m(lwin),          Arc::new (move || wmfn (dx*m, dy*m) ));
    }
    // caps-win-[j,k,i,comma] should  move window [left, right, top, bottom] respectively
    setup_win_move_key ( &k, J,     win_fgnd_move,  -1,   0, 20 );
    setup_win_move_key ( &k, K,     win_fgnd_move,   1,   0, 20 );
    setup_win_move_key ( &k, I,     win_fgnd_move,   0,  -1, 20 );
    setup_win_move_key ( &k, Comma, win_fgnd_move,   0,   1, 20 );

    // caps-win-[h,semicolon,period,o] should stretch window [narrower, wider, shorter, taller] respectively
    setup_win_move_key ( &k, H,         win_fgnd_stretch,  -1,   0, 20 );
    setup_win_move_key ( &k, O,         win_fgnd_stretch,   0,  -1, 20 );
    setup_win_move_key ( &k, Period,    win_fgnd_stretch,   0,   1, 20 );
    //setup_win_move_key ( &k, L,       win_fgnd_stretch,   1,   0, 20 );     // win-L is reserved by windows for lock
    setup_win_move_key ( &k, Semicolon, win_fgnd_stretch,   1,   0, 20 );



    // some additional caps-win combos
    // caps-win-c being used to launch winmerge diff from last two clipboard entries
    k.cm .add_cnsm_bare_af_combo (&k.ks, &k.cg(C).m(caps).m(lwin), Arc::new (|| start_winmerge_clipboard()));
    // gaah we'll just throw in iDEA diff for drag-drop diffing (just coz winmerge doesnt do dark mode)
    //k.cm .add_af_combo (&k.ks, &k.cg(C).m(lwin), &k.cg_af (Arc::new (|| start_idea_diff() )));
    // ^^ cant do from here, turns out idea diff from cmd line can ONLY be opened with two files pointed, unlike empty from Idea shortcut!





    // then we can add in any l4 quick-keys shortcuts combos we want
    // note: there are 3 quick-keys modes (qks, qks2, qks3) on keys (q, 2, 3) respectively! .. all are pretty ergonomic!
    // note also that during combo gen, the 'caps' mod-key is auto added to caps-based modes, incl these quick-keys

    // we'll add some nav overloading for IDES on qks2 for starters!!
    // and this one to travel along bookmarks in IDE
    k.cm .add_combo (&k.ks, &k.cg(I    ).s(qks2), &k.cg(ExtUp  ).m(lalt).m(lctrl).m(lshift));
    k.cm .add_combo (&k.ks, &k.cg(Comma).s(qks2), &k.cg(ExtDown).m(lalt).m(lctrl).m(lshift));
    // and to toggle a bookmark at the current caret location
    k.cm .add_combo (&k.ks, &k.cg(U).s(qks2), &k.cg(F11).m(lctrl).m(lshift));
    // and to bring up the bookmarks viewer
    k.cm .add_combo (&k.ks, &k.cg(K).s(qks2), &k.cg(F11).m(lshift));
    // this toggles IDE col edit mode via Alt-Shift-C (but can be done w/o moving hands much)
    k.cm .add_combo (&k.ks, &k.cg(C).s(qks2), &k.cg(C).m(lalt).m(lshift));



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
    handle_input_events();

}

