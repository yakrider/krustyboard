#![ allow (non_snake_case, non_upper_case_globals, unused_doc_comments) ]


use std::{ time, thread, sync::Arc, };
use std::sync::RwLock;
use once_cell::sync::Lazy;
use rustc_hash::FxHashSet;

use crate::{*, KbdKey::*, key_utils::*, ModKey::*, ModeState_T::*};





/// handling for any 'special' keys that need to be bound/handled directly (like for mouse btns) rather than via combo-maps
pub mod special_keys_setups {
    use std::sync::Arc;
    use crate::{*, KbdEventCbMapKeyType::*, EventPropagationDirective::*, KbdEvCbComboProcDirective::*, KbdEventCallbackFnType::*};

    pub fn setup_direct_binding_keys (k:&Krusty) {

        // currently, we dont need to do it for anything .. (was more essential when mod-tracking / combo-maps mechanisms were more limited)

        // HOWEVER, there might be cases where it might be simpler to just do direct bindings than to populate all the combo maps etc etc
        // ALSO, for things that require actual action in key-up callbacks, we'd do it direclty here because key-dn actions arent registered
        // > specifically in maps and so combo-maps composed callbacks can only either set key-up to do nothing, or at most clear mode flags

        // IMPORTANT : note that whatever we put here, we will NOT want to include them in ANY action maps (we'll overwrite w these at the end) !!


        // we'll use one for unstick-all ..
        // regular combo-maps are painful for this as expectation is when we want to use that any modkey/flag might be stuck for whatever reason
        // .. so we'd need to make combos to trigger that will all possible combinations of modkeys .. which quickly gets out of hand
        // hence we'll directly code that up here
        // Note again that when we do direct binding here, anything via combomaps for the key will be overwritten

        let ks = k.ks.clone();
        let epd_passthrough = KbdEvProcDirectives { event_prop_d: EventProp_Continue, combo_proc_d: ComboProc_Disable };
        k.iproc .kbd_bindings .bind_kbd_event (Key::Insert, KeyEventCb_KeyDown, KbdEventCallbackEntry {
            event_proc_d: epd_passthrough,
            cb: KbdEvCbFn_InlineCallback ( Arc::new ( move |_| {
                if ks.mod_keys.caps.dbl_tap.is_set() { ks.unstick_all() }
                epd_passthrough
            } ) ),
        } )
        // wont need a key-up for this as the keydown is pasthrough anyway


    }

}





/// setup for the entire krusty-board application, incl setting up key/btn bindings and combos
pub fn setup_krusty_board () {

    // lets import various windows/process/brightness util fns
    use crate::utils::{windows_utils::*, process_utils::*, /*brightness_utils::*,*/};
    use WinGroups_E::*;
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
            k.cm .add_combo ( k.ks.cg(key).m(caps),  k.ks.ag(key).m(lshift) )
        )
    } );


    // now, windows itself seems to treat shift-arrow keys as special, injecting its own logic of shift-up/dn in
    // >  the stream when it sees left/right/up/down w shift held down .. and that ofc screws up with our own handling for that
    // so instead, we'll swap those (when w shift) with the ext-left/right/up/down etc versions
    k.cm .add_combo ( k.ks.cg(Left ).m(shift),  k.ks.ag(ExtLeft ).m(shift)  );
    k.cm .add_combo ( k.ks.cg(Right).m(shift),  k.ks.ag(ExtRight).m(shift)  );
    k.cm .add_combo ( k.ks.cg(Up   ).m(shift),  k.ks.ag(ExtUp   ).m(shift)  );
    k.cm .add_combo ( k.ks.cg(Down ).m(shift),  k.ks.ag(ExtDown ).m(shift)  );
    // might as well do the same for the other similar keys (w MS special shift handling)
    k.cm .add_combo ( k.ks.cg(Home    ).m(shift),  k.ks.ag(ExtHome).m(shift)  );
    k.cm .add_combo ( k.ks.cg(End     ).m(shift),  k.ks.ag(ExtEnd ).m(shift)  );
    k.cm .add_combo ( k.ks.cg(PageUp  ).m(shift),  k.ks.ag(ExtPgUp).m(shift)  );
    k.cm .add_combo ( k.ks.cg(PageDown).m(shift),  k.ks.ag(ExtPgDn).m(shift)  );




    // lets also disable the win-number combos as they annoyingly activate/minimize items from taskbar etc
    // NOTE that these could be removed now that we've made win into TMK_dbl ..
    // .. but if we set win-combo single-press fallback to win-combo, these will still be useful, so we'll let them be!,
    // .. note ofc that everything disabled like this will be accessible on win-dbl press combos!
    "1234567890" .chars() .map (|c| Key::from_char(c)) .flatten() .for_each ( |key| {
        k.cm .add_combo_af ( k.ks.cg(key).m(lwin),                   k.ks.ag_af(no_action()) );
        k.cm .add_combo_af ( k.ks.cg(key).m(lwin).m(caps),           k.ks.ag_af(no_action()) );
        k.cm .add_combo_af ( k.ks.cg(key).m(lwin).m(lalt),           k.ks.ag_af(no_action()) );
        k.cm .add_combo_af ( k.ks.cg(key).m(lwin).m(ralt),           k.ks.ag_af(no_action()) );
        k.cm .add_combo_af ( k.ks.cg(key).m(lwin).m(caps).m(lalt),   k.ks.ag_af(no_action()) );
        k.cm .add_combo_af ( k.ks.cg(key).m(lwin).m(caps).m(ralt),   k.ks.ag_af(no_action()) );
        k.cm .add_combo_af ( k.ks.cg(key).m(lwin).m(caps).m(shift),  k.ks.ag_af(no_action()) );
    } ); // some of ^^ these will get overwritten by specific win-combos added later .. which is fine
    // note that at least for now, we're choosing to ignore caps-shift, caps-ctrl etc combos, though ofc could impl if need arises

    // we'll disable win-d too, as I never use that show/hide desktop and it's disruptive
    k.cm .add_combo_af ( k.ks.cg(D).m(lwin),  k.ks.ag_af(no_action()) );




    fn register_mode_key (k:&Krusty, key:Key, ms_t:ModeState_T) {

        // first we'll do the registration, then we can try and set any auxillary combos here too
        // note that mode-keys down flag will track its physical state, but combo trigger on mode-state requires caps to be down too
        k.ks.mode_states .register_mode_key (key, ms_t);

        // since pressing mode-keys sets their flags first, we want to map their own presses w their own flags back to base-action
        // (note the mode-state-kdn_no-consume (msk_nc) specified below so we're not disabling key-repeats for base action)
        k.cm .add_combo ( k.ks.cg(key).s(ms_t).msk_nc(),  k.ks.ag(key).mkg_nw() );
        // ^^ the modkey-guard-no-wrap is specified to make it explicit, but isnt strictly necessary here as there are no mod-keys when this triggers

        // to avoid stragglers, we'll set the mode-keys pressed w caps to disable their repeat until release (ie. even after caps is released!)
        // note that the following works because any mode-state specified in combo-gen is auto marked for consumption (unless do .msk_nc())
        k.cm .add_combo_af ( k.ks.cg(key).m(caps).s(ms_t),  k.ks.ag_af(no_action()).mkg_w() );

        // and we could do the same for alt/win etc, but we'd rather leave those open and they can be done later when/if such combos are set
        // (this allows mod-key combos for mode-keys, and we can disable it only for specific cases like qks1 which modifies other mode-state-keys)
        //k.cm .add_combo_af ( k.ks.cg(key).m(lalt).s(ms_t),  k.ks.ag_af(no_action()).mkg_w() );
        //k.cm .add_combo_af ( k.ks.cg(key).m(lwin).s(ms_t),  k.ks.ag_af(no_action()).mkg_w() );

        // note that we want to disable mode-keys across most mod-key combos when caps down ..
        // .. and thats painful to do via combos-maps, so we're now instead just disabling them in runtime fallbacks
        // further, in fallback, we'll also layer base action w mod-keys for these mode-trigger-keys when qks1 down!

        // however, we could at least restore caps-alt-<mode-key> to the expected ctrl-alt by default .. can ofc override these later
        //k.cm .add_combo ( k.ks.cg(key).m(caps).s(ms_t).m(lalt),  k.ks.ag(key).m(ctrl).m(alt) );
        // ^^ naah, some of these we need to be silent and modify other combos (e.g. qks1), so lets do them individually later

        // aight, so at the very least we can open back up the expected caps-as-ctrl behavior for mode-trigger keys on caps-dbl
        k.cm .add_combo ( k.ks.cg(key).m(caps_dbl),  k.ks.ag(key).m(ctrl) );
    }

    fn register_latch_key (k:&Krusty, key:Key, ms_t:ModeState_T) {
        // latch key registration are much simpler than mode keys, as they only trigger on dbl-caps, so normal behavior is unimpeded
        k.ks.mode_states.register_latch_key (key, ms_t)
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

    // now we'll set up the four Fn keys for latching state triggers
    // (note that latches only toggle on its trigger-key press when with caps_dbl)
    register_latch_key ( &k, F1, latch_1 );
    register_latch_key ( &k, F2, latch_2 );
    register_latch_key ( &k, F3, latch_3 );
    register_latch_key ( &k, F4, latch_4 );



    // we want to overlay some additional combos on some of these w Alt (others that modify other combos should remain silent)
    k.cm .add_combo ( k.ks.cg(E).m(caps).s(sel ).m(lalt),   k.ks.ag(E).m(ctrl).m(alt) );
    k.cm .add_combo ( k.ks.cg(D).m(caps).s(del ).m(lalt),   k.ks.ag(D).m(ctrl).m(alt) );
    //k.cm .add_combo ( k.ks.cg(F).m(caps).s(word).m(lalt),   k.ks.ag(F).m(ctrl).m(alt) );   // overriden below
    k.cm .add_combo ( k.ks.cg(R).m(caps).s(fast).m(lalt),   k.ks.ag(R).m(ctrl).m(alt) );

    // since F is in caret mode, we'll remap some of the other combos to replace ctr-f etc
    k.cm .add_combo ( k.ks.cg(F).m(lalt),                  k.ks.ag(F).m(ctrl) );     // alt-f to ctrl-f
    k.cm .add_combo ( k.ks.cg(F).m(caps).s(word).m(lalt),  k.ks.ag(F).m(lalt) );     // caps-lalt-f to alt-f, though it goes against typical mode-key usage

    // e in caret mode, so we'll put our left-handed-enter on alt-e instead .. (note that there are also caps-space-* combos for *-enter)
    k.cm .add_combo ( k.ks.cg(E).m(lalt),           k.ks.ag(Enter) );            // alt-e       ->  Enter
    k.cm .add_combo ( k.ks.cg(E).m(caps).s(qks),    k.ks.ag(Enter).m(ctrl) );    // caps-q-e    ->  ctrl-Enter
    k.cm .add_combo ( k.ks.cg(E).m(caps).m(lalt),   k.ks.ag(Enter).m(ctrl) );    // caps-alt-e  ->  ctrl-Enter

    // and while doing Enter, we'll also add lalt-comma as enter (ralt-space can be taxing on the right thumb)
    // .. note that comma is in l2 keys, but this independent layering on alt should still work
    //k.cm .add_combo ( k.ks.cg(Comma).m(lalt),  k.ks.ag(Enter) );        // lalt-comma  -> Enter
    // ^^ clashes w switche nav during alt-tab (alt-as-caps for l2 nav eqv)
    // and just to try out, we'll do alt-9 too .. maybe thats even more ergonomic
    k.cm .add_combo ( k.ks.cg(Numrow_9).m(lalt),  k.ks.ag(Enter) );     // lalt-9  -> Enter




    // we'll setup some keys on caps double tap first, esp those that modify global-ish behavior
    //
    // dbl-caps T to toggle capslock
    k.cm .add_combo ( k.ks.cg(T).m(caps_dbl),   k.ks.ag(CapsLock) );

    // we'll set dbl-caps-alt-S/C/A/W as tmp shift/ctrl/alt/win lock (useful for doing mouse horiz scroll on say moon-reader etc)
    let ks = k.ks.clone();
    k.cm .add_combo_af ( k.ks.cg(S).m(caps_dbl).m(lalt),          k.ks.ag_af ( Arc::new (move || ks.mod_keys.lshift.ensure_active()) ) );
    let ks = k.ks.clone();
    k.cm .add_combo_af ( k.ks.cg(C).m(caps_dbl).m(lalt),          k.ks.ag_af ( Arc::new (move || ks.mod_keys.lctrl.ensure_active()) ) );
    let ks = k.ks.clone();
    k.cm .add_combo_af ( k.ks.cg(A).m(caps_dbl).m(lalt),          k.ks.ag_af ( Arc::new (move || ks.mod_keys.lalt.ensure_active()) ) );
    //let ks = k.ks.clone();
    //k.cm .add_combo_af ( k.ks.cg(W).m(caps_dbl).m(lalt),          k.ks.ag_af ( Arc::new (move || ks.mod_keys.lwin.ensure_active()) ) );
    // win is now dbled modkey (and so not full-managed), and so ensure-active doesnt make sense for it


    // we'll set dbl-caps-p to bring up process explorer (via ctrl-shift-esc)
    k.cm .add_combo ( k.ks.cg(P).m(caps_dbl),  k.ks.ag(Escape).m(lctrl).m(lshift) );



    // UNSTICK-ALL considerations
    // we want to set up a combo to unstick-all in case we get into weird states due to other hooks stealing/suppressing key events etc
    // lets do dbl-caps (Insert) for reset for now, can settle on one later .. (Insert because End is on Fn key F12, Insert is direct key on this pc)
    //let ks = k.ks.clone(); let clear = Arc::new (move || ks.unstick_all());
    //k.cm .add_combo_af ( k.ks.cg(Insert).m(caps_dbl),  k.ks.ag_af (clear.clone()) );
    // ^^ nuh-uh ..
    // however since the whole point is to invoke it at stuck times, we'd need to overload that also for alt/ctr/shift etc and their combinations!
    // .. otoh, they can all be unstuck by just press/releasing them, incl mouse btns, so for now we'll ignore that, can update as necessary
    // further, combos incl all mode states and some other flags .. for a combo to trigger those would have to match too ..
    // soo .. we'll let these simple combo be, but it will mostly only be useful to clear mouse-btn states .. or single mod-key sticking
    //
    //k.cm .add_combo_af ( k.ks.cg(Insert).m(caps).m(lalt),          k.ks.ag_af (clear.clone()) );
    //k.cm .add_combo_af ( k.ks.cg(Insert).m(caps).m(lalt).m(ctrl),  k.ks.ag_af (clear.clone()) );
    //k.cm .add_combo_af ( k.ks.cg(Insert).m(caps).m(lalt).m(win ),  k.ks.ag_af (clear.clone()) );
    // ^^ nuh-uh ..
    // sooo .. instead we'll just put this in direct-binding keys .. ig finally a use for that ..
    // note however, that anything put there wlil not be usable in any combo-maps combos!1

    // could prob add something simple to quit though? .. and thatd be easier coz expectation is usage while nothing-stuck?







    // we'll setup caps-dbl-alt-tab as toggling whether switche handles alt-tab or not
    let ks = k.ks.clone();
    k.cm .add_combo_af ( k.ks.cg(Tab).m(caps_dbl).m(lalt),     k.ks.ag_af ( Arc::new (move || ks.toggle_alt_tab_replace()) ) );

    // todo .. since alt-tab state is in ks .. see if the alt-tab impl shouldnt be out here in app-code?

    // setup alt-tab
    let ks = k.ks.clone();
    let cb : AF = Arc::new (move || {
        if ks.is_replacing_alt_tab.is_set() && ks.mod_keys.lalt.down.is_set() {
            ks.in_right_btn_scroll_state.set(); // we'll reuse the flag from the other mechanism of triggering switche
            if ks.mod_keys.some_shift_down() || ks.mod_keys.ralt.down.is_set() {
                //ks.mod_keys.lshift.inactive_action (ks.mod_keys.lalt.inactive_on_key(F17))()
                // ^^ passthrough shift-F16 instead of F17 is more efficient
                ks.mod_keys.lshift.active_action (ks.mod_keys.lalt.inactive_on_key(F16))()
            } else { ks.mod_keys.lalt.inactive_on_key(F16)() }
        }
        else {
            // for natural alt-tab again, for just shift, it would layer naturally, but we'll make it explicity to support ralt
            if ks.mod_keys.some_shift_down() || ks.mod_keys.ralt.down.is_set() {
                ks.mod_keys.lshift.active_action (ks.mod_keys.lalt.active_on_key(Tab))()
            } else { press_release(Tab) }
        }
    } );
    k.cm .add_combo_af ( k.ks.cg(Tab).m(lalt),           k.ks.ag_af (cb.clone()) );
    k.cm .add_combo_af ( k.ks.cg(Tab).m(lalt).m(shift),  k.ks.ag_af (cb.clone()) );
    k.cm .add_combo_af ( k.ks.cg(Tab).m(lalt).m(ralt ),  k.ks.ag_af (cb.clone()) );


    // setup ctrl-tab .. caps-as-ctrl for caps-tab switching (and shift/ralt combos will work out naturally in fallbacks)
    // note that caps-as-ctrl is default in fallbacks anyway, but IDE doesnt like the ctrl being pressed/rel for every tab press ..
    // .. so instead, we keep the ctrl active throughout the caps-tabbing, hence the need for the defs below
    let ks = k.ks.clone();
    let cb : AF = Arc::new (move || {
        if ks.mod_keys.caps.down.is_set() || ks.mod_keys.some_ctrl_down() {
            if ks.mod_keys.caps.down.is_set() {
                ks.in_managed_ctrl_down_state.set();
                ks.mod_keys.lctrl.ensure_active();  // this enables caps-as-ctrl for caps-tab switching
                // ^^ we're not gonna release ctrl immediately, but keep track and release when caps is released
            }
            ks.in_ctrl_tab_scroll_state.set();
            press_release(Tab)
        }
    } );
    // lets add support for this in caps tab ..
    k.cm .add_combo_af ( k.ks.cg(Tab).m(caps),            k.ks.ag_af(cb.clone()) );
    // the rest of these below (for ctrl/shift/ralt) work naturally via fallback, but we'll keep them here for reference/reminder
    //k.cm .add_combo_af ( k.ks.cg(Tab).m(ctrl),          k.ks.ag_af(cb.clone()) );
    //k.cm .add_combo_af ( k.ks.cg(Tab).m(caps).m(ralt),  k.ks.ag_af(cb.clone()) );
    //k.cm .add_combo_af ( k.ks.cg(Tab).m(ctrl).m(ralt),  k.ks.ag_af(cb.clone()) );



    // aight, and this is prob excessive, but specifically for IntelliJ, wanted to add a quick switch from tab-switcher to searchable one
    // (we'll do it by escaping it first (via space then ctrl rel), then invoking the searchable switcher)
    fn gen_ide_switcher_switch_af (k:&Krusty, key:Key) -> AF {
        let ks = k.ks.clone();
        let ctrl_key_af = k.ks.mod_keys.lctrl.active_on_key(key);
        Arc::new ( move || {
            if ks.in_ctrl_tab_scroll_state.is_set() && get_fgnd_win_exe().filter(|s| s == "idea64.exe").is_some() {
                // space defocuses from list so we wont actually switch tabs when we release the ctrl
                press_release(Space);
                // now release the ctrl so the transient-switcher popup goes away
                ks.mod_keys.lctrl.ensure_inactive();
                ks.in_managed_ctrl_down_state.clear(); ks.in_ctrl_tab_scroll_state.clear();
                // then do actual ctrl-e to bring up the persistent-switcher (ctrl-e is default shortcut in IDE for that)
                // we'll want to give a tiny delay so IDE has time to process focus changes appropriately .. just spawning is enough for that
                let ks = ks.clone();
                thread::spawn ( move || {
                    thread::sleep (time::Duration::from_millis(10));
                    ks.mod_keys.lctrl.active_on_key(E)()
                } );
            } else { ctrl_key_af() }
        } )
    }
    k.cm .add_combo_af ( k.ks.cg(Space).m(caps),  k.ks.ag_af(gen_ide_switcher_switch_af(&k, Space)) );
    //k.cm .add_combo_af ( k.ks.cg(Space).m(caps).s(mngd_ctrl_dn),  k.ks.ag_af(gen_ide_switcher_switch_af(&k, Space)) );
    //k.cm .add_combo_af ( k.ks.cg(Space).m(caps).s(mngd_ctrl_dn).s(ctrl_tab_scrl),  k.ks.ag_af(gen_ide_switcher_switch_af(&k, Space)) );



    // setup backquote .. make normal case be Delete, caps can do back-tick, and shift or ralt do its tilde
    k.cm .add_combo ( k.ks.cg(Backquote),          k.ks.ag(ExtDelete) );
    k.cm .add_combo ( k.ks.cg(Backquote).m(caps),  k.ks.ag(Backquote) );
    //k.cm .add_combo ( k.ks.cg(Backquote).m(lalt),  k.ks.ag(Backquote) );
    //k.cm .add_combo ( k.ks.cg(Backquote).m(shift),   k.ks.ag(Backquote).m(shift) );
    //k.cm .add_combo ( k.ks.cg(Backquote).m(ralt),    k.ks.ag(Backquote).m(shift) );
    // ^^ not strictly necessary as cb composition now defaults to this, but also useful to see here for reference

    // for alt-backquote, we'll set that up to give ctrl-tab as more ergo alternative, and tying in w alt-tab
    k.cm .add_combo ( k.ks.cg(Backquote).m(lalt),  k.ks.ag(Tab).m(lctrl) );
    // and for now, we'll do the same for alt-1, kinda tying in with out alt-f1 window switching
    //k.cm .add_combo ( k.ks.cg(Numrow_1).m(lalt),   k.ks.ag(Tab).m(lctrl) );
    // ^^ naah .. thats cur used in the way complex fast/slow modification of vol/bright w/ win/alt 2/3 combos or mouse scrolls


    // setup space key .. ralt-space as enter, caps-space as ctrl-space, caps-lalt-space as alt-enter for intellij
    k.cm .add_combo ( k.ks.cg(Space).m(ralt),          k.ks.ag(Enter) );                  // ralt-space       -> Enter
    k.cm .add_combo ( k.ks.cg(Space).m(caps).m(ralt),  k.ks.ag(Escape) );                 // caps-ralt-space  -> Escape
    k.cm .add_combo ( k.ks.cg(Space).m(caps).s(qks1),  k.ks.ag(Space).m(ctrl).m(shift) ); // qks1-space       -> ctrl_shift_space for IDE
    k.cm .add_combo ( k.ks.cg(Space).m(caps).s(qks2),  k.ks.ag(Space).m(ctrl).m(shift) ); // qks1-space       -> ctrl_shift_space for IDE
    //k.cm .add_combo ( k.ks.cg(Space).m(caps),          k.ks.cg(Space).m(ctrl) );        // caps-space       -> ctrl-space
    // ^^ not strictly necessary as cb composition now defaults to this, but also useful to see here for reference
    k.cm .add_combo ( k.ks.cg(Space).m(caps).m(lalt),  k.ks.ag(Enter).m(lalt) );          // caps-lalt-space  -> alt-enter
    k.cm .add_combo ( k.ks.cg(Space).m(caps).s(qks),   k.ks.ag(Enter).m(ctrl) );          // caps-q-space     -> ctrl-enter


    fn compose_seq_actions (af_a:AF, af_b:AF) -> AF {
        Arc::new ( move || { af_a(); af_b(); } )
    }
    fn ide_find_in_new_window_af (k:&Krusty) -> AF {
        compose_seq_actions ( k.ks.ag(H).m(alt).gen_af(), k.ks.ag(Enter).m(ctrl).gen_af() )
    }
    k.cm .add_combo_af ( k.ks.cg(H).m(caps).s(qks),   k.ks.ag_af (ide_find_in_new_window_af(&k)) );




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

    // win-m by default minimized all windows .. we just want to disable it .. (note that win-d still does show-desktop)
    k.cm .add_combo_af ( k.ks.cg(M).m(lwin),  k.ks.ag_af(no_action()) );

    // win-f can toggle window full-screen .. (the OS default feedback-hub will stay on double-win-f)
    k.cm .add_combo ( k.ks.cg(F).m(lwin),  k.ks.ag(F11) );

    // win-e should bring up whatever we configured for file-explorer alternative
    k.cm .add_combo_af ( k.ks.cg(E).m(lwin), k.ks.ag_af(action(start_alt_file_explorer)) );

    // win-i should start irfanview
    k.cm .add_combo_af ( k.ks.cg(I).m(lwin),  k.ks.ag_af(action(start_irfanview)) );

    // win-n should start chrome-incognitoa
    k.cm .add_combo_af ( k.ks.cg(N).m(lwin),  k.ks.ag_af(action(start_chrome_incognito)) );

    // win-caps-b for bard .. hah we'll see
    k.cm .add_combo_af ( k.ks.cg(B).m(lwin).m(caps),  k.ks.ag_af(action(start_chrome_bard)) );

    // win-v can bring up vlc .. note that this will override native win-c for win clipboard (can get that win dbl-win-v)
    k.cm .add_combo_af ( k.ks.cg(V).m(lwin),  k.ks.ag_af(action(start_vlc)) );

    // we'll setup win-C to quickly bring up chrome Tabs-Outliner via switche Ctrl-Alt-F20 hotkey
    k.cm .add_combo ( k.ks.cg(C).m(lwin),  k.ks.ag(F20).m(alt).m(lctrl) );      // win-c -> ctrl-F20 .. switche tabs-outliner

    // we'll set win-s to quickly bringup the windows start menu via ctrl-esc shortcut (what double win press also does)
    k.cm .add_combo ( k.ks.cg(S).m(lwin),  k.ks.ag(Escape).m(lctrl) );

    // and win-ctrl-s to actually bring up the windows settings (via default win-i)
    k.cm .add_combo ( k.ks.cg(S).m(caps).m(lwin),  k.ks.ag(I).m(lwin) );

    // and win-a to bring up the launchy popup (which is win-a in practice, but for us would only be dbl-win-a otherwise)
    // (we've assigned that to win-ctrl-shift-a to not interfere with win-a doing windows-action center by default)
    k.cm .add_combo ( k.ks.cg(A).m(lwin),  k.ks.ag(A).m(lwin).m(lctrl).m(lshift) );

    // we'll also setup a shortcut to pull up our taskbar shortcuts folder ...
    // (by focusing on tray btn first, then nav to our toolbar)
    //fn taskbar_focus_yak_tools_bar (k:&Krusty) -
    let cb_focus_yak_tools_bar : AF = {
        let hk_tray_focus = k.ks.clone().ag(B).m(lwin).gen_af();
        let af_shift_tab  = k.ks.clone().ag(Tab).m(lshift).gen_af();
        let af_ext_down   = k.ks.clone().ag(ExtDown).gen_af();
        Arc::new ( move || { hk_tray_focus(); af_shift_tab(); af_shift_tab(); af_ext_down(); af_ext_down(); } )
    };
    k.cm .add_combo_af ( k.ks.cg(Numrow_1).m(lwin), k.ks.ag_af(cb_focus_yak_tools_bar) );

    // in cur laptop, Fn-F6/F7 do brightness, but at +10 incrs .. set them to do small incrs with alt combos
    k.cm .add_combo_af ( k.ks.cg(F6).m(lalt),  k.ks.ag_af (Arc::new (|| incr_brightness(-1))) );
    k.cm .add_combo_af ( k.ks.cg(F7).m(lalt),  k.ks.ag_af (Arc::new (|| incr_brightness(1))) );

    // actually, since we use win-1/2/3 as vol mute/down/up, might as well also set alt-1/2/3 for brightness zero/down/up
    // .. and we'll set these to have a 'fine-mode' when qks-1 key is held with alt
    // (note that numrow 1/2/3 with caps are qks* keys, so they cant be used with any caps combos as those would be silent!)
    // (also note that e.g. by explicitly specifying qks-x for Numrow_x, it will be marked for consumption etc (i.e. suppresses key events until released))
    k.cm .add_combo_af ( k.ks.cg(Numrow_1).m(lalt).s(qks1),  k.ks.ag_af (no_action()) );
    k.cm .add_combo_af ( k.ks.cg(Numrow_1).m(lalt).s(qks2),  k.ks.ag_af (no_action()) );
    k.cm .add_combo_af ( k.ks.cg(Numrow_1).m(lalt).s(qks3),  k.ks.ag_af (no_action()) );
    k.cm .add_combo_af ( k.ks.cg(Numrow_2).m(lalt),          k.ks.ag_af (Arc::new (|| incr_brightness(-4))) );
    k.cm .add_combo_af ( k.ks.cg(Numrow_3).m(lalt),          k.ks.ag_af (Arc::new (|| incr_brightness( 4))) );
    k.cm .add_combo_af ( k.ks.cg(Numrow_2).m(lalt).s(qks1),  k.ks.ag_af (Arc::new (|| incr_brightness(-1))) );
    k.cm .add_combo_af ( k.ks.cg(Numrow_3).m(lalt).s(qks1),  k.ks.ag_af (Arc::new (|| incr_brightness( 1))) );

    // win-2 is vol down, win-3 is vol up, win-1 can do fast-mode ..... update: win-1 moved to trigger yak-tools-bar
    //k.cm .add_combo_af ( k.ks.cg(Numrow_1).m(lwin).s(qks1),  k.ks.ag_af (no_action()) );
    //k.cm .add_combo_af ( k.ks.cg(Numrow_1).m(lwin).s(qks2),  k.ks.ag_af (no_action()) );
    //k.cm .add_combo_af ( k.ks.cg(Numrow_1).m(lwin).s(qks3),  k.ks.ag_af (no_action()) );
    k.cm .add_combo_af ( k.ks.cg(Numrow_2).m(lwin),          k.ks.ag_af (Arc::new (|| incr_volume(-1))) );
    k.cm .add_combo_af ( k.ks.cg(Numrow_3).m(lwin),          k.ks.ag_af (Arc::new (|| incr_volume( 1))) );
    //k.cm .add_combo_af ( k.ks.cg(Numrow_2).m(lwin).s(qks1),  k.ks.ag_af (Arc::new (|| incr_volume(-1))) );
    //k.cm .add_combo_af ( k.ks.cg(Numrow_3).m(lwin).s(qks1),  k.ks.ag_af (Arc::new (|| incr_volume( 1))) );

    // for now we'll use win-alt-F1 as mute .. we'll see if makes sense to keep it that way
    k.cm .add_combo ( k.ks.cg(F1).m(lwin).m(lalt),  k.ks.ag (VolumeMute) );


    // win-f1 play/pause, caps-f1 toggle mute, base-case: switche-invoke alt-F1: switche silent-switch, ralt for actual F1
    k.cm .add_combo ( k.ks.cg(F1),          k.ks.ag(F15) );                   // switche invoke/next
    //k.cm .add_combo_af ( k.ks.cg(F1),          k.ks.ag_af (Arc::new (|| switche_next_w_taskbar_pop())) );                   // switche next
    //k.cm .add_combo ( k.ks.cg(F1).m(shift), k.ks.ag(F17) );                 // switche prev
    k.cm .add_combo ( k.ks.cg(F1).m(shift), k.ks.ag(F16).m(shift) );          // switche prev (passthrough shift-F16 instead of F17 is more efficient)
    k.cm .add_combo ( k.ks.cg(F1).m(lalt),  k.ks.ag(F19).m(alt).m(ctrl) );    // switche no-popup next switch
    k.cm .add_combo ( k.ks.cg(F1).m(ralt),  k.ks.ag(F1) );
    k.cm .add_combo ( k.ks.cg(F1).m(caps),  k.ks.ag(VolumeMute) );
    //k.cm .add_combo ( k.ks.cg(F1).m(lwin),  k.ks.ag(MediaPlayPause) );
    // ^^ media keys seems to get captured by elev apps in fgnd (e.g. switche) and not pass to musicbee .. so we'll setup alts
    k.cm .add_combo ( k.ks.cg(F1).m(lwin), k.ks.ag(VolumeUp).m(lctrl).m(lshift) );  // gotta match w music-bee/winamp settings

    // and keeping w the theme, set caps-win-F1 (key with vol-mute printed on it) to toggle microphone mute
    //k.cm .add_combo_af ( k.ks.cg(F1).m(caps).m(lwin),  k.ks.ag_af (Arc::new (|| {mic_mute_toggle(); open_mic_cpl();})) );
    k.cm .add_combo_af ( k.ks.cg(F1).m(caps).m(lwin),  k.ks.ag_af (Arc::new (|| mic_mute_toggle())) );
    // we'll set Alt-F2 to bring chrome tabs-outliner (via switche) to keep w the theme of Alt-F<n> keys for task switching
    k.cm .add_combo ( k.ks.cg(F2).m(lalt),  k.ks.ag(F20).m(alt).m(ctrl) );     // switche no-popup tabs-outliner switch

    // we'll overload regular F2 with special IDE scala-console action if IDEA window is foreground
    fn gen_line_to_repl_action (k:&Krusty) -> AF {
        // note that its better not to specify no-modkey-guard-wrap below in case the action moves from F2 to some combo with a mod-key
        let line_sel         = k.ks.ag(ExtHome).m(alt).m(shift).gen_af();  // IDE alt-shift-home to sel line
        let line_repl_send   = k.ks.ag(ExtEnd ).m(alt).m(shift).gen_af();  // IDE alt-shift-end to send sel to repl
        let caret_sel_start  = k.ks.ag(ExtLeft).gen_af();                  // unselect the line (caret to line beginning)
        let caret_line_start = k.ks.ag(ExtHome).gen_af();                  // caret to beginning of first word in line
        Arc::new ( move || { line_sel(); line_repl_send(); caret_sel_start(); caret_line_start() } )
    }
    fn gen_f2_action(k:&Krusty) -> AF {
        let af = gen_line_to_repl_action(k);
        Arc::new ( move || {
            if get_fgnd_win_exe().filter(|s| s == "idea64.exe").is_some() { af() }
            else { press_release(F2) }
        } )
    }
    k.cm .add_combo_af ( k.ks.cg(F2),  k.ks.ag_af(gen_f2_action(&k)) );

    // and caps-F2 will simply send selection to repl as is (w/o selecting full line etc)
    k.cm .add_combo ( k.ks.cg(F2).m(caps),  k.ks.ag(End).m(alt).m(shift) );



    // want win-f2 for next with some initial skip .. we'll use caps-win-f2 for prev, so we'll set it up for both
    // note that our mechanism for wrapping mod-key-state restoring guards operates via AFs, hence setting those up (instead of fns)

    // skips work by alt-ctrl-volUp (needs to guard win-inactive since its on win-combo)
    fn media_skips_action (n_skips:u32, ks:&KrustyState, fwd_not_bkwd:bool) -> AF {
        let action_key = if fwd_not_bkwd {VolumeUp} else {VolumeDown};
        //ks.mod_keys.lwin.inactive_action ( ks.mod_keys.lalt.active_action ( ks.mod_keys.lctrl.active_action (
        ks.mod_keys.lalt.active_action ( ks.mod_keys.lctrl.active_action (
            Arc::new ( move || { (0 .. n_skips) .into_iter() .for_each (|_| { press_release(action_key) }) } )
    ) ) }
    // ^^ gives an AF with specified number of skips

    fn media_next_action (ks:&KrustyState, next_not_prev:bool)  -> AF {
        let media_next_af = {
            if next_not_prev { ks.ag(VolumeUp  ).m(lalt ).m(lshift).gen_af() }
            else             { ks.ag(VolumeDown).m(lalt ).m(lshift).gen_af() }
        };
        let media_next_skips_af = media_skips_action (2, ks, true);
        Arc::new ( move || {
            media_next_af();
            let mnsaf = media_next_skips_af.clone();  // clone again to move into spawned thread (spawned since combos run in single queued side-thread)
            thread::spawn ( move || { thread::sleep(time::Duration::from_millis(2000));  mnsaf(); } );
    } ) }
    // note that in the above, although we're using win-combos, we dont have to wrap in win-action guards anymore as win is now as TMK_dbl

    // win-f2 for next with some initial skip
    k.cm .add_combo_af ( k.ks.cg(F2).m(lwin),          k.ks.ag_af (media_next_action (&k.ks, true )) );
    k.cm .add_combo_af ( k.ks.cg(F2).m(lwin).m(caps),  k.ks.ag_af (media_next_action (&k.ks, false)) );

    // win-f3 for skip forward a bit (w/ caps for rewind)
    k.cm .add_combo_af ( k.ks.cg(F3).m(lwin),           k.ks.ag_af (media_skips_action (1, &k.ks, true )) );
    k.cm .add_combo_af ( k.ks.cg(F3).m(lwin).m(caps),   k.ks.ag_af (media_skips_action (2, &k.ks, false)) );
    k.cm .add_combo_af ( k.ks.cg(F3).m(lwin).m(shift),  k.ks.ag_af (media_skips_action (2, &k.ks, false)) );

    // gaah, for track trawling, even that is being annoying to press, wanted to set up right hand alternative too
    k.cm .add_combo_af ( k.ks.cg(Down ).m(caps_dbl), k.ks.ag_af (media_next_action  (&k.ks, true )) );
    k.cm .add_combo_af ( k.ks.cg(Up   ).m(caps_dbl), k.ks.ag_af (media_next_action  (&k.ks, false)) );
    k.cm .add_combo_af ( k.ks.cg(Right).m(caps_dbl), k.ks.ag_af (media_skips_action (1, &k.ks, true)) );
    k.cm .add_combo_af ( k.ks.cg(Left ).m(caps_dbl), k.ks.ag_af (media_skips_action (1, &k.ks, false)) );

    // hmm, now that we have latching layer states, lets set that on F1 for this track trawling w arrow keys!
    k.cm .add_combo_af ( k.ks.cg(Down )  .s(latch_1),  k.ks.ag_af (media_next_action  (&k.ks, true )) );
    k.cm .add_combo_af ( k.ks.cg(Up   )  .s(latch_1),  k.ks.ag_af (media_next_action  (&k.ks, false)) );
    k.cm .add_combo_af ( k.ks.cg(Right)  .s(latch_1),  k.ks.ag_af (media_skips_action (1, &k.ks, true)) );
    k.cm .add_combo_af ( k.ks.cg(Left )  .s(latch_1),  k.ks.ag_af (media_skips_action (1, &k.ks, false)) );



    // escape is just escape, but we want it to do press-release immediately (so switche is faster)
    k.cm .add_combo ( k.ks.cg(Escape),                                          k.ks.ag(Escape) );
    k.cm .add_combo ( k.ks.cg(Escape).m(caps),                                  k.ks.ag(Escape) );

    // use the apps key to send shift-escape ..
    k.cm .add_combo ( k.ks.cg(Apps),            k.ks.ag(Escape).m(shift) );

    // for alt-escape, we want to override the default send-to-back behavior, as it has issues detailed in notes
    // .. we'll use alt-escape to switch to next window in switche and send cur to back
    //  .. however if switche is fgnd, we simply send a esc so the swiche window is simply dismissed instead
    // k.cm .add_combo_af ( k.ks.cg(Escape).m(lalt),  k.ks.ag_af(no_action()) );
    let switche_next_af = k.ks.ag(F19).m(alt).m(ctrl).gen_af();
    let non_alt_esc_af  = k.ks.mod_keys.lalt.inactive_on_key(Escape);
    let alt_esc_action = Arc::new ( move || {
        let h = win_get_fgnd();
        if get_exe_by_hwnd(h).filter(|s| s == "switche.exe").is_some() { non_alt_esc_af(); }
        else { switche_next_af(); win_send_to_back(h); }
    } );
    //k.cm .add_combo_af ( k.ks.cg(Escape).m(lalt),  k.ks.ag_af (action (|| {win_send_to_back(win_get_fgnd()); })) );
    k.cm .add_combo_af ( k.ks.cg(Escape).m(lalt),  k.ks.ag_af (alt_esc_action) );


    // we have win-mouse window drag/resize .. we'd like to cancel any in-progress action via escape
    fn gen_cancel_win_mouse_action (key:Key, ks:&KrustyState) -> AF {
        let ks = ks.clone();
        Arc::new ( move || {
            if ks.mouse.lbtn.down.is_set() {
                ks.mouse.lbtn.consumed.set();
                handle_pointer_action_cancel (&ks);
            } else {
                //press_release(key)
                // ^^ hmm, instead of win-esc being just esc otherwise, we'll use it as window minimize
                if key==Escape { win_fgnd_min() } else { press_release(key) }
            }
        } )
    }
    // we'll allow Escape to cancel in-progress win-drag-to-move/resize operations
    k.cm .add_combo_af ( k.ks.cg(Escape).m(lwin),          k.ks.ag_af (gen_cancel_win_mouse_action (Escape, &k.ks)) );
    k.cm .add_combo_af ( k.ks.cg(Escape).m(lwin).m(caps),  k.ks.ag_af (gen_cancel_win_mouse_action (Escape, &k.ks)) );
    // and since Esc is hard to press w caps-win, we'll let Q do the same too
    //k.cm .add_combo_af ( k.ks.cg(Q).m(lwin),                 k.ks.ag_af (gen_cancel_win_mouse_action (Q, &k.ks)) );
    k.cm .add_combo_af ( k.ks.cg(Q).m(lwin).m(caps).s(qks),  k.ks.ag_af (gen_cancel_win_mouse_action (Q, &k.ks)) );
    // ^^ we'll let caps-win-q continue to do that, but reallocate win-q to alt-q for everything-search (to match other win-a/s etc)
    k.cm .add_combo ( k.ks.cg(Q).m(lwin),                   k.ks.ag(Q).m(lalt) );
    k.cm .add_combo ( k.ks.cg(Q).m(lwin).m(caps).s(qks),    k.ks.ag(Q).m(lalt).m(lctrl) );
    k.cm .add_combo ( k.ks.cg(Q).m(lalt).m(caps).s(qks),    k.ks.ag(Q).m(lalt).m(lctrl) );
    // ^^ note that alt-q is set in 'everything' as global invocation hotkey, and alt-ctrl-q as new search window hotkey



    // 'w' should have caps-ctrl mapping, but when w/ alt, send alt-f4 (to close all-tabs, windows etc)
    //k.cm .add_combo ( k.ks.cg(W).m(caps).m(shift), k.ks.cg(F4).m(alt) );
    // ^^ note: initially we wanted this with caps-shift-w, but turns out (at least on my kbd, turns out caps+shift+[F1, 2, w, s, x]
    // >  dont produce any key event at the hook at all .. nothing .. its like the keyboard driver not sending those out
    // funnily enough, there's a bunch of complaints about specifically those keys for dell/hp laptops .. looks like hardware
    // >  appears to be a common kbd pcb layout issue .. heres from 2007: (https://www.joachim-breitner.de/blog/250-Shift-Caps-2)
    // sooo .. to makeup, we'll do alt-caps-w do the alt-f4 business instead
    k.cm .add_combo ( k.ks.cg(W).m(caps).m(lalt),  k.ks.ag(F4).m(lalt) );
    // huh .. we seem to be instinctively trying to use win-w for that .. so we'll just set that up too
    k.cm .add_combo ( k.ks.cg(W).m(lwin),  k.ks.ag(F4).m(lalt) );




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
        k.cm .add_combo_af ( k.ks.cg(key).m(caps),          k.ks.ag_af (base_action(l2k)) );
        k.cm .add_combo_af ( k.ks.cg(key).m(caps).s(word),  k.ks.ag_af (wafg(l2k)) );
        k.cm .add_combo_af ( k.ks.cg(key).m(caps).s(fast),  k.ks.ag_af (fafg(l2k)) );

        // selection actions are via wrapping those with shift press-release
        k.cm .add_combo_af ( k.ks.cg(key).m(caps).s(sel),          k.ks.ag_af (base_action(l2k)) .m(shift) );
        k.cm .add_combo_af ( k.ks.cg(key).m(caps).s(sel).s(word),  k.ks.ag_af (wafg(l2k)) .m(shift) );
        k.cm .add_combo_af ( k.ks.cg(key).m(caps).s(sel).s(fast),  k.ks.ag_af (fafg(l2k)) .m(shift) );

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

        k.cm .add_combo_af ( k.ks.cg(key).m(caps).s(del),          k.ks.ag_af(da ) );
        k.cm .add_combo_af ( k.ks.cg(key).m(caps).s(del).s(word),  k.ks.ag_af(dwa) );
        k.cm .add_combo_af ( k.ks.cg(key).m(caps).s(del).s(fast),  k.ks.ag_af(dfa) );


        // also add these to l2-key registry that gets used to enable their l2+ fallbacks in combo processor
        //.. the fallbacks will layer extensive l2+ mod-key combos functionality on the l2keys (e.g alt/ctrl etc combos on nav arrows keys)
        //.. in brief, w caps + l2-key, alt, ctrl, shift, and ralt-as-shift, qks1-as-ctrl will layer on the l2k-nav-key!!
        k.cm .register_l2_key (key, l2k);

        // and on lalt w/o caps, we'll layer qks1 as ctrl like it is used elsewhere
        k.cm .add_combo ( k.ks.cg(key).m(lalt).s(qks1),  k.ks.ag(key).m(alt).m(ctrl) );

        // plus we'll layer caps-as-ctrl on caps-dbl for these for easy access
        k.cm .add_combo ( k.ks.cg(key).m(caps_dbl),  k.ks.ag(key).m(ctrl) );
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
    k.cm .add_combo_af ( k.ks.cg(Space).m(caps).s(sel ),           k.ks.ag_af (wsa.clone()) );
    //k.cm .add_combo_af ( k.ks.cg(Space).m(caps).s(word),           k.ks.ag_af (wsa.clone()) );
    k.cm .add_combo_af ( k.ks.cg(Space).m(caps).s(sel ).s(word),   k.ks.ag_af (wsa        ) );

    // (word del action is a composite of move-to-word-end then del-to-word-beginning)
    let _wda = Arc::new ( || {
        LCtrl.press(); press_release(ExtRight); shift_press_release(ExtLeft); LCtrl.release();
        press_release(Backspace);
    } );
    //k.cm .add_combo_af ( k.ks.cg(Space).m(caps).s(del),  k.ks.ag_af(wda) );
    // ^^ naah, not worthwhile as can do eqv from just caps-<del>-<arrows> .. instead we'll use that for easy 'Enter'

    // ralt-space for Enter was causing thumb pain, so other quick substitutes for that
    k.cm .add_combo ( k.ks.cg(Space).m(caps).s(del),         k.ks.ag(Enter) );    // caps-d-space --> Enter
    k.cm .add_combo ( k.ks.cg(Space).m(caps).s(word),        k.ks.ag(Enter) );    // caps-f-space --> Enter
    k.cm .add_combo ( k.ks.cg(Space).m(caps).s(word).s(del), k.ks.ag(Enter) );    // caps-d-f-space --> Enter

    // (line sel/del actions are composite of move-to-line-end then sel/del-to-line-start)
    let lsa = Arc::new ( || { press_release(ExtEnd); shift_press_release(ExtHome); } );
    let lda = Arc::new ( || { press_release(ExtEnd); shift_press_release(ExtHome); press_release(Backspace); } );
    k.cm .add_combo_af ( k.ks.cg(Space).m(caps).m(lalt).s(sel ),           k.ks.ag_af (lsa.clone()) );
    //k.cm .add_combo_af ( k.ks.cg(Space).m(caps).m(lalt).s(word),           k.ks.ag_af (lsa.clone()) );
    k.cm .add_combo_af ( k.ks.cg(Space).m(caps).m(lalt).s(sel ).s(word),   k.ks.ag_af (lsa) );
    k.cm .add_combo_af ( k.ks.cg(Space).m(caps).m(lalt).s(del),            k.ks.ag_af (lda) );


    // this one is here simply coz its Space and on l2, but we'll use it for Find-Window trigger in IDE via Ctrl-Enter
    k.cm .add_combo ( k.ks.cg(Space).m(caps).s(fast),  k.ks.ag(Enter).m(ctrl) );        // caps-r-space --> Ctrl-Enter
    // similar for this one on expanding things in find window (for txt-search)
    k.cm .add_combo ( k.ks.cg(Slash).m(caps).s(word),  k.ks.ag(Equal).m(ctrl).m(alt).m(shift) );
    // (but since we've essentially associated Enter with E, we've added combos e.g. caps-q-e for ctrl-Enter etc there too)


    // we can further add cut/copy/paste actions on the sel mode (caps-e-x/c/v)
    fn gen_wxcv_af (key:Key) -> AF { Arc::new ( move || {
        LCtrl.press(); press_release(ExtRight); shift_press_release(ExtLeft); press_release(key); LCtrl.release();
    } ) }
    k.cm .add_combo_af ( k.ks.cg(X).m(caps).s(sel),  k.ks.ag_af (gen_wxcv_af(X)) );
    k.cm .add_combo_af ( k.ks.cg(C).m(caps).s(sel),  k.ks.ag_af (gen_wxcv_af(C)) );
    k.cm .add_combo_af ( k.ks.cg(V).m(caps).s(sel),  k.ks.ag_af (gen_wxcv_af(V)) );

    // specifically for IDE, we can also add move actions to nearest matching brace/paren (via ctrl-shift-p)
    k.cm .add_combo ( k.ks.cg(Numrow_9).m(caps).s(word),  k.ks.ag(P).m(ctrl).m(shift) );
    k.cm .add_combo ( k.ks.cg(Numrow_0).m(caps).s(word),  k.ks.ag(P).m(ctrl).m(shift) );
    // unfortunately, there's no support in IDE for selecting while doing so !! :(

    // and lets support nav to code-block (brace) start/end (and the IDE supports select for this, but its much less useful, oh well)
    //k.cm .add_combo ( k.ks.cg(LBracket).m(lalt),       k.ks.ag(LBracket).m(alt) );
    //k.cm .add_combo ( k.ks.cg(RBracket).m(lalt),       k.ks.ag(RBracket).m(alt) );
    k.cm .add_combo ( k.ks.cg(LBracket).m(caps).s(sel),  k.ks.ag(LBracket).m(alt).m(shift) );
    k.cm .add_combo ( k.ks.cg(RBracket).m(caps).s(sel),  k.ks.ag(RBracket).m(alt).m(shift) );

    // also lets add an even quicker way to switche column mode during IDE edit (using the cur alt-shift-c)
    // the I and Comma both do toggle but its better to have both for muscle memory on shift-move up/down
    k.cm .add_combo ( k.ks.cg(I       ).m(lalt).m(caps).s(sel),  k.ks.ag(C).m(alt).m(shift) );
    k.cm .add_combo ( k.ks.cg(Comma   ).m(lalt).m(caps).s(sel),  k.ks.ag(C).m(alt).m(shift) );
    k.cm .add_combo ( k.ks.cg(Numrow_9).m(caps).s(sel),          k.ks.ag(C).m(alt).m(shift) );




    // then the caps-win combo (l4?) actions :

    // caps-win-U should vert-max (via shift-win-up) if not already, or else restore window from vert-max
    k.cm .add_combo_af ( k.ks.cg(U).m(caps).m(lwin),  k.ks.ag_af (Arc::new (|| win_fgnd_toggle_vertmax())) );
    // caps-win-m should maximize (via win-m) if not, else restore from max
    k.cm .add_combo_af ( k.ks.cg(M).m(caps).m(lwin),  k.ks.ag_af (Arc::new (|| win_fgnd_toggle_max())) );
    // caps-win-t should toggle always on top for fgnd window
    k.cm .add_combo_af ( k.ks.cg(T).m(caps).m(lwin),  k.ks.ag_af (Arc::new (|| win_fgnd_toggle_always_on_top())) );
    // caps-win-n should minimize
    //k.cm .add_combo_af ( k.ks.cg(N).m(caps).m(lwin),  k.ks.ag_af (Arc::new (|| win_fgnd_min())) );
    // ^^ actually, we already do that with win-esc which is easier .. so we'll repurpose that for non-incognito chrome window
    k.cm .add_combo_af ( k.ks.cg(N).m(caps).m(lwin),  k.ks.ag_af(action(start_chrome)) );

    // we also have some additional more drastic ones with double-caps-win combos
    k.cm .add_combo_af ( k.ks.cg(S).m(caps_dbl).m(lwin),  k.ks.ag_af (Arc::new (|| win_fgnd_toggle_titlebar())) );
    k.cm .add_combo_af ( k.ks.cg(C).m(caps_dbl).m(lwin),  k.ks.ag_af (Arc::new (|| win_fgnd_toggle_titlebar())) );
    k.cm .add_combo_af ( k.ks.cg(T).m(caps_dbl).m(lwin),  k.ks.ag_af (Arc::new (|| win_fgnd_toggle_always_on_top())) );

    fn setup_win_move_key (k:&Krusty, key:Key, wmfn:fn(i32, i32), dx:i32, dy:i32, m:i32) {
        // we'll setup caps-win combos for regular move/stretch etc, and caps-ctrl or caps-qks1 combos for fineer control
        k.cm .add_combo_af ( k.ks.cg(key).m(caps).m(lwin).m(lctrl),  k.ks.ag_af (Arc::new (move || wmfn (dx, dy) )) );
        k.cm .add_combo_af ( k.ks.cg(key).m(caps).m(lwin).s(qks1),   k.ks.ag_af (Arc::new (move || wmfn (dx, dy) )) );
        k.cm .add_combo_af ( k.ks.cg(key).m(caps).m(lwin),           k.ks.ag_af (Arc::new (move || wmfn (dx*m, dy*m) )) );
    }
    // caps-win-[j,k,i,comma] should  move window [left, right, top, bottom] respectively
    setup_win_move_key (&k, J,     win_fgnd_move_rel, -1,  0, 20 );
    setup_win_move_key (&k, K,     win_fgnd_move_rel,  1,  0, 20 );
    setup_win_move_key (&k, I,     win_fgnd_move_rel,  0, -1, 20 );
    setup_win_move_key (&k, Comma, win_fgnd_move_rel,  0,  1, 20 );

    // caps-win-[h,semicolon,period,o] should stretch window [narrower, wider, shorter, taller] respectively
    setup_win_move_key ( &k, H,         win_fgnd_stretch,  -1,   0, 20 );
    setup_win_move_key ( &k, O,         win_fgnd_stretch,   0,  -1, 20 );
    setup_win_move_key ( &k, Period,    win_fgnd_stretch,   0,   1, 20 );
    //setup_win_move_key ( &k, L,       win_fgnd_stretch,   1,   0, 20 );     // win-L is reserved by windows for lock
    setup_win_move_key ( &k, Semicolon, win_fgnd_stretch,   1,   0, 20 );



    // some additional caps-win combos
    // caps-win-c being used to launch winmerge diff from last two clipboard entries
    k.cm .add_combo_af ( k.ks.cg(C).m(caps).m(lwin),  k.ks.ag_af (Arc::new (|| start_winmerge_clipboard())) );
    // gaah we'll just throw in iDEA diff for drag-drop diffing (just coz winmerge doesnt do dark mode)
    //k.cm .add_combo_af  ( k.ks, k.ks.cg(C).m(lwin),  k.ks.cg_af (Arc::new (|| start_idea_diff() )));
    // ^^ cant do from here, turns out idea diff from cmd line can ONLY be opened with two files pointed, unlike empty from Idea shortcut!





    // then we can add in any l4 quick-keys shortcuts combos we want
    // note: there are 3 quick-keys modes (qks, qks2, qks3) on keys (q, 2, 3) respectively! .. all are pretty ergonomic!
    // note also that during combo gen, the 'caps' mod-key is auto added to caps-based modes, incl these quick-keys

    // we'll add some nav overloading for IDES on qks2 for starters!!
    // and this one to travel along bookmarks in IDE
    k.cm .add_combo ( k.ks.cg(I    ).m(caps).s(qks2),  k.ks.ag(ExtUp  ).m(alt).m(ctrl).m(shift) );
    k.cm .add_combo ( k.ks.cg(Comma).m(caps).s(qks2),  k.ks.ag(ExtDown).m(alt).m(ctrl).m(shift) );
    // and to toggle a bookmark at the current caret location
    k.cm .add_combo ( k.ks.cg(U).m(caps).s(qks2),  k.ks.ag(F11).m(ctrl).m(shift) );
    // and to bring up the bookmarks viewer
    k.cm .add_combo ( k.ks.cg(K).m(caps).s(qks2),  k.ks.ag(F11).m(shift) );


    // but for switche-hotkeys, instead of caps, we'll do on lalt, and on qks1
    k.cm .add_combo ( k.ks.cg(L).m(lalt).s(qks1),  k.ks.ag(F19).m(alt).m(ctrl) );   // L switches to last
    k.cm .add_combo ( k.ks.cg(O).m(lalt).s(qks1),  k.ks.ag(F20).m(alt).m(ctrl) );   // O switches to TabsOutliner
    k.cm .add_combo ( k.ks.cg(N).m(lalt).s(qks1),  k.ks.ag(F21).m(alt).m(ctrl) );   // N switches to Notepad++
    k.cm .add_combo ( k.ks.cg(I).m(lalt).s(qks1),  k.ks.ag(F22).m(alt).m(ctrl) );   // I switches to first IDEA window
    k.cm .add_combo ( k.ks.cg(M).m(lalt).s(qks1),  k.ks.ag(F23).m(alt).m(ctrl) );   // M switches to winamp (music)
    k.cm .add_combo ( k.ks.cg(B).m(lalt).s(qks1),  k.ks.ag(F24).m(alt).m(ctrl) );   // B switches to first browser window







    // win-groups actions combo setups
    // first lets set up a helper fn that sets up a provided action for all three wingroups
    fn set_win_grp_af_combos <F> (k:&Krusty, key:Option<Key>, f:F)
        where F : Fn (&KrustyState, WinGroups_E) + Clone + Send + Sync + 'static
    {
        let key = |wg:WinGroups_E| key.unwrap_or_else(|| wg.key());
        // ^^ for activation, no key will be supplied, and we'll use the trigger key associated for that particular win-group
        let gen_af = |wg:WinGroups_E, f:&F| {
            let ks = k.ks.clone(); let f = f.clone();
            Arc::new ( move || f (&ks, wg) )
        };
        k.cm .add_combo_af ( k.ks.cg(key(wg1)).s(qks1).m(caps).m(lwin),  k.ks.ag_af (gen_af (wg1, &f)) );
        k.cm .add_combo_af ( k.ks.cg(key(wg2)).s(qks2).m(caps).m(lwin),  k.ks.ag_af (gen_af (wg2, &f)) );
        k.cm .add_combo_af ( k.ks.cg(key(wg3)).s(qks3).m(caps).m(lwin),  k.ks.ag_af (gen_af (wg3, &f)) );
    }
    // finally we can now set up actions (which will be set up for each of the three win-groups)
    set_win_grp_af_combos ( &k, None,    |ks,wg| ks.win_groups.toggle_grp_activation(wg) );
    set_win_grp_af_combos ( &k, Some(T), |ks,wg| ks.win_groups.toggle_grp_always_on_top(wg) );
    set_win_grp_af_combos ( &k, Some(W), |ks,wg| ks.win_groups.close_grp_windows(wg) );






    // this one is a hack around intellij not giving a shortcut action to hide floating tool windows
    fn _ide_float_tools_clear (ks:&KrustyState) {
        let ks = ks.clone();
        thread::spawn ( move || {  //println!("---");
            let mut n_trials = 10;  // how many times do we want to keep finding and escaping matching windows
            while {
                let hwnd = win_find_by_win_class ("SunAwtDialog");
                // ^^ Note that if the found window does not close on escape, that same fgnd window might keep being retried ignoring others
                if hwnd.0 != 0 {
                    win_set_fgnd(hwnd);
                    let mut n_wait = 10;  // how many times to wait <t>ms for the set fgnd to work
                    while {
                        thread::sleep(time::Duration::from_millis(10)); //print!("{:?}",'.');
                        n_wait -= 1;
                        hwnd != win_get_fgnd() && n_wait > 0
                    } { }
                    ks.mod_keys.lalt.inactive_action(base_action(Escape))();
                    ks.mod_keys.lalt.inactive_action(shift_action(Escape))();
                    // ^^ ugh turns out theres a bug in IntelliJ where if the tool window has a 'split' it doesnt close by Esc .. wth!
                    thread::sleep(time::Duration::from_millis(50));
                }
                n_trials -= 1;
                hwnd.0 != 0  && n_trials > 0
            } { }
        } );
    }
    fn ide_float_tools_clear (do_restore:bool) {
        static hidden_hwnds : Lazy <Arc <RwLock <FxHashSet <Hwnd>>>> = Lazy::new (|| Arc::new (RwLock::new (FxHashSet::default())));
        thread::spawn ( move || {
            if do_restore {
                let hwnds = hidden_hwnds.read().unwrap().iter().copied().collect::<Vec<Hwnd>>();
                hwnds .iter() .for_each (|&hwnd| {
                    if win_check_hwnd(hwnd) { win_activate(hwnd); }
                    else { hidden_hwnds.write().unwrap().remove(&hwnd); }
                });
            } else {
                win_get_ide_dialog_hwnds() .into_iter() .for_each (|hwnd| {
                    win_hide(hwnd);
                    hidden_hwnds.write().unwrap().insert(hwnd);
                } );
        } } );
    }
    k.cm .add_combo_af ( k.ks.cg(Numrow_0).m(lalt), k.ks.ag_af (Arc::new (move || ide_float_tools_clear(true))) );
    //k.cm .add_combo_af ( k.ks.cg(Numrow_9).m(lalt), k.ks.ag_af (Arc::new (move || ide_float_tools_clear(true))) );



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






    // finally we can start binding key maps .. first the specialized handling for mode/latch trigger keys
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

