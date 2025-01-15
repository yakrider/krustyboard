use std::sync::Mutex;
use std::time::Instant;
use once_cell::sync::{Lazy, OnceCell};
use crate::*;



pub fn build_grid_provider (k:&Krusty) -> GetGridFn  {

    use { KbdKey::*, ModKey::*};

    let (ks, wel, qb) = (k.ks, k.wel, k.qb);


    let cell = ActionCell {
        label : "Volume".into(),
        on_wheel_bkwd : ag().k(VolumeDown).gen_af(),    // vol down
        on_wheel_frwd : ag().k(VolumeUp  ).gen_af(),    // vol up
        on_click      : ag().k(VolumeMute).gen_af(),    // mute
        ..Default::default()
    };
    static _volume : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let volume = _volume .get_or_init ( move || { Arc::new (cell) } );
    let volume = || volume.clone();


    let cell = ActionCell {
        label : "Tracks".to_string(),
        on_wheel_bkwd : media_next_action (ks, true),                  // next track
        on_wheel_frwd : media_next_action (ks, false),                 // prev track
        on_click      : ag().k(VolumeUp).m(lctrl).m(lshift).gen_af(),  // play / pause
        ..Default::default()
    };
    static _tracks : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let tracks = _tracks .get_or_init ( move || { Arc::new (cell) } );
    let tracks = || tracks.clone();


    let cell = ActionCell {
        label : "Scrub".to_string(),
        on_wheel_bkwd : media_skips_action (1, ks, true),              // skip fwd  on track-bar
        on_wheel_frwd : media_skips_action (1, ks, false),             // skip bkwd on track-bar
        on_click      : ag().k(VolumeUp).m(lctrl).m(lshift).gen_af(),  // play / pause
        ..Default::default()
    };
    static _scrub : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let scrub = _scrub .get_or_init ( move || { Arc::new (cell) } );
    let scrub = || scrub.clone();


    // for switche task-switching ..
    let sw_wh_af = Arc::new ( move || {
        // this is a lil funky, because we want to get into alt-tab and clear out fsc (which will hide qbar) ..
        // then upon rbtn release, alt will be ensured-inactive, and that will activate sw selection like it should!
        // But in qbar persistent mode, we dont want it armed, so we'll immediately disarm it
        // (Note that sending shift-tabs wont work here coz switche hook checks for physical shift down)

        // we'll want a guard against re-entrancy as wheel spin can be fast, and our invocation scheme needs delays
        static stamp: Lazy<Mutex<Instant>> = Lazy::new (|| Mutex::new (Instant::now()));
        if let Ok (mut t_last) = stamp.lock() {
            if t_last.elapsed().as_millis() > 200 {
                *t_last = Instant::now();
            } else { return }
        } else {return }

        thread::spawn ( move || {
            // ^^ we'll need delays so we spawn out
            ks.clear_cur_sticky_fsc();
            // ^^ this will also do a hide if we were in fsc (ie. not persistent)
            thread::sleep (Duration::from_millis(100));
            // ^^ this delay helps avoid the fgnd changes from qb hide while switche is coming up switche
            ks.mod_keys.lalt.ensure_active();
            Tab.press_release();
            if ks.mouse.rbtn.down.is_set() {
                ks.in_right_btn_scroll_state.set();
                // ^^ this will prime the rbtn release to activate selection
            } else {
                // and if in persistent mode (rbtn not held), we want to disarm and release Alt)
                thread::sleep (Duration::from_millis(100));
                // ^^ let switche hear the alt-tab and its popup come up armed
                Space.press_release();
                ks.mod_keys.lalt.ensure_inactive();
            }
        } );
    } );
    let sw_wh_af = || sw_wh_af.clone();

    let sw_release = {
        // now, scrolls here bring up switche, so a click can do sw selection activation .. (no click-to-close as thad be confusing)
        // (^^ we're putting this on release (cf click) coz clicks come out at release anyway, except if held too long, theres nothing)
        let select = ag().k(Space).m(lctrl).mkg_nw().gen_af();    // again ctrl-alt-space is the most harmless (cf Enter)
        Arc::new ( move || {
            if check_switche_fgnd(wel) {
                if ks.mod_keys.lalt.active.is_set() {       // if Alt is active we're prob still in qbar sw cell
                    select();                               // so first disarm
                    ks.mod_keys.lalt.ensure_inactive();     // then release Alt
                }
                select();    // and this one to actually activate the selection
            }
            // (could have sent Enter etc too, but those have more consequences if they land on other windows)
        } )
    };

    let cell = ActionCell {
        label : "Switche".to_string(),
        on_wheel_bkwd : ag().af(sw_wh_af()).gen_af(),    // invoke switche
        on_wheel_frwd : ag().af(sw_wh_af()).gen_af(),    // invoke switche
        on_release    : ag().af(sw_release).gen_af(),    // activate switche selection
        ..Default::default()
    };
    static _switche : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let switche = _switche .get_or_init ( move || { Arc::new (cell) } );
    let switche = || switche.clone();



    // blind switch is just sending prev/next, but we gotta refresh the snapshot before we start
    // the nav-keys should be .. refresh:F15,  next:F16,  prev:F17,  top:F18,  bottom:F19  (w/ alt-shift)
    let nav_ag = |nav_key:Key| ag().k(nav_key).m(alt).m(shift).gen_af();

    // we're going to track whether we've refreshed, and have it clear whenever cursor leaves the cell
    static FLAG : Lazy<Flag> = Lazy::new (Flag::default);
    let refreshed = &FLAG;   // &'static that can be moved to threads without cloning

    let hov_end = Arc::new ( move || refreshed.clear() );

    // now the actual nav-fn-gen
    let init_af = move |is_bkwd| {
        let refresh = nav_ag(F15);
        let nav = if is_bkwd { nav_ag(F16) } else { nav_ag(F17) };
        Arc::new ( move || {
            if refreshed.is_set() { nav() }
            else { // we'll have to refresh, and give some time for the win-enum snap to be taken
                let (refresh, nav) = (refresh.clone(), nav.clone());
                thread::spawn ( move || {
                    refresh(); refreshed.set();
                    thread::sleep (Duration::from_millis(15));
                    // ^^ switche needs time to process the refresh hotkey and take the win-enum snap
                    nav();
            } ); }
        } )
    };

    let cell = ActionCell {
        label : "Switche Blind".to_string(),
        on_wheel_bkwd : init_af (true ),               // next window
        on_wheel_frwd : init_af (false),               // prev window
        on_hover_end  : hov_end,                       // clear refreshed flag
        on_click      : ag().k(F4).m(lalt).gen_af(),   // close tab
        ..Default::default()
    };
    static _switche_blind : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let switche_blind = _switche_blind .get_or_init ( move || { Arc::new (cell) } );
    let switche_blind = || switche_blind.clone();



    fn tabs_wh_af (ks: &'static KrustyState, dir_bkwd:bool) -> AF {
        Arc::new ( move || {
            ks.mod_keys.lctrl.ensure_active();
            if dir_bkwd { Tab.press_release() }
            else { LShift.press(); Tab.press_release(); LShift.release(); }
        } )
    }
    let tabs_hover_end_af = Arc::new ( move || {
        ks.mod_keys.lctrl.ensure_inactive();
    } );
    let tabs_release = {
        // again like for switche above, we'll put on release
        // and if ctrl-active, we'll just release ctrl to have any ctrl-tabs activate
        // Sadly, unlike for sw, there's no way to disarm things like ide switcher (so they escape on fgnd change)
        // .. as such, actually just doing hover-out of the cell is more robust in most apps
        Arc::new ( move || {
            ks.mod_keys.lctrl.ensure_inactive();
            // ^^ and here too, we disabled close-on-click coz its too easy to mix-up
        } )
    };
    let cell = ActionCell {
        label : "Tabs".to_string(),
        on_wheel_bkwd : tabs_wh_af (ks, true ),          // ctrl-tab
        on_wheel_frwd : tabs_wh_af (ks, false),          // ctrl-shift-tab
        on_hover_end  : tabs_hover_end_af,               // ensure ctrl inactive
        on_release    : ag().af(tabs_release).gen_af(),  // end ctrl-tab
        ..Default::default()
    };
    static _tabs : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let tabs = _tabs .get_or_init ( move || { Arc::new (cell ) } );
    let tabs = || tabs.clone();


    let cell = ActionCell {
        label : "Tabs Blind".to_string(),
        on_wheel_bkwd : ag().k(PageDown).m(ctrl).gen_af(),   // tab next
        on_wheel_frwd : ag().k(PageUp  ).m(ctrl).gen_af(),   // tab prev
        on_click      : ag().k(W).m(lctrl).gen_af(),         // close tab
        ..Default::default()
    };
    static _tabs_blind : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let tabs_blind = _tabs_blind .get_or_init ( move || { Arc::new (cell) } );
    let tabs_blind = || tabs_blind.clone();



    fn gen_incr_brightness (incr:i32) -> AF {
        Arc::new ( move || { let _ = incr_brightness(incr); } )
    }
    let skip_inj_lbtn_rel = Arc::new(Flag::default());
    let skip_inj = skip_inj_lbtn_rel.clone();
    // we'll prep an AF to overload click-drag on any cell's press handling (no win-key needed)
    let drag_af = Arc::new ( move || {
        // note regarding slowness during drag, that even using an empty ui, the lag remains ..
        // .. basically that most of the cost is outside the 'update' call, no point trying to fiddle there
        // (and no easy way to prevent egui repainting due to external moves either)
        //
        // we'll prep the drag setup .. (this version of drag is via krusty, not egui!!)
        if !qb.is_drag_active()  {
            ks.capture_pointer_win_snap_dat(None);
            // we'll do a (krusty-unseen) lbtn release so we dont have kbd-focus clamped on qbar
            // should be mostly harmless, as the actual lbtn release later will sync everything up anyway
            //dbg!(ks.mouse.lbtn);
            ks.mouse.lbtn.down.set();
            // ^^ this is just in case qbar was not defocused and so kr didnt even hear the btn down (should be rare)
            qb.set_dragging(true);
            // ^^ will be cleared by krusty combo on rbtn-rel itself (i.e outside qbar)
            skip_inj.set();
            delayed_action (20, move || {
                LeftButton.release();
                thread::sleep (Duration::from_millis (10));
                qb.defocus();
            }) ();
            // ^^ then send out actual lbtn release so OS doesnt clamp down kbd focus on egui (due to held btn)
        }
        // now, we'll also use the click on drag loc to make qb persistent
        qb.show(true);               // updates persist flag and exits since its already visible
        ks.clear_cur_sticky_fsc();   // gives viz feedback of change .. (wont close qb coz we set persist flag)
    } );
    let skip_inj = skip_inj_lbtn_rel.clone();
    let drag_rel_af = Arc::new ( move || {
        //dbg!((&skip_inj, ks.mouse.lbtn));
        // note, that the fake lbtn-rel above WILL get here, as qbar doesnt get to filter our own injected events!
        // .. and so we'll skip it if injected flag was set earlier .. (meh, hacky but mostly harmless)
        if skip_inj.is_set() { skip_inj.clear(); return }
        // now we'll handle the case of actual rel while qb has focus .. (else krusty combos would handle it)
        // (mostly happens only at the very first invocation when lbtn clicking to make it persistent)
        ks.mouse.lbtn.down.clear(); qb.set_dragging(false);
        if ks.mouse.lbtn.active.is_set() { ks.mouse.lbtn.active.clear(); LeftButton.release() }
    } );

    let cell = ActionCell {
        label : "Brightness".into(),
        on_wheel_frwd : gen_incr_brightness ( 2),   // increase brightness
        on_wheel_bkwd : gen_incr_brightness (-2),   // decrease brightness
        on_press      : drag_af,                    // enable frame dragging
        on_release    : drag_rel_af,                // sync btn flags if need be
        ..Default::default()
    };
    static _brightness : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let brightness = _brightness .get_or_init ( move || { Arc::new (cell) } );
    let brightness = || brightness.clone();


    let cell = ActionCell {
        label : "Arrows".to_string(),
        on_wheel_bkwd : ag().k(ExtDown).gen_af(),    // arrow down
        on_wheel_frwd : ag().k(ExtUp  ).gen_af(),    // arrow up
        ..Default::default()
    };
    static _arrows : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let arrows = _arrows .get_or_init ( move || { Arc::new (cell) } );
    let arrows = || arrows.clone();


    let cell = ActionCell {
        label : "Refresh".to_string(),
        on_click      : ag().k(F5).gen_af(),                  // refresh
        on_wheel_bkwd : ag().k(ExtLeft ).m(lalt).gen_af(),    // pg-bkwd
        on_wheel_frwd : ag().k(ExtRight).m(lalt).gen_af(),    // pg-fwd
        ..Default::default()
    };
    static _refresh : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let refresh = _refresh .get_or_init ( move || { Arc::new (cell) } );
    let refresh = || refresh.clone();


    let cell = ActionCell {
        label : "Diff".to_string(),
        on_wheel_bkwd : ag().k(ExtDown).m(ctrl).m(alt).gen_af(),   // next diff
        on_wheel_frwd : ag().k(ExtUp  ).m(ctrl).m(alt).gen_af(),   // prev diff
        on_click      : ag().k(ExtRight).m(ctrl).m(alt).gen_af(),  // accept left -> right
        ..Default::default()
    };
    static _diff : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let diff = _diff .get_or_init ( move || { Arc::new (cell) } );
    let diff = || diff.clone();


    // note that to make these chrome shortcuts work .. first installed shortkeys extension ..
    // .. then there, set the hotkeys as below, and set them to exec javascript copied directly from bookmarklets
    // .. (directly trying to trigger the bookmarklets didnt work .. oh well)
    let darken  = ag().k(Slash    ).m(ctrl).gen_af();
    let lighten = ag().k(Backslash).m(ctrl).gen_af();
    let cell = ActionCell {
        label : "Page Dark".to_string(),
        on_wheel_bkwd : ag().af(darken ).gen_af(),
        on_wheel_frwd : ag().af(lighten).gen_af(),
        ..Default::default()
    };
    static _pg_dark : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let pg_dark = _pg_dark .get_or_init ( move || { Arc::new (cell) } );
    let pg_dark = || pg_dark.clone();


    let im_darken   = ag().k(LBracket).m(ctrl).gen_af();
    let im_brighten = ag().k(RBracket).m(ctrl).gen_af();
    let cell = ActionCell {
        label : "Image Dark".to_string(),
        on_wheel_bkwd : ag().af(im_darken  ).gen_af(),
        on_wheel_frwd : ag().af(im_brighten).gen_af(),
        ..Default::default()
    };
    static _im_dark : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let im_dark = _im_dark .get_or_init ( move || { Arc::new (cell) } );
    let im_dark = || im_dark.clone();



    static _empty : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let empty = _empty .get_or_init ( move || { Arc::new (ActionCell::default()) } );
    let empty = || empty.clone();




    // we can now start constructing the grid variants for various conditions
    let cell_sz = CellDims::new (85,25);

    // general use
    static _base : OnceCell < Arc < ActionGrid>> = OnceCell::new();
    let grid = vec! (
        vec! ( arrows(),      empty(),          volume() ),
        vec! ( switche(),     switche_blind(),  tracks() ),
        vec! ( tabs_blind(),  tabs(),           scrub()  ),
        vec! ( brightness(),  empty(),          empty()  ),
    );
    let label = "base".into();
    let grid_sz = GridDims::new (4,3);
    let base = _base.get_or_init ( move || {
        Arc::new ( ActionGrid { label, cell_sz, grid_sz, grid } )
    } );
    let base = || base.clone();


    // ide specific
    static _ide : OnceCell < Arc < ActionGrid>> = OnceCell::new();
    let grid = vec! (
        vec! ( arrows(),      diff(),          volume() ),
        vec! ( switche(),     switche_blind(), tracks() ),
        vec! ( tabs_blind(),  tabs(),          scrub()  ),
        vec! ( brightness(),  empty(),         empty()  ),
    );
    let label = "ide".into();
    let grid_sz = GridDims::new (4,3);
    let ide = _ide.get_or_init ( move || {
        Arc::new ( ActionGrid { label, cell_sz, grid_sz, grid } )
    } );
    let ide = || ide.clone();


    // browser specific
    static _web : OnceCell < Arc < ActionGrid>> = OnceCell::new();
    let grid = vec! (
        vec! ( arrows(),      refresh(),       volume()  ),
        vec! ( switche(),     switche_blind(), tracks()  ),
        vec! ( tabs_blind(),  empty(),         scrub()   ),
        vec! ( brightness(),  pg_dark(),       im_dark() ),
    );
    let label = "web".into();
    let grid_sz = GridDims::new (4,3);
    let web = _web.get_or_init ( move || {
        Arc::new ( ActionGrid { label, cell_sz, grid_sz, grid } )
    } );
    let web = || web.clone();



    // finally we can build the grid provider itself
    Arc::new ( move || {
        if check_intellij_fgnd (wel) { ide() }
        else if check_browser_fgnd (wel) { web() }
        else { base() }
    } )

}




