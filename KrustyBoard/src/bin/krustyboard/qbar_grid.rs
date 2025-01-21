use std::sync::Mutex;
use std::time::Instant;

use once_cell::sync::{Lazy, OnceCell};
use egui::{Context, Vec2};
use include_dir::{include_dir, Dir};

use crate::*;



// we'll include all the icons in our 'assets' dir
static ASSETS: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/assets");


struct Icons {
    _private : (),
    bright     : Option <Icon>,
    volume     : Option <Icon>,
    tracks     : Option <Icon>,
    scrub      : Option <Icon>,
    switche    : Option <Icon>,
    sw_blind   : Option <Icon>,
    tabs       : Option <Icon>,
    tabs_blind : Option <Icon>,
    refresh    : Option <Icon>,
    min_back   : Option <Icon>,
    arrows     : Option <Icon>,
    diff       : Option <Icon>,
    darken_pg  : Option <Icon>,
    darken_im  : Option <Icon>,
}


fn load_icons (ctx: &Context) -> Icons {

    let load = |id: &str, src: &str, width: u32, height: u32| {
        let sz_hint = Vec2::new (width as f32, height as f32);
        ASSETS.get_file (src) .and_then (|f| Icon::load (ctx, id, sz_hint, f))
    };

    Icons {
        _private :  (),
        bright     :  load ( "brightness",  "brightness-01-2.png",  24,  24 ),
        volume     :  load ( "volume",      "volume-01-2.png",      18,  18 ),
        tracks     :  load ( "play-pause",  "play-pause-01-2.png",  14,  14 ),
        scrub      :  load ( "scrub-fwd",   "scrub-fwd-01-2.png",   18,  18 ),
        switche    :  load ( "switche",     "switche-01-2.png",     18,  18 ),
        sw_blind   :  load ( "sw-blind",    "sw-blind-01-2.png",    28,  16 ),
        tabs       :  load ( "tabs",        "tabs-03-2.png",        26,  14 ),
        tabs_blind :  load ( "tabs-blind",  "tabs-blind-03-2.png",  36,  16 ),
        refresh    :  load ( "refresh",     "refresh-01-2.png",     16,  16 ),
        min_back   :  load ( "min_back",    "min-back-03-2.png",    20,  20 ),
        arrows     :  load ( "arrows",      "arrows-01-2.png",      20,  20 ),
        diff       :  load ( "diff",        "diff-02-h40-2.png",    30,  20 ),
        darken_pg  :  load ( "darken_pg",   "darken-pg-03-3.png",   36,  24 ),
        darken_im  :  load ( "darken_im",   "darken-im-03-2.png",   36,  24 ),
    }

}


/*
    - notes on why the convoluted steps of registering/passing the GetGrid and GetGridBuilder Fns ..
        - loading textures in egui requires ctx, which we only get on starting app .. by which time, we'd want to have the grid ready
        - alt. could try to load just the image data, and try and load image from bytes on update fn .. but thats slower/sucky too
        - or for the option of directly loading using their macros, requires that they get actual literal string for path-names .. ughh)
    - sooo
        - we'll want to call here to load icons right at egui startup callback ..
        - and for that we'll want to register a icon-loader fn with qbar
        - but also, we were already registering a grid-provider produced by a grid-prov-builder fn (that we called at setup time)
        - but now, we want that to be built only AFTER icons have been loaded (to avoid having to mutex guard them if writing later)
        - so then, we'd want to now have the grid-prov-builder have ctx be passed in ..
            - that'd mean we register the grid-prov-b, then qbar would only call that when ctx available
            - and in there, we'd use the ctx to load icons etc
            - and then itd return the actual grid-provider, which would then be saved for actual render time usage
    - (and ofc, this complication is mostly coz we wanted to keep the separation between core/lib and user-conf combo/grid/icons sections)
 */

//.. copied for ref :
// pub type GetGridBuilderFn = Arc <dyn Fn (egui::Context) -> GetGridFn + Send + Sync + 'static>;
// pub type GetGridFn = Arc <dyn Fn() -> Arc<ActionGrid> + Send + Sync + 'static>;



pub fn grid_provider_builder (ctx: &Context) -> GetGridFn  {

    use { KbdKey::*, ModKey::*};

    let (ks, wel, qb) = (KrustyState::instance(), WinEventsListener::instance(), QuickBar::instance());

    let icons = load_icons (ctx);


    /// __** Volume, Mute **__
    let cell = ActionCell {
        label : "Volume".into(),
        icon  : icons.volume.clone(),
        on_wheel_bkwd : ag().k(VolumeDown).gen_af(),    // vol down
        on_wheel_frwd : ag().k(VolumeUp  ).gen_af(),    // vol up
        on_press      : ag().k(VolumeMute).gen_af(),    // mute
        ..Default::default()
    };
    static _volume : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let volume = _volume .get_or_init ( move || { Arc::new (cell) } );
    let volume = || volume.clone();



    /// __** Media : Tracks Play, Pause, Next, Prev **__
    let cell = ActionCell {
        label : "Tracks".to_string(),
        icon  : icons.tracks.clone(),
        on_wheel_bkwd : media_next_action (ks, true),                  // next track
        on_wheel_frwd : media_next_action (ks, false),                 // prev track
        on_press      : ag().k(VolumeUp).m(lctrl).m(lshift).gen_af(),  // play / pause
        ..Default::default()
    };
    static _tracks : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let tracks = _tracks .get_or_init ( move || { Arc::new (cell) } );
    let tracks = || tracks.clone();



    /// __** Media : Scrub track-bar .. skip fwd/bkwd **__
    let cell = ActionCell {
        label : "Scrub".to_string(),
        icon  : icons.scrub.clone(),
        on_wheel_bkwd : media_skips_action (1, ks, true),              // skip fwd  on track-bar
        on_wheel_frwd : media_skips_action (1, ks, false),             // skip bkwd on track-bar
        on_press      : ag().k(VolumeUp).m(lctrl).m(lshift).gen_af(),  // play / pause
        ..Default::default()
    };
    static _scrub : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let scrub = _scrub .get_or_init ( move || { Arc::new (cell) } );
    let scrub = || scrub.clone();




    /// __** Switche Task Switching **__
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
    // lets also kick it once to get the lazy cached Instant fill up (else it would ignore first call)
    sw_wh_af()();

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
        icon  : icons.switche.clone(),
        on_wheel_bkwd : sw_wh_af(),    // invoke switche
        on_wheel_frwd : sw_wh_af(),    // invoke switche
        on_release    : sw_release,    // activate switche selection
        ..Default::default()
    };
    static _switche : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let switche = _switche .get_or_init ( move || { Arc::new (cell) } );
    let switche = || switche.clone();




    /// _** Direct Task Switching (switche-blind) **__
    // blind switch is just sending prev/next, but we gotta refresh the snapshot before we start
    // the nav-keys should be .. refresh:F15,  next:F16,  prev:F17,  top:F18,  bottom:F19  (w/ alt-shift)
    let nav_ag = |nav_key:Key| ag().k(nav_key).m(alt).m(shift).gen_af();

    // we're going to track whether we've refreshed, and have it clear whenever cursor leaves the cell
    // .. but the switching can trigger grid layout change, which can end/restart hover
    // .. so instead we'll only clear refresh flag if we're hovered out for a bit
    static _refreshed : Lazy<Flag> = Lazy::new (Flag::default);
    let refreshed = &_refreshed;   // &'static that can be moved to threads without cloning
    static _refr_armed : Lazy<Flag> = Lazy::new (Flag::default);
    let refr_armed = &_refr_armed;

    let hov_start = Arc::new ( move || { refr_armed.clear(); } );
    let hov_end = Arc::new ( move || {
        refr_armed.set();
        thread::spawn ( move || {
            thread::sleep (Duration::from_millis(100));
            if refr_armed.is_set() { refr_armed.clear(); refreshed.clear(); }
        } );
    } );

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
        icon  : icons.sw_blind.clone(),
        on_wheel_bkwd  : init_af (true ),               // next window
        on_wheel_frwd  : init_af (false),               // prev window
        on_hover_start : hov_start,                     // clear snap refresh flag
        on_hover_end   : hov_end,                       // clear snap refresh flag if done
        on_rbtn_press  : ag().k(F4).m(lalt).gen_af(),   // close window
        ..Default::default()
    };
    static _switche_bl : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let switche_bl = _switche_bl .get_or_init ( move || { Arc::new (cell) } );
    let switche_bl = || switche_bl.clone();





    /// _** Ctrl-Tab switching **__
    fn tabs_wh_af (ks:KSR, dir_bkwd:bool) -> AF {
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
        icon  : icons.tabs.clone(),
        on_wheel_bkwd : tabs_wh_af (ks, true ),    // ctrl-tab
        on_wheel_frwd : tabs_wh_af (ks, false),    // ctrl-shift-tab
        on_hover_end  : tabs_hover_end_af,         // ensure ctrl inactive
        on_release    : tabs_release,              // end ctrl-tab
        ..Default::default()
    };
    static _tabs : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let tabs = _tabs .get_or_init ( move || { Arc::new (cell ) } );
    let tabs = || tabs.clone();




    /// _** Direct Tabs Switching **__
    let cell = ActionCell {
        label : "Tabs Blind".to_string(),
        icon  : icons.tabs_blind.clone(),
        on_wheel_bkwd : ag().k(PageDown).m(ctrl).gen_af(),   // tab next
        on_wheel_frwd : ag().k(PageUp  ).m(ctrl).gen_af(),   // tab prev
        on_press      : ag().k(W).m(lctrl).gen_af(),         // close tab
        ..Default::default()
    };
    static _tabs_bl : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let tabs_bl = _tabs_bl .get_or_init ( move || { Arc::new (cell) } );
    let tabs_bl = || tabs_bl.clone();




    /// _** Brightness .. plus ..  Lbtn Qbar Dragging ..  Rbtn Qbar Close **__
    fn gen_incr_brightness (incr:i32) -> AF {
        Arc::new ( move || { let _ = incr_brightness(incr); } )
    }
    let qbar_drag = Arc::new ( move || {
        // for drag, we just set a flag and let krusty handle it (incl clearing flag on lbtn release)
        qb.set_dragging(true);
        // plus we'll also make lbtn click make qbar persist
        qb.show(true, true);         // updates persist flag and exits since its already visible
        ks.clear_cur_sticky_fsc();   // gives viz feedback of change .. (wont close qb coz we set persist flag)
    });
    //let exit_drag = Arc::new ( move || qb.set_dragging(false));
    // we'll explicitly not set it here and rely on global release, just to face potential issues sooner
    let qbar_close = Arc::new ( move ||  qb.hide(true) );

    let cell = ActionCell {
        label : "Brightness".into(),
        icon  : icons.bright.clone(),
        on_wheel_frwd : gen_incr_brightness ( 2),   // increase brightness
        on_wheel_bkwd : gen_incr_brightness (-2),   // decrease brightness
        on_press      : qbar_drag,                  // persist-qbar, enable drag
        on_rbtn_click : qbar_close,                 // close-qbar
        ..Default::default()
    };
    static _brightness : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let brightness = _brightness .get_or_init ( move || { Arc::new (cell) } );
    let brightness = || brightness.clone();




    /// _** Wheel to Arrows **__
    let cell = ActionCell {
        label : "Arrows".to_string(),
        icon  : icons.arrows.clone(),
        on_wheel_bkwd : ag().k(ExtDown).gen_af(),    // arrow down
        on_wheel_frwd : ag().k(ExtUp  ).gen_af(),    // arrow up
        ..Default::default()
    };
    static _arrows : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let arrows = _arrows .get_or_init ( move || { Arc::new (cell) } );
    let arrows = || arrows.clone();



    /// _** Refresh for browser etc **__
    let cell = ActionCell {
        label : "Refresh".to_string(),
        icon  : icons.refresh.clone(),
        on_wheel_bkwd : ag().k(ExtLeft ).m(lalt).gen_af(),    // pg-bkwd
        on_wheel_frwd : ag().k(ExtRight).m(lalt).gen_af(),    // pg-fwd
        on_press      : ag().k(F5).gen_af(),                  // refresh
        ..Default::default()
    };
    static _refresh : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let refresh = _refresh .get_or_init ( move || { Arc::new (cell) } );
    let refresh = || refresh.clone();




    /// _** Minimize and window Send-to-Back **__
    let af_min_back = Arc::new (move || {
        if let Ok(fgi) = wel.fgnd_info.read() {
            win_min_and_back(fgi.hwnd)
        }
    } );
    let cell = ActionCell {
        label : "MinBack".to_string(),
        icon  : icons.min_back.clone(),
        on_wheel_bkwd : af_min_back.clone(),    // min-and-back
        on_wheel_frwd : af_min_back.clone(),    // min-and-back
        on_press      : af_min_back.clone(),    // min-and-back
        ..Default::default()
    };
    static _min_back : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let min_back = _min_back .get_or_init ( move || { Arc::new (cell) } );
    let min_back = || min_back.clone();




    /// _** Diff Nav (while IDE fgnd) **__
    let cell = ActionCell {
        label : "Diff".to_string(),
        icon  : icons.diff.clone(),
        on_wheel_bkwd : ag().k(ExtDown).m(ctrl).m(alt).gen_af(),   // next diff
        on_wheel_frwd : ag().k(ExtUp  ).m(ctrl).m(alt).gen_af(),   // prev diff
        on_press      : ag().k(ExtRight).m(ctrl).m(alt).gen_af(),  // accept left -> right
        ..Default::default()
    };
    static _diff : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let diff = _diff .get_or_init ( move || { Arc::new (cell) } );
    let diff = || diff.clone();




    /// _** Chrome bookmarklets trigger for darkening or brightening of page/image **__
    // note that to make these chrome shortcuts work .. first installed shortkeys extension ..
    // .. then there, set the hotkeys as below, and set them to exec javascript copied directly from bookmarklets
    // .. (directly trying to trigger the bookmarklets didnt work .. oh well)
    let darken  = ag().k(Slash    ).m(ctrl).gen_af();
    let lighten = ag().k(Backslash).m(ctrl).gen_af();
    let cell = ActionCell {
        label : "Page Dark".to_string(),
        icon  : icons.darken_pg.clone(),
        on_wheel_bkwd : ag().af(darken ).gen_af(),
        on_wheel_frwd : ag().af(lighten).gen_af(),
        ..Default::default()
    };
    static _pg_dark : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let pg_dark = _pg_dark .get_or_init ( move || { Arc::new (cell) } );
    let pg_dark = || pg_dark.clone();



    /// _** Darkening / brightening for Images only **__
    let im_darken   = ag().k(LBracket).m(ctrl).gen_af();
    let im_brighten = ag().k(RBracket).m(ctrl).gen_af();
    let cell = ActionCell {
        label : "Image Dark".to_string(),
        icon  : icons.darken_im.clone(),
        on_wheel_bkwd : ag().af(im_darken  ).gen_af(),
        on_wheel_frwd : ag().af(im_brighten).gen_af(),
        ..Default::default()
    };
    static _im_dark : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let im_dark = _im_dark .get_or_init ( move || { Arc::new (cell) } );
    let im_dark = || im_dark.clone();



    /// _** Empty Action-Cell for placeholder purposes **__
    static _empty : OnceCell < Arc < ActionCell>> = OnceCell::new();
    let empty = _empty .get_or_init ( move || { Arc::new (ActionCell::default()) } );
    let empty = || empty.clone();




    // we can now start constructing the grid variants for various conditions

    let cell_sz = CellDims::new (48,26);

    let start_pos = Some ( Point { x: 3840-450, y: 250 } );
    // we'll start just a bit towards the top-right corner
    // (specifying None would make it stay invisible until invoked at cursor)


    // general use
    static _base : OnceCell < Arc < ActionGrid>> = OnceCell::new();
    let grid = vec! (
        vec! ( switche(),     switche_bl(),  arrows(),  refresh()  ),
        vec! ( tabs_bl(),     tabs(),        empty(),   min_back() ),
        vec! ( brightness(),  volume(),      tracks(),  scrub()    ),
    );
    let label = "base".into();
    let grid_sz = GridDims::new (3, 4);
    let base = _base.get_or_init ( move || {
        Arc::new ( ActionGrid { label, cell_sz, grid_sz, grid, start_pos } )
    } );
    let base = || base.clone();



    // ide specific
    static _ide : OnceCell < Arc < ActionGrid>> = OnceCell::new();
    let grid = vec! (
        vec! ( switche(),     switche_bl(),  arrows(),   refresh()  ),
        vec! ( tabs_bl(),     tabs(),        diff(),     min_back() ),
        vec! ( brightness(),  volume(),      tracks(),   scrub()    ),
    );
    let label = "ide".into();
    let grid_sz = GridDims::new (3, 4);
    let ide = _ide.get_or_init ( move || {
        Arc::new ( ActionGrid { label, cell_sz, grid_sz, grid, start_pos } )
    } );
    let ide = || ide.clone();



    // browser specific
    static _web : OnceCell < Arc < ActionGrid>> = OnceCell::new();
    let grid = vec! (
        vec! ( switche(),     switche_bl(),  arrows(),   refresh()  ),
        vec! ( tabs_bl(),     pg_dark(),     im_dark(),  min_back() ),
        vec! ( brightness(),  volume(),      tracks(),   scrub()    ),
    );
    let label = "web".into();
    let grid_sz = GridDims::new (3, 4);
    let web = _web.get_or_init ( move || {
        Arc::new ( ActionGrid { label, cell_sz, grid_sz, grid, start_pos } )
    } );
    let web = || web.clone();



    // finally we can build the grid provider itself
    Box::new ( move || {
        if check_intellij_fgnd (wel) { ide() }
        else if check_browser_fgnd (wel) { web() }
        else { base() }
    } )


}




