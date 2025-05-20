use std::sync::{Mutex, OnceLock, LazyLock};
use std::time::Instant;

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


static ICONS : OnceLock<Icons> = OnceLock::new();

fn get_icon_fn (ico: &'static Option <Icon>) -> Option <IconFn> {
    if let Some(ico) = ico.as_ref() {
        Some ( Arc::new (move || Some(ico)) )
    } else { None }
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

    use {KbdKey::*, ModKey::*};

    let (kr, ks) = ( Krusty::instance(), KrustyState::instance() );

    let icons = ICONS .get_or_init ( || load_icons (ctx));




    /// __** Volume, Mute, .. w/Caps Tracks Play, Pause, Next, Prev, .. w/Shift Skip Fwd/Bkwd **__
    let on_press : AF = {
        let vol_mute   = ag().k(VolumeMute).gen_af();
        let play_pause = ag().k(VolumeUp).m(lctrl).m(lshift).gen_af();  // as set in music player
        Arc::new (move || {
            match ks.mod_keys.caps.down.is_set() || ks.mod_keys.some_shift_down() {
                false => vol_mute(),
                true  => play_pause(),
            };
        } )
    };
    fn af_media_wheel (ks:KSR, is_bkwd:bool) -> AF {
        let vol_dn = ag().k(VolumeDown).gen_af();
        let vol_up = ag().k(VolumeUp  ).gen_af();
        let next_track = media_next_action (ks, true);
        let prev_track = media_next_action (ks, false);
        let skip_fwd   = media_skips_action (1, ks, true);
        let skip_bkwd  = media_skips_action (1, ks, false);
        Arc::new (move || {
            match (is_bkwd, ks.mod_keys.caps.down.is_set(), ks.mod_keys.some_shift_down()) {
                (true,  false, false) => vol_dn(),
                (false, false, false) => vol_up(),
                (true,   true, false) => next_track(),
                (false,  true, false) => prev_track(),
                (true,      _,  true) => skip_fwd(),
                (false,     _,  true) => skip_bkwd(),
            };
        } )
    }
    let icon_fn : Option<IconFn> = Some ( {
        let (vol, tracks, scrub) = (icons.volume.as_ref(), icons.tracks.as_ref(), icons.scrub.as_ref());
        Arc::new (move || {
            match (ks.mod_keys.caps.down.is_set(), ks.mod_keys.some_shift_down()) {
                (false, false) => vol,
                ( true, false) => tracks,
                (    _,  true) => scrub,
            }
        } )
    } );
    let cell = ActionCell {
        label : Some (Label::new ("Media")),
        icon_fn,
        on_press,
        on_wheel_bkwd : af_media_wheel (ks,  true),
        on_wheel_frwd : af_media_wheel (ks, false),
        // we'll also cover horiz-wheel since egui converts shift-wheel to h-wheel
        on_hwheel_bkwd : af_media_wheel (ks,  true),
        on_hwheel_frwd : af_media_wheel (ks, false),
        ..Default::default()
    };
    static _volume : OnceLock <Arc <ActionCell>> = OnceLock::new();
    let volume = _volume .get_or_init ( move || { Arc::new (cell) } );
    let volume = || volume.clone();




    /// __** Switche Task Switching **__
    let sw_wh_af = Arc::new ( move || {
        // this is a lil funky, because we want to get into alt-tab and clear out fsc (which will hide qbar) ..
        // then upon rbtn release, alt will be ensured-inactive, and that will activate sw selection like it should!
        // But in qbar persistent mode, we dont want it armed, so we'll immediately disarm it
        // (Note that sending shift-tabs wont work here coz switche hook checks for physical shift down)

        // we'll want a guard against re-entrancy as wheel spin can be fast, and our invocation scheme needs delays
        static stamp: LazyLock<Mutex<Instant>> = LazyLock::new (|| Mutex::new (Instant::now()));
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
            if kr.wel.fg_eval (check_switche_fgnd) {
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
        label : Some (Label::new ("Switche")),
        icon_fn : get_icon_fn (&icons.switche),
        on_wheel_bkwd : sw_wh_af(),    // invoke switche
        on_wheel_frwd : sw_wh_af(),    // invoke switche
        on_release    : sw_release,    // activate switche selection
        ..Default::default()
    };
    static _switche : OnceLock <Arc <ActionCell>> = OnceLock::new();
    let switche = _switche .get_or_init ( move || { Arc::new (cell) } );
    let switche = || switche.clone();




    /// _** Direct Task Switching (switche-blind) **__
    // blind switch is just sending prev/next, but we gotta refresh the snapshot before we start
    // we're going to track whether we've refreshed, and have it clear whenever cursor leaves the cell
    // .. but the switching can trigger grid layout change, which can end/restart hover
    // .. so instead we'll only clear refresh flag if we're hovered out for a bit
    static _refreshed : LazyLock<Flag> = LazyLock::new (Flag::default);
    let refreshed = &_refreshed;   // &'static that can be moved to threads without cloning
    static _refr_clr_armed : LazyLock<Flag> = LazyLock::new (Flag::default);
    let refr_clr_armed = &_refr_clr_armed;

    let hov_start = Arc::new ( move || {
        refr_clr_armed.clear();
        kr.qbar .set_freeze_grid (true);
    } );
    let hov_end = Arc::new ( move || {
        refr_clr_armed.set();
        kr.qbar .set_freeze_grid (false);
        thread::spawn ( move || {
            thread::sleep (Duration::from_millis(100));
            if refr_clr_armed.is_set() { refr_clr_armed.clear(); refreshed.clear(); }
        } );
    } );

    // now the actual nav-fn-gen
    let nav_af = move |is_bkwd| {
        use SwitchePipeCmd::*;
        let refresh = SnapListRefresh.send_af();
        let nav = if is_bkwd { SnapListSwitchNext.send_af() } else { SnapListSwitchPrev.send_af() };
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

    // for lbtn-press, we want it to switch to last-active window, but also have snapshot refreshed
    let lbtn_af = nav_af (true);
    let lbtn_af = Arc::new (move || {
        refreshed.clear(); lbtn_af();
        // we'll also want to clear things up for any wheel scrolls that follow
        // and do that past the delay from actual nav, and from hov-end-restart (from fgnd change)
        thread::spawn (move || { thread::sleep (Duration::from_millis(150)); refreshed.clear(); } );
    });

    let cell = ActionCell {
        label : Some (Label::new ("Switche Blind")),
        icon_fn : get_icon_fn (&icons.sw_blind),
        on_wheel_bkwd  : nav_af (true ),               // next window
        on_wheel_frwd  : nav_af (false),               // prev window
        on_hover_start : hov_start,                    // clear snap refresh flag
        on_hover_end   : hov_end,                      // clear snap refresh flag if done
        on_press       : lbtn_af,                      // last window
        on_rbtn_press  : ag().k(F4).m(lalt).gen_af(),  // close window
        on_mbtn_press  : ag().k(F4).m(lalt).gen_af(),  // close window
        ..Default::default()
    };
    static _switche_bl : OnceLock <Arc <ActionCell>> = OnceLock::new();
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
        label : Some (Label::new ("Tabs")),
        icon_fn : get_icon_fn (&icons.tabs),
        on_wheel_bkwd : tabs_wh_af (ks, true ),    // ctrl-tab
        on_wheel_frwd : tabs_wh_af (ks, false),    // ctrl-shift-tab
        on_hover_end  : tabs_hover_end_af,         // ensure ctrl inactive
        on_release    : tabs_release,              // end ctrl-tab
        ..Default::default()
    };
    static _tabs : OnceLock <Arc <ActionCell>> = OnceLock::new();
    let tabs = _tabs .get_or_init ( move || { Arc::new (cell ) } );
    let tabs = || tabs.clone();




    /// _** Direct Tabs Switching **__
    let cell = ActionCell {
        label : Some (Label::new ("Tabs Blind")),
        icon_fn : get_icon_fn (&icons.tabs_blind),
        on_wheel_bkwd : ag().k(PageDown).m(ctrl).gen_af(),   // next tab
        on_wheel_frwd : ag().k(PageUp  ).m(ctrl).gen_af(),   // prev tab
        on_press      : ag().k(Tab).m(ctrl).gen_af(),        // last tab
        on_rbtn_press : ag().k(W).m(lctrl).gen_af(),         // close tab
        on_mbtn_press : ag().k(W).m(lctrl).gen_af(),         // close tab
        ..Default::default()
    };
    static _tabs_bl : OnceLock <Arc <ActionCell>> = OnceLock::new();
    let tabs_bl = _tabs_bl .get_or_init ( move || { Arc::new (cell) } );
    let tabs_bl = || tabs_bl.clone();




    /// _** Brightness .. plus .. dusky, gamma, gamgee overloads !! **__
    fn gen_incr_af (kr:KR, incr:i32) -> AF {
        Arc::new ( move || {
            if kr.ks.mod_keys.caps.down.is_clear() {
                let _ = incr_brightness (2 * incr);
            } else {
                kr.overlay.incr_dimming (-4 * incr);
            }
        } )
    }
    fn af_wheel (kr:KR, is_bkwd:bool) -> AF {
        let af_dusky_eff_next   = ag().k(Period).m(alt).m(win).gen_af();
        let af_dusky_eff_prev   = ag().k(Comma ).m(alt).m(win).gen_af();
        let af_dusky_gamma_next = ag().k(Period).m(shift).m(win).gen_af();
        let af_dusky_gamma_prev = ag().k(Comma ).m(shift).m(win).gen_af();
        let af_br_incr_pos = gen_incr_af (kr,  1);
        let af_br_incr_neg = gen_incr_af (kr, -1);
        Arc::new ( move || {
            match (is_bkwd, kr.ks.mod_keys.caps.down.is_set(), kr.ks.mod_keys.some_shift_down()) {
                (true,  false, false) => af_br_incr_neg(),
                (false, false, false) => af_br_incr_pos(),
                (true,   true, false) => af_dusky_gamma_next(),
                (false,  true, false) => af_dusky_gamma_prev(),
                (true,      _,  true) => af_dusky_eff_next(),
                (false,     _,  true) => af_dusky_eff_prev(),
            };
        } )
    }
    let on_click = {
        let af_dusky_gamma_tog = ag().k(G).m(shift).m(win).gen_af();
        let af_dusky_mode_tog  = ag().k(I).m(alt).m(win).m(shift).gen_af();
        let af_dusky_eff_tog   = ag().k(I).m(alt).m(win).gen_af();
        Arc::new ( move || {
            win_allow_fgnd_all();   // <- helps overlay come to fgnd if we toggle-on any
            match (ks.mod_keys.some_shift_down(), ks.mod_keys.caps.down.is_set()) {
                (true,   true) => af_dusky_mode_tog(),
                (true,  false) => af_dusky_eff_tog(),
                (false,  true) => af_dusky_gamma_tog(),
                (false, false) => { },
            };
        } )
    };
    let on_rbtn_click : AF = {
        // note that caps-rbtn gets suppressed to support rbtn-scroll etc (and wont come out till release)
        let af_dusky_clear_overlays  = ag().k(F12).m(alt).m(win).gen_af();
        let af_dusky_clear_overrides = ag().k(Insert).m(alt).m(win).gen_af();
        Arc::new ( move || {
            match ks.mod_keys.some_shift_down() {
                false => af_dusky_clear_overlays(),
                true  => af_dusky_clear_overrides(),
            }
        } )
    };
    let on_mbtn_click : AF = Arc::new (bringup_gamgee);

    let ov_indc = Arc::new (move || kr.overlay.is_active());
    // ^^ dimming overlay active indicator

    let cell = ActionCell {
        label         : Some (Label::new ("Brightness")),
        icon_fn : get_icon_fn (&icons.bright),
        indicator     : Some(ov_indc),
        on_click,
        on_rbtn_click,
        on_mbtn_click,
        on_wheel_bkwd  : af_wheel (kr,  true),
        on_wheel_frwd  : af_wheel (kr, false),
        // egui translates shift-wheel to horiz-wheel, so we'll catch those too
        on_hwheel_bkwd : af_wheel (kr,  true),
        on_hwheel_frwd : af_wheel (kr, false),
        ..Default::default()
    };
    static _brightness : OnceLock <Arc <ActionCell>> = OnceLock::new();
    let brightness = _brightness .get_or_init ( move || { Arc::new (cell) } );
    let brightness = || brightness.clone();




    /// _** Wheel to Arrows **__
    let arrow_up   = ag().k(ExtUp  ).gen_af();
    let arrow_down = ag().k(ExtDown).gen_af();
    let line_up    = ag().k(I      ).m(alt).m(ctrl).gen_af();
    let line_down  = ag().k(Comma  ).m(alt).m(ctrl).gen_af();
    let arrow_af = |dir_bkwd:bool| -> AF {
        Arc::new ( move || {
            match (dir_bkwd, ks.mod_keys.caps.down.is_set()) {
                (true,  false) => arrow_down(),
                (false, false) => arrow_up(),
                (true,  true ) => line_down(),
                (false, true ) => line_up(),
            };
        } )
    };
    let cell = ActionCell {
        label : Some (Label::new ("Arrows")),
        icon_fn : get_icon_fn (&icons.arrows),
        on_wheel_bkwd : arrow_af.clone()(true),
        on_wheel_frwd : arrow_af(false),
        ..Default::default()
    };
    static _arrows : OnceLock <Arc <ActionCell>> = OnceLock::new();
    let arrows = _arrows .get_or_init ( move || { Arc::new (cell) } );
    let arrows = || arrows.clone();



    /// _** Refresh for browser etc **__
    let nav_bkwd  = ag().k(ExtLeft ).m(lalt).gen_af();
    let nav_fwd   = ag().k(ExtRight).m(lalt).gen_af();
    let zoom_in  = ag().k(Equal).m(lctrl).gen_af();
    let zoom_out = ag().k(Minus).m(lctrl).gen_af();
    let af_fwd  = Arc::new ( move || if ks.mouse.lbtn.down.is_set() { nav_fwd()  } else { zoom_in()  } );
    let af_bkwd = Arc::new ( move || if ks.mouse.lbtn.down.is_set() { nav_bkwd() } else { zoom_out() } );
    let cell = ActionCell {
        label : Some (Label::new ("Refresh")),
        icon_fn : get_icon_fn (&icons.refresh),
        on_wheel_bkwd : af_bkwd,              // pg-bkwd
        on_wheel_frwd : af_fwd,               // pg-fwd
        on_press      : ag().k(F5).gen_af(),  // refresh
        on_rbtn_press : ag().k(Numrow_1).m(alt).m(shift).gen_af(),  // reader-view
        ..Default::default()
    };
    static _refresh : OnceLock <Arc <ActionCell>> = OnceLock::new();
    let refresh = _refresh .get_or_init ( move || { Arc::new (cell) } );
    let refresh = || refresh.clone();




    /// _** Minimize and window Send-to-Back (RBtn) .. plus .. LBtn Qbar drag, MBtn Qbar close **__
    // .. and overloaded on this, we wanted to set it so if upon invoc-at-cursor if we release in this square ..
    // .. then we'd hide the qbar instead of the default of making it go back to persisted state/pos (and vice-versa)
    // but rbtn-rel is handled by kr itself, so we wont hear it here .. so we'll just set/clear flag on hover-in/out
    let af_min_back = Arc::new (move || {
        if let Ok(fgi) = kr.wel.fgnd_info.read() {
            win_min_and_back(fgi.hwnd)
        }
    } );
    static _pers_flag_cache : LazyLock<Flag> = LazyLock::new (Flag::default);
    let pers_flag_cache = &_pers_flag_cache;
    let af_hov_start = Arc::new (move || {
        if ks.mouse.rbtn.down.is_set() {
            let prior_persist = kr.qbar.cur_grid_check_prior_persist();
            pers_flag_cache.store (prior_persist);                 // cache prior flag state
            kr.qbar.cur_grid_set_prior_persist (!prior_persist);   // toggle actual flag
        }
    } );
    let af_hov_end = Arc::new (move || {
        // wanna restore flag from cache .. but only if qb is still visible .. (since qb-hide also gives hov-end)
        if kr.qbar.cur_grid_check_visible() {
            kr.qbar.cur_grid_set_prior_persist (pers_flag_cache.is_set())
        }
    } );
    let qbar_drag = Arc::new ( move || {
        // for drag, we just set a flag and let krusty handle it (incl clearing flag on lbtn release)
        kr.qbar.set_dragging(true);
        // we'll also make it not do snap
        ks.no_snap.store(kr.qbar.hwnd());
        // plus we'll also make lbtn click make qbar persist
        kr.qbar.set_persistent();
        // and give some viz feedback of change .. (wont close qb coz we've set persist flag)
        ks.clear_cur_sticky_fsc();
    });
    //let exit_drag = Arc::new ( move || kr.qbar.set_dragging(false));
    // we'll explicitly not set it here and rely on global release, just to face potential issues sooner
    let qbar_close = Arc::new ( move ||  kr.qbar.hide(true) );

    let cell = ActionCell {
        label  : Some (Label::new ("MinBack")),
        icon_fn : get_icon_fn (&icons.min_back),
        on_press       : qbar_drag,            // persist-qbar, enable drag
        on_rbtn_click  : af_min_back.clone(),  // min-and-back
        on_mbtn_click  : qbar_close,           // close-qbar
        on_hover_start : af_hov_start,         // mark to hide upon rbtn-rel
        on_hover_end   : af_hov_end,           // un-mark hide upon rbtn-rel
        ..Default::default()
    };
    static _min_back : OnceLock <Arc <ActionCell>> = OnceLock::new();
    let min_back = _min_back .get_or_init ( move || { Arc::new (cell) } );
    let min_back = || min_back.clone();




    /// _** Diff Nav (while IDE fgnd) **__
    let cell = ActionCell {
        label : Some (Label::new ("Diff")),
        icon_fn : get_icon_fn (&icons.diff),
        on_wheel_bkwd : ag().k(ExtDown ).m(ctrl).m(alt).gen_af(),   // next diff
        on_wheel_frwd : ag().k(ExtUp   ).m(ctrl).m(alt).gen_af(),   // prev diff
        on_press      : ag().k(ExtRight).m(ctrl).m(alt).gen_af(),   // accept left  -> right
        on_rbtn_press : ag().k(ExtLeft ).m(ctrl).m(alt).gen_af(),   // accept right -> left
        ..Default::default()
    };
    static _diff : OnceLock <Arc <ActionCell>> = OnceLock::new();
    let diff = _diff .get_or_init ( move || { Arc::new (cell) } );
    let diff = || diff.clone();




    // _** Chrome bookmarklets trigger for darkening or brightening of page/image **__
    // note that to make these chrome shortcuts work .. first installed shortkeys extension ..
    // .. then there, set the hotkeys as below, and set them to exec javascript copied directly from bookmarklets
    // .. (directly trying to trigger the bookmarklets didnt work .. oh well)

    // re. short-keys two stroke combo usage (not krusty two-stroke) .. again similar to that for for IDE, its very restrictive as we'd want
    // .. the second (unsuppressed) stroke to not trigger anything either .. plus, for shortkeys, can only use till f19 ..
    // so we'll use .. alt-ctrl-F13-F15 as first strokes, and alt-F13-F19 as second strokes .. (7*3=21)
    fn shortkeys_two_stroke_combo (s1k:Key, s2k:Key) -> AF {
        let s1c = ag().k(s1k).m(alt).m(ctrl).gen_af();
        let s2c = ag().k(s2k).m(alt).gen_af();
        Arc::new ( move || { s1c(); s2c(); } )
    }
    let darken      = shortkeys_two_stroke_combo (F13, F13);
    let lighten     = shortkeys_two_stroke_combo (F13, F14);
    let im_darken   = shortkeys_two_stroke_combo (F13, F15);
    let im_brighten = shortkeys_two_stroke_combo (F13, F16);
    let dark_mode   = shortkeys_two_stroke_combo (F13, F17);
    let invert_mode = shortkeys_two_stroke_combo (F13, F18);
    let im_zap      = shortkeys_two_stroke_combo (F13, F19);
    let im_inv      = shortkeys_two_stroke_combo (F14, F13);
    let inv_darken  = shortkeys_two_stroke_combo (F14, F14);
    let inv_lighten = shortkeys_two_stroke_combo (F14, F15);
    let zap_all     = shortkeys_two_stroke_combo (F14, F16);

    let wheel_bkwd_af = Arc::new ( move || {
        if ks.mod_keys.caps.down.is_clear() { darken() }
        else { inv_darken() }
    } );
    let wheel_frwd_af = Arc::new ( move || {
        if ks.mod_keys.caps.down.is_clear() { lighten() }
        else { inv_lighten() }
    } );

    let cell = ActionCell {
        label : Some (Label::new ("Page Dark")),
        icon_fn : get_icon_fn (&icons.darken_pg),
        on_wheel_bkwd : wheel_bkwd_af,
        on_wheel_frwd : wheel_frwd_af,
        on_release    : invert_mode,
        on_rbtn_press : dark_mode,
        on_mbtn_press : zap_all,
        ..Default::default()
    };
    static _pg_dark : OnceLock <Arc <ActionCell>> = OnceLock::new();
    let pg_dark = _pg_dark .get_or_init ( move || { Arc::new (cell) } );
    let pg_dark = || pg_dark.clone();


    /// __** Darkening / brightening for Images only **__
    let cell = ActionCell {
        label : Some (Label::new ("Image Dark")),
        icon_fn : get_icon_fn (&icons.darken_im),
        on_wheel_bkwd : im_darken,
        on_wheel_frwd : im_brighten,
        on_press      : im_inv,
        on_rbtn_press : im_zap.clone(),
        on_mbtn_press : im_zap,
        ..Default::default()
    };
    static _im_dark : OnceLock <Arc <ActionCell>> = OnceLock::new();
    let im_dark = _im_dark .get_or_init ( move || { Arc::new (cell) } );
    let im_dark = || im_dark.clone();



    /// __** y-axis percent toggle for TradingView **__
    let cell = ActionCell {
        label : Some (Label::new("%").sz(18)),
        on_wheel_bkwd : ag().k(P).m(lalt).gen_af(),
        on_wheel_frwd : ag().k(P).m(lalt).gen_af(),
        on_press      : ag().k(P).m(lalt).gen_af(),
        ..Default::default()
    };
    static _tv_y_toggle : OnceLock <Arc <ActionCell>> = OnceLock::new();
    let tv_y_toggle = _tv_y_toggle .get_or_init ( move || { Arc::new (cell) } );
    let tv_y_toggle = || tv_y_toggle.clone();





    /// _** Empty Action-Cell for placeholder purposes **__
    static _empty : OnceLock <Arc <ActionCell>> = OnceLock::new();
    let empty = _empty .get_or_init ( move || { Arc::new (ActionCell::default()) } );
    let empty = || empty.clone();




    // we can now start constructing the grid variants for various conditions

    //let cell_sz = CellDims::new (48,26);
    let cell_sz = CellDims::new (38,22);

    // we'll start just a bit towards the top-right corner
    let start_pos = Point { x: 3840-450, y: 250 };

    // and we'll create a shared storage for position for grids that dont specify their own
    let default_pos = Arc::new ( PointAtomic::new (start_pos) );


    // general use
    static _base : OnceLock <Arc <ActionGrid>> = OnceLock::new();
    let grid = vec! (
        vec! ( switche(),   switche_bl(),  brightness(),  volume() ),
        vec! ( tabs_bl(),   tabs(),        arrows(),      empty()  ),
        vec! ( empty(),     empty(),       min_back(),    empty()  ),
    );
    let label = "base".into();
    let grid_sz = GridDims::new (3, 4);
    let cur_pos = default_pos.clone();
    let base : &'static Arc <ActionGrid> = _base.get_or_init ( move || {
        Arc::new ( ActionGrid { label, cell_sz, grid_sz, grid, cur_pos, ..ActionGrid::default() } )
    } );


    // disabled grid .. (for cases where we want no grid, but w/o having to toggle the general/base grid)
    static _disabled_s : OnceLock <Arc <ActionGrid>> = OnceLock::new();
    let _disabled = _disabled_s.get_or_init ( move || {
        Arc::new ( ActionGrid { enabled: Flag::new(false), ..ActionGrid::default() } )
    } );



    // ide specific
    static _ide : OnceLock <Arc <ActionGrid>> = OnceLock::new();
    let grid = vec! (
        vec! ( switche(),   switche_bl(),  brightness(),  volume() ),
        vec! ( tabs_bl(),   tabs(),        arrows(),      empty()  ),
        vec! ( empty(),     diff(),        min_back(),    empty()  ),
    );
    let label = "ide".into();
    let grid_sz = GridDims::new (3, 4);
    let cur_pos = Arc::new ( PointAtomic::new (start_pos) );
    // ^^ we'll set IDE qbar-grid positioning to be remembered separately
    let ide = _ide.get_or_init ( move || {
        Arc::new ( ActionGrid { label, cell_sz, grid_sz, grid, cur_pos, ..ActionGrid::default() } )
    } );



    // browser specific
    static _web : OnceLock <Arc <ActionGrid>> = OnceLock::new();
    let grid = vec! (
        vec! ( switche(),   switche_bl(),  brightness(),  volume() ),
        vec! ( tabs_bl(),   refresh(),     arrows(),      empty()  ),
        vec! ( pg_dark(),   im_dark(),     min_back(),    empty()  ),
    );
    let label = "web".into();
    let grid_sz = GridDims::new (3, 4);
    let cur_pos = default_pos.clone();
    let web = _web.get_or_init ( move || {
        Arc::new ( ActionGrid { label, cell_sz, grid_sz, grid, cur_pos, ..ActionGrid::default() } )
    } );


    // TradingView
    static _trading_view : OnceLock <Arc <ActionGrid>> = OnceLock::new();
    let grid = vec! (
        vec! ( switche(),   switche_bl(),  brightness(),   volume() ),
        vec! ( tabs_bl(),   refresh(),     tv_y_toggle(),  empty()  ),
        vec! ( pg_dark(),   im_dark(),     min_back(),     empty()  ),
    );
    let label = "TradingView".into();
    let grid_sz = GridDims::new (3, 4);
    let cur_pos =  Arc::new ( PointAtomic::new ( Point { x: 2500, y: 4 } ) );
    let trading_view = _trading_view.get_or_init ( move || {
        Arc::new ( ActionGrid { label, cell_sz, grid_sz, grid, cur_pos, ..ActionGrid::default() } )
    } );



    //let grids = [base(), _disabled, ide(), web(), trading_view()];
    //let dbg_grids = Arc::new ( move || {
    //    for g in grids.as_ref() {
    //        println! ("vpp{:?}", (&g.label, g.is_visible.is_set(), g.is_persist.is_set(), g.prior_persist.is_set()))
    //    };
    //} );


    // finally we can build the grid provider itself

    #[allow (static_mut_refs)]
    // ^^ we'll maintain a 'cur-grid' so we can impl 'ignore' aka keep cur grid for some fgnds like switche
    // .. but this should all be safe as we're only keeping/swapping &'static pointers in there
    //
    Box::new ( move || { unsafe {
        use ExeGroups::*;

        let fi = kr.wel .fgnd_info .read().unwrap();

        static mut last_grid : OnceLock <&'static Arc <ActionGrid>> = OnceLock::new();
        let _ = last_grid .get_or_init (|| base);

        // Lets filter out common windows classes .. menus, dialogs, task-switcher, icon-titles
        // .. ^^ naah its pointless for now .. but we'll keep it commented in case comes in handy later
        //if let Some (g) = match fi.class.as_str() {
        //    //"#32768" | "#32770" | "#32771" | "#32772" => Some (disabled),
        //    _ => None,
        //} { return g };

        // next we'll check exe groups
        let exe_grp = ExeGroups::check (fi.exe.as_str());

        // but first lets cover cases that need title lookup
        if exe_grp == Browser && fi.title.contains("TradingView") {
            if let Some (lg) = last_grid.get_mut() { *lg = trading_view };
            return trading_view
        }

        // and finally the generic exe based groups
        // Note that for cases we want to make no changes to grid (e.g. switche or gamgee), we'll return the currently active grid itself
        // .. for Switche, we want to avoid its auto-hide from triggering due to qbar fgnd changes
        // .. for gamgee, the btnd wont work well (esp the hide btn) if last fgnd has qbar on, but general qbar is off .. oh well
        let next_grid = match ExeGroups::check (fi.exe.as_str()) {
            Switche | Gamgee => last_grid.get().unwrap_or(&base),
            Browser => web,
            IDE     => ide,
            _       => base,
        };

        if let Some (lg) = last_grid.get_mut() { *lg = next_grid };
        next_grid

    } } )


}




