use std::sync::Mutex;
use std::time::Instant;
use once_cell::sync::Lazy;
use crate::*;


pub fn build_qbar_action_grid (k:&Krusty) -> ActionGrid {

    let (ks, wel, _qb) = (k.ks, k.wel, k.qb);

    use { KbdKey::*, ModKey::*};

    fn gen_incr_brightness (incr:i32) -> AF {
        Arc::new ( move || { let _ = incr_brightness(incr); } )
    }
    let brightness = ActionCell {
        label : "Brightness".into(),
        on_wheel_frwd : gen_incr_brightness ( 2),   // increase brightness
        on_wheel_bkwd : gen_incr_brightness (-2),   // decrease brightness
        ..Default::default()
    };

    let volume = ActionCell {
        label : "Volume".into(),
        on_wheel_bkwd : ag().k(VolumeDown).gen_af(),    // vol down
        on_wheel_frwd : ag().k(VolumeUp  ).gen_af(),    // vol up
        on_click      : ag().k(VolumeMute).gen_af(),    // mute
        ..Default::default()
    };

    let tracks = ActionCell {
        label : "Tracks".to_string(),
        on_wheel_bkwd : media_next_action (ks, true),                  // next track
        on_wheel_frwd : media_next_action (ks, false),                 // prev track
        on_click      : ag().k(VolumeUp).m(lctrl).m(lshift).gen_af(),  // play / pause
        ..Default::default()
    };

    let scrub = ActionCell {
        label : "Scrub".to_string(),
        on_wheel_bkwd : media_skips_action (1, ks, true),              // skip fwd  on track-bar
        on_wheel_frwd : media_skips_action (1, ks, false),             // skip bkwd on track-bar
        on_click      : ag().k(VolumeUp).m(lctrl).m(lshift).gen_af(),  // play / pause
        ..Default::default()
    };


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

    let switche = ActionCell {
        label : "Switche".to_string(),
        on_wheel_bkwd : ag().af(sw_wh_af()).gen_af(),    // invoke switche
        on_wheel_frwd : ag().af(sw_wh_af()).gen_af(),    // invoke switche
        on_release    : ag().af(sw_release).gen_af(),    // activate switche selection
        ..Default::default()
    };



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
    let switche_blind = ActionCell {
        label : "Switche Blind".to_string(),
        on_wheel_bkwd : init_af (true ),               // next window
        on_wheel_frwd : init_af (false),               // prev window
        on_hover_end  : hov_end,                       // clear refreshed flag
        on_click      : ag().k(F4).m(lalt).gen_af(),   // close tab
        ..Default::default()
    };



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
    let tabs = ActionCell {
        label : "Tabs".to_string(),
        on_wheel_bkwd : tabs_wh_af (ks, true ),          // ctrl-tab
        on_wheel_frwd : tabs_wh_af (ks, false),          // ctrl-shift-tab
        on_hover_end  : tabs_hover_end_af,               // ensure ctrl inactive
        on_release    : ag().af(tabs_release).gen_af(),  // end ctrl-tab
        ..Default::default()
    };

    let tabs_blind = ActionCell {
        label : "Tabs Blind".to_string(),
        on_wheel_bkwd : ag().k(PageDown).m(ctrl).gen_af(),   // tab next
        on_wheel_frwd : ag().k(PageUp  ).m(ctrl).gen_af(),   // tab prev
        on_click      : ag().k(W).m(lctrl).gen_af(),         // close tab
        ..Default::default()
    };



    let arrows = ActionCell {
        label : "Arrows".to_string(),
        on_wheel_bkwd : ag().k(ExtDown).gen_af(),    // arrow down
        on_wheel_frwd : ag().k(ExtUp  ).gen_af(),    // arrow up
        ..Default::default()
    };

    let diff = ActionCell {
        label : "Diff".to_string(),
        on_wheel_bkwd : ag().k(ExtDown).m(ctrl).m(alt).gen_af(),   // next diff
        on_wheel_frwd : ag().k(ExtUp  ).m(ctrl).m(alt).gen_af(),   // prev diff
        on_click      : ag().k(ExtRight).m(ctrl).m(alt).gen_af(),  // accept left -> right
        ..Default::default()
    };


    // note that to make these chrome shortcuts work .. first installed shortkeys extension ..
    // .. then there, set the hotkeys as below, and set them to exec javascript copied directly from bookmarklets
    // .. (directly trying to trigger the bookmarklets didnt work .. oh well)
    let darken  = ag().k(Slash    ).m(ctrl).gen_af();
    let lighten = ag().k(Backslash).m(ctrl).gen_af();
    let pg_dark = ActionCell {
        label : "Page Dark".to_string(),
        on_wheel_bkwd : ag().af(darken ).gen_af(),
        on_wheel_frwd : ag().af(lighten).gen_af(),
        ..Default::default()
    };

    let im_dark   = ag().k(LBracket).m(ctrl).gen_af();
    let im_bright = ag().k(RBracket).m(ctrl).gen_af();
    let im_dark = ActionCell {
        label : "Image Dark".to_string(),
        on_wheel_bkwd : ag().af(im_dark  ).gen_af(),
        on_wheel_frwd : ag().af(im_bright).gen_af(),
        ..Default::default()
    };


    ActionGrid {
        rows: 4, cols: 3,
        grid: vec! (
            vec! (arrows, diff, volume),
            vec! (switche, switche_blind, tracks),
            vec! (tabs_blind, tabs, scrub),
            vec! (brightness, pg_dark, im_dark),
        )
        // ^^ note the asymmetrical placement of sw/tabs in the grid ..
        // .. coz keep the most accessed towards edge was felt more important
    }

}




