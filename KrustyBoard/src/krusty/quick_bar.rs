#![allow (non_upper_case_globals)]

use std::thread;
use std::time::Duration;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicIsize, Ordering};

use derive_deref::Deref;
use once_cell::sync::{Lazy, OnceCell};
use eframe::emath::{pos2, Rect, vec2};
use eframe::epaint::Color32;
use egui::{Align2, FontFamily, FontId, PointerButton, Pos2, ViewportBuilder};
use tao::rwh_06::{HasWindowHandle, RawWindowHandle};

use windows::Win32::Foundation::{POINT};
use windows::Win32::UI::WindowsAndMessaging::{GetCursorPos, SetWindowPos, ShowWindow, HWND_TOPMOST, SW_HIDE, SW_MINIMIZE, SW_RESTORE, SWP_NOACTIVATE, SWP_SHOWWINDOW, SWP_NOZORDER, SWP_NOMOVE, SWP_ASYNCWINDOWPOS, SW_SHOWNOACTIVATE};

use crate::*;



#[derive (Clone)]
/// ActionCell holds all the behaviors for a cell in the Quick-bar ActionGrid
/// .. (incl wheel-fwd/bkwd, hover/hover-end, press/release, click) <br>
/// The order of reporting for clicks seems to be .. press -> release -> click <br>
/// (Note ofc that there are many other egui reported interactions that we're not ignoring here)
pub struct ActionCell {
    pub label : String,
    pub on_wheel_bkwd : AF,
    pub on_wheel_frwd : AF,
    pub on_hover      : AF,
    pub on_hover_end  : AF,
    pub on_press      : AF,
    pub on_release    : AF,
    pub on_click      : AF,
}
impl Default for ActionCell {
    fn default() -> ActionCell { ActionCell {
        label : "".to_string(),
        on_wheel_bkwd : Arc::new (|| {}),
        on_wheel_frwd : Arc::new (|| {}),
        on_hover      : Arc::new (|| {}),
        on_hover_end  : Arc::new (|| {}),
        on_press      : Arc::new (|| {}),
        on_release    : Arc::new (|| {}),
        on_click      : Arc::new (|| {}),

    } }
}
impl ActionCell {
    // we'll just add some syntactic sugar for easy calling
    pub fn wheel_bkwd_fn (&self) { (self.on_wheel_bkwd)() }
    pub fn wheel_frwd_fn (&self) { (self.on_wheel_frwd)() }
    pub fn hover_fn      (&self) { (self.on_hover     )() }
    pub fn hover_end_fn  (&self) { (self.on_hover_end )() }
    pub fn press_fn      (&self) { (self.on_press     )() }
    pub fn release_fn    (&self) { (self.on_release   )() }
    pub fn click_fn      (&self) { (self.on_click     )() }
}



/// full grid of ActionCells that will be rendered by egui
pub struct ActionGrid {
    pub label   : String,
    pub cell_sz : CellDims,
    pub grid_sz : GridDims,
    pub grid    : Vec <Vec <Arc <ActionCell>>>,
}
impl Default for ActionGrid {
    fn default() -> ActionGrid {
        ActionGrid {
            label   : String::new(),
            cell_sz : CellDims::default(),
            grid_sz : GridDims::default(),
            grid    : vec![vec![]]
        }
    }
}
impl ActionGrid {
    const OUTER_MARGIN : u32 = 3;
    fn grid_px_sz (&self) -> CellDims {
        let width  = self.cell_sz.width  * self.grid_sz.cols as u32  +  2 * Self::OUTER_MARGIN;
        let height = self.cell_sz.height * self.grid_sz.rows as u32  +  2 * Self::OUTER_MARGIN;
        CellDims::new (width, height)
    }
}


#[derive (Copy, Clone)]
pub struct CellDims { pub width: u32, pub height: u32 }

impl Default for CellDims {
    // we'll set default cell dims 90px x 30px (dpi-aware)
    fn default() -> Self { CellDims::new (90, 30) }
}
impl CellDims {
    pub fn new (width: u32, height: u32) -> CellDims {
        CellDims {width, height}
    }
}


#[derive (Copy, Clone)]
pub struct GridDims { pub rows: u8, pub cols: u8 }

impl Default for GridDims {
    // we'll set default grid to be 3x3
    fn default() -> Self { GridDims::new (3,3) }
}
impl GridDims {
    pub fn new (rows:u8, cols:u8) -> GridDims {
        GridDims {rows, cols}
    }
}



pub type GetGridFn = Arc <dyn Fn() -> Arc<ActionGrid> + Send + Sync + 'static>;

pub struct QuickBarDat {

    visible  : Flag,
    persist  : Flag,
    dragging : Flag,

    // dims and grid_provider to be populated by user at combos setup
    get_grid : Arc <Mutex <GetGridFn>>,

    // we'll acquire ks ref at init
    ks : &'static KrustyState,

    // we'll grab hwnd and ctx when the bar comes up
    ctx  : Arc <Mutex <Option <egui::Context>>>,
    hwnd : AtomicIsize,

}

#[derive (Deref, Clone)]
pub struct QuickBar ( Arc <QuickBarDat> );



impl QuickBar {

    pub fn instance () -> &'static QuickBar {
        let ag_empty : Arc<ActionGrid> = Arc::default();
        static INSTANCE: OnceCell<QuickBar> = OnceCell::new();
        INSTANCE .get_or_init ( ||
            QuickBar ( Arc::new ( QuickBarDat {
                visible  : Flag::default(),
                persist  : Flag::default(),
                dragging : Flag::default(),
                get_grid : Arc::new ( Mutex::new ( Arc::new (move || ag_empty.clone()))),
                ks       : KrustyState::instance(),
                ctx      : Arc::new (Mutex::new (None)),
                hwnd     : AtomicIsize::default(),
            } ) )
        )
    }

    pub fn set_grid_provider (&self, gpfn: GetGridFn) {
        *self.get_grid.lock().unwrap() = gpfn;
    }

    pub fn set_dragging (&self, state:bool) {
        self.dragging.store (state);
    }
    pub fn is_drag_active (&self) -> bool {
        self.dragging.is_set()
    }

    fn get_cursor_pos() -> Pos2 {
        unsafe {
            let mut pos = POINT::default();
            let _ = GetCursorPos (&mut pos);
            pos2 (pos.x as f32, pos.y as f32)
        }
    }

    pub fn is_visible    (&self) -> bool { self.visible.is_set() }
    pub fn is_persistent (&self) -> bool { self.persist.is_set() }

    pub fn toggle (&self) {
        // toggling will open/close it with persistence (unlike for mouse invocations with fsc)
        if self.visible.is_set() {
            self.hide(true)
        } else {
            self.show(true)
        }
    }

    pub fn hide (&self, force:bool) {

        if self.visible.is_clear() { return };

        if !force && self.persist.is_set() { return };
        // ^^ if we were shown w persist flag, only a force close should hide it

        self.visible.clear();
        self.persist.clear();

        // want to hide and minimze .. (coz egui bug, minimized windows stop event-loop but hidden windows dont!)
        // .. but then when we bring it back, it will have to be shown/restored before locating it, which causes flashing
        // .. so we'd rather move this off-screen first before we minimize and hide it
        let hwnd = Hwnd (self.hwnd.load(Ordering::Relaxed));
        unsafe {
            //SetWindowPos (hwnd, HWND_BOTTOM, -200, 0, 0, 0, SWP_NOSIZE | SWP_NOACTIVATE);
            // ^^ off-screen co-ords seem to get adjusted, so we'll instead make it to zero-sized square at zero co-ords
            SetWindowPos (hwnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE);
            ShowWindow (hwnd, SW_MINIMIZE | SW_HIDE);
        }
    }

    pub fn show (&self, persist:bool) {

        self.persist.store(persist);
        // ^^ we'll update this even if we were already open

        if self.visible.is_set() { return }

        self.visible.set();
        // we're going to hide/unhide manually, as sending Viewport cmd to unset visible appears irreversible (egui bug)
        let hwnd = Hwnd (self.hwnd.load(Ordering::Relaxed));
        let pos = Self::get_cursor_pos();

        // we'll want to restore/unhide window .. (but not activate it as we'd rather it not immediately consume kbd events)
        unsafe {
            ShowWindow (hwnd, SW_RESTORE);
            ShowWindow (hwnd, SW_SHOWNOACTIVATE);
            self.defocus();
        }

        // and to move it to the right location .. (and this must come after un-minimize for the move to work)
        unsafe {
            //utils::win_set_thread_dpi_aware();
            // lets calc the qbar's new position (around the cursor)
            let scaling = self.ctx.lock().unwrap().as_ref() .map_or (2.0, |ctx| ctx.pixels_per_point());
            let grid = self.get_grid.lock().expect("grid isnt setup").as_ref()();
            let grid_sz = grid.grid_px_sz();
            let x = pos.x as i32 - (grid_sz.width  as i32 as f32 * scaling / 2.0) as i32;
            let y = pos.y as i32 - (grid_sz.height as i32 as f32 * scaling / 2.0) as i32 - 4;
            // ^^ exact centering lands on grid border, so we'll add slight vertical displacement

            let width  = (scaling * grid_sz.width  as f32) as i32;
            let height = (scaling * grid_sz.height as f32) as i32;
            // ^^ manually coz auto dpi-scaling didnt seem to happen for w/h .. (setting thread dpi-aware didnt help)

            SetWindowPos (hwnd, HWND_TOPMOST, x, y, width, height, SWP_SHOWWINDOW | SWP_NOACTIVATE);
            // ^^ since we couldnt put it off-screen, we resized to 0, so have to restore size now too

            // ugh, this thing has the same issue as tray-icon re the ui event loop not waking until next mouse-motion
            // so we'll just trigger one instead .. (no obvious way to do the event proxy soln here like for tray)
            key_utils::delayed_action (30, || MousePointer::move_rel(1,0))();
        }
    }

    fn resize (&self, grid: &ActionGrid) { unsafe {
        let hwnd = Hwnd (self.hwnd.load(Ordering::Relaxed));
        let scaling = self.ctx.lock().unwrap().as_ref() .map_or (1.0, |ctx| ctx.pixels_per_point());
        let grid_sz = grid.grid_px_sz();
        let width  = (scaling * grid_sz.width  as f32) as i32;
        let height = (scaling * grid_sz.height as f32) as i32;
        SetWindowPos (hwnd, HWND_TOPMOST, 0, 0, width, height, SWP_NOZORDER | SWP_NOMOVE | SWP_ASYNCWINDOWPOS);
    } }

    pub fn defocus (&self) {
        // setting fgnd to desktop helps avoid us getting kbd input, coz then apparently even krusty cant hear it!
        // (.. just doing SetFocus didnt seem to help) ..
        // (nor surrendering focus at the grid cell action itself.. which just seem to give focus to some OS accesibility overlay )

        //SetForegroundWindow (GetDesktopWindow());
        // ^^ this is barely better .. we'd want to have the focus go back to whatever top window we were sending kbd events to before

        //unsafe {
        //    use utils::*;
        //    let qb_hwnd = Hwnd ( self.hwnd.load(Ordering::Acquire) );
        //    let fgnd_old = Hwnd ( self.fgnd.load(Ordering::Acquire) );
        //    let fgnd = utils::win_get_fgnd();
        //    // ^^ previously stashed fgnd hwnd at time of invocation
        //    // (dbg note .. also uncomment the line in update_fgnd_info that prints fgnd exe names)
        //    dbg! ((qb_hwnd.0, fgnd_old.0, fgnd.0));
        //    dbg! (GetWindow (win_get_fgnd(), GW_HWNDNEXT));
        //    dbg! (GetWindow (fgnd_old, GW_HWNDNEXT));
        //    dbg! (win_get_class_hwnd__z_first (fgnd_old));
        //    dbg! (win_get_class_hwnd__z_first (fgnd));
        //    dbg! (win_get_class_hwnd__z_next (fgnd_old));
        //    dbg! (win_get_class_hwnd__z_next (fgnd));
        //    dbg! (win_get_switcher_hwnd__z_first());
        //    dbg! (win_get_switcher_hwnd__z_second());
        //}
        // ^^ only the last one there works reasonably enough .. (due to overlays, topmost vs regular groups etc etc)

        let qb_hwnd = Hwnd ( self.hwnd.load(Ordering::Acquire) );
        if let Some(zsec) = utils::win_get_switcher_hwnd__z_second() {
            //dbg! ((qb_hwnd.0, fgnd.0, zsec.0));
            if zsec != qb_hwnd { utils::win_set_fgnd(zsec) }
        }

    }

    fn kick_win_key_rehab (&self) {
        // because egui eats kbd events, but has no concept of win-key, krusty states can get out-of-sync
        // .. (causing krusty-unaware win outside, or krusty in win-dn while it is not .. pretty damaging)
        // so instead, if we ever find lwin dn, we'll mark lwin active .. (which we check/set in all repaints)
        //   and if we we think we're active (whether cur down or not) by the time we hover out, we'll straight up masked-release
        //   basically, qbar will be a zone where lwin is immediately 'canceled' .. we can live w that
        // now, since fast drags will mover cursor temp out of qbar, we want to delay this until the drag btn is released
        // (ideally, ofc, all of this would be moot if we could detect win-press/rel inside egui and update state)
        static _armed : Lazy<Flag> = Lazy::new (Flag::default);
        let armed = &_armed;
        if armed.is_clear() {
            armed.set();
            let ks = self.ks ;
            thread::spawn (move || { loop {
                thread::sleep (Duration::from_millis(100));
                if ks.mouse.lbtn.down.is_clear() {
                    armed.clear();
                    if ks.mod_keys.lwin.active.is_set() {
                        ks.mod_keys.lwin.down.clear();
                        ks.mod_keys.lwin.release_w_masking();
                    }
                    break;
                }
            } } );
        }
    }
    fn win_key_dirty_check (&self) {
        // so now the checking part for the above hack ..
        // (and this should be called just in hover-out, but every repaint .. its just a few atomic ops)
        if self.ks.mod_keys.lwin.down.is_set() {
            self.ks.mod_keys.lwin.active.set();
            self.ks.mod_keys.lwin.consumed.set();
        }
    }


    pub fn start (&'static self) {

        thread::spawn ( move || {

            let grid = self.get_grid.lock().expect("grid isnt setup").as_ref()();

            let options = eframe::NativeOptions {
                //centered : true,
                viewport : ViewportBuilder::default()
                    .with_visible(false)      // set hidden .. doesnt seem to work
                    .with_active(false)       // dont grab focus
                    .with_decorations(false)  // no titlebar etc
                    .with_taskbar(false)      // dont show up in taskbar
                    .with_resizable(false)    // no manual resizing, just from code
                    .with_always_on_top()     // always on top
                    .with_inner_size (vec2 (grid.grid_px_sz().width as f32, grid.grid_px_sz().height as f32))
                    // ^^ without this theres extra space around the painted area
                    .with_position (pos2 (-1.0 * grid.grid_px_sz().width as f32, 0.0)),
                    // ^^ helps keep it offscreen (since setting visible false didnt keep it hidden at startup)

                event_loop_builder: Some (Box::new(|builder| {
                    use winit::platform::windows::EventLoopBuilderExtWindows;
                    builder.with_any_thread(true);
                })),
                ..Default::default()
            };

            let app = || Box::new(self.clone());
            let _ = eframe::run_native (
                "QuickBar",
                options,
                Box::new ( |cc| {
                    let Ok(h) = cc.window_handle() else { return Ok(app()) };
                    let RawWindowHandle::Win32(hr) = h.as_raw() else { return Ok(app()) };
                    self.hwnd.store (hr.hwnd.into(), Ordering::Relaxed);
                    WinEventsListener::instance().record_self_hwnd(Hwnd(hr.hwnd.into()));
                    utils::win_set_anim_disabled (Hwnd(hr.hwnd.into()), true);
                    thread::spawn ( move || {
                        thread::sleep (Duration::from_millis(20));
                        self.hide(true);
                    } );
                    Ok (app())
                } )
            );

        } );
    }

}




impl eframe::App for QuickBar {

    fn update (&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {

        if self.ctx.lock().ok() .is_some_and (|o| o.is_none()) {
            *self.ctx.lock().unwrap() = Some (ctx.clone());
        }
        if self.visible.is_clear() {
            return
        }

        // we'll simulate 'mouse-leave' on a cell by tracking last-hovered cell and comparing with cur-hc
        // (and e.g. we'll use the hover-end on the ctrl-tab cell to release the ctrl)
        // note that using static mut rquires us to use unsafe .. should be ok here coz only the same thread calls here
        static mut hov_cell : Option <Arc <ActionCell>> = None;

        // and sadly, will have to do the same for modkeys to track release..
        // (.. esp win-key as that being delayed, makes kr think its down when its released w qb focus, as when doing win-drag)
        //static mut mods : Option<egui::Modifiers> = None;
        // ^^ disabled as egui seems blind to win-key, and even Get[Async]KeyStatea isnt reliable w/o fgnd and due to kr hooks

        // we also allow dynamically updating the grid based on fgnd context, so we'll want to cache a grid ref for cur painting ..
        // then if the grid changes, we'll pick up the change in the next repaint
        static mut grid_s : OnceCell <Arc <ActionGrid>> = OnceCell::new();
        let grid = unsafe { grid_s .get_or_init (||  self.get_grid.lock().expect("grid isnt setup").as_ref()()) .clone() };

        let update_grid_s = || unsafe {
            if let Some(grid) = grid_s.get_mut() {
                let new_grid = self.get_grid.lock().unwrap().as_ref()();
                if !Arc::ptr_eq (grid, &new_grid) {
                    self.resize(&new_grid);
                    *grid = new_grid;
                    ctx.request_repaint();
                }
            }
        };

        egui::CentralPanel::default()
            .frame ( egui::Frame::none().outer_margin (egui::Margin::same(ActionGrid::OUTER_MARGIN as f32)) )
            .show (ctx, |ui|
        {
            // now we can setup the grid
            let last_hc = unsafe { hov_cell.clone() };
            unsafe { hov_cell = None };

            // first, lets add all the cells (and their behaviors)
            for row in 0 .. grid.grid_sz.rows {
                for col in 0 .. grid.grid_sz.cols {

                    let cur_hc = &grid.grid[row as usize][col as usize];

                    // allocate the cell

                    let rect = Rect::from_min_size (
                        pos2 ( ActionGrid::OUTER_MARGIN as f32 + col as f32 * grid.cell_sz.width as f32,
                               ActionGrid::OUTER_MARGIN as f32 + row as f32 * grid.cell_sz.height as f32 ),
                        vec2 ( grid.cell_sz.width as f32, grid.cell_sz.height as f32 ),
                    );

                    let cell = ui.allocate_rect(rect, egui::Sense::click());

                    ui.painter().rect_filled ( rect, 0.0,
                        if cell.hovered() { Color32::from_gray(60) } else { Color32::from_gray(20) },
                    );

                    ui.painter().rect_stroke ( rect, 0.0, egui::Stroke::new (1.0, Color32::from_gray(120)) );
                    // ^^ adds the border between cells that makeup the grid

                    ui.painter() .text (
                        rect.center(), Align2::CENTER_CENTER, cur_hc.label.clone(),
                        FontId::new (12.0, FontFamily::Proportional),
                        Color32::from_rgb (0, 255, 255),    // text in aqua
                    );

                    if cell.hovered() {
                        // we'll eval hover-end using last stashed hover-cell id
                        // and we cant listen to wheel here, so we'll just mark which cell we're on
                        if let Some(lhc) = last_hc.as_ref() {
                            if !Arc::ptr_eq (lhc, cur_hc) {
                                //println! ("hover-changed .. cur: {:?} .. last: {:?}", &cur_hc.label, &lhc.label);
                                lhc.hover_end_fn();
                                self.defocus();
                                // ^^ we never want focus .. faster we get rid, the better
                                update_grid_s();
                                // ^^ want dyanmic grid, but every frame is too much, so we check on hover events
                            }
                        } else {
                            //println! ("hover-start .. cur: {:?}", &cur_hc.label);
                            cur_hc.hover_fn();
                            self.defocus();
                            update_grid_s();
                        }
                        unsafe { hov_cell = Some (cur_hc.clone()) };
                    }
                    if cell.clicked() {
                        //println!("Clicked @ {:?}", &cur_hc.label);
                        cur_hc.click_fn();
                        // (note that we expect focus to be already away as both press/rel do defocus calls)
                    }
                }
            }

            // next we'll setup whole-grid behavior .. (and mouse wheels as those arent in cell inputs)

            let cur_hc = unsafe { hov_cell.as_ref() };

            if cur_hc.is_none() {
                if let Some (lhc) = last_hc {
                    //println! ("hover-end .. last: {:?}", &lhc.label);
                    lhc.hover_end_fn();
                }
                // since we hovered out of the widget, we should give up focus too (if we had it)
                self.defocus();

                // temp hack until egui fixes win-key support .. see comments on fn for details
                self.kick_win_key_rehab();

            }
            // second part of the egui win-key issue hack ..
            // ( we're checking not just in hover-out, but every repaint as its just a few atomic ops)
            self.win_key_dirty_check();

            // ^^ ughh .. now this means when dragging, if it goes out qbar, we'll rel/clear win, but since lwin spams, it will
            // soon get set down again, upon which, we'll start a drag of whatever is under the pointer .. gaaaah
            // .. k we updated to filter out hook level reported repeats .. but we'd still stop dragging
            // hence why we've added the dirty-checking in every frame, and win-rehab on hover-out ..
            // seems ok for a temp solution untill egui adds in win-key to their modifiers list


            // for any press, we want to immediately give focus back, and send up any applicable event
            if ui.input ( |inp| inp.pointer.button_pressed(PointerButton::Primary) ) {
                self.defocus();
                if let Some (chc) = cur_hc {
                    // and if it belonged to a cell, pass up the event
                    //println! ("lbtn-pressed .. cur: {:?}", &chc.label);
                    chc.press_fn();
                }
            }
            // same for release as well
            if ui.input ( |inp| inp.pointer.button_released(PointerButton::Primary) ) {
                self.defocus();
                // ^^ useful if its press-held coz while being held, even the defocus (on press) wont work
                if let Some (chc) = cur_hc {
                    //println! ("lbtn-released .. cur: {:?}", &chc.label);
                    chc.release_fn();
                }
            }

            // right-click anywhere in the window should get us out of mode (upon release)
            if ui.input ( |inp| inp.pointer.button_released(PointerButton::Secondary)) {
                //println! ("rbtn-released .. cur: {:?}", cur_hc);
                self.hide(true);    // the force flag ensure it exits despite persist flag setting
            }

            // if we were in a cell, we'll check for wheel
            let Some (chc) = cur_hc else { return };

            if ui.input ( |inp| inp.raw_scroll_delta.y.abs() > 0.1 ) {
                // println! ("r_del:{:?} \t s_del:{:?}", ui.input (|inp| inp.raw_scroll_delta.y), ui.input (|inp| inp.scroll_delta.y))
                // ^^ looks like on mine, raw comes out w 40 for one frame, smooth gets smeared across frames
                use std::cmp::Ordering::*;
                ui.input ( |inp| {
                    match inp.raw_scroll_delta.y.total_cmp(&0.0) {
                        Greater => chc.wheel_frwd_fn(),
                        Less    => chc.wheel_bkwd_fn(),
                        Equal   => { },
                    }
                } );
            }


            // ugh, we wanted to track win mod-key state change to avoid kr getting out of sync when egui eats a win-release ..
            // .. but egui doesnt even seem to have any registration for win-key .. nothing in their Modifiers struct tracks it!
            //
            //if ui.input ( |inp| inp.key_released (egui::data::key::Key::) );
            //let cur_mods = ui.input ( |inp| inp.modifiers);
            //unsafe {
            //    if mods != Some(cur_mods) {
            //        println! ("modifs changed");
            //        dbg! ((mods, Some(cur_mods)));
            //        mods = Some(cur_mods);
            //    }
            //}
            //^^^ Nope, no registraion of win-key at all .. (in their struct with alt, ctrl, shift, cmd)

            // So instead, we'll have to try and directly query the OS
            //let ks = KrustyState::instance();
            //if ks.mod_keys.lwin.down.is_set() {
            //    if unsafe { dbg!(GetAsyncKeyState (VK_LWIN.0 as i32)) } >= 0 { dbg!("got it");
            //        ks.mod_keys.lwin.down.clear();
            //        ks.mod_keys.lwin.dbl_tap.clear();
            //    }
            //}
            //  ^^^^ even this doesnt work coz apparently neither GetKeyState nor GetAsyncKeyState can be relied upon to ..
            // .. always return the actual state .. limitations re fgnd thread etc .. and further due to kr hooks and suppression
            //
            // Further, there seem to be way too many issues and weirdness with how the ui event-loop responds to held keys ..
            // We had seen some issues in sw/kr tray too, and similar here make the whole panel non-responsive or the win-state
            // stuck out of sync both w/ kr w phantom key-dn states, and outside w stuck win keys etc .. when doing win-drags
            //
            // so whatever .. for now .. we'll instead make entire qbar a (delayed) win-key-kill-zone .. avoids stuck states
            // (basically upon leaving qbar, if at any point lwin was seen down, we straight setup a masked release for when lbtn comes up)


        } );
    }


}
