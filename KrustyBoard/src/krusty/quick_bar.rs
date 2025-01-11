#![allow (non_upper_case_globals)]

use std::thread;
use std::time::Duration;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicIsize, Ordering};

use derive_deref::Deref;
use once_cell::sync::OnceCell;
use eframe::emath::{pos2, Rect, vec2};
use eframe::epaint::Color32;
use egui::{Align2, FontFamily, FontId, PointerButton, Pos2, ViewportBuilder};
use tao::rwh_06::{HasWindowHandle, RawWindowHandle};

use windows::Win32::Foundation::{POINT};
use windows::Win32::UI::WindowsAndMessaging::{GetCursorPos, SetWindowPos, ShowWindow, HWND_TOPMOST, SW_HIDE, SW_MINIMIZE, SW_RESTORE, SW_SHOW, SWP_NOACTIVATE, SWP_SHOWWINDOW};

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


pub struct ActionGrid {
    pub rows : u8,
    pub cols : u8,
    pub grid : Vec <Vec <ActionCell>>,
}
impl Default for ActionGrid {
    fn default() -> ActionGrid {
        ActionGrid { rows: 0, cols: 0, grid: vec![vec![]] }
    }
}


#[derive (Copy, Clone)]
pub struct QbarDims {
    pub x: u32,
    pub y: u32,
}
impl Default for QbarDims {
    fn default() -> Self {
        // we'll set default dims 300px x 100px (dpi-aware)
        Self { x: 300, y: 100 }
    }
}
impl QbarDims {
    pub fn xy (x:u32, y:u32) -> QbarDims {
        QbarDims { x, y }
    }
}



pub struct QuickBarDat {

    visible : Flag,
    persist : Flag,

    // dims and grid to be populated by user at combos setup
    dims    : Arc <Mutex <QbarDims>>,
    grid    : Arc <Mutex <ActionGrid>>,

    // we'll grab hwnd and ctx when the bar comes up
    ctx     : Arc <Mutex <Option <egui::Context>>>,
    hwnd    : AtomicIsize,

}


#[derive (Deref, Clone)]
pub struct QuickBar ( Arc <QuickBarDat> );


impl Default for QuickBar {
    fn default() -> QuickBar {
        QuickBar ( Arc::new ( QuickBarDat {
            visible : Flag::default(),
            persist : Flag::default(),
            dims    : Arc::new (Mutex::new (QbarDims::default())),
            grid    : Arc::new (Mutex::new (ActionGrid::default())),
            ctx     : Arc::new (Mutex::new (None)),
            hwnd    : AtomicIsize::default(),
        } ) )
    }
}


impl QuickBar {

    pub fn instance () -> &'static QuickBar {
        static INSTANCE: OnceCell<QuickBar> = OnceCell::new();
        INSTANCE .get_or_init ( QuickBar::default )
    }

    pub fn set_grid (&self, grid:ActionGrid) {
        *self.grid.lock().unwrap() = grid;
    }
    pub fn set_dims (&self, dims:QbarDims) {
        *self.dims.lock().unwrap() = dims;
    }

    fn get_cursor_pos() -> Pos2 {
        unsafe {
            let mut pos = POINT::default();
            let _ = GetCursorPos (&mut pos);
            pos2 (pos.x as f32, pos.y as f32)
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
        let cmd = if utils::win_check_minimized (hwnd) {SW_RESTORE } else {SW_SHOW};
        unsafe { ShowWindow (hwnd, cmd); }

        // and to move it to the right location .. (and this must come after un-minimize for the move to work)
        unsafe {
            //utils::win_set_thread_dpi_aware();
            let dims = *self.dims.lock().unwrap();
            //let (x,y) = (pos.x as i32 -200, pos.y as i32 -200);   // just a bit below cursor
            //let (x,y) = (pos.x as i32 - dims.x as i32, pos.y as i32 - dims.y as i32);   // centered around the cursor
            let (x,y) = (pos.x as i32 - dims.x as i32, pos.y as i32 - dims.y as i32 - 4);
            // ^^ exact centering lands on grid border, so we'll add minor displacement

            //let (w,h) = (dims.x as i32, dims.y as i32);
            let scaling = self.ctx.lock().unwrap().as_ref() .map_or (2.0, |ctx| ctx.pixels_per_point());
            let (w,h) = ((scaling * dims.x as f32) as i32, (scaling * dims.y as f32) as i32);
            // ^^ auto dpi-scaling seems to not happen for the w/h .. (setting thread dpi-aware didnt help)

            //SetWindowPos (hwnd, HWND_TOPMOST, pos.x as i32, pos.y as i32, 0, 0, SWP_NOSIZE | SWP_NOACTIVATE);
            SetWindowPos (hwnd, HWND_TOPMOST, x, y, w, h, SWP_SHOWWINDOW | SWP_NOACTIVATE);
            // ^^ since we couldnt put it off-screen, we resized to 0, so have to restore size now too

            // ugh, this thing has the same issue as tray-icon re the ui event loop not waking until next mouse-motion
            // so we'll just trigger one instead .. (no obvious way to do the event proxy soln here like for tray)
            key_utils::delayed_action (30, || MousePointer::move_rel(1,0))();
        }
    }

    pub fn toggle (&self) {
        // toggling will open/close it with persistence (unlike for mouse invocations with fsc)
        if self.visible.is_set() {
            self.hide(true)
        } else {
            self.show(true)
        }
    }

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


    pub fn start (&self) {

        let quickbar = self.clone();
        let dims = *self.dims.lock().unwrap();

        thread::spawn ( move || {

            let options = eframe::NativeOptions {
                //centered : true,
                viewport : ViewportBuilder::default()
                    .with_visible(false)      // set hidden .. doesnt seem to work
                    .with_active(false)       // dont grab focus
                    .with_decorations(false)  // no titlebar etc
                    .with_taskbar(false)      // dont show up in taskbar
                    .with_inner_size (vec2 (dims.x as f32, dims.y as f32)) // without this theres extra space around the painted area
                    .with_position (pos2 (-1.0 * dims.x as f32, 0.0))      // helps keep it offscreen (since invis wasnt effective)
                    .with_resizable(false)
                    .with_always_on_top(),

                event_loop_builder: Some (Box::new(|builder| {
                    use winit::platform::windows::EventLoopBuilderExtWindows;
                    builder.with_any_thread(true);
                })),
                ..Default::default()
            };

            let _ = eframe::run_native (
                "QuickBar",
                options,
                Box::new ( |cc| {
                    if let Ok(h) = cc.window_handle() {
                        if let RawWindowHandle::Win32(hr) = h.as_raw() {
                            quickbar.hwnd.store (hr.hwnd.into(), Ordering::Release);
                            utils::win_set_anim_disabled (Hwnd(hr.hwnd.into()), true);
                            let quickbar = quickbar.clone();
                            thread::spawn ( move || {
                                thread::sleep (Duration::from_millis(20));
                                quickbar.hide(true);
                            } );
                        }
                    }
                    Ok ( Box::new (quickbar) )
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
        // (and we'll use the mouse-out from ctrl-tab to release the ctrl)
        // note that using static mut rquires us to use unsafe .. should be ok here coz only the same thread calls here
        static mut hov_cell : Option<(u8,u8)> = None;

        // and sadly, will have to do the same for modkeys to track release..
        // (.. esp win-key as that being delayed, makes kr think its down when its released w qb focus, as when doing win-drag)
        //static mut mods : Option<egui::Modifiers> = None;
        // ^^ disabled as egui seems blind to win-key, and even Get[Async]KeyStatea isnt reliable w/o fgnd and due to kr hooks


        egui::CentralPanel::default() .show (ctx, |ui| {

            let canvas = ui.available_rect_before_wrap();
            let grid = self.grid.lock().unwrap();
            let (w,h) = ( canvas.width() / grid.cols as f32, canvas.height() / grid.rows as f32);

            // now we can setup the grid
            let last_hc = unsafe { hov_cell };
            unsafe { hov_cell = None };

            for row in 0 .. grid.rows {
                for col in 0 .. grid.cols {

                    let cell_dat : &ActionCell = &grid.grid[row as usize][col as usize];

                    // allocate the cell
                    let rect = Rect::from_min_size (
                        pos2 ( canvas.min.x + col as f32 * w, canvas.min.y + row as f32 * h ),
                        vec2 (w, h),
                    );

                    let cell = ui.allocate_rect(rect, egui::Sense::click_and_drag());
                    ui.painter().rect_filled ( rect, 0.0,
                        if cell.hovered() { Color32::from_gray(60) } else { Color32::from_gray(20) },
                    );

                    ui.painter().rect_stroke ( rect, 0.0, egui::Stroke::new (1.0, Color32::from_gray(120)) );
                    // ^^ adds the border between cells that makeup the grid

                    ui.painter() .text (
                        rect.center(), Align2::CENTER_CENTER, cell_dat.label.clone(),
                        FontId::new (12.0, FontFamily::Proportional),
                        Color32::from_rgb (0, 255, 255),    // text in aqua
                    );

                    if cell.hovered() {
                        // we'll eval hover-end using last stashed hover-cell id
                        // and we cant listen to wheel here, so we'll just mark which cell we're on
                        let cur_hc = (row, col);
                        if let Some(last_hc) = last_hc {
                            if last_hc != cur_hc {
                                //println! ("hover-changed .. cur: {:?} .. last: {:?}", cur_hc, last_hc);
                                grid.grid[last_hc.0 as usize][last_hc.1 as usize].hover_end_fn();
                            }
                        } else {
                            //println! ("hover-start .. cur: {:?}", cur_hc);
                            grid.grid[row as usize][col as usize].hover_fn();
                        }
                        unsafe { hov_cell = Some (cur_hc) };
                    }
                    if cell.clicked() {
                        //println!("Clicked @ {:?}", &grid.grid[row as usize][col as usize].label);
                        cell_dat.click_fn();
                        // (note that we expect focus to be already away as both press/rel do defocus calls)
                    }
                }
            }
            let cur_hc = unsafe { hov_cell };

            if cur_hc.is_none() {
                if let Some ((row, col)) = last_hc {
                    //println! ("hover-end .. last: {:?}", last_hc);
                    grid.grid[row as usize][col as usize].hover_end_fn();
                }
            }


            // for any press, we want to immediately give focus back, and send up any applicable event
            if ui.input ( |inp| inp.pointer.button_pressed(PointerButton::Primary) ) {
                self.defocus();
                if let Some ((row, col)) = cur_hc {
                    // and if it belonged to a cell, pass up the event
                    //println! ("lbtn-pressed .. cur: {:?}", cur_hc);
                    grid.grid[row as usize][col as usize].press_fn();
                }
            }
            // same for release as well
            if ui.input ( |inp| inp.pointer.button_released(PointerButton::Primary) ) {
                self.defocus();
                // ^^ useful if its press-held coz while being held, even the defocus (on press) wont work
                if let Some ((row, col)) = cur_hc {
                    //println! ("lbtn-released .. cur: {:?}", cur_hc);
                    grid.grid[row as usize][col as usize].release_fn();
                }
            }

            // right-click anywhere in the window should get us out of mode (upon release)
            if ui.input ( |inp| inp.pointer.button_released(PointerButton::Secondary)) {
                //println! ("rbtn-released .. cur: {:?}", cur_hc);
                self.hide(true);    // the force flag ensure it exits despite persist flag setting
            }

            // if we were in a cell, we'll check for wheel
            let Some ((row,col)) = cur_hc else { return };
            let ac : &ActionCell = &grid.grid[row as usize][col as usize];

            if ui.input ( |inp| inp.raw_scroll_delta.y.abs() > 0.1 ) {
                // println! ("r_del:{:?} \t s_del:{:?}", ui.input (|inp| inp.raw_scroll_delta.y), ui.input (|inp| inp.scroll_delta.y))
                // ^^ looks like on mine, raw comes out w 40 for one frame, smooth gets smeared across frames
                use std::cmp::Ordering::*;
                ui.input ( |inp| {
                    match inp.raw_scroll_delta.y.total_cmp(&0.0) {
                        Greater => ac.wheel_frwd_fn(),
                        Less    => ac.wheel_bkwd_fn(),
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
            // whatever .. its not worthwhile .. just dont drag w win-key .. it'll have its own drag spot if one really wants it

            // Further, there seem to be way too many issues and weirdness with how the ui event-loop responds to held keys ..
            // We had seen some issues in sw/kr tray too, and similar here make the whole panel non-responsive or the win-state
            // stuck out of sync both w/ kr w phantom key-dn states, and outside w stuck win keys etc .. when doing win-drags
            // In general this just doesnt feel robust enough for win-drags .. not even worth the trouble of trying out workarounds

        } );
    }
}
