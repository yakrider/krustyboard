#![allow (non_upper_case_globals)]

use std::thread;
use std::time::Duration;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicIsize, Ordering};

use once_cell::sync::OnceCell;
use eframe::emath::{pos2, Rect, vec2};
use eframe::epaint::{Color32, Vec2};
use egui::{Align2, Context, FontFamily, FontId, PointerButton, TextureHandle, ViewportBuilder};
use tao::rwh_06::{HasWindowHandle, RawWindowHandle};

use windows::Win32::UI::WindowsAndMessaging::{SetWindowPos, ShowWindow, HWND_TOPMOST, SW_MINIMIZE, SW_RESTORE, SWP_NOACTIVATE, SWP_SHOWWINDOW, SWP_NOZORDER, SWP_NOMOVE, SWP_ASYNCWINDOWPOS, HWND_BOTTOM};

use crate::*;



#[derive (Clone)]
pub struct Icon {
    txh     : TextureHandle,
    sz_hint : Vec2,
}

impl Icon {
    pub fn load (ctx: &Context, name:&str, sz_hint:Vec2, incd_file: &include_dir::File) -> Option<Icon> {
        if let Ok(img) = image::load_from_memory (incd_file.contents()) .map (|im| im.to_rgba8()) {
            let (width, height) = img.dimensions();
            let img = egui::ColorImage::from_rgba_unmultiplied ([width as usize, height as usize], &img);
            let txh = ctx.load_texture (name, img, Default::default());
            Some ( Icon { txh, sz_hint } )
        } else { None }
    }
}



/// ActionCells can have an optional pixel indicator that gets drawn at rendering time
///   if a query to the cell's IndicatorFn (if any) returns true
pub type IndicatorFn = Arc <dyn Fn() -> bool + Send + Sync + 'static>;



#[derive (Clone)]
/// ActionCell holds all the behaviors for a cell in the Quick-bar ActionGrid
/// .. (incl wheel-fwd/bkwd, hover/hover-end, press/release, click) <br>
/// The order of reporting for clicks seems to be .. press -> release -> click <br>
/// (Note ofc that there are many other egui reported interactions that we're not ignoring here)
///
pub struct ActionCell {

    pub label : String,
    pub icon  : Option<Icon>,

    pub indicator : Option <IndicatorFn>,

    pub on_wheel_bkwd : AF,
    pub on_wheel_frwd : AF,

    pub on_hover_start : AF,
    pub on_hover_end   : AF,

    pub on_press   : AF,
    pub on_release : AF,
    pub on_click   : AF,

    pub on_rbtn_press   : AF,
    pub on_rbtn_release : AF,
    pub on_rbtn_click   : AF,

    pub on_mbtn_press   : AF,
    pub on_mbtn_release : AF,
    pub on_mbtn_click   : AF,

}
impl Default for ActionCell {
    fn default() -> ActionCell { ActionCell {
        label           : "".to_string(),
        icon            : None,
        indicator       : None,
        on_wheel_bkwd   : Arc::new (|| {}),
        on_wheel_frwd   : Arc::new (|| {}),
        on_hover_start  : Arc::new (|| {}),
        on_hover_end    : Arc::new (|| {}),
        on_press        : Arc::new (|| {}),
        on_release      : Arc::new (|| {}),
        on_click        : Arc::new (|| {}),
        on_rbtn_press   : Arc::new (|| {}),
        on_rbtn_release : Arc::new (|| {}),
        on_rbtn_click   : Arc::new (|| {}),
        on_mbtn_press   : Arc::new (|| {}),
        on_mbtn_release : Arc::new (|| {}),
        on_mbtn_click   : Arc::new (|| {}),
    } }
}
impl ActionCell {
    // we'll just add some syntactic sugar for easy calling
    pub fn wheel_bkwd_fn   (&self)  { (self.on_wheel_bkwd   )() }
    pub fn wheel_frwd_fn   (&self)  { (self.on_wheel_frwd   )() }
    pub fn hover_start_fn  (&self)  { (self.on_hover_start  )() }
    pub fn hover_end_fn    (&self)  { (self.on_hover_end    )() }
    pub fn press_fn        (&self)  { (self.on_press        )() }
    pub fn release_fn      (&self)  { (self.on_release      )() }
    pub fn click_fn        (&self)  { (self.on_click        )() }
    pub fn rbtn_press_fn   (&self)  { (self.on_rbtn_press   )() }
    pub fn rbtn_release_fn (&self)  { (self.on_rbtn_release )() }
    pub fn rbtn_click_fn   (&self)  { (self.on_rbtn_click   )() }
    pub fn mbtn_press_fn   (&self)  { (self.on_mbtn_press   )() }
    pub fn mbtn_release_fn (&self)  { (self.on_mbtn_release )() }
    pub fn mbtn_click_fn   (&self)  { (self.on_mbtn_click   )() }
}



/// full grid of ActionCells that will be rendered by egui
pub struct ActionGrid {
    pub label     : String,
    pub cell_sz   : CellDims,
    pub grid_sz   : GridDims,
    pub grid      : Vec <Vec <Arc <ActionCell>>>,
    pub start_pos : Option <Point>,
}
impl Default for ActionGrid {
    fn default() -> ActionGrid {
        ActionGrid {
            label     : String::new(),
            cell_sz   : CellDims::default(),
            grid_sz   : GridDims::default(),
            grid      : vec![vec![]],
            start_pos : None,
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


#[derive (Copy, Clone, Eq, PartialEq)]
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


#[derive (Copy, Clone, Eq, PartialEq)]
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



pub type GetGridBuilderFn = Box <dyn Fn (&Context) -> GetGridFn + Send + Sync + 'static>;

pub type GetGridFn = Box <dyn Fn() -> Arc<ActionGrid> + Send + Sync + 'static>;


# [derive ()]
pub struct QuickBar {

    visible : Flag,
    persist : Flag,

    drag_active : Flag,

    prior_pos     : PointAtomic,
    prior_persist : Flag,

    // we'll acquire ks ref at init
    //ks : &'static KrustyState,

    // we'll grab hwnd and ctx when the bar comes up
    ctx  : Arc <Mutex <Option <Context>>>,
    hwnd : AtomicIsize,

    // to get the grid-provider, we'll need to pass the egui context (upon egui start)
    // (which can then be used to preload textures into the ctx etc)
    // .. so we'll let users register a grid-provider-builder fn that we'll call then
    get_grid_builder : Arc <Mutex <Option <GetGridBuilderFn>>>,

    // and so thatd return a grid_provider which we'll store and use at rendering time
    // (this is called at render-time .. we're gonna even forgo option wrapping here)
    get_grid : Arc <Mutex <GetGridFn>>,

    // and finally we'll keep a flag on when the update fn might need to refresh grid (coz fgnd change etc)
    refresh_grid : Flag,

}



impl QuickBar {

    pub fn instance () -> &'static QuickBar {

        // we'll prep a default get-grid fn that returns an empty grid
        // (instead of having the get-gird be option wrapped just for init purposes)
        let gg_empty : Arc<ActionGrid> = Arc::default();

        static INSTANCE: OnceCell<QuickBar> = OnceCell::new();
        INSTANCE .get_or_init ( ||
            QuickBar {
                visible  : Flag::default(),
                persist  : Flag::default(),

                drag_active : Flag::default(),

                prior_pos     : PointAtomic::default(),
                prior_persist : Flag::default(),

                //ks   : KrustyState::instance(),
                ctx  : Arc::new (Mutex::new (None)),
                hwnd : AtomicIsize::default(),

                get_grid_builder : Arc::new ( Mutex::new ( None ) ),
                get_grid         : Arc::new ( Mutex::new ( Box::new (move || gg_empty .clone()))),
                refresh_grid     : Flag::default(),
            }
        )
    }

    pub fn set_grid_provider_builder (&self, gpbfn: GetGridBuilderFn) {
        *self.get_grid_builder.lock().unwrap() = Some (gpbfn);
    }

    pub fn set_dragging (&self, state:bool) {
        self.drag_active.store (state);
    }
    pub fn is_drag_active (&self) -> bool {
        self.drag_active.is_set()
    }
    pub fn hwnd (&self) -> Hwnd {
        self.hwnd .load (Ordering::Relaxed) .into()
    }

    pub fn is_visible        (&self) -> bool { self.visible.is_set() }
    pub fn is_persistent     (&self) -> bool { self.persist.is_set() }

    pub fn has_prior_persist (&self) -> bool { self.prior_persist.is_set() }
    pub fn set_prior_persist (&self, v:bool) { self.prior_persist.store(v); }

    pub fn request_repaint (&self) {
        if let Some(ctx) = self.ctx.lock().unwrap().as_ref() {
            ctx.request_repaint();
        }
    }

    pub fn toggle (&self) {
        // toggling will open/close it with persistence (unlike for mouse invocations with fsc)
        if self.visible.is_clear() {
            self.show (true, false)  // bools : persist, and open at stored loc
        } else {
            self.hide (true)         // bool : hide w force flag (if was persistent)
        }
    }

    fn persist_loc (&self) {
        let hwnd = Hwnd (self.hwnd.load(Ordering::Relaxed));
        let rect = utils::win_get_window_rect(hwnd);
        let loc = Point { x: rect.left, y: rect.top };
        self.prior_pos.store (loc);
    }

    pub fn set_persistent (&self) {
        self.persist.set();
        if self.visible.is_set() { self.persist_loc() };
    }

    pub fn hide (&self, force:bool) {

        if self.persist.is_set() && !force { return };
        // ^^ if we were shown w persist flag, only a force close should hide it

        let hwnd = Hwnd (self.hwnd.load(Ordering::Relaxed));

        // lets save the qbar window position (if it was persistant) in case it has moved around
        if self.persist.is_set() { self.persist_loc() }

        self.visible.clear();
        self.persist.clear();

        // want to hide and minimze .. (coz due to egui bug, minimized windows stop event-loop but hidden windows dont!)
        // .. but then when we bring it back, it will have to be shown/restored before locating it, which causes flashing
        // .. so we'd rather move this off-screen first before we minimize it ..
        // .. but OS doesnt let us move it off-screen via SetWindowPos, so instead we'll set it to zero-sized at zero co-ords
        unsafe {
            //SetWindowPos (hwnd, HWND_BOTTOM, -200, 0, 0, 0, SWP_NOSIZE | SWP_NOACTIVATE);
            // ^^ off-screen co-ords seem to get adjusted, so we'll instead make it to zero-sized square at zero co-ords
            let _ = SetWindowPos (hwnd.into(), Some(HWND_BOTTOM), 0, 0, 0, 0, SWP_NOACTIVATE);

            //ShowWindow (hwnd, SW_HIDE);
            // ^^ cant do that, will make one core busy wait!
            let _ = ShowWindow (hwnd.into(), SW_MINIMIZE);
        }
    }

    pub fn show (&self, persist:bool, at_cursor:bool) {

        self.persist.store(persist);
        self.visible.set();
        self.refresh_grid.set();

        // we're going to hide/unhide manually, as sending Viewport cmd to unset visible appears irreversible (egui bug)
        // .. and on top, hidden egui windows seem to not stop event-loop and therefore consume cpu .. so we'll have to minimze instead
        // .. but if we're just doing minimize, we'd rather also set window size to zero etc to avoid them showing up user actions etc
        // and in general, sending egui viewport cmds to reposition etc seem unreliable, partly due to how they deal w seqeuening ..
        // e.g. windows reposition requires un-minimize to have happened first (not at same time) .. and egui queues up cmds and so on
        // so instead, we'll do most of that w win-api cmds directly which seem more robust and reliable

        let hwnd = Hwnd (self.hwnd.load(Ordering::Relaxed));

        let grid = self.get_grid.lock().expect("grid isnt setup").as_ref()();
        let grid_sz = grid.grid_px_sz();

        // we'll need scaling to calc the qbar's new position/size
        let scaling = self.ctx.lock().unwrap().as_ref() .map_or (2.0, |ctx| ctx.pixels_per_point());

        let pos = if !at_cursor {
            self.prior_pos.load()
        } else {
            let mut pos = utils::get_pointer_loc();
            pos.x -= (grid_sz.width  as i32 as f32 * scaling / 2.0) as i32;
            pos.y -= (grid_sz.height as i32 as f32 * scaling / 2.0) as i32 - 4;
            // ^^ exact centering lands on grid border, so we'll add slight vertical displacement
            pos
        };

        let width  = (scaling * grid_sz.width  as f32) as i32;
        let height = (scaling * grid_sz.height as f32) as i32;
        // ^^ manually coz auto dpi-scaling didnt seem to happen for w/h .. (setting thread dpi-aware didnt help)

        // we'll want to restore/unhide window .. (but not activate it as we'd rather it not immediately consume kbd events)
        // and to move it to the right location .. (and this must come after un-minimize for the move to work)
        unsafe {
            let _ = ShowWindow (hwnd.into(), SW_RESTORE);
            let _ = SetWindowPos (hwnd.into(), Some(HWND_TOPMOST), pos.x, pos.y, width, height, SWP_SHOWWINDOW | SWP_NOACTIVATE);
            // ^^ and this is since we couldnt put it off-screen, we resized to 0, so now have to restore size too
        }
        self.defocus();
    }

    pub fn handle_invocation (&self) {
        if self.is_persistent() {
            self.prior_persist.set();  // mark that we might want this restored upon invoc-clear
            self.persist_loc();        // store the loc to restore to later
        }
        // we simply re-open/move but w/o persist flag (ofc can click on drag-spot to make it persist)
        self.show (false, true);
    }
    pub fn handle_invocation_clear (&self) {
        // if we had prior persistent, we move qbar back there, else we hide it
        if self.prior_persist.is_clear() {
            self.hide(false);
            // ^^ closing w/o force flag means it wont close if persist flag set, e.g by kbd invocation
        } else {
            self.prior_persist.clear();
            self.show (true, false);
            // ^^ show persistent at stored loc (not at cursor)
        }
    }

    pub fn handle_fgnd_change (&self, fgnd_hwnd:Hwnd) {
        if self.visible.is_clear() { return }
        // we'd like the qb grid to check for updates, so we'll mark a flag, and wake up the ui-event-loop
        if fgnd_hwnd != Hwnd (self.hwnd.load (Ordering::Relaxed)) {
            if let Some(ctx) = self.ctx.lock().unwrap().as_ref() {
                self.refresh_grid.set();
                ctx.request_repaint();
            }
        } else {
            // but if it was qbar itself coming to fgnd, we should just give the focus back up
            self.defocus()
        }
    }

    fn resize (&self, grid: &ActionGrid) { unsafe {
        let hwnd = Hwnd (self.hwnd.load(Ordering::Relaxed));
        let scaling = self.ctx.lock().unwrap().as_ref() .map_or (1.0, |ctx| ctx.pixels_per_point());
        let grid_sz = grid.grid_px_sz();
        let width  = (scaling * grid_sz.width  as f32) as i32;
        let height = (scaling * grid_sz.height as f32) as i32;
        let _ = SetWindowPos (hwnd.into(), Some(HWND_TOPMOST), 0, 0, width, height, SWP_NOZORDER | SWP_NOMOVE | SWP_ASYNCWINDOWPOS);
    } }

    pub fn defocus (&self) {
        // the idea here is that we want all kbd input going to underlying windows rather than the qbar .. so we never want it have focus
        // .. so whenever it might be getting focus, we'd rather it quickly give it up (if it actually was fgnd)
        // and since just surrendering focus doesnt get it back to where it was, we'd rather manate that directly ourselves

        // now, we do have a fgnd-tracker that filters out for qb-hwnd, so we could in theory just send focus back to there ..
        // but that has occasional issues, e.g. if we close the top hwnd (that it was tracking as fgnd), and if the OS sends fgnd to qb ..
        // then for defocus we'd only have the now closed hwnd in the fgnd-tracker to send focus to .. which ofc wont do anything

        // so .. instead, we'll do a full win-enum req below if we are cur fgnd, and send focus to the topmost non-self hwnd (i.e second hwnd)
        // .. and as benefit, we can do more filtering on that enum to limit to likely visible/top hwnds (which the fgnd tracker doesnt do)

        let qb_hwnd = Hwnd ( self.hwnd.load(Ordering::Relaxed) );
        if utils::win_get_fgnd() == qb_hwnd {
            if let Some(zsec) = utils::win_get_switcher_hwnd__z_second() {
               if zsec != qb_hwnd { utils::win_set_fgnd(zsec) }
            }
        }
    }


    pub fn start (&'static self) {

        thread::spawn ( move || {

            // now, ideally we'd startup with the right size, positioning etc ..
            // but our grid init requires ctx to load up icons, so we cant query grid-sizes etc right away
            // .. so instead, we'll just let the window come up then resize/reposition/unhide in the startup hook below

            let options = eframe::NativeOptions {

                viewport : ViewportBuilder::default()
                    .with_active(false)      // dont grab focus
                    .with_decorations(false) // no titlebar etc
                    .with_taskbar(false)     // dont show up in taskbar
                    .with_resizable(false)   // no manual resizing, just from code
                    .with_always_on_top()    // always on top
                    .with_visible(false)     // set hidden .. but doesnt seem to do anything
                    // and for pos and size, we'll set to zeros for init .. will set them upon call to show
                    .with_position   ( pos2 (0.0, 0.0) )
                    .with_inner_size ( vec2 (0.0, 0.0) ),

                event_loop_builder: Some (Box::new (|builder| {
                    use winit::platform::windows::EventLoopBuilderExtWindows;
                    builder.with_any_thread(true);
                })),

                ..Default::default()
            };

            let app = || Box::new(self);
            let _ = eframe::run_native (
                "QuickBar",
                options,
                Box::new ( |cc| {
                    // install image loaders for image support
                    egui_extras::install_image_loaders(&cc.egui_ctx);

                    // we also want to grab/store the ctx
                    *self.ctx.lock().unwrap() = Some (cc.egui_ctx.clone());

                    // we'll grab/store the hwnd as well
                    let Ok(h) = cc.window_handle() else { return Ok(app()) };
                    let RawWindowHandle::Win32(hr) = h.as_raw() else { return Ok(app()) };
                    self.hwnd.store (hr.hwnd.into(), Ordering::Relaxed);
                    WinEventsListener::instance().record_self_hwnd(Hwnd(hr.hwnd.into()));
                    // and tweak our quick-bar window a bit
                    utils::win_set_anim_disabled (Hwnd(hr.hwnd.into()), true);
                    //utils::win_set_full_transparent (Hwnd(hr.hwnd.into())); // <- no difference

                    // and we can use the ctx to call the configured grid-provider builder ..
                    // .. which will preload any icons, and generate the grid-provider for us
                    // .. then we'll use that to further setup the startup state for the qbar window
                    if let Some(ggbfn) = self.get_grid_builder.lock().unwrap().as_ref() {
                        let get_grid_fn = ggbfn (&cc.egui_ctx);
                        let cur_grid = get_grid_fn.as_ref()();
                        thread::spawn ( move || {
                            thread::sleep (Duration::from_millis(20));
                            if cur_grid.start_pos.is_some() {
                                self.prior_pos.store (cur_grid.start_pos.unwrap());
                                self.show (true, false);    // persist, but not at cursor
                            } else {
                                self.hide(true);
                            }
                        } );
                        // and finally we can store the grid-provider fn itself
                        *self.get_grid.lock().unwrap() = get_grid_fn;
                    }

                    // and finally we can let things start
                    Ok (app())
                } )
            );

        } );
    }

}




impl eframe::App for &QuickBar {

    fn update (&mut self, ctx: &Context, _frame: &mut eframe::Frame) {

        // we'll simulate 'mouse-leave' on a cell by tracking last-hovered cell and comparing with cur-hc
        // (and e.g. we'll use the hover-end on the ctrl-tab cell to release the ctrl)
        // note that using static mut rquires us to use unsafe .. should be ok here coz only the same thread calls here
        static mut hov_cell : Option <Arc <ActionCell>> = None;

        // we also allow dynamically updating the grid based on fgnd context, so we'll want to cache a grid ref for cur painting ..
        // then if the grid changes, we'll pick up the change in the next repaint
        static mut grid_s : OnceCell <Arc <ActionGrid>> = OnceCell::new();
        let grid = unsafe {
            grid_s .get_or_init (||  self.get_grid.lock().expect("grid isnt setup").as_ref()()) .clone()
        };
        // and if the refresh-grid flag is set (typically due to fgnd change), we'll requery and update to a new grid
        if self.refresh_grid.is_set() { unsafe {
            self.refresh_grid.clear();
            if let Some(grid) = grid_s.get_mut() {
                let new_grid = self.get_grid.lock().unwrap().as_ref()();
                if !Arc::ptr_eq (grid, &new_grid) {
                    if grid.grid_sz != new_grid.grid_sz || grid.cell_sz != new_grid.cell_sz {
                        self.resize(&new_grid);
                    }
                    *grid = new_grid;
                    // now we'll come back again to pick up this updated mut static
                    ctx.request_repaint();
                    return
                }
            }
        } }

        egui::CentralPanel::default()
            .frame ( egui::Frame::none().outer_margin (egui::Margin::same(ActionGrid::OUTER_MARGIN as f32)) )
            .show ( ctx, |ui|
        {
            // now we can setup the grid
            let last_hc = unsafe { hov_cell.clone() };
            unsafe { hov_cell = None };

            // we'll also want to grab the rect of the cur hovered cell if we want to draw highlight border on it
            //let mut hov_cell_rect : Option<Rect> = None;

            // first, lets add all the cells (and their behaviors)
            for row in 0 .. grid.grid_sz.rows {
                for col in 0 .. grid.grid_sz.cols {

                    let cur_cell = &grid.grid[row as usize][col as usize];

                    // allocate the cell
                    let cpos = pos2 (
                        ActionGrid::OUTER_MARGIN as f32 + col as f32 * grid.cell_sz.width as f32,
                        ActionGrid::OUTER_MARGIN as f32 + row as f32 * grid.cell_sz.height as f32
                    );
                    let csz = vec2 (grid.cell_sz.width as f32, grid.cell_sz.height as f32);
                    let rect = Rect::from_min_size (cpos, csz);

                    let cell = ui.allocate_rect (rect, egui::Sense::click());

                    ui.painter().rect_filled ( rect, 0.0,
                        if cell.hovered() { Color32::from_gray(80) } else { Color32::from_gray(20) },
                    );
                    //ui.painter().rect_filled ( rect, 0.0, Color32::from_gray(20) );

                    ui.painter().rect_stroke ( rect, 0.0, egui::Stroke::new (0.5, Color32::from_gray(100)) );
                    // ^^ adds the border between cells that makeup the grid

                    if let Some(ico) = cur_cell.icon.as_ref() {
                        let ico_pos = pos2 ( cpos.x + (csz.x - ico.sz_hint.x)/2.0, cpos.y + (csz.y - ico.sz_hint.y)/2.0 );
                        let im_rect = Rect::from_min_size (ico_pos, ico.sz_hint);
                        egui::Image::from_texture (&ico.txh) .paint_at (ui, im_rect);
                    } else {
                        ui.painter() .text (
                            rect.center(), Align2::CENTER_CENTER, cur_cell.label.clone(),
                            FontId::new (12.0, FontFamily::Proportional),
                            Color32::from_rgb (0, 255, 255),    // text in aqua
                        );
                    }

                    // we'll then paint any optional indicator for the action-cell
                    if cur_cell.indicator.as_ref().is_some_and (|p| p()) {
                        // paint a 3px yellow square at bottom corner of cell with 1px margin
                        let sq_xy = pos2 ( rect.left() + 1.0, rect.bottom() - 4.0 );
                        let sq_wh = Rect::from_min_size (sq_xy, Vec2::splat(3.0));
                        ui.painter() .rect_filled (sq_wh, 0.0, Color32::from_rgb(255, 255, 0));
                    }

                    if cell.hovered() {
                        // we'll eval hover-end using last stashed hover-cell id
                        // and we cant listen to wheel here, so we'll just mark which cell we're on
                        if let Some(lhc) = last_hc.as_ref() {
                            if !Arc::ptr_eq (lhc, cur_cell) {
                                //println! ("hover-changed .. cur: {:?} .. last: {:?}", &cur_cell.label, &lhc.label);
                                lhc.hover_end_fn();
                                cur_cell.hover_start_fn();
                                self.defocus();
                            } else {
                                // i.e. we're hovering on the same cell as last frame
                                // we wont even do defocus here as thats a lil too frequent
                            }
                        } else {
                            //println! ("hover-start .. cur: {:?}", &cur_cell.label);
                            cur_cell.hover_start_fn();
                            self.defocus();
                        }
                        unsafe { hov_cell = Some (cur_cell.clone()) };
                        //hov_cell_rect = Some(rect);
                    }

                    if cell.clicked() {
                        // (note that we expect focus to be already away as both press/rel do defocus calls)
                        cur_cell.click_fn();
                    } else if cell.secondary_clicked() {
                        cur_cell.rbtn_click_fn();
                    } else if cell.clicked_by(PointerButton::Middle) {
                        cur_cell.mbtn_click_fn();
                    }
                }
            }

            //// lets highlight the cur covered cell (if any)
            //if let Some(rect) = hov_cell_rect {
            //    //ui.painter().rect_stroke ( rect, 0.0, egui::Stroke::new (1.0, Color32::from_rgb(180,140,0)) );
            //    ui.painter().rect_stroke ( rect, 0.0, egui::Stroke::new (1.0, Color32::from_rgb(70,170,170)) );
            //}

            // next we'll setup whole-grid behavior .. (and mouse wheels as those arent in cell inputs)

            let cur_hc = unsafe { hov_cell.as_ref() };

            if cur_hc.is_none() {
                if let Some (lhc) = last_hc {
                    //println! ("hover-end .. last: {:?}", &lhc.label);
                    lhc.hover_end_fn();
                }
                // since we hovered out of the widget, we should give up focus too (if we had it)
                self.defocus();
            }

            // for any btn action, we want to immediately give focus back, and send up any applicable event
            if ui.input (|inp| inp.pointer.button_pressed (PointerButton::Primary)) {
                self.defocus();
                if let Some(ac) = cur_hc { ac.press_fn() }
            }
            if ui.input (|inp| inp.pointer.button_released (PointerButton::Primary)) {
                self.defocus();
                if let Some(ac) = cur_hc { ac.release_fn() }
            }
            if ui.input (|inp| inp.pointer.button_pressed (PointerButton::Secondary)) {
                self.defocus();
                if let Some(ac) = cur_hc { ac.rbtn_press_fn() }
            }
            if ui.input (|inp| inp.pointer.button_released (PointerButton::Secondary)) {
                self.defocus();
                if let Some(ac) = cur_hc { ac.rbtn_release_fn() }
            }
            if ui.input (|inp| inp.pointer.button_pressed (PointerButton::Middle)) {
                self.defocus();
                if let Some(ac) = cur_hc { ac.mbtn_press_fn() }
            }
            if ui.input (|inp| inp.pointer.button_released (PointerButton::Middle)) {
                self.defocus();
                if let Some(ac) = cur_hc { ac.mbtn_release_fn() }
            }

            // if we were in a cell, we'll also check for wheel
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

        } );
    }


}
