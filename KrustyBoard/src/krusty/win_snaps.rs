#![ allow (non_snake_case, non_upper_case_globals) ]


use std::sync::{Arc, RwLock, Mutex};

use once_cell::sync::Lazy;
use rand::Rng;
use rustc_hash::FxHashMap;

use windows::Win32::Foundation::{BOOL, HWND, LPARAM, POINT, RECT};

use crate::{ *, utils::*};



# [ derive (Debug, Default, Copy, Clone) ]
pub struct Edge {xy:i32, tl:i32, br: i32}
// ^^ basically we're representing both vert and horiz edge sections .. if v, need {x, top, bottom}, if h need {y, l, r}


# [ derive (Debug, Default, Copy, Clone) ]
pub struct RectEdges { left:Edge, top:Edge, right:Edge, bottom:Edge }



# [ derive (Debug, Default, Clone) ]
pub struct RectEdgeLists { vert:Vec<Edge>, horiz:Vec<Edge> }
impl RectEdgeLists {
    pub fn new_w_capacity (n:usize) -> RectEdgeLists {
        RectEdgeLists { vert: Vec::with_capacity(n), horiz:Vec::with_capacity(n) }
    }
}


# [ derive (Debug, Default, Clone) ]
pub struct PreDragDat {
    pub hwnd        : Hwnd,
    pub rect        : RECT,
    pub padding     : RECT,
    pub snap_thresh : u32,
    pub pointer     : POINT,
    pub edge_lists  : RectEdgeLists,
    pub grp_rects   : FxHashMap <Hwnd,RECT>,
}


# [ derive (Debug, Default, Copy, Clone) ]
struct Bounds { low:i32, high:i32 }
impl Bounds {
    # [ allow (dead_code) ]
    // The low and high bounds must be ordered for use in edge unification ..
    // .. however, the use of this is expected to be minimal as the ordering should be specified that way during creation
    // We'll leave it here both as a reminder, and for future reference/use
    fn ordered(&self) -> Bounds {
        if self.low <= self.high { *self } else { Bounds {low:self.high, high:self.low} }
    }
}





fn handle_pointer_window_drag (x:i32, y:i32, ks:&KrustyState) {
    // we'll have saved mouse loc on win-lclick, so we can reference that on where/how to move the window
    let pdd = ks.mouse.pre_drag_dat.read().unwrap();    // we'll hold this read-lock until this fn exits .. should be ok
    let (dx, dy) = ( x - pdd.pointer.x, y - pdd.pointer.y );
    let dest =  RECT {
        left   : pdd.rect.left   + dx,
        top    : pdd.rect.top    + dy,
        right  : pdd.rect.right  + dx,
        bottom : pdd.rect.bottom + dy,
    };
    let mut dr_snap = RECT::default();
    if !ks.mod_keys.lshift.down.is_set() {
        // if shift is down, we'll disable snap to allow finer drag/resize motions
        dr_snap = snap_to_edge_rect_delta (&dest, &pdd);
    }
    // we want to keep width/height same so we'll snap to left (and top) in preference to right (and bottom) if both of are available
    let dx_snap = if dr_snap.left != 0 { dr_snap.left} else { dr_snap.right };
    let dy_snap = if dr_snap.top  != 0 { dr_snap.top } else { dr_snap.bottom};
    win_move_to (
        pdd.hwnd,
        dest.left + dx_snap,
        dest.top  + dy_snap,
        dest.right  - dest.left,
        dest.bottom - dest.top
    );
    if ks.mode_states.qks1.down.is_set() { win_grp_move_mirrored (dx + dx_snap, dy + dy_snap, ks) }
}
fn win_grp_move_mirrored (dx:i32, dy:i32, ks:&KrustyState) {     //println!("{:?}",(dx,dy));
    let pdd = ks.mouse.pre_drag_dat.read().unwrap();
    pdd.grp_rects .iter() .for_each (|(&hwnd,&rect)| {
        win_move_to (
            hwnd,
            rect.left + dx - pdd.padding.left,
            rect.top  + dy - pdd.padding.top,
            rect.right  - rect.left + pdd.padding.left + pdd.padding.right,
            rect.bottom - rect.top  + pdd.padding.top  + pdd.padding.bottom,
        );
    } )
}

pub fn handle_pointer_window_drag_spaced (x:i32, y:i32, ks:&KrustyState) {
    // pointer move events stream much faster than reasonable to repaint for smooth perf .. so we'll redraw only for a fraction
    if rand::thread_rng().gen_range(0..10) < 8 {  handle_pointer_window_drag (x,y,ks) }
}




fn handle_pointer_window_resize (x:i32, y:i32, ks:&KrustyState) {
    let pdd = ks.mouse.pre_drag_dat.read().unwrap();    // will hold read-lock until this fn exits
    let dest = RECT {
        left   : pdd.rect.left,
        top    : pdd.rect.top,
        right  : pdd.rect.right  + (x - pdd.pointer.x),
        bottom : pdd.rect.bottom + (y - pdd.pointer.y),
    };
    let mut dr = RECT::default();
    if !ks.mod_keys.lshift.down.is_set() {
        // if shift is down, we'll disable snap to allow finer drag/resize motions
        dr = snap_to_edge_rect_delta (&dest, &pdd);
    }
    win_move_to (
        pdd.hwnd,  dest.left,  dest.top,
        dest.right  - dest.left + dr.right,
        dest.bottom - dest.top  + dr.bottom
    );
}
pub fn handle_pointer_window_resize_spaced (x:i32, y:i32, ks:&KrustyState) {
    // pointer move events stream much faster than reasonable to repaint for smooth perf ..
    // .. this is even more critical for resize as compared to drag, so we'll redraw for even smaller fraction of reports
    if rand::thread_rng().gen_range(0..10) < 2 {  handle_pointer_window_resize (x,y,ks) }
}




pub fn handle_pointer_action_cancel (ks:&KrustyState) {
    let pdd = ks.mouse.pre_drag_dat.read().unwrap();
    win_move_to (
        pdd.hwnd,  pdd.rect.left,  pdd.rect.top,
        pdd.rect.right - pdd.rect.left,
        pdd.rect.bottom - pdd.rect.top
    );
}







pub fn capture_pre_drag_dat (ks:&KrustyState) -> PreDragDat {    //println!("{:?}",("pre-cache"));
    // first set thread dpi-aware in case we're on some spawned thread not inited w that (unlike our event queues)
    win_set_thread_dpi_aware();

    // now lets get the data on the window we want to drag/resize
    let point = MousePointer::pos();
    let hwnd = win_get_hwnd_from_point(point);

    // we also want all the windows to calc the current set of visible edge sections to enable snapping on
    // NOTE that we want as little time gap between these and self-hwnd query to avoid picking up moving mouse translation in these
    let mut rects = gather_win_rects(hwnd);
    // and now lets get more data for this hwnd
    let rect = win_get_window_rect (hwnd);
    let frame = win_get_window_frame (hwnd);
    let padding = calc_window_padding (&rect, &frame);

    // we'll want to store rects for any group windows to make mirrored moves if in grp mode
    let grp_rects : FxHashMap <Hwnd,RECT> = if ks.mode_states.qks1.down.is_set() {    //println!("{:?}",("caching"));
        let grp_hwnds = ks.win_groups.check_win_group(hwnd) .map (|g| ks.win_groups.get_grp_hwnds(g)) .unwrap_or_default();
        grp_hwnds .into_iter() .filter_map ( |gh| rects.iter() .find (|(h,_)| gh == *h) .copied() ) .collect()
    } else { FxHashMap::default() };

    // plus lets get the full workable area of the screen to add screen edges to edge-list
    // we'll also use that as cheap dpi-awareness mechanism by setting snap threshold to 2% of screen height
    let wa = win_get_work_area();
    let snap_thresh = ((wa.bottom - wa.top) / 50) as u32;
    rects.push ((Hwnd(0), wa));
    // lets finally calculate our visible edges lists
    let edge_lists = rects_to_viz_edgelists (&mut rects);

    // and thats it, can store it all away so the actual pointer-move callbacks can be fast!
    PreDragDat { hwnd, rect, padding, snap_thresh, pointer: point, edge_lists, grp_rects }
}



fn calc_window_padding (rect:&RECT, frame:&RECT) -> RECT {
    RECT {  // rect includes padding of the 'drop shadow' around the frame
        left   : ( rect.left   -  frame.left   ).abs(),
        top    : ( rect.top    -  frame.top    ).abs(),
        right  : ( rect.right  -  frame.right  ).abs(),
        bottom : ( rect.bottom -  frame.bottom ).abs(),
    }
}



fn rect_to_edges (rect:&RECT) -> RectEdges {
    let left   = Edge { xy: rect.left,   tl: rect.top,  br: rect.bottom };
    let top    = Edge { xy: rect.top,    tl: rect.left, br: rect.right  };
    let right  = Edge { xy: rect.right,  tl: rect.top,  br: rect.bottom };
    let bottom = Edge { xy: rect.bottom, tl: rect.left, br: rect.right  };
    RectEdges { left, top, right, bottom }
}
fn _rects_to_edgelists (rects: &Vec <RECT>) -> RectEdgeLists {
    let mut rels = rects .iter() .map (|r| rect_to_edges(r)) .fold ( RectEdgeLists::new_w_capacity (rects.len()), |mut rels, re| {
        rels.vert.push(re.left); rels.horiz.push(re.top); rels.vert.push(re.right); rels.horiz.push(re.bottom);
        rels
    } );
    RectEdgeLists { vert: reduce_edge_sects(&mut rels.vert), horiz: reduce_edge_sects(&mut rels.horiz)}
}

fn rects_to_viz_edgelists (rects: &mut Vec<(Hwnd, RECT)>) -> RectEdgeLists {
    // Note: we rely on the assumption here that the rects are straight from win-enum call and are therefore in z-order
    let mut viz_edge_lists = RectEdgeLists::new_w_capacity (2 * rects.len()); // x2 because we make horiz/vert vecs each w 2 edges per rect
    for i in 0 .. rects.len() {
        let rect_edges = rect_to_edges(&rects[i].1);
        rects[..i] .sort_unstable_by_key ( |(_,r)| -1 * (r.right - r.left) * (r.bottom - r.top) );
        // ^^ sorting by decreasing area ensures we check the largest windows first (as they're most likely to occlude)
        // .. (and since we only need to re-sort the section before our cur iteration location, it's safe to do in place!)
        viz_edge_lists.vert  .append ( &mut calc_edges_viz_sects (vec![rect_edges.left, rect_edges.right ], true,  &rects[..i]) );
        viz_edge_lists.horiz .append ( &mut calc_edges_viz_sects (vec![rect_edges.top,  rect_edges.bottom], false, &rects[..i]) );
    };
    viz_edge_lists.vert  = reduce_edge_sects (&mut viz_edge_lists.vert);
    viz_edge_lists.horiz = reduce_edge_sects (&mut viz_edge_lists.horiz);
    //println!("\n{:#?}\n",(&viz_edge_lists.vert));
    viz_edge_lists
}

fn calc_edges_viz_sects (sects:Vec<Edge>, is_vert:bool, rects: &[(Hwnd, RECT)]) -> Vec<Edge> {
    //println!("\nrects-len: {:#?}",(rects.len()));
    // if we've already gone through all the rects above us, we're done
    if rects.is_empty() { return sects }
    // else we'll calc the viz results from the current rect
    let sects_viz = sects .iter() .map (|&edge| {
        calc_edge_viz_sects_for_rect (&edge, is_vert, rects.first().unwrap())
    } ) .flatten() .collect::<Vec<Edge>>();
    //println!("\nsects-viz: {:#?}",(sects_viz));
    // and again, if nothing is viz anymore, we're done for this edge
    if sects_viz.is_empty() { return sects_viz }
    // else we'll recurse down the remaining rects underneath it (but above our edges z layer)
    calc_edges_viz_sects (sects_viz, is_vert, &rects[1 ..])
}

fn calc_edge_viz_sects_for_rect (edge:&Edge, is_vert:bool, hrect: &(Hwnd, RECT)) -> Vec<Edge> {     //println!("\n{:#?}",(edge,rect));
    let (_, rect) = hrect;
    if is_vert {
        if edge.xy <= rect.left || edge.xy >= rect.right { return vec![*edge] }
        calc_edge_viz_sects_for_bounds (edge, &Bounds {low: rect.top, high: rect.bottom} )
    } else {
        if edge.xy <= rect.top || edge.xy >= rect.bottom { return vec![*edge] }
        calc_edge_viz_sects_for_bounds (edge, &Bounds {low: rect.left, high: rect.right} )
    }
}

fn calc_edge_viz_sects_for_bounds (edge:&Edge, bounds:&Bounds) -> Vec<Edge> {
    //let bounds = bounds.ordered();    // prob can skip this redundant check as only calls from our own fns get here
    // Note: we assume that by the time we're here there must be occlusion (so we should end up with either one or two shorter edges)
    let mut edges = Vec::<Edge>::with_capacity(2);
    if edge.tl < bounds.low  { edges .push ( Edge { xy:edge.xy, tl:edge.tl,     br:bounds.low } ) }
    if edge.br > bounds.high { edges .push ( Edge { xy:edge.xy, tl:bounds.high, br:edge.br    } ) }
    //println!("\n{:#?}",(edge,bounds,&edges));
    edges
}


fn reduce_edge_sects(edges: &mut Vec<Edge>) -> Vec<Edge> {
     use itertools::Itertools;      // for group_by trait
    // Note that this only works for edges of the same type (cant mix horiz and vert edges)
    // Our goal is to make only a single pass through groups of edges at the same displacement sorted by linear dim (x for vert, y for horiz)
    // .. then we can start going through the sorted list combining edges where possible or else starting new sections
    let mut reduced = Vec::new();
    edges .sort_unstable_by_key (|e| (e.xy, e.tl));
    // ^^ sorting by displacement first, then by start of section means we can do everything in one pass
    edges.iter() .group_by (|e| e.xy) .into_iter() .for_each ( |(_xy, mut grp)| {
        // for each grp, we seed its portion of reduced edge-list with its first edge
        // (we'll maintain the invariant that the last edge in reduced list is the leftmost/topmost edge for cur group)
        reduced .push (*grp.next().unwrap());
        // ^^ this unwrap should be safe as otherwise there wouldnt be this group in the first place
        grp .for_each (|e| {
            let r_last = reduced.last_mut().unwrap();
            // edges in a grp are tl-sorted, so we'll have e.tl >= r_last.tl, so we need only check against r_last.br
            if r_last.br < e.tl {         // i.e. there's a gap between last edge in reduced list and cur edge
                reduced.push(*e);         //   .. so insert it as new segment
            } else if r_last.br < e.br {  // i.e. there's overlap between last edge in reduced list and cur edge
                r_last.br = e.br;         //   .. so elongate the last segment in reduced list
            } else { }                    // i.e. this edge is completely within our last reduced edge .. so do nothing
        } );
    } );
    reduced
}

fn snap_to_edge_rect_delta (wr:&RECT, pdd:&PreDragDat) -> RECT {
    let wres = rect_to_edges(wr);
    // we'll calc delta to snap for each window edge (similar to how the pad rect is specified)
    let d_left   = snap_to_edgelist_nearest__delta (&wres.left,    pdd.padding.left,   &pdd.edge_lists.vert,  pdd.snap_thresh);
    let d_top    = snap_to_edgelist_nearest__delta (&wres.top,     pdd.padding.top,    &pdd.edge_lists.horiz, pdd.snap_thresh);
    let d_right  = snap_to_edgelist_nearest__delta (&wres.right,  -pdd.padding.right,  &pdd.edge_lists.vert,  pdd.snap_thresh);
    let d_bottom = snap_to_edgelist_nearest__delta (&wres.bottom, -pdd.padding.bottom, &pdd.edge_lists.horiz, pdd.snap_thresh);
    // and package that up in a RECT (although the data in it are individual independent possible snap deltas for each edge)
    RECT { left: d_left, top: d_top, right: d_right, bottom: d_bottom }
}


fn snap_to_edgelist_nearest__delta (win_edge:&Edge, pad_v:i32, edges:&Vec<Edge>, thresh:u32) -> i32 {
    let delta = edges .iter() .fold ( i32::MAX,  |delta_acc, edge| {
        let delta_e = edge.xy - win_edge.xy - pad_v;
        if { delta_e.abs() < delta_acc.abs()
            && edge.br >= win_edge.tl
            && edge.tl <= win_edge.br
        } { delta_e } else { delta_acc }
    } );
    if delta.abs() < thresh as i32 { delta } else { 0 }
}





// we'll use a static rwlocked vec to store child-windows from callbacks, and a mutex to ensure only one child-windows call is active
static enum_rects_lock : Lazy <Arc <Mutex <()>>> = Lazy::new (|| Arc::new ( Mutex::new(())));
static enum_rects : Lazy <Arc <RwLock <Vec <(Hwnd,RECT)>>>> = Lazy::new (|| Arc::new ( RwLock::new (vec!()) ) );


fn gather_win_rects (pdd_hwnd: Hwnd) -> Vec<(Hwnd, RECT)> { unsafe {
    use windows::Win32::UI::WindowsAndMessaging::EnumWindows;
    let lock = enum_rects_lock.lock().unwrap();
    *enum_rects.write().unwrap() = Vec::with_capacity(50);   // setting up some excess capacity to reduce reallocations
    let _ = EnumWindows ( Some(enum_windows_callback), LPARAM(pdd_hwnd.0) );
    let rects = enum_rects.read().unwrap().clone();
    drop(lock);
    rects
} }

pub unsafe extern "system" fn enum_windows_callback (hwnd:HWND, pdd_hwnd:LPARAM) -> BOOL {
    let retval = BOOL (true as i32);

    if  pdd_hwnd.0 == hwnd.0           { return retval }        // ignore self window for snap calcs

    if !check_window_visible   (hwnd.into())  { return retval }
    if  check_window_cloaked   (hwnd.into())  { return retval }

    if !check_if_app_window (hwnd.into()) {
        if  check_window_has_owner (hwnd.into())  { return retval }
        if  check_if_tool_window   (hwnd.into())  { return retval }
    }

    let frame = win_get_window_frame (hwnd.into());

    // minimized windows have left/top at -32000 (and frame is same as rect for top (only) .. we'll exclude those)
    if frame.top == -32000  { return retval }

    // note: the WDADesktopService.exe ghost window still gets here, similar to seen in switche ..
    // .. dont think its worthwhile trying to filter that by querying exe/class etc ..
    // .. seems to sit in a slender wide rectangle in SE corner giving ghost edges .. oh well

    //println!("{:?}",(hwnd, &frame, get_exe_by_hwnd(hwnd.into()), get_win_class_by_hwnd(hwnd.into())));

    enum_rects.write().unwrap() .push ((hwnd.into(),frame));

    retval
}







