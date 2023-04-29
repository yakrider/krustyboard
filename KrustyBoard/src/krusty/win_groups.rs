#![ allow (non_camel_case_types) ]


use std::collections::{HashSet};
use std::ops::Deref;
use std::sync::{Arc, RwLock};
use std::thread;
use std::time::Duration;

use crate::{Flag, Key, ModeState_T};
use crate::utils::*;


pub const NUM_WIN_GROUPS : usize = 3;

# [ derive (Debug, Copy, Clone) ]
pub enum WinGroups_E { wg1, wg2, wg3 }
impl WinGroups_E {
    pub fn idx (&self) -> usize {
        use WinGroups_E::*;
        match self { wg1 => 0, wg2 => 1, wg3 => 2 }
    }
    pub fn key (&self) -> Key {
        use WinGroups_E::*; use crate::KbdKey::*;
        match self { wg1 => Numrow_1, wg2 => Numrow_2, wg3 => Numrow_3 }
    }
}
impl TryFrom<usize> for WinGroups_E {
    type Error = ();
    fn try_from (idx: usize) -> Result <Self, Self::Error> {
        use WinGroups_E::*;
        match idx { 0 => Ok(wg1), 1 => Ok(wg2), 2 => Ok(wg3), _ => Err(()) }
    }
}
impl TryFrom <ModeState_T> for WinGroups_E {
    type Error = ();
    fn try_from (ms: ModeState_T) -> Result<Self, Self::Error> {
        use ModeState_T::*;  use WinGroups_E::*;
        match ms { qks1 => Ok(wg1), qks2 => Ok(wg2), qks3 => Ok(wg3), _ => Err(()) }
    }
}
impl From <WinGroups_E> for ModeState_T {
    fn from (wg : WinGroups_E) -> Self {
        use ModeState_T::*;  use WinGroups_E::*;
        match wg { wg1 => qks1, wg2 => qks2, wg3 => qks3 }
    }
}



# [ derive (Debug, Default, Clone) ]
// Note: this is intended as quick throwaway grouping and set that we'd rather replace than update
pub struct WinGroup {
    grp     : Vec <Hwnd>,
    grp_set : HashSet <Hwnd>,
    topmost : Flag,
}


# [ derive (Debug, Clone) ]
pub struct _WinGroups {
    grps: [WinGroup; NUM_WIN_GROUPS],
}

# [ derive (Debug, Clone) ]
pub struct WinGroups ( Arc <RwLock <_WinGroups>> );

impl Deref for WinGroups {
    type Target = RwLock<_WinGroups>;
    fn deref (&self) -> &RwLock<_WinGroups> { &self.0 }
}

impl WinGroups {

    pub fn new() -> WinGroups {
        WinGroups ( Arc::new ( RwLock::new ( _WinGroups {
            grps : [ WinGroup::default(), WinGroup::default(), WinGroup::default() ]
        } ) ) )
    }


    pub fn check_win_group (&self, hwnd:Hwnd) -> Option <WinGroups_E> {
        self.read().unwrap() .grps .iter() .enumerate() .into_iter()
            .find (|&(_,g)| g.grp.contains(&hwnd.into())) .and_then (|(idx,_)| idx.try_into().ok())
    }
    pub fn get_grp_hwnds (&self, grp_e:WinGroups_E) -> Vec<Hwnd> {
        self.read().unwrap() .grps[grp_e.idx()] .grp .iter() .copied() .collect()
    }

    pub fn add_to_group (&self, grp_e: WinGroups_E, hwnd:Hwnd) {
        let grps = &mut self.write().unwrap().grps;
        let mut grp = grps[grp_e.idx()] .grp .iter() .filter(|v| **v != hwnd) .map(|i| *i) .collect::<Vec<_>>();
        grp.push(hwnd);
        let grp_set = grp .iter() .copied() .collect::<HashSet<_>>();
        let topmost = Flag::new (grps [grp_e.idx()] .topmost .is_set());
        grps[grp_e.idx()] = WinGroup {grp, grp_set, topmost};
    }

    pub fn remove_from_group (&self, grp_e:WinGroups_E, hwnd:Hwnd) {
        let grps = &mut self.write().unwrap().grps;
        if grps [grp_e.idx()] .grp_set .contains(&hwnd) {
            let grp = grps[grp_e.idx()] .grp .iter() .filter(|v| **v != hwnd) .copied() .collect::<Vec<_>>();
            let grp_set = grp .iter() .map(|i| *i) .collect::<HashSet<_>>();
            let topmost = Flag::new (grps [grp_e.idx()] .topmost .is_set());
            grps [grp_e.idx()] = WinGroup {grp, grp_set, topmost};
    } }

    fn clear_dead_grp_hwnds (&self, wg:WinGroups_E, hwnds:&Vec<Hwnd>) {
        let hwnds_set = hwnds .iter() .copied() .collect::<HashSet<_>>();
        let grp_hwnds = self.read().unwrap() .grps[wg.idx()] .grp .iter()
            .filter (|h| !hwnds_set.contains(h)) .copied().collect::<Vec<_>>();
        // ^^ we want to have a local copy of this so we're not extending read lock context into the remove calls
        grp_hwnds .into_iter() .for_each (|hwnd| self.remove_from_group(wg,hwnd))
    }

    pub fn activate_win_group (&self, grp_e:WinGroups_E) {
        let grp = &self.read().unwrap() .grps[grp_e.idx()];
        let topmost = grp.topmost.is_set();
        grp.grp .iter() .enumerate() .for_each ( |(i, &hwnd)| {
            // api calls causing focus change seem to need some delay .. lowering the delay below starts giving unreliable activation
            thread::spawn (move || {
                thread::sleep (Duration::from_millis((i as u64 + 0) * 30));
                win_activate (hwnd);
                if topmost { win_set_always_on_top(hwnd) };
            } );
        } );
    }
    pub fn toggle_grp_activation (&self, wg:WinGroups_E) {
        let grp_size = self.read().unwrap() .grps[wg.idx()] .grp.len();
        if grp_size == 0 { return }
        let wgs = self.clone();
        // this could take long enough that we should get off the events queue thread that called us
        thread::spawn ( move || {
            // to toggle, we need to find out z order of grp windows, so we'll do a win-enum call
            // .. which is a good time to cleanup dead-hwnds from our groups too
            let mut grp_ztops_count : usize = 0;
            let hwnds = win_get_switcher_filt_hwnds();
            wgs.clear_dead_grp_hwnds (wg, &hwnds);
            let grp_set = &wgs.read().unwrap() .grps[wg.idx()] .grp_set;
            for &hwnd in &hwnds {
                if grp_set.contains(&hwnd) {
                    grp_ztops_count += 1;
                    if grp_ztops_count == grp_size {
                        // we found all grp windows at top z-order, means we're active, so send grp back to toggle it
                        //but first, lets activate the next-in-line hwnd so active windwo focus transfers seamlessly
                        hwnds .iter() .find (|h| !grp_set.contains(h)) .map (|&h| win_activate(h));
                        // now we can send our grp hwnds back
                        grp_set .iter() .for_each (|&h| win_send_to_back(h));
                        break;
                } } else if !win_check_if_topmost(hwnd) {
                    // finding any non-topmost non-grp window before we're done means grp is not activated
                    wgs.activate_win_group(wg);
                    break;
                } else { /* can ignore topmost non-grp windows */ }
            }
        } );
    }

    pub fn set_grp_always_on_top (&self, wg:WinGroups_E) {
        let grp = &self.read().unwrap() .grps[wg.idx()];
        grp.topmost.set();
        for &hwnd in &grp.grp { win_set_always_on_top (hwnd) }
    }
    pub fn unset_grp_always_on_top (&self, wg:WinGroups_E) {
        let grp = &self.read().unwrap() .grps[wg.idx()];
        grp.topmost.clear();
        for &hwnd in &grp.grp { win_unset_always_on_top (hwnd) }
    }
    pub fn toggle_grp_always_on_top (&self, wg:WinGroups_E) {
        let mut are_grp_wins_topmost = true;
        for &hwnd in &self.read().unwrap() .grps[wg.idx()] .grp {
            if !win_check_if_topmost(hwnd) {
                are_grp_wins_topmost = false; break;
        } }
        if are_grp_wins_topmost { self.unset_grp_always_on_top(wg) } else { self.set_grp_always_on_top(wg) }
    }

    pub fn close_grp_windows (&self, wg:WinGroups_E) {
        self.read().unwrap() .grps[wg.idx()] .grp .iter() .for_each (|hwnd| win_close(*hwnd))
    }

}








