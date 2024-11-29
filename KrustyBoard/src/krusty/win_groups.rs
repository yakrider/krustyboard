#![ allow (non_camel_case_types) ]


use std::sync::RwLock;
use std::thread;
use std::time::Duration;

use derive_deref::Deref;
use once_cell::sync::OnceCell;
use rustc_hash::FxHashSet;

use crate::Flag;
use crate::utils::*;



pub const NUM_WIN_GROUPS : usize = 4;

# [ derive (Debug, Copy, Clone, Eq, PartialEq) ]
pub enum WinGroups_E { wg1, wg2, wg3, wg4 }
impl WinGroups_E {
    pub fn idx (&self) -> usize {
        use WinGroups_E::*;
        match self { wg1 => 0, wg2 => 1, wg3 => 2, wg4 => 3 }
    }
}
impl TryFrom<usize> for WinGroups_E {
    type Error = ();
    fn try_from (idx: usize) -> Result <Self, Self::Error> {
        use WinGroups_E::*;
        match idx { 0 => Ok(wg1), 1 => Ok(wg2), 2 => Ok(wg3), 3 => Ok(wg4), _ => Err(()) }
    }
}



# [ derive (Debug, Default) ]
// Note: this is intended as quick throwaway grouping and set that we'd rather replace than update
pub struct _WinGroup {
    grp     : Vec <Hwnd>,
    grp_set : FxHashSet <Hwnd>,
    topmost : Flag,
}
# [ derive (Debug, Deref) ]
pub struct WinGroup ( RwLock <_WinGroup> );



# [ derive (Debug) ]
pub struct WinGroups {
    grps: [&'static WinGroup; NUM_WIN_GROUPS],
}



impl WinGroup {

    fn new() -> WinGroup {
        WinGroup ( RwLock::new ( _WinGroup::default() ) )
    }

    fn get_hwnds (&self) -> Vec<Hwnd> {
        self.read().unwrap().grp.to_vec()
    }
    pub fn check (&self, hwnd:&Hwnd) -> bool {
        self.read().unwrap().grp_set.contains(hwnd)
    }
    fn add (&self, hwnd:&Hwnd) {
        let wg = &mut self.write().unwrap();
        wg.grp .retain (|&h| h != *hwnd);
        wg.grp .push (*hwnd);
        wg.grp_set .insert (*hwnd);
    }
    fn remove (&self, hwnd:&Hwnd) {
        let wg = &mut self.write().unwrap();
        wg.grp .retain (|h| *h != *hwnd);
        wg.grp_set .remove (hwnd);
    }
    fn clear_dead (&self, hwnds:&[Hwnd]) {
        let wg = &mut self.write().unwrap();
        let hset = hwnds.iter().copied().collect::<FxHashSet<_>>();
        wg.grp .retain (|h| hset.contains(h));
        wg.grp_set .retain (|h| hset.contains(h));
    }

    fn set_always_on_top (&self) {
        self.write().unwrap().topmost.set();
        for &hwnd in &self.read().unwrap().grp { win_set_always_on_top (hwnd) }
    }
    fn unset_always_on_top (&self) {
        self.write().unwrap().topmost.clear();
        for &hwnd in &self.read().unwrap().grp { win_unset_always_on_top (hwnd) }
    }
    fn toggle_always_on_top (&self) {
        let mut are_grp_wins_topmost = true;
        for &hwnd in &self.read().unwrap().grp {
            if !win_check_if_topmost(hwnd) {
                are_grp_wins_topmost = false; break
        } }
        if are_grp_wins_topmost { self.unset_always_on_top() } else { self.set_always_on_top() }
    }

    fn close (&self) {
        for &hwnd in &self.read().unwrap().grp {
            win_close(hwnd)
        }
    }


    // for the activation and toggle fns below, we want to spawn out the action
    // .. and so we'll specify &'static receiver to send into thread (w/o needing it to be Arc-wrapped for cloneability)

    fn activate (&'static self) {
        thread::spawn ( move || {
            let topmost = self.read().unwrap().topmost.is_set();
            self.get_hwnds().iter().for_each (|&hwnd| {
                // api calls causing focus change seem to need some delay .. lowering the delay below starts giving unreliable activation
                thread::sleep (Duration::from_millis(30));
                win_activate (hwnd);
                if topmost { win_set_always_on_top(hwnd) };
            });
        } );
    }

    fn toggle_activation (&'static self) {
        if self.read().unwrap().grp.is_empty() { return }
        // this could take long enough that we should get off the events queue thread that called us
        thread::spawn ( move || {
            // to toggle, we need to find out z order of grp windows, so we'll do a win-enum call
            // .. which is a good time to cleanup dead-hwnds from our groups too
            let mut grp_ztops_count : usize = 0;
            let grp_len = self.read().unwrap().grp.len();
            let hwnds = win_get_switcher_filt_hwnds();
            self.clear_dead(&hwnds);
            for &hwnd in &hwnds {
                if self.read().unwrap().grp_set.contains(&hwnd) {
                    grp_ztops_count += 1;
                    if grp_ztops_count == grp_len {
                        // we found all grp windows at top z-order, means we're active, so send grp back to toggle it
                        //but first, lets activate the next-in-line hwnd so active window focus transfers seamlessly
                        for &h in &hwnds {
                            if !self.read().unwrap().grp_set.contains(&h) {
                                win_activate(h); break
                        } }
                        // now we can send our grp hwnds back
                        self.read().unwrap().grp_set .iter() .for_each (|&h| { win_send_to_back(h); win_minimize(h); } );
                        break;
                } } else if !win_check_if_topmost(hwnd) {
                    // finding any non-topmost non-grp window before we're done means grp is not activated
                    self.activate();
                    break;
                } else { /* can ignore topmost non-grp windows */ }
            }
        } );
    }
}





impl WinGroups {

    pub fn instance() -> &'static WinGroups {

        static WG_STATES : [OnceCell<WinGroup>; 4] = [
            OnceCell::new(), OnceCell::new(), OnceCell::new(), OnceCell::new()
        ];
        static INSTANCE : OnceCell<WinGroups> = OnceCell::new();

        INSTANCE .get_or_init ( || {
            let grps = WG_STATES .iter() .map (|wg| wg.get_or_init (WinGroup::new))
                        .collect::<Vec<&WinGroup>>() .try_into() .unwrap();
            WinGroups { grps }
        } )
    }

    pub fn grp_contains (&self, wg:WinGroups_E, hwnd:Hwnd) -> bool {
        if let Some(wg) = self.grps.get(wg.idx()) { wg.get_hwnds().contains(&hwnd) } else { false }
    }
    pub fn get_grp_hwnds (&self, wg:WinGroups_E) -> Vec<Hwnd> {
        if let Some(wg) = self.grps.get(wg.idx()) {  wg.get_hwnds() } else { Vec::new() }
    }
    pub fn add_to_group (&self, wg: WinGroups_E, hwnd:Hwnd) {
        if let Some(wg) = self.grps.get(wg.idx()) { wg.add(&hwnd) }
    }
    pub fn remove_from_group (&self, wg:WinGroups_E, hwnd:Hwnd) {
        if let Some(wg) = self.grps.get(wg.idx()) { wg.remove(&hwnd) }
    }
    pub fn toggle_grp_always_on_top (&self, wg:WinGroups_E) {
        if let Some(wg) = self.grps.get(wg.idx()) { wg.toggle_always_on_top() }
    }
    pub fn activate_win_group (&self, wg:WinGroups_E) {
        if let Some(wg) = self.grps.get(wg.idx()) { wg.activate() }
    }
    pub fn toggle_grp_activation (&self, wg:WinGroups_E) {
        if let Some(wg) = self.grps.get(wg.idx()) { wg.toggle_activation() }
    }
    pub fn close_grp_windows (&self, wg:WinGroups_E) {
        if let Some(wg) = self.grps.get(wg.idx()) { wg.close() }
    }

}








