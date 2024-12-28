
use std::{thread, time::Duration};
use once_cell::sync::OnceCell;

use windows::core::PCWSTR;
use windows::Win32::Foundation::HINSTANCE;
use windows::Win32::UI::WindowsAndMessaging;
use windows::Win32::Graphics::Gdi::{
    BITMAP, HGDIOBJ, CreateBitmap, DeleteObject, GetBitmapBits, SetBitmapBits, GetObjectW
};
use windows::Win32::UI::WindowsAndMessaging::{
    ICONINFO, HCURSOR, HICON, SYSTEM_CURSOR_ID,
    SPI_SETCURSORS, SPIF_SENDCHANGE, IMAGE_CURSOR, LR_LOADFROMFILE,
    LoadImageW, GetIconInfo, CopyIcon, LoadCursorW, SetSystemCursor, CreateIconIndirect, SystemParametersInfoW,
};
use crate::utils;
use crate::Flag;




# [ derive (Debug, Copy, Clone) ]
pub struct Cursor {
    // Note here that HICON is a pointer underneath, and making it copy/clone isnt exactly kosher ..
    // .. to avoid a retained HICON ever being dropped/modified by OS, we should only keep owned/copied hicons here
    pub hicon  : HICON,             // isize
    pub sys_id : SYSTEM_CURSOR_ID,  // u32
}




pub struct CursorSet {
    pub hc_no           : Cursor,
    pub hc_normal       : Cursor,
    pub hc_help         : Cursor,
    pub hc_hand         : Cursor,
    pub hc_cross        : Cursor,
    pub hc_wait         : Cursor,
    pub hc_ibeam        : Cursor,
    pub hc_size_ns      : Cursor,
    pub hc_size_we      : Cursor,
    pub hc_size_nwse    : Cursor,
    pub hc_size_nesw    : Cursor,
    pub hc_size_all     : Cursor,
    pub hc_app_starting : Cursor,
}
impl CursorSet {
    fn get_swap_set (&self) -> [&Cursor; 10] { [
        // instead of swapping every icon, we'll try and limit to subset we're likely to see
        // .. further we might want to avoid likely animated things like app-starting or even wait if need be
        &self.hc_no,      &self.hc_normal,  &self.hc_hand,      &self.hc_ibeam,     &self.hc_wait,
        &self.hc_size_ns, &self.hc_size_we, &self.hc_size_nwse, &self.hc_size_nesw, &self.hc_size_all
    ] }
}




pub struct Cursors {

    enabled : Flag,      // we'll only swap cursors if explicitly enabled

    // the idea is to keep a backup of orig sys cursors, but use our triad colors for normal, sticky-fsc, latching-fsc
    sys   : CursorSet,    // copy of system cursors
    norm  : CursorSet,    // colorized -> normal use
    sfsc  : CursorSet,    // colorized -> first-stroke-combos : sticky
    lfsc  : CursorSet,    // colorized -> first-stroke-combos : latching
    flash : CursorSet,    // colorized -> flashing transition between modes
}


#[derive (Debug, Default, Copy, Clone, Eq, PartialEq)]
struct RGB { pub r:u8, pub g:u8, pub b:u8 }

const NORM_COLOR : RGB = RGB {
    //r: 0xFF, g: 0xFF, b: 0x40     // yellowish
    //r: 0xE0, g: 0xFF, b: 0x00     // yellowish
      r: 0xC0, g: 0xE0, b: 0x00     // yellow-greenish
    //r: 0xC0, g: 0xFF, b: 0x00     // yellow-greenish
};
const SFSC_COLOR : RGB = RGB {
    //r: 0x00, g: 0xC0, b: 0xFF     // blueish
      r: 0x00, g: 0xE0, b: 0xFF     // blueish
};
const LFSC_COLOR : RGB = RGB {
    //r: 0xFF, g: 0x50, b: 0xE0     // pinkish
      r: 0xFF, g: 0x50, b: 0xFF     // pinkish
};
const FLASH_COLOR : RGB = RGB {
  //r: 0xFF, g: 0x20, b: 0x20,      // red
    r: 0xF0, g: 0xF0, b: 0xF0,      // white
};



trait CursorSelector : Fn(&Cursors) -> Option<&CursorSet> + Send + Sync + 'static {
    /*
        - ^^ we have Cursors instance as 'static (so is Send + Sync), but individual Cursors in it are not
        - so we'll instead send around selector fns for the specific cursor when needed
        - and for that we're defining this trait out here rather than specifying the type repeatedly in fn defs
     */
}
impl <U> CursorSelector for U
    where U : Fn(&Cursors) -> Option<&CursorSet> + Send + Sync + 'static
{
    /* - ^^ we're defining blanket trait for matching closures so we can use that in param types
            (otherwise compiler would complain that each closure types is unique)
       - This lets us use constructions of form .. fn some_fn <C1,C2> (f1:C1, f2:C2) where C1:SEL, C2:SEL { }
         .. although something like .. fn some_fn (f1:SEL, f2:SEL) { }  .. would still be disallowed as SEL has to be trait not type
         .. nor even something like .. fn some_fn <C> (f1:C, f2:C) where C:SEL { } .. as f1, f2 would still be different closures/types
     */
}




impl Cursors {

    // once this is inited, it can serve as immutable backup of original system cursors
    pub fn instance() -> &'static Cursors {
        static INSTANCE : OnceCell<Cursors> = OnceCell::new();

        INSTANCE .get_or_init ( || {

            // before we start grabbing system cursors, we want to ensure our thraad is dpi-aware so we get the scaled bitmaps
            utils::win_set_thread_dpi_aware();
            // and reset system cursors in case we were restarted from some prior switched cursors (by ourselves or others)
            Cursors::reset_system_cursors();

            let sys   = Self::load_sys_cursors() .expect ("error loading system cursors");
            let norm  = Self::colorized_sys_cursors (&sys, &NORM_COLOR)  .expect("error colorizing cursors");
            let sfsc  = Self::colorized_sys_cursors (&sys, &SFSC_COLOR)  .expect("error colorizing cursors");
            let lfsc  = Self::colorized_sys_cursors (&sys, &LFSC_COLOR)  .expect("error colorizing cursors");
            let flash = Self::colorized_sys_cursors (&sys, &FLASH_COLOR) .expect("error colorizing cursors");

            Cursors { enabled : Flag::default(), sys, norm, sfsc, lfsc, flash }
        } )
    }


    pub fn set_swaps_enabled (&self, enabled:bool) {
        // Note: because of how windows seems to refresh rendering etc ..
        // .. for the very first call, looks like swapping without delay right after the instantiation happened ..
        // .. will screw up the appearance of the cursors, (esp the ibeam middle is rendered black)
        // .. (almost as if it tried to downscale the cursor and rescaled back losing resolution)
        // .. (although it will fix itself for subsequent cursor swaps)
        // either way, if we swap cursors after at least 30ms+ delay (in my machine) seems to display everything as expected
        // hence the spawning with delay below before applying colored cursors upon enablement
        //
        if enabled && !self.enabled.is_set() {
            //self.apply_norm();
            thread::spawn ( || {
                thread::sleep (Duration::from_millis(100));
                Cursors::instance().apply_norm()
            } );
        }
        else if !enabled && self.enabled.is_set() {
            self.apply_sys();
            // ^^ will actually reset cursors as cached sys cursors are still lower res than native
        }
        // finally we can update the flags
        self.enabled.store(enabled);
    }
    pub fn is_enabled (&self) -> bool {
        self.enabled.is_set()
    }


    fn load_sys_cursors() -> Option<CursorSet> {
        use WindowsAndMessaging as wm;

        // Note that while there are only these 13 system-cursors defined ..
        // .. there are IDC_<?> constants for a bunch of other cursors which apps can load and use ..
        // .. so anything like that wont be globally swappable like here .. (e.g right-arrow, up-arrrow, idc-pin etc)

        Some ( CursorSet {
            hc_no           :  Cursor::cache_sys_cursor ( wm::IDC_NO,           wm::OCR_NO          )?,
            hc_normal       :  Cursor::cache_sys_cursor ( wm::IDC_ARROW,        wm::OCR_NORMAL      )?,
            hc_help         :  Cursor::cache_sys_cursor ( wm::IDC_HELP,         wm::OCR_HELP        )?,
            hc_hand         :  Cursor::cache_sys_cursor ( wm::IDC_HAND,         wm::OCR_HAND        )?,
            hc_cross        :  Cursor::cache_sys_cursor ( wm::IDC_CROSS,        wm::OCR_CROSS       )?,
            hc_wait         :  Cursor::cache_sys_cursor ( wm::IDC_WAIT,         wm::OCR_WAIT        )?,
            hc_ibeam        :  Cursor::cache_sys_cursor ( wm::IDC_IBEAM,        wm::OCR_IBEAM       )?,
            hc_size_ns      :  Cursor::cache_sys_cursor ( wm::IDC_SIZENS,       wm::OCR_SIZENS      )?,
            hc_size_we      :  Cursor::cache_sys_cursor ( wm::IDC_SIZEWE,       wm::OCR_SIZEWE      )?,
            hc_size_nwse    :  Cursor::cache_sys_cursor ( wm::IDC_SIZENWSE,     wm::OCR_SIZENWSE    )?,
            hc_size_nesw    :  Cursor::cache_sys_cursor ( wm::IDC_SIZENESW,     wm::OCR_SIZENESW    )?,
            hc_size_all     :  Cursor::cache_sys_cursor ( wm::IDC_SIZEALL,      wm::OCR_SIZEALL     )?,
            hc_app_starting :  Cursor::cache_sys_cursor ( wm::IDC_APPSTARTING,  wm::OCR_APPSTARTING )?,
        } )
    }

    fn colorized_sys_cursors (sys:&CursorSet, rgb:&RGB) -> Option<CursorSet> {
        Some ( CursorSet {
            hc_no           :  sys .hc_no           .colorized (rgb)?,
            hc_normal       :  sys .hc_normal       .colorized (rgb)?,
            hc_help         :  sys .hc_help         .colorized (rgb)?,
            hc_hand         :  sys .hc_hand         .colorized (rgb)?,
            hc_cross        :  sys .hc_cross        .colorized (rgb)?,
            hc_wait         :  sys .hc_wait         .colorized (rgb)?,
            hc_ibeam        :  sys .hc_ibeam        .colorized (rgb)?,
            hc_size_ns      :  sys .hc_size_ns      .colorized (rgb)?,
            hc_size_we      :  sys .hc_size_we      .colorized (rgb)?,
            hc_size_nwse    :  sys .hc_size_nwse    .colorized (rgb)?,
            hc_size_nesw    :  sys .hc_size_nesw    .colorized (rgb)?,
            hc_size_all     :  sys .hc_size_all     .colorized (rgb)?,
            hc_app_starting :  sys .hc_app_starting .colorized (rgb)?,
        } )
    }


    # [ allow (non_camel_case_types) ]
    fn apply_fsc <SEL_1, SEL_2> (selector:SEL_1, flash_sel:SEL_2)
        where SEL_1 : CursorSelector,
              SEL_2 : CursorSelector,
        // ^^ gotta specify the two selectors as separate types matching the trait, as every passed in closure type is unique
    {
        thread::spawn ( move || {
            let cursors = Cursors::instance();
            if cursors.enabled.is_set() {
                if let Some(flash) = flash_sel(cursors) {
                    for hc in flash.get_swap_set() { hc.apply() }
                    thread::sleep (Duration::from_millis(150));
                }
                if let Some(cs) = selector(cursors) {
                    //for hc in selector(cursors).get_swap_set() { hc.apply() }
                    // ^^ stored sys cursors are still lower res, so we'd rather just reset cursors :
                    if std::ptr::eq (cs, &cursors.sys) {
                        Self::reset_system_cursors()
                    } else {
                        for hc in cs.get_swap_set() { hc.apply() }
                    }
                }
            }
        } );
    }
    pub fn apply_sys  (&self) { Self::apply_fsc (|cs| Some(&cs.sys),  |cs| Some(&cs.flash)) }
    pub fn apply_norm (&self) { Self::apply_fsc (|cs| Some(&cs.norm), |cs| Some(&cs.flash)) }
    pub fn apply_sfsc (&self) { Self::apply_fsc (|cs| Some(&cs.sfsc), |cs| Some(&cs.flash)) }
    pub fn apply_lfsc (&self) { Self::apply_fsc (|cs| Some(&cs.lfsc), |cs| Some(&cs.flash)) }

    pub fn apply_norm_no_flash (&self) { Self::apply_fsc (|cs| Some(&cs.norm), |_| None) }

    pub fn apply_sfsc_w_lfsc_flash (&self) { Self::apply_fsc (|cs| Some(&cs.sfsc), |cs| Some(&cs.lfsc)) }
    pub fn apply_sfsc_w_norm_flash (&self) { Self::apply_fsc (|cs| Some(&cs.sfsc), |cs| Some(&cs.norm)) }

    /// Resets any system cursor customizations and reloads them from OS configs. <br>
    /// (Instead of making this public, we'd rather encourage using apply_sys which flashes before reset)
    fn reset_system_cursors () { unsafe {
        SystemParametersInfoW ( SPI_SETCURSORS, 0, None, SPIF_SENDCHANGE );
    } }
}






impl Cursor {

    unsafe fn _cursor_from_file (path:&str) -> Option<HICON> {
        use std::os::windows::ffi::OsStrExt;
        let wide_path: Vec<u16> = std::ffi::OsStr::new(path) .encode_wide() .chain(std::iter::once(0)) .collect();
        let hc = LoadImageW ( HINSTANCE(0), PCWSTR(wide_path.as_ptr()), IMAGE_CURSOR, 64, 64, LR_LOADFROMFILE ) .ok()?;
        if hc.is_invalid() { None } else { Some ( HICON (hc.0) ) }
    }
    // ^^ no longer used as we just load system cursors, but we'll leave here for reference, as it does allow higher-res cursors

    unsafe fn get_sys_cursor (id:PCWSTR) -> Option<HICON> {
        LoadCursorW (HINSTANCE(0), id) .ok() .and_then (|hc| CopyIcon(hc).ok())
        // ^^ we copy the hicon before we store it, as LoadCursor gives handles to the live cursor set
        // Note also that ms-docs say LoadCursor is apparently a dpi-unaware function ..
        //   however, trying to load cursors before calling SetThreadDpiAwarenessContext gives small dim cursors .. \\_(_)_//
    }

    fn cache_sys_cursor (id_str:PCWSTR, sys_id:SYSTEM_CURSOR_ID) -> Option<Cursor> {
        let hicon = unsafe { Cursor::get_sys_cursor (id_str) } ?;
        Some ( Cursor { hicon, sys_id } )
    }

    fn colorized (&self, rgb:&RGB) -> Option<Cursor> {
        let hicon = unsafe { colorize_cursor (&self.hicon, rgb) }?;
        Some ( Cursor { hicon, ..*self } )
    }

    /// replace system cursor of this cursors id by this cursor
    pub fn apply (&self) { unsafe {
        if self.sys_id.0 == 0 { return }
        if let Ok(cc) = CopyIcon(self.hicon) {
            SetSystemCursor (HCURSOR(cc.0), self.sys_id);
        }
    } }

    /// replace system cursor of given id by this cursor for a short period
    pub fn _tmp_swap_cursor (&self, millis:u64,  after:Cursor) { unsafe {
        if after.sys_id.0 == 0 { return }
        // gotta copy before we send, as these get consumed
        if let (Some(hicon_tmp), Some(hicon_restore)) = (CopyIcon(self.hicon).ok(), CopyIcon(after.hicon).ok()) {
            thread::spawn ( move || {
                SetSystemCursor (HCURSOR(hicon_tmp.0), after.sys_id);
                thread::sleep (Duration::from_millis(millis));
                SetSystemCursor (HCURSOR(hicon_restore.0), after.sys_id);
            } );
        }
    } }
    // ^^ no longer use cursor flashing, but we'll leave here as reference

}






unsafe fn colorize_cursor (hicon:&HICON, rgb:&RGB) -> Option<HICON> {

    if hicon.is_invalid() { return None }

    // first gotta get the details on the icon
    let mut info = ICONINFO::default();
    let res = GetIconInfo (*hicon, &mut info as *mut _);
    if !res.as_bool() { return None }

    // then we'll get the actual bitmap (and later its mask if necessary)
    let mut bmp = BITMAP::default();
    let _ = GetObjectW ( HGDIOBJ (info.hbmColor.0), size_of::<BITMAP>() as i32, Some ( &mut bmp as *mut BITMAP as _) );
    let buf_size = bmp.bmWidth * bmp.bmHeight * 4;
    let mut buf = vec![0u8; buf_size as usize];
    let _ = GetBitmapBits (info.hbmColor, buf_size, buf.as_mut_ptr() as _);

    //dbg!(("icon bitmap width, height", bmp.bmWidth, bmp.bmHeight));
    // ^^ interestingly enough, what we get here for system-cursors depends on the dpi-awareness context we called in
    // (even though ms-docs says the LoadCursor fn itself is dpi-unaware .. oh well, we'll have to keep in mind to get dpi scaled bitmaps)

    // requesting info has the system allocate the bitmap and mask, should release that memory
    // we'll keep most fields and use them in new icon-info
    let _ = DeleteObject (info.hbmColor);

    // now lets try colorizing the hicons by rgba chunks
    // The cursors themselves are colored/white shapes with black border .. (apparently with some smoothing?)
    // Its not obvious what thresholding to use to swap just the colored parts .. instead we'll just ignore all black/grey
    // (works decently, as long as we get the dpi-scaled bitmap, though there's some minor pixelation and border thickening)
    for chunk in buf.chunks_exact_mut(4) {
        let [b, g, r, _a] = chunk else { unreachable!() };
        if !(*r==*g && *g==*b)  {
            *r = rgb.r;  *g = rgb.g;  *b = rgb.b;
        }
    }

    // create new bitmap from the modified buffer
    let new_bitmap = CreateBitmap (bmp.bmWidth, bmp.bmHeight, 1,  32, None);
    if new_bitmap.is_invalid() { return None }

    // copy our modified buffer into the new bitmap
    let bytes_set = SetBitmapBits (new_bitmap, buf_size as _, buf.as_ptr() as _);
    if bytes_set == 0 {
        let _ = DeleteObject(new_bitmap);
        return None;
    }

    // now we're ready to build the new icon
    let new_icon_info = ICONINFO { hbmColor: new_bitmap, ..info };
    let new_icon = CreateIconIndirect (&new_icon_info) .ok()?;

    let _ = DeleteObject(new_bitmap);

    if new_icon.is_invalid() { None } else { Some(new_icon) }

}







