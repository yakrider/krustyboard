

use std::sync::atomic::{AtomicI32, Ordering};
use once_cell::sync::OnceCell;
use windows::{
    core::*,
    Win32::Foundation::*,
    Win32::Graphics::Gdi::*,
    Win32::UI::WindowsAndMessaging::*,
    Win32::System::LibraryLoader::*,
};

use crate::{Flag, Hwnd, HwndAtomic};

const TIMER_ID : usize = 0xdeadbeef;
const TIMER_TICK_MS : u32 = 20;
const MAX_DIM_PERC : i32 = 70;


pub struct DimmingOverlay {
    hwnd: HwndAtomic,
    timer_id: HwndAtomic,
    monitor: DisplayInfo,
}

pub struct DisplayInfo {
    pub rect: RECT,
    pub overlay_enabled: Flag,
    pub overlay_enforced: Flag,
    pub ov_opacity: AtomicI32,    // clamp 0 to MAX_DIM_PERC
}

impl DisplayInfo {
    pub fn get_cur () -> DisplayInfo {
        Self {
            rect: RECT { left: 0, top: 0, right: 3840, bottom: 2400 },
            overlay_enabled: Flag::new(true),
            overlay_enforced: Flag::new(false),
            ov_opacity: AtomicI32::new(0),
        }
    }
}

// todo .. this whole setup needs support for separate overlay per monitor

impl DimmingOverlay {

    pub fn instance() -> &'static DimmingOverlay {
        static INSTANCE: OnceCell<DimmingOverlay> = OnceCell::new();
        INSTANCE .get_or_init ( || {
            let _ = register_window_class();
            let ov = DimmingOverlay {
                hwnd: Default::default(),
                timer_id: Default::default(),
                monitor: DisplayInfo::get_cur(),
            };
            let _ = ov.update();
            ov
        } )
    }

    fn disable (&self) { unsafe {
        self.kill_top_enforce_timer();
        if self.hwnd.load().is_valid() {
            DestroyWindow (self.hwnd.load());
            self.hwnd.clear();
        }
    } }

    pub fn incr_dimming (&self, incr:i32) { dbg!(incr);
        let opc = (incr + self.monitor.ov_opacity.load(Ordering::Relaxed)) .clamp(0, MAX_DIM_PERC);
        self.monitor.ov_opacity.store (opc, Ordering::Relaxed);
        let res = self.update();
        let _ = dbg!(res);
    }

    pub fn update (&self) -> Result<()> { unsafe {

        let opc = self.monitor.ov_opacity.load(Ordering::Relaxed);

        if self.monitor.overlay_enabled.is_clear() || opc == 0 {
            self.disable();
            return Ok(());
        }

        let x = self.monitor.rect.left;
        let y = self.monitor.rect.top;
        let width = self.monitor.rect.right - x;
        let height = self.monitor.rect.bottom - y;

        //dbg!((self.monitor.ov_opacity.load(Ordering::Relaxed), self.hwnd.load().is_valid(), self.monitor.rect));

        if !self.hwnd.load().is_valid() {
            let hwnd = CreateWindowExW (
                WS_EX_LAYERED | WS_EX_TRANSPARENT | WS_EX_TOPMOST | WS_EX_TOOLWINDOW,
                PCWSTR::from_raw("DimmingOverlayClass\0".encode_utf16().collect::<Vec<u16>>().as_ptr()),
                PCWSTR::from_raw("DimmingOverlayWindow\0".encode_utf16().collect::<Vec<u16>>().as_ptr()),
                WINDOW_STYLE(WS_POPUP.0),
                x, y, width, height,
                HWND(0),
                HMENU(0),
                GetModuleHandleA(None)?,
                None,
            );
            if !Hwnd::from(hwnd).is_valid() {
                return Err(Error::from_win32());
            }
            self.hwnd.store (hwnd.into());
        }
        let hwnd = self.hwnd.load();

        let opacity = (opc as f32 * 2.55) .clamp (0.0, (255 * MAX_DIM_PERC) as f32) as u8;

        SetLayeredWindowAttributes (hwnd, COLORREF(0), opacity, LWA_ALPHA);
        SetWindowPos (hwnd, HWND_TOPMOST, x, y, width, height, SWP_SHOWWINDOW | SWP_NOACTIVATE);
        UpdateWindow (hwnd);

        if self.monitor.overlay_enforced.is_set() {
            self.start_top_enforce_timer();
        }

        Ok(())
    } }

    fn start_top_enforce_timer(&self) { unsafe {
        self.kill_top_enforce_timer();
        let hwnd = self.hwnd.load();
        if self.monitor.overlay_enforced.is_set() && hwnd.is_valid() {
            let timer_id = SetTimer (hwnd, TIMER_ID, TIMER_TICK_MS, None);
            self.timer_id .store (Hwnd::from (timer_id as isize))
        }
    } }

    fn kill_top_enforce_timer(&self) { unsafe {
        let hwnd = self.hwnd.load();
        let timer_id = self.timer_id.load();
        if timer_id.is_valid() && hwnd.is_valid() {
            KillTimer (hwnd, timer_id.0 as usize);
            self.timer_id.clear();
        }
    } }
}

impl Drop for DimmingOverlay {
    fn drop(&mut self) {
        self.disable();
    }
}

// onet-time dimming window class registration on module initialization
fn register_window_class() -> Result<()> {
    let wc = WNDCLASSEXA {
        cbSize: size_of::<WNDCLASSEXA>() as u32,
        style: WNDCLASS_STYLES(0),
        lpfnWndProc: Some(wnd_proc),
        cbClsExtra: 0,
        cbWndExtra: 0,
        hInstance: unsafe { GetModuleHandleA(None)? },
        hIcon: HICON(0),
        hCursor: HCURSOR(0),
        hbrBackground: HBRUSH(0),
        lpszMenuName: PCSTR::null(),
        lpszClassName: PCSTR::from_raw("DimmingOverlayClass\0".as_ptr()),
        hIconSm: HICON(0),
    };
    unsafe {
        if RegisterClassExA(&wc) == 0 {
            return Err(Error::from_win32());
        }
    }
    Ok(())
}

unsafe extern "system" fn wnd_proc ( hwnd: HWND, msg: u32, w_param: WPARAM, l_param: LPARAM ) -> LRESULT { unsafe {
    match msg {
        WM_PAINT => {
            let mut ps = PAINTSTRUCT::default();
            let hdc = BeginPaint (hwnd, &mut ps);
            FillRect (hdc, &ps.rcPaint, CreateSolidBrush(COLORREF(0)));
            EndPaint (hwnd, &ps);
            LRESULT(0)
        }
        WM_TIMER => {
            if w_param.0 == TIMER_ID {
                SetWindowPos ( hwnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
                LRESULT(0)
            } else {
                DefWindowProcA(hwnd, msg, w_param, l_param)
            }
        }
        _ => DefWindowProcA(hwnd, msg, w_param, l_param)
    }
} }
