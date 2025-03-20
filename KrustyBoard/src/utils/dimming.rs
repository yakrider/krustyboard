

use std::sync::atomic::{AtomicI32, Ordering};
use once_cell::sync::OnceCell;

use windows::core::PCWSTR;
use windows::Win32::Foundation::{COLORREF, HINSTANCE, HWND, LPARAM, LRESULT, RECT, WPARAM};
use windows::Win32::Graphics::Gdi::{BeginPaint, CreateSolidBrush, DeleteObject, EndPaint, FillRect, UpdateWindow, HBRUSH, PAINTSTRUCT};
use windows::Win32::System::LibraryLoader::GetModuleHandleW;
use windows::Win32::UI::WindowsAndMessaging::{CreateWindowExW, DefWindowProcW, DestroyWindow, DispatchMessageW, GetMessageW, KillTimer, RegisterClassExW, SetLayeredWindowAttributes, SetTimer, SetWindowPos, TranslateMessage, HCURSOR, HICON, HWND_TOPMOST, LWA_ALPHA, MSG, SWP_NOACTIVATE, SWP_NOMOVE, SWP_NOSIZE, SWP_SHOWWINDOW, WM_PAINT, WM_TIMER, WNDCLASSEXW, WNDCLASS_STYLES, WS_EX_LAYERED, WS_EX_NOACTIVATE, WS_EX_TOOLWINDOW, WS_EX_TOPMOST, WS_EX_TRANSPARENT, WS_POPUP};

use crate::{Flag, Hwnd, HwndAtomic};



const TIMER_ID : usize = 0xdeadbeef;
const TIMER_TICK_MS : u32 = 20;
const MAX_DIM_PERC : i32 = 72;



pub struct DisplayInfo {
    pub rect: RECT,
    pub overlay_enabled: Flag,
    pub overlay_enforced: Flag,
    pub ov_opacity: AtomicI32,    // clamp 0 to MAX_DIM_PERC
}

// todo .. this whole setup needs support for separate overlay per monitor

impl DisplayInfo {
    // TODO : NOTE : this impl should be temporary until we hadd mult-mon support
    pub fn get_cur () -> DisplayInfo {
        Self {
            rect: RECT { left: 0, top: 0, right: 3840, bottom: 2400 },
            overlay_enabled: Flag::new(true),
            overlay_enforced: Flag::new(false),
            ov_opacity: AtomicI32::new(0),
        }
    }
}



pub struct DimmingOverlay {
    hwnd: HwndAtomic,
    timer_id: HwndAtomic,
    monitor: DisplayInfo,
}


impl Drop for DimmingOverlay {
    fn drop(&mut self) {
        self.disable();
    }
}


impl DimmingOverlay {

    pub fn instance() -> &'static DimmingOverlay {
        static INSTANCE: OnceCell<DimmingOverlay> = OnceCell::new();
        INSTANCE .get_or_init ( || {
            let _ = register_window_class();
            DimmingOverlay {
                hwnd: Default::default(),
                timer_id: Default::default(),
                monitor: DisplayInfo::get_cur(),
            }
        } )
    }

    fn disable (&self) { unsafe {
        self.kill_top_enforce_timer();
        let hwnd = self.hwnd.load();
        if hwnd.is_valid() {
            let _ = DestroyWindow (hwnd.into());
            self.hwnd.clear();
        }
    } }

    pub fn incr_dimming (&'static self, incr:i32) {
        let opc = (incr + self.monitor.ov_opacity.load(Ordering::Relaxed)) .clamp(0, MAX_DIM_PERC);
        self.monitor.ov_opacity.store (opc, Ordering::Relaxed);
        let res = self.update();
        res.err().iter() .for_each (|e| { dbg!(e); });
    }

    fn spawn_window (&'static self) {

        std::thread::spawn (move || { unsafe {

            if self.hwnd.load().is_valid() {
                return;
            }

            let x = self.monitor.rect.left;
            let y = self.monitor.rect.top;
            let width = self.monitor.rect.right - x;
            let height = self.monitor.rect.bottom - y;

            let h_inst : Option<HINSTANCE> = GetModuleHandleW(None) .ok() .map(|h| h.into());

            let res = CreateWindowExW (
                WS_EX_LAYERED | WS_EX_TRANSPARENT | WS_EX_TOPMOST | WS_EX_TOOLWINDOW | WS_EX_NOACTIVATE,
                PCWSTR::from_raw("DimmingOverlayClass\0".encode_utf16().collect::<Vec<u16>>().as_ptr()),
                PCWSTR::from_raw("DimmingOverlayWindow\0".encode_utf16().collect::<Vec<u16>>().as_ptr()),
                WS_POPUP, x, y, width, height, None, None, h_inst, None
            );
            if let Ok(hwnd) = res {
                self.hwnd.store (Hwnd (hwnd.0 as isize));
            }

            std::thread::spawn (move || {
                std::thread::sleep (std::time::Duration::from_millis(10));
                let _ = self.update();
            });

            let mut msg = MSG::default();
            while GetMessageW(&mut msg, None, 0, 0).into() {
                let _ = TranslateMessage(&msg);
                DispatchMessageW(&msg);
            }

        } } );
    }


    pub fn update (&'static self) -> windows::core::Result<()> { unsafe {

        let opc = self.monitor.ov_opacity.load(Ordering::Relaxed);

        if self.monitor.overlay_enabled.is_clear() || opc == 0 {
            self.disable();
            return Ok(());
        }

        if !self.hwnd.load().is_valid() {
            self.spawn_window();
            return Ok(());
        }

        let hwnd = self.hwnd.load();
        let opacity = (2.55 * opc.clamp (0, MAX_DIM_PERC) as f32) as u8;

        SetLayeredWindowAttributes (hwnd.into(), COLORREF(0), opacity, LWA_ALPHA)?;
        let _ = UpdateWindow (hwnd.into());

        bring_to_top(hwnd.into());

        if self.monitor.overlay_enforced.is_set() {
            self.start_top_enforce_timer();
        }

        Ok(())
    } }

    fn start_top_enforce_timer (&self) { unsafe {
        self.kill_top_enforce_timer();
        let hwnd = self.hwnd.load();
        if self.monitor.overlay_enforced.is_set() && hwnd.is_valid() {
            let timer_id = SetTimer (Some(hwnd.into()), TIMER_ID, TIMER_TICK_MS, None);
            self.timer_id .store (Hwnd::from (timer_id as isize))
        }
    } }

    fn kill_top_enforce_timer (&self) { unsafe {
        let hwnd = self.hwnd.load();
        let timer_id = self.timer_id.load();
        if timer_id.is_valid() && hwnd.is_valid() {
            let _ = KillTimer(Some(hwnd.into()), timer_id.0 as usize);
            self.timer_id.clear();
        }
    } }

}




// one-time dimming window class registration on module initialization
fn register_window_class() -> windows::core::Result<()> { unsafe {
    let wc = WNDCLASSEXW {
        cbSize: size_of::<WNDCLASSEXW>() as u32,
        style: WNDCLASS_STYLES(0),
        lpfnWndProc: Some(wnd_proc),
        cbClsExtra: 0,
        cbWndExtra: 0,
        hInstance: GetModuleHandleW(None)?.into(),
        hIcon: HICON::default(),
        hCursor: HCURSOR::default(),
        hbrBackground: HBRUSH::default(),
        lpszMenuName: PCWSTR::null(),
        lpszClassName: PCWSTR ("DimmingOverlayClass\0".encode_utf16().collect::<Vec<u16>>().as_ptr()),
        hIconSm: HICON::default(),
    };
    if RegisterClassExW(&wc) == 0 {
        return Err (windows::core::Error::from_win32());
    }
    Ok(())
} }


fn bring_to_top (hwnd:HWND) { unsafe {
    let show_flags =  SWP_NOSIZE | SWP_NOMOVE | SWP_SHOWWINDOW | SWP_NOACTIVATE;
    let _ = SetWindowPos (hwnd, Some(HWND_TOPMOST), 0, 0, 0, 0, show_flags);
} }


unsafe extern "system" fn wnd_proc (
    hwnd: HWND, msg: u32, w_param: WPARAM, l_param: LPARAM
) -> LRESULT { unsafe {
    match msg {
        WM_PAINT => {
            let mut ps = PAINTSTRUCT::default();
            let brush = CreateSolidBrush (COLORREF(0));
            let hdc = BeginPaint (hwnd, &mut ps);
            FillRect (hdc, &ps.rcPaint, brush);
            let _ = DeleteObject (brush.into());
            let _ = EndPaint (hwnd, &ps);
            LRESULT(0)
        },
        WM_TIMER if w_param.0 == TIMER_ID => {
            bring_to_top(hwnd);
            LRESULT(0)
        },
        _ => DefWindowProcW (hwnd, msg, w_param, l_param)
    }
} }
