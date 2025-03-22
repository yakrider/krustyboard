use std::os::windows::io::AsRawHandle;
use std::sync::atomic::{AtomicI32, Ordering};
use std::thread;
use once_cell::sync::OnceCell;

use windows::core::PCWSTR;
use windows::Win32::Foundation::{COLORREF, HANDLE, HINSTANCE, HWND, LPARAM, LRESULT, RECT, WPARAM};
use windows::Win32::Graphics::Gdi::{BeginPaint, CreateSolidBrush, DeleteObject, EndPaint, FillRect, UpdateWindow, HBRUSH, PAINTSTRUCT};
use windows::Win32::System::LibraryLoader::GetModuleHandleW;
use windows::Win32::System::Threading::GetThreadId;
use windows::Win32::UI::WindowsAndMessaging::{CreateWindowExW, DefWindowProcW, DestroyWindow, DispatchMessageW, GetMessageW, GetSystemMetrics, KillTimer, PostMessageW, PostThreadMessageW, RegisterClassExW, SetLayeredWindowAttributes, SetTimer, SetWindowPos, HCURSOR, HICON, HWND_TOPMOST, LWA_ALPHA, MSG, SM_CXVIRTUALSCREEN, SM_CYVIRTUALSCREEN, SWP_NOACTIVATE, SWP_NOMOVE, SWP_NOSIZE, SWP_SHOWWINDOW, WM_APP, WM_CLOSE, WM_PAINT, WM_TIMER, WNDCLASSEXW, WNDCLASS_STYLES, WS_EX_LAYERED, WS_EX_NOACTIVATE, WS_EX_TOOLWINDOW, WS_EX_TOPMOST, WS_EX_TRANSPARENT, WS_POPUP};

use crate::{Flag, Hwnd, HwndAtomic};



const TIMER_ID : usize = 0xdeadbeef;
const TIMER_TICK_MS : u32 = 20;
const MAX_DIM_PERC : i32 = 72;

const MSG_OVERLAY_CREATE : u32 = WM_APP + 1;



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
        let width  = unsafe { GetSystemMetrics (SM_CXVIRTUALSCREEN) };
        let height = unsafe { GetSystemMetrics (SM_CYVIRTUALSCREEN) };
        Self {
            rect: RECT { left: 0, top: 0, right: width, bottom: height },
            overlay_enabled: Flag::new(true),
            overlay_enforced: Flag::new(false),
            ov_opacity: AtomicI32::new(0),
        }
    }
}



pub struct DimmingOverlay {
    thread_id : u32,
    monitor: DisplayInfo,
    hwnd: HwndAtomic,
    timer_id: HwndAtomic,
}


impl DimmingOverlay {

    pub fn instance() -> &'static DimmingOverlay {

        static INSTANCE: OnceCell<DimmingOverlay> = OnceCell::new();

        INSTANCE .get_or_init ( || {

            // first we'll do one-time registration of overlay-window class
            let _ = register_window_class();

            // then we'll create the thread that will handle msg-loop for all overlay-windows
            let mgr_thread = thread::spawn ( || { unsafe {
                let mut msg = MSG::default();
                while GetMessageW(&mut msg, None, 0, 0).into() {
                    match msg.message {
                        MSG_OVERLAY_CREATE => {
                            let overlay = DimmingOverlay::instance();
                            overlay.create_window();
                            overlay.update();
                        },
                        _ => { DispatchMessageW(&msg); }
                    };
                }
            } } );

            // and finally we can bring up our static instance
            DimmingOverlay {
                thread_id: unsafe { GetThreadId (HANDLE (mgr_thread.as_raw_handle())) },
                monitor: DisplayInfo::get_cur(),
                hwnd: Default::default(),
                timer_id: Default::default(),
            }
        } )
    }

    pub fn is_active (&'static self) -> bool {
        self.hwnd.is_valid()
    }

    pub fn incr_dimming (&'static self, incr:i32) {
        let opc = (incr + self.monitor.ov_opacity.load(Ordering::Relaxed)) .clamp(0, MAX_DIM_PERC);
        self.monitor.ov_opacity.store (opc, Ordering::Relaxed);
        self.update();
    }

    unsafe fn create_window (&'static self) {
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
    }


    pub fn update (&'static self) { unsafe {

        let opc = self.monitor.ov_opacity.load(Ordering::Relaxed);

        // if we shouldnt have an overlay, and we do, we should close it out
        if self.monitor.overlay_enabled.is_clear() || opc == 0 {
            self.kill_top_enforce_timer();
            let hwnd = self.hwnd.load();
            if hwnd.is_valid() {
                let _ = PostMessageW (Some(hwnd.into()), WM_CLOSE, WPARAM::default(), LPARAM::default());
                self.hwnd.clear();
            }
            return;
        }

        // if we should have an overlay, and dont yet, we should create one
        if !self.hwnd.load().is_valid() {
            // we'll send a msg to our mgr-thread to create a window for us (and do the update)
            let _ = PostThreadMessageW (self.thread_id, MSG_OVERLAY_CREATE, Default::default(), Default::default());
            return;
        }

        // else, we want and overlay, and we got one .. we just update it
        let hwnd = self.hwnd.load();
        let opacity = (2.55 * opc.clamp (0, MAX_DIM_PERC) as f32) as u8;

        let _ = SetLayeredWindowAttributes (hwnd.into(), COLORREF(0), opacity, LWA_ALPHA);
        let _ = UpdateWindow (hwnd.into());

        bring_to_top(hwnd.into());

        if self.monitor.overlay_enforced.is_set() {
            self.start_top_enforce_timer();
        }
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
        WM_CLOSE => {
            let _ = DestroyWindow(hwnd);
            LRESULT(0)
        },
        WM_TIMER if w_param.0 == TIMER_ID => {
            bring_to_top(hwnd);
            LRESULT(0)
        },
        _ => DefWindowProcW (hwnd, msg, w_param, l_param)
    }
} }
