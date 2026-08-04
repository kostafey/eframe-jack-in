use std::cell::RefCell;
use std::ffi::c_void;
use std::path::Path;

use windows_sys::Win32::Foundation::{
    CloseHandle, BOOL, FALSE, HANDLE, HWND, LPARAM, MAX_PATH, TRUE,
};
use windows_sys::Win32::Graphics::Dwm::{DwmGetWindowAttribute, DWMWA_CLOAKED};
use windows_sys::Win32::System::Threading::{
    AttachThreadInput, GetCurrentThreadId, OpenProcess, QueryFullProcessImageNameW,
    PROCESS_QUERY_LIMITED_INFORMATION,
};
use windows_sys::Win32::UI::Input::KeyboardAndMouse::{
    GetKeyboardLayout, GetKeyboardLayoutList, SetFocus, HKL,
};
use windows_sys::Win32::UI::WindowsAndMessaging::{
    AllowSetForegroundWindow, BringWindowToTop, EnumWindows, GetClassNameW, GetForegroundWindow,
    GetWindow, GetWindowLongPtrW, GetWindowTextLengthW, GetWindowTextW, GetWindowThreadProcessId,
    IsIconic, IsWindowVisible, PostMessageW, SetForegroundWindow, ShowWindow, ASFW_ANY,
    GWL_EXSTYLE, GW_OWNER, SW_MINIMIZE, SW_RESTORE, SW_SHOW, WM_INPUTLANGCHANGEREQUEST,
    WS_EX_TOOLWINDOW,
};

use crate::config::Matcher;
use crate::util;

#[derive(Debug, Clone)]
pub struct WindowInfo {
    pub hwnd: isize,
    pub class: String,
    pub title: String,
    pub exe_path: String,
    pub exe_name: String,
}

thread_local! {
    static ENUM_BUFFER: RefCell<Vec<WindowInfo>> = const { RefCell::new(Vec::new()) };
}

pub fn enumerate_windows() -> Vec<WindowInfo> {
    ENUM_BUFFER.with(|b| b.borrow_mut().clear());
    unsafe {
        EnumWindows(Some(enum_proc), 0);
    }
    ENUM_BUFFER.with(|b| b.borrow_mut().drain(..).collect())
}

unsafe extern "system" fn enum_proc(hwnd: HWND, _: LPARAM) -> BOOL {
    if IsWindowVisible(hwnd) == FALSE {
        return TRUE;
    }
    if !GetWindow(hwnd, GW_OWNER).is_null() {
        return TRUE;
    }
    let ex_style = GetWindowLongPtrW(hwnd, GWL_EXSTYLE) as u32;
    if ex_style & WS_EX_TOOLWINDOW != 0 {
        return TRUE;
    }
    let mut cloaked: u32 = 0;
    let hr = DwmGetWindowAttribute(
        hwnd,
        DWMWA_CLOAKED as u32,
        &mut cloaked as *mut _ as *mut c_void,
        std::mem::size_of::<u32>() as u32,
    );
    if hr == 0 && cloaked != 0 {
        return TRUE;
    }

    let class = get_class_name(hwnd);
    let title = get_window_text(hwnd);
    let (exe_path, exe_name) = get_process_image(hwnd);

    if class.is_empty() && title.is_empty() && exe_path.is_empty() {
        return TRUE;
    }

    ENUM_BUFFER.with(|b| {
        b.borrow_mut().push(WindowInfo {
            hwnd: hwnd as isize,
            class,
            title,
            exe_path,
            exe_name,
        });
    });
    TRUE
}

unsafe fn get_class_name(hwnd: HWND) -> String {
    let mut buf = [0u16; 256];
    let n = GetClassNameW(hwnd, buf.as_mut_ptr(), buf.len() as i32);
    if n <= 0 {
        return String::new();
    }
    String::from_utf16_lossy(&buf[..n as usize])
}

unsafe fn get_window_text(hwnd: HWND) -> String {
    let len = GetWindowTextLengthW(hwnd);
    if len <= 0 {
        return String::new();
    }
    let mut buf = vec![0u16; (len as usize) + 1];
    let n = GetWindowTextW(hwnd, buf.as_mut_ptr(), buf.len() as i32);
    if n <= 0 {
        return String::new();
    }
    String::from_utf16_lossy(&buf[..n as usize])
}

unsafe fn get_process_image(hwnd: HWND) -> (String, String) {
    let mut pid: u32 = 0;
    GetWindowThreadProcessId(hwnd, &mut pid);
    if pid == 0 {
        return (String::new(), String::new());
    }
    let handle: HANDLE = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, 0, pid);
    if handle.is_null() {
        return (String::new(), String::new());
    }
    let mut buf = [0u16; MAX_PATH as usize];
    let mut size: u32 = buf.len() as u32;
    let ok = QueryFullProcessImageNameW(handle, 0, buf.as_mut_ptr(), &mut size);
    CloseHandle(handle);
    if ok == 0 {
        return (String::new(), String::new());
    }
    let full = String::from_utf16_lossy(&buf[..size as usize]);
    let name = Path::new(&full)
        .file_name()
        .map(|s| s.to_string_lossy().into_owned())
        .unwrap_or_default();
    (full, name)
}

/// Return all windows matching a target's predicates. HWNDs are sorted by their
/// numeric value for a stable cycling order.
pub fn match_windows(matcher: &Matcher, windows: &[WindowInfo]) -> Vec<isize> {
    let mut hits: Vec<isize> = windows
        .iter()
        .filter(|w| matcher.matches(&w.class, &w.exe_name, &w.title))
        .map(|w| w.hwnd)
        .collect();
    hits.sort_unstable();
    hits
}

/// Bring `hwnd` to the foreground. Handles the AttachThreadInput dance that
/// Windows requires when SetForegroundWindow's first attempt is refused.
pub fn activate_window(hwnd: isize) -> bool {
    unsafe {
        let hwnd = hwnd as HWND;
        if IsIconic(hwnd) != 0 {
            ShowWindow(hwnd, SW_RESTORE);
        } else {
            ShowWindow(hwnd, SW_SHOW);
        }
        if SetForegroundWindow(hwnd) != 0 {
            return true;
        }
        let fg = GetForegroundWindow();
        if fg.is_null() {
            return false;
        }
        let fg_tid = GetWindowThreadProcessId(fg, std::ptr::null_mut());
        let my_tid = GetCurrentThreadId();
        AttachThreadInput(my_tid, fg_tid, TRUE);
        BringWindowToTop(hwnd);
        let ok = SetForegroundWindow(hwnd) != 0;
        SetFocus(hwnd);
        AttachThreadInput(my_tid, fg_tid, FALSE);
        ok
    }
}

/// Pick the next HWND to activate from `hits`, given the currently foreground
/// window and the target's last-activated hwnd. Cycles through hits.
pub fn pick_next(hits: &[isize], last: Option<isize>) -> isize {
    let fg = unsafe { GetForegroundWindow() } as isize;
    if let Some(pos) = hits.iter().position(|&h| h == fg) {
        return hits[(pos + 1) % hits.len()];
    }
    if let Some(last) = last {
        if let Some(pos) = hits.iter().position(|&h| h == last) {
            return hits[(pos + 1) % hits.len()];
        }
    }
    hits[0]
}

/// Grant the next spawned foreground process the right to steal focus. Called
/// just before launching a target that isn't currently visible.
pub fn allow_next_foreground_any() {
    unsafe {
        AllowSetForegroundWindow(ASFW_ANY);
    }
}

/// Cycle the foreground window's input language to the next installed
/// keyboard layout. Returns a "0xOLD -> 0xNEW" diagnostic string, or None
/// if there's no foreground / only one layout / API failure.
///
/// Uses PostMessageW(WM_INPUTLANGCHANGEREQUEST) so the target app receives
/// the change through the same code path Windows itself uses for the
/// system Alt+Shift hotkey — apps that track layout changes (Emacs, IDEs,
/// browsers) all pick it up.
pub fn switch_keyboard_layout() -> Option<String> {
    unsafe {
        let hwnd = GetForegroundWindow();
        if hwnd.is_null() {
            return None;
        }
        let tid = GetWindowThreadProcessId(hwnd, std::ptr::null_mut());
        if tid == 0 {
            return None;
        }
        let current: HKL = GetKeyboardLayout(tid);

        let count = GetKeyboardLayoutList(0, std::ptr::null_mut());
        if count <= 1 {
            return None; // nothing to cycle to
        }
        let mut layouts: Vec<HKL> = vec![std::ptr::null_mut(); count as usize];
        let got = GetKeyboardLayoutList(count, layouts.as_mut_ptr());
        if got <= 0 {
            return None;
        }
        layouts.truncate(got as usize);

        let next = if let Some(pos) = layouts.iter().position(|&h| h == current) {
            layouts[(pos + 1) % layouts.len()]
        } else {
            layouts[0]
        };

        PostMessageW(hwnd, WM_INPUTLANGCHANGEREQUEST, 0, next as isize);
        Some(format!("0x{:X} -> 0x{:X}", current as usize, next as usize))
    }
}

/// Minimize whatever window has the foreground right now. Returns the class
/// name of the affected window (or None if we deliberately skipped it —
/// desktop / taskbar / no foreground). Called by the "minimize-foreground"
/// action.
pub fn minimize_foreground() -> Option<String> {
    unsafe {
        let hwnd = GetForegroundWindow();
        if hwnd.is_null() {
            return None;
        }
        let mut buf = [0u16; 128];
        let n = GetClassNameW(hwnd, buf.as_mut_ptr(), buf.len() as i32);
        let class = if n > 0 {
            String::from_utf16_lossy(&buf[..n as usize])
        } else {
            String::new()
        };
        // Skip shell windows — minimizing the desktop or the taskbar is
        // never what the user wants.
        if matches!(class.as_str(), "Progman" | "Shell_TrayWnd" | "WorkerW") {
            return None;
        }
        ShowWindow(hwnd, SW_MINIMIZE);
        Some(class)
    }
}

pub fn format_window_table(windows: &[WindowInfo]) -> String {
    let mut lines = Vec::with_capacity(windows.len() + 2);
    lines.push(format!(
        "{:<12}  {:<28}  {:<50}  {}",
        "HWND", "CLASS", "EXE", "TITLE"
    ));
    lines.push("-".repeat(120));
    for w in windows {
        lines.push(format!(
            "0x{:010X}  {:<28}  {:<50}  {}",
            w.hwnd, w.class, w.exe_path, w.title
        ));
    }
    lines.join("\n")
}

/// Used by --check-config to determine whether a launch command would resolve.
/// Returns None on success, or a diagnostic string on failure.
pub fn check_launch(cmd: &str) -> Option<String> {
    let expanded = util::expand_env(cmd);
    let (program, _) = match util::split_command(&expanded) {
        Some(x) => x,
        None => return Some("empty launch command".to_string()),
    };
    if program.contains('\\') || program.contains('/') {
        if !Path::new(&program).is_file() {
            return Some(format!("file not found: {}", program));
        }
        return None;
    }
    if util::resolve_app_paths(&program).is_some() {
        return None;
    }
    if let Ok(path_env) = std::env::var("PATH") {
        for dir in path_env.split(';') {
            if Path::new(dir).join(&program).is_file() {
                return None;
            }
        }
    }
    Some(format!("not found in App Paths or PATH: {}", program))
}
