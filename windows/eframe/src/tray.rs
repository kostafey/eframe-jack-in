use std::path::PathBuf;

use windows_sys::core::PCWSTR;
use windows_sys::Win32::Foundation::{
    GetLastError, HINSTANCE, HWND, LPARAM, LRESULT, POINT, WPARAM,
};
use windows_sys::Win32::Graphics::Gdi::HBRUSH;
use windows_sys::Win32::System::LibraryLoader::GetModuleHandleW;
use windows_sys::Win32::UI::Input::KeyboardAndMouse::{RegisterHotKey, UnregisterHotKey};
use windows_sys::Win32::UI::Shell::{
    Shell_NotifyIconW, NIF_ICON, NIF_MESSAGE, NIF_TIP, NIM_ADD, NIM_DELETE, NIM_MODIFY,
    NOTIFYICONDATAW,
};
use windows_sys::Win32::UI::WindowsAndMessaging::{
    AppendMenuW, CreateIconFromResourceEx, CreatePopupMenu, CreateWindowExW, DefWindowProcW,
    DestroyIcon, DestroyMenu, DispatchMessageW, GetCursorPos, GetMessageW, GetWindowLongPtrW,
    LoadCursorW, PostMessageW, PostQuitMessage, RegisterClassExW, RegisterWindowMessageW,
    SetForegroundWindow, SetWindowLongPtrW, TrackPopupMenu, TranslateMessage, CS_HREDRAW,
    CS_VREDRAW, CW_USEDEFAULT, GWLP_USERDATA, HICON, HMENU, IDC_ARROW, LR_DEFAULTCOLOR, MF_SEPARATOR,
    MF_STRING, MSG, TPM_BOTTOMALIGN, TPM_LEFTALIGN, TPM_RIGHTBUTTON, WM_APP, WM_COMMAND,
    WM_DESTROY, WM_HOTKEY, WM_LBUTTONDBLCLK, WM_RBUTTONUP, WM_USER, WNDCLASSEXW,
};

use crate::config::{Config, Target};
use crate::util::{self, wide};
use crate::window;

const TRAY_MSG: u32 = WM_APP + 1;
const TRAY_ICON_UID: u32 = 1;

const MENU_ID_TARGET_BASE: u32 = 100;
const MENU_ID_RELOAD: u32 = 900;
const MENU_ID_LIST: u32 = 901;
const MENU_ID_QUIT: u32 = 902;

const HOTKEY_ID_BASE: i32 = 1;

const ICON_BYTES: &[u8] = include_bytes!("emacs.ico");

pub struct App {
    pub config: Config,
    pub config_override: Option<PathBuf>,
    hwnd: HWND,
    hicon: HICON,
    hmenu_wnd_class: Vec<u16>,
    taskbar_created_msg: u32,
    registered_ids: Vec<i32>,
    hotkey_conflicts: Vec<String>,
}

/// Global-ish pointer to the running App, set from wndproc via GWLP_USERDATA.
/// Wrapped in a helper because we route messages through a C callback.
unsafe fn app_from_hwnd(hwnd: HWND) -> Option<&'static mut App> {
    let ptr = GetWindowLongPtrW(hwnd, GWLP_USERDATA) as *mut App;
    if ptr.is_null() {
        None
    } else {
        Some(&mut *ptr)
    }
}

pub fn run(config: Config, config_override: Option<PathBuf>) -> Result<(), String> {
    let hinstance = unsafe { GetModuleHandleW(std::ptr::null()) as HINSTANCE };
    let class_name = wide("eframe-jack-in-tray");

    let wc = WNDCLASSEXW {
        cbSize: std::mem::size_of::<WNDCLASSEXW>() as u32,
        style: CS_HREDRAW | CS_VREDRAW,
        lpfnWndProc: Some(wndproc),
        cbClsExtra: 0,
        cbWndExtra: 0,
        hInstance: hinstance,
        hIcon: std::ptr::null_mut(),
        hCursor: unsafe { LoadCursorW(std::ptr::null_mut(), IDC_ARROW) },
        hbrBackground: std::ptr::null_mut::<std::ffi::c_void>() as HBRUSH,
        lpszMenuName: std::ptr::null(),
        lpszClassName: class_name.as_ptr(),
        hIconSm: std::ptr::null_mut(),
    };
    if unsafe { RegisterClassExW(&wc) } == 0 {
        return Err(format!(
            "RegisterClassExW failed: err={}",
            unsafe { GetLastError() }
        ));
    }

    let title = wide("eframe-jack-in");
    let hwnd: HWND = unsafe {
        CreateWindowExW(
            0,
            class_name.as_ptr(),
            title.as_ptr(),
            0, // no WS_VISIBLE — invisible but still receives broadcast msgs
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            0,
            0,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            hinstance,
            std::ptr::null(),
        )
    };
    if hwnd.is_null() {
        return Err(format!(
            "CreateWindowExW failed: err={}",
            unsafe { GetLastError() }
        ));
    }

    let hicon = load_embedded_icon().ok_or_else(|| "failed to load embedded icon".to_string())?;

    let taskbar_created_msg =
        unsafe { RegisterWindowMessageW(wide("TaskbarCreated").as_ptr()) };

    let mut app = Box::new(App {
        config,
        config_override,
        hwnd,
        hicon,
        hmenu_wnd_class: class_name,
        taskbar_created_msg,
        registered_ids: Vec::new(),
        hotkey_conflicts: Vec::new(),
    });

    unsafe {
        SetWindowLongPtrW(hwnd, GWLP_USERDATA, app.as_mut() as *mut App as isize);
    }

    app.install_tray_icon()?;
    app.register_hotkeys();
    app.notify_hotkey_conflicts();

    util::log(&format!(
        "app started: {} targets, hwnd=0x{:X}",
        app.config.targets.len(),
        hwnd as usize
    ));

    // Message loop.
    let mut msg: MSG = unsafe { std::mem::zeroed() };
    loop {
        let r = unsafe { GetMessageW(&mut msg, std::ptr::null_mut(), 0, 0) };
        if r <= 0 {
            break;
        }
        unsafe {
            TranslateMessage(&msg);
            DispatchMessageW(&msg);
        }
    }

    app.remove_tray_icon();
    app.unregister_hotkeys();
    unsafe {
        DestroyIcon(app.hicon);
    }
    let _ = app.hmenu_wnd_class; // keep alive for the lifetime of the app
    Ok(())
}

impl App {
    fn install_tray_icon(&self) -> Result<(), String> {
        let mut nid: NOTIFYICONDATAW = unsafe { std::mem::zeroed() };
        nid.cbSize = std::mem::size_of::<NOTIFYICONDATAW>() as u32;
        nid.hWnd = self.hwnd;
        nid.uID = TRAY_ICON_UID;
        nid.uFlags = NIF_ICON | NIF_MESSAGE | NIF_TIP;
        nid.uCallbackMessage = TRAY_MSG;
        nid.hIcon = self.hicon;
        write_tip(&mut nid.szTip, "eframe-jack-in");
        let ok = unsafe { Shell_NotifyIconW(NIM_ADD, &nid) };
        if ok == 0 {
            return Err("Shell_NotifyIconW(NIM_ADD) failed".to_string());
        }
        Ok(())
    }

    fn remove_tray_icon(&self) {
        let mut nid: NOTIFYICONDATAW = unsafe { std::mem::zeroed() };
        nid.cbSize = std::mem::size_of::<NOTIFYICONDATAW>() as u32;
        nid.hWnd = self.hwnd;
        nid.uID = TRAY_ICON_UID;
        unsafe {
            Shell_NotifyIconW(NIM_DELETE, &nid);
        }
    }

    fn refresh_tray_icon(&self) {
        // Explorer restarted — re-add and update tip.
        let mut nid: NOTIFYICONDATAW = unsafe { std::mem::zeroed() };
        nid.cbSize = std::mem::size_of::<NOTIFYICONDATAW>() as u32;
        nid.hWnd = self.hwnd;
        nid.uID = TRAY_ICON_UID;
        nid.uFlags = NIF_ICON | NIF_MESSAGE | NIF_TIP;
        nid.uCallbackMessage = TRAY_MSG;
        nid.hIcon = self.hicon;
        write_tip(&mut nid.szTip, "eframe-jack-in");
        unsafe {
            if Shell_NotifyIconW(NIM_MODIFY, &nid) == 0 {
                Shell_NotifyIconW(NIM_ADD, &nid);
            }
        }
    }

    fn register_hotkeys(&mut self) {
        self.registered_ids.clear();
        self.hotkey_conflicts.clear();
        for (i, t) in self.config.targets.iter().enumerate() {
            let id = HOTKEY_ID_BASE + i as i32;
            let ok = unsafe {
                RegisterHotKey(self.hwnd, id, t.hotkey.modifiers, t.hotkey.vk)
            };
            if ok == 0 {
                self.hotkey_conflicts
                    .push(format!("{} ({})", t.name, t.hotkey.display));
            } else {
                self.registered_ids.push(id);
            }
        }
    }

    fn unregister_hotkeys(&mut self) {
        for id in self.registered_ids.drain(..) {
            unsafe {
                UnregisterHotKey(self.hwnd, id);
            }
        }
    }

    fn notify_hotkey_conflicts(&self) {
        if self.hotkey_conflicts.is_empty() {
            return;
        }
        let msg = format!(
            "The following hotkeys couldn't be registered (probably already in \
             use by another program):\n\n  {}\n\nThe remaining hotkeys are active.",
            self.hotkey_conflicts.join("\n  ")
        );
        util::message_box_warning("eframe-jack-in", &msg);
        util::log(&format!("hotkey conflicts: {:?}", self.hotkey_conflicts));
    }

    fn activate_target(&mut self, idx: usize) {
        // Dispatch on the target's action. Non-activate actions ignore
        // match/launch entirely.
        match self.config.targets[idx].action {
            crate::config::Action::Activate => self.do_activate(idx),
            crate::config::Action::MinimizeForeground => self.do_minimize_foreground(idx),
        }
    }

    fn do_minimize_foreground(&self, idx: usize) {
        let name = self.config.targets[idx].name.clone();
        match window::minimize_foreground() {
            Some(class) => util::log(&format!("\"{}\": minimized window (class={})", name, class)),
            None => util::log(&format!(
                "\"{}\": nothing to minimize (no foreground / shell window)",
                name
            )),
        }
    }

    fn do_activate(&mut self, idx: usize) {
        let windows = window::enumerate_windows();
        // Split-borrow: hits computation and last_hwnd update happen in the
        // same &mut Self reference below.
        let (name, launch, always_launch_if_no_window, hits, last) = {
            let t: &Target = &self.config.targets[idx];
            let hits = window::match_windows(&t.matcher, &windows);
            (
                t.name.clone(),
                t.launch.clone(),
                t.always_launch_if_no_window,
                hits,
                t.last_hwnd,
            )
        };

        let mut activated = false;
        if !hits.is_empty() {
            let hwnd = window::pick_next(&hits, last);
            let ok = window::activate_window(hwnd);
            self.config.targets[idx].last_hwnd = Some(hwnd);
            activated = ok;
            util::log(&format!(
                "activate \"{}\": hwnd=0x{:X} ok={}",
                name, hwnd, ok
            ));
        }

        let should_launch =
            (hits.is_empty() || always_launch_if_no_window) && launch.is_some();
        if should_launch {
            if let Some(cmd) = &launch {
                if !activated {
                    window::allow_next_foreground_any();
                }
                let expanded = util::expand_env(cmd);
                if let Err(e) = util::spawn_detached(&expanded) {
                    util::log(&format!("launch \"{}\" failed: {}", name, e));
                    util::message_box_warning(
                        "eframe-jack-in",
                        &format!("Failed to launch {}:\n{}", name, e),
                    );
                } else {
                    util::log(&format!("launch \"{}\": {}", name, expanded));
                }
            }
        } else if hits.is_empty() && launch.is_none() {
            util::message_box_warning(
                "eframe-jack-in",
                &format!(
                    "No window found for \"{}\" and no launch command configured.",
                    name
                ),
            );
        }
    }

    fn show_tray_menu(&mut self) {
        unsafe {
            let hmenu: HMENU = CreatePopupMenu();
            if hmenu.is_null() {
                return;
            }
            for (i, t) in self.config.targets.iter().enumerate() {
                let label = format!("{}\t{}", t.name, t.hotkey.display);
                let wl = wide(&label);
                AppendMenuW(
                    hmenu,
                    MF_STRING,
                    (MENU_ID_TARGET_BASE + i as u32) as usize,
                    wl.as_ptr(),
                );
            }
            AppendMenuW(hmenu, MF_SEPARATOR, 0, std::ptr::null());
            let reload = wide("Reload config");
            AppendMenuW(hmenu, MF_STRING, MENU_ID_RELOAD as usize, reload.as_ptr());
            let list = wide("List windows...");
            AppendMenuW(hmenu, MF_STRING, MENU_ID_LIST as usize, list.as_ptr());
            AppendMenuW(hmenu, MF_SEPARATOR, 0, std::ptr::null());
            let quit = wide("Quit");
            AppendMenuW(hmenu, MF_STRING, MENU_ID_QUIT as usize, quit.as_ptr());

            let mut pt: POINT = std::mem::zeroed();
            GetCursorPos(&mut pt);
            SetForegroundWindow(self.hwnd);
            TrackPopupMenu(
                hmenu,
                TPM_RIGHTBUTTON | TPM_BOTTOMALIGN | TPM_LEFTALIGN,
                pt.x,
                pt.y,
                0,
                self.hwnd,
                std::ptr::null(),
            );
            PostMessageW(self.hwnd, 0, 0, 0); // WM_NULL — see MSDN
            DestroyMenu(hmenu);
        }
    }

    fn handle_menu_command(&mut self, id: u32) {
        match id {
            MENU_ID_QUIT => unsafe {
                PostQuitMessage(0);
            },
            MENU_ID_RELOAD => self.reload_config(),
            MENU_ID_LIST => self.dump_window_list(),
            id if (MENU_ID_TARGET_BASE..MENU_ID_RELOAD).contains(&id) => {
                let idx = (id - MENU_ID_TARGET_BASE) as usize;
                if idx < self.config.targets.len() {
                    self.activate_target(idx);
                }
            }
            _ => {}
        }
    }

    fn reload_config(&mut self) {
        let override_path = self.config_override.clone();
        let loaded = crate::config::locate_and_load(override_path);
        match loaded {
            Ok((new_cfg, origin)) => {
                self.unregister_hotkeys();
                self.config = new_cfg;
                self.register_hotkeys();
                self.notify_hotkey_conflicts();
                let src = match origin {
                    crate::config::ConfigOrigin::Explicit(p) => p.display().to_string(),
                    crate::config::ConfigOrigin::AppData(p) => p.display().to_string(),
                    crate::config::ConfigOrigin::NextToExe(p) => p.display().to_string(),
                    crate::config::ConfigOrigin::Default(_) => "<embedded default>".into(),
                };
                util::message_box_warning(
                    "eframe-jack-in",
                    &format!(
                        "Config reloaded from:\n{}\n\n{} target(s) active.",
                        src,
                        self.config.targets.len()
                    ),
                );
            }
            Err(e) => {
                util::message_box_error(
                    "eframe-jack-in",
                    &format!("Config reload failed:\n{}", e),
                );
            }
        }
    }

    fn dump_window_list(&self) {
        let windows = window::enumerate_windows();
        let text = window::format_window_table(&windows);
        let target = std::env::temp_dir().join("eframe-jack-in-windows.txt");
        if let Ok(()) = std::fs::write(&target, &text) {
            open_in_shell(&target);
        } else {
            util::message_box_error(
                "eframe-jack-in",
                &format!("Could not write window list to {}", target.display()),
            );
        }
    }
}

fn open_in_shell(path: &std::path::Path) {
    let cmd = format!("notepad.exe \"{}\"", path.display());
    let _ = util::spawn_detached(&cmd);
}

fn write_tip(dest: &mut [u16], src: &str) {
    let encoded: Vec<u16> = src.encode_utf16().collect();
    let n = encoded.len().min(dest.len().saturating_sub(1));
    dest[..n].copy_from_slice(&encoded[..n]);
    if n < dest.len() {
        dest[n] = 0;
    }
}

fn load_embedded_icon() -> Option<HICON> {
    // ICO layout: ICONDIR (6 bytes) + ICONDIRENTRY[count] (16 bytes each).
    // We pick the largest entry by width, then bitcount as a tie-breaker.
    if ICON_BYTES.len() < 6 {
        return None;
    }
    let count = u16::from_le_bytes([ICON_BYTES[4], ICON_BYTES[5]]) as usize;
    if count == 0 {
        return None;
    }
    let entry_size = 16;
    let mut best_index: Option<usize> = None;
    let mut best_score: i32 = -1;
    for i in 0..count {
        let base = 6 + i * entry_size;
        if base + entry_size > ICON_BYTES.len() {
            return None;
        }
        let mut w = ICON_BYTES[base] as i32;
        if w == 0 {
            w = 256;
        }
        let bit_count = u16::from_le_bytes([ICON_BYTES[base + 6], ICON_BYTES[base + 7]]) as i32;
        let score = w * 1000 + bit_count;
        if score > best_score {
            best_score = score;
            best_index = Some(i);
        }
    }
    let i = best_index?;
    let base = 6 + i * entry_size;
    let mut w = ICON_BYTES[base] as i32;
    if w == 0 {
        w = 256;
    }
    let mut h = ICON_BYTES[base + 1] as i32;
    if h == 0 {
        h = 256;
    }
    let size = u32::from_le_bytes([
        ICON_BYTES[base + 8],
        ICON_BYTES[base + 9],
        ICON_BYTES[base + 10],
        ICON_BYTES[base + 11],
    ]);
    let offset = u32::from_le_bytes([
        ICON_BYTES[base + 12],
        ICON_BYTES[base + 13],
        ICON_BYTES[base + 14],
        ICON_BYTES[base + 15],
    ]) as usize;
    if offset + size as usize > ICON_BYTES.len() {
        return None;
    }
    let data_ptr = ICON_BYTES[offset..].as_ptr();
    unsafe {
        let icon = CreateIconFromResourceEx(
            data_ptr as *mut u8,
            size,
            1,
            0x00030000,
            w,
            h,
            LR_DEFAULTCOLOR,
        );
        if icon.is_null() {
            None
        } else {
            Some(icon)
        }
    }
}

unsafe extern "system" fn wndproc(
    hwnd: HWND,
    msg: u32,
    wparam: WPARAM,
    lparam: LPARAM,
) -> LRESULT {
    if let Some(app) = app_from_hwnd(hwnd) {
        if msg == app.taskbar_created_msg {
            app.refresh_tray_icon();
            return 0;
        }
        match msg {
            WM_HOTKEY => {
                let id = wparam as i32;
                let idx = (id - HOTKEY_ID_BASE) as usize;
                if idx < app.config.targets.len() {
                    app.activate_target(idx);
                }
                return 0;
            }
            TRAY_MSG => {
                let event = (lparam as u32) & 0xFFFF;
                match event {
                    WM_RBUTTONUP | WM_USER => {
                        app.show_tray_menu();
                    }
                    WM_LBUTTONDBLCLK => {
                        // Double-click = activate first target as a shortcut.
                        if !app.config.targets.is_empty() {
                            app.activate_target(0);
                        }
                    }
                    _ => {}
                }
                return 0;
            }
            WM_COMMAND => {
                let id = (wparam as u32) & 0xFFFF;
                app.handle_menu_command(id);
                return 0;
            }
            WM_DESTROY => {
                PostQuitMessage(0);
                return 0;
            }
            _ => {}
        }
    }
    DefWindowProcW(hwnd, msg, wparam, lparam)
}

// Reference to silence "unused" if the linker prunes:
#[allow(dead_code)]
fn _keep_pcwstr_used(_: PCWSTR) {}
