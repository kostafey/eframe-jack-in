use std::ffi::OsStr;
use std::fs::OpenOptions;
use std::io::Write as _;
use std::os::windows::ffi::OsStrExt;
use std::path::PathBuf;
use std::sync::Mutex;

use windows_sys::Win32::Foundation::{
    CloseHandle, GetLastError, ERROR_ALREADY_EXISTS, ERROR_FILE_NOT_FOUND, HANDLE,
};
use windows_sys::Win32::System::Console::{
    AttachConsole, GetConsoleWindow, ATTACH_PARENT_PROCESS,
};
use windows_sys::Win32::System::Environment::ExpandEnvironmentStringsW;
use windows_sys::Win32::System::Registry::{
    RegCloseKey, RegOpenKeyExW, RegQueryValueExW, HKEY, HKEY_CURRENT_USER, HKEY_LOCAL_MACHINE,
    KEY_READ, REG_SZ, REG_EXPAND_SZ,
};
use windows_sys::Win32::System::Threading::{
    CreateMutexW, CreateProcessW, PROCESS_INFORMATION, STARTUPINFOW,
};
use windows_sys::Win32::UI::WindowsAndMessaging::{
    MessageBoxW, MB_ICONERROR, MB_ICONWARNING, MB_OK,
};

pub fn wide<S: AsRef<OsStr>>(s: S) -> Vec<u16> {
    s.as_ref().encode_wide().chain(std::iter::once(0)).collect()
}

pub fn from_wide(buf: &[u16]) -> String {
    let end = buf.iter().position(|&c| c == 0).unwrap_or(buf.len());
    String::from_utf16_lossy(&buf[..end])
}

pub fn message_box_error(title: &str, msg: &str) {
    let t = wide(title);
    let m = wide(msg);
    unsafe {
        MessageBoxW(std::ptr::null_mut(), m.as_ptr(), t.as_ptr(), MB_OK | MB_ICONERROR);
    }
}

pub fn message_box_warning(title: &str, msg: &str) {
    let t = wide(title);
    let m = wide(msg);
    unsafe {
        MessageBoxW(std::ptr::null_mut(), m.as_ptr(), t.as_ptr(), MB_OK | MB_ICONWARNING);
    }
}

pub struct SingletonGuard {
    handle: HANDLE,
}

impl Drop for SingletonGuard {
    fn drop(&mut self) {
        unsafe {
            if !self.handle.is_null() {
                CloseHandle(self.handle);
            }
        }
    }
}

/// Returns Ok(Some(guard)) if we acquired the lock, Ok(None) if another instance
/// already holds it, Err on genuine failure.
pub fn acquire_singleton(name: &str) -> Result<Option<SingletonGuard>, String> {
    let n = wide(name);
    unsafe {
        let h = CreateMutexW(std::ptr::null(), 1, n.as_ptr());
        if h.is_null() {
            return Err(format!("CreateMutexW failed: last_error={}", GetLastError()));
        }
        if GetLastError() == ERROR_ALREADY_EXISTS {
            CloseHandle(h);
            return Ok(None);
        }
        Ok(Some(SingletonGuard { handle: h }))
    }
}

/// Expand %VARIABLE% and leading ~ in a path/string. Returns the input unchanged
/// on any failure (matches how the .bat fallback behaves).
pub fn expand_env(s: &str) -> String {
    let s = if let Some(rest) = s.strip_prefix('~') {
        match std::env::var("USERPROFILE") {
            Ok(home) => format!("{}{}", home, rest),
            Err(_) => s.to_string(),
        }
    } else {
        s.to_string()
    };

    let src = wide(&s);
    unsafe {
        let needed = ExpandEnvironmentStringsW(src.as_ptr(), std::ptr::null_mut(), 0);
        if needed == 0 {
            return s;
        }
        let mut buf = vec![0u16; needed as usize];
        let written = ExpandEnvironmentStringsW(src.as_ptr(), buf.as_mut_ptr(), needed);
        if written == 0 {
            return s;
        }
        from_wide(&buf)
    }
}

/// Look up an exe by name under App Paths (HKLM then HKCU). Returns the fully
/// qualified path if found.
pub fn resolve_app_paths(exe_name: &str) -> Option<String> {
    let subkey = format!(
        "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\App Paths\\{}",
        exe_name
    );
    for root in [HKEY_LOCAL_MACHINE, HKEY_CURRENT_USER] {
        if let Some(v) = read_reg_string(root, &subkey, "") {
            let expanded = expand_env(&v);
            if std::path::Path::new(&expanded).is_file() {
                return Some(expanded);
            }
            return Some(expanded);
        }
    }
    None
}

fn read_reg_string(root: HKEY, subkey: &str, value: &str) -> Option<String> {
    let sk = wide(subkey);
    let vn = wide(value);
    unsafe {
        let mut hkey: HKEY = std::ptr::null_mut();
        let rc = RegOpenKeyExW(root, sk.as_ptr(), 0, KEY_READ, &mut hkey);
        if rc != 0 {
            return None;
        }
        let mut kind: u32 = 0;
        let mut cb: u32 = 0;
        let rc = RegQueryValueExW(
            hkey,
            vn.as_ptr(),
            std::ptr::null_mut(),
            &mut kind,
            std::ptr::null_mut(),
            &mut cb,
        );
        if rc != 0 || (kind != REG_SZ && kind != REG_EXPAND_SZ) {
            RegCloseKey(hkey);
            return None;
        }
        let mut buf = vec![0u16; (cb as usize / 2) + 1];
        let rc = RegQueryValueExW(
            hkey,
            vn.as_ptr(),
            std::ptr::null_mut(),
            &mut kind,
            buf.as_mut_ptr() as *mut u8,
            &mut cb,
        );
        RegCloseKey(hkey);
        if rc != 0 {
            return None;
        }
        Some(from_wide(&buf))
    }
}

/// Resolve a launch command's exe part against App Paths + PATH. If it's an
/// absolute path or contains a separator, we only expand env vars.
pub fn resolve_exe(name: &str) -> Option<String> {
    let expanded = expand_env(name);
    if expanded.contains('\\') || expanded.contains('/') {
        return Some(expanded);
    }
    if let Some(path) = resolve_app_paths(&expanded) {
        return Some(path);
    }
    // PATH lookup
    if let Ok(path_env) = std::env::var("PATH") {
        for dir in path_env.split(';') {
            let candidate = std::path::Path::new(dir).join(&expanded);
            if candidate.is_file() {
                return Some(candidate.to_string_lossy().into_owned());
            }
        }
    }
    None
}

/// Tokenize a Windows-style command line into program + args, honouring
/// double-quoted spans.
pub fn split_command(cmd: &str) -> Option<(String, Vec<String>)> {
    let mut tokens: Vec<String> = Vec::new();
    let mut buf = String::new();
    let mut in_quotes = false;
    let mut has_token = false;
    for c in cmd.chars() {
        match c {
            '"' => {
                in_quotes = !in_quotes;
                has_token = true;
            }
            c if c.is_whitespace() && !in_quotes => {
                if has_token {
                    tokens.push(std::mem::take(&mut buf));
                    has_token = false;
                }
            }
            c => {
                buf.push(c);
                has_token = true;
            }
        }
    }
    if has_token {
        tokens.push(buf);
    }
    if tokens.is_empty() {
        return None;
    }
    let program = tokens.remove(0);
    Some((program, tokens))
}

/// Spawn the given command detached, with no window and no inherited handles.
/// Returns Err(String) with diagnostics; never blocks on child output.
pub fn spawn_detached(command: &str) -> Result<(), String> {
    let (program, _args) =
        split_command(command).ok_or_else(|| "empty launch command".to_string())?;
    let program = expand_env(&program);
    let resolved = resolve_exe(&program).unwrap_or(program);

    // Rebuild the command line with the resolved program path so CreateProcessW
    // knows what to launch even when the caller only gave a bare name.
    let rest_start = command.find(char::is_whitespace).unwrap_or(command.len());
    let rest = &command[rest_start..];
    let full_line = if resolved.contains(' ') {
        format!("\"{}\"{}", resolved, rest)
    } else {
        format!("{}{}", resolved, rest)
    };

    let mut cmdline = wide(&full_line);
    let mut si: STARTUPINFOW = unsafe { std::mem::zeroed() };
    si.cb = std::mem::size_of::<STARTUPINFOW>() as u32;
    let mut pi: PROCESS_INFORMATION = unsafe { std::mem::zeroed() };
    let ok = unsafe {
        CreateProcessW(
            std::ptr::null(),
            cmdline.as_mut_ptr(),
            std::ptr::null(),
            std::ptr::null(),
            0, // do not inherit handles
            0, // no DETACHED_PROCESS / CREATE_NO_WINDOW — those are EDR-heuristic triggers

            std::ptr::null(),
            std::ptr::null(),
            &si,
            &mut pi,
        )
    };
    if ok == 0 {
        let err = unsafe { GetLastError() };
        let hint = if err == ERROR_FILE_NOT_FOUND {
            format!(" — file not found: '{}'", resolved)
        } else {
            String::new()
        };
        return Err(format!(
            "CreateProcessW failed (err={}){}\ncommand: {}",
            err, hint, full_line
        ));
    }
    unsafe {
        CloseHandle(pi.hProcess);
        CloseHandle(pi.hThread);
    }
    Ok(())
}

/// Attach to the parent console (if any) so println!/eprintln! output is
/// visible when the program is launched from a shell in diagnostic mode.
pub fn attach_parent_console() {
    unsafe {
        if GetConsoleWindow().is_null() {
            AttachConsole(ATTACH_PARENT_PROCESS);
        }
    }
}

// ---- Simple opt-in file logger --------------------------------------------

static LOG_PATH: Mutex<Option<PathBuf>> = Mutex::new(None);

pub fn set_log_path(p: Option<PathBuf>) {
    if let Ok(mut g) = LOG_PATH.lock() {
        *g = p;
    }
}

pub fn log(msg: &str) {
    let path = match LOG_PATH.lock() {
        Ok(g) => g.clone(),
        _ => return,
    };
    let Some(path) = path else { return };
    let ts = format_timestamp();
    if let Ok(mut f) = OpenOptions::new().create(true).append(true).open(&path) {
        let _ = writeln!(f, "[{}] {}", ts, msg);
    }
}

fn format_timestamp() -> String {
    static START: std::sync::OnceLock<std::time::Instant> = std::sync::OnceLock::new();
    let start = START.get_or_init(std::time::Instant::now);
    format!("+{:.3}s", start.elapsed().as_secs_f64())
}
