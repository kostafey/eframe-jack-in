use std::fs;
use std::path::{Path, PathBuf};

use serde::Deserialize;
use windows_sys::Win32::UI::Input::KeyboardAndMouse::{
    MOD_ALT, MOD_CONTROL, MOD_NOREPEAT, MOD_SHIFT, MOD_WIN,
};

pub const DEFAULT_CONFIG_STR: &str = include_str!("default_config.toml");

#[derive(Debug, Clone, Deserialize)]
pub struct RawConfig {
    #[serde(default, rename = "target")]
    pub targets: Vec<RawTarget>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct RawTarget {
    pub name: String,
    pub hotkey: String,
    #[serde(default, rename = "match")]
    pub matcher: RawMatcher,
    #[serde(default)]
    pub launch: Option<String>,
    #[serde(default)]
    pub always_launch_if_no_window: bool,
    /// Optional action override. Absent / "activate" (default) = find and raise
    /// the matched window, launch when no match. "minimize-foreground" =
    /// minimize whatever window has focus right now (`match` / `launch` ignored).
    #[serde(default)]
    pub action: Option<String>,
}

#[derive(Debug, Clone, Default, Deserialize)]
pub struct RawMatcher {
    #[serde(default)]
    pub class: Option<String>,
    #[serde(default)]
    pub exe: Option<String>,
    #[serde(default)]
    pub title: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Hotkey {
    pub modifiers: u32,
    pub vk: u32,
    pub display: String,
}

pub struct Target {
    pub name: String,
    pub hotkey: Hotkey,
    pub matcher: Matcher,
    pub launch: Option<String>,
    pub always_launch_if_no_window: bool,
    pub last_hwnd: Option<isize>,
    pub action: Action,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Action {
    /// Find matching window and bring to foreground; launch if none.
    Activate,
    /// Minimize the current foreground window; ignores `match` / `launch`.
    MinimizeForeground,
}

pub struct Matcher {
    pub class: Option<String>,
    pub exe: Option<String>,
    pub title: Option<TitlePattern>,
}

/// Minimal title matcher covering the two constructs the config actually uses:
///   - Optional leading `(?i)` for case-insensitive match.
///   - `|`-separated alternatives, any of which may match as a substring.
///
/// Anchors, character classes, and escapes are not supported — use
/// `--list-windows` and pick literal fragments if you need something exotic.
#[derive(Debug, Clone)]
pub struct TitlePattern {
    case_insensitive: bool,
    alternatives: Vec<String>,
}

impl TitlePattern {
    fn new(src: &str) -> Self {
        let (case_insensitive, rest) = if let Some(rest) = src.strip_prefix("(?i)") {
            (true, rest)
        } else {
            (false, src)
        };
        let alternatives = rest
            .split('|')
            .map(|s| {
                if case_insensitive {
                    s.to_ascii_lowercase()
                } else {
                    s.to_string()
                }
            })
            .collect();
        TitlePattern {
            case_insensitive,
            alternatives,
        }
    }

    pub fn is_match(&self, haystack: &str) -> bool {
        if self.case_insensitive {
            let lower = haystack.to_ascii_lowercase();
            self.alternatives.iter().any(|a| lower.contains(a))
        } else {
            self.alternatives.iter().any(|a| haystack.contains(a))
        }
    }
}

impl Matcher {
    pub fn matches(&self, class: &str, exe: &str, title: &str) -> bool {
        if let Some(c) = &self.class {
            if class != c {
                return false;
            }
        }
        if let Some(e) = &self.exe {
            if !exe.eq_ignore_ascii_case(e) {
                return false;
            }
        }
        if let Some(r) = &self.title {
            if !r.is_match(title) {
                return false;
            }
        }
        true
    }

    pub fn is_empty(&self) -> bool {
        self.class.is_none() && self.exe.is_none() && self.title.is_none()
    }
}

pub struct Config {
    pub targets: Vec<Target>,
}

pub fn parse_hotkey(s: &str) -> Result<Hotkey, String> {
    let mut modifiers: u32 = 0;
    let mut vk: Option<u32> = None;
    for raw_part in s.split('+') {
        let part = raw_part.trim();
        if part.is_empty() {
            return Err(format!("empty token in hotkey \"{}\"", s));
        }
        let lower = part.to_ascii_lowercase();
        match lower.as_str() {
            "ctrl" | "control" => modifiers |= MOD_CONTROL,
            "alt" => modifiers |= MOD_ALT,
            "shift" => modifiers |= MOD_SHIFT,
            "win" | "super" | "meta" => modifiers |= MOD_WIN,
            other => {
                if vk.is_some() {
                    return Err(format!(
                        "hotkey \"{}\" has more than one non-modifier key",
                        s
                    ));
                }
                vk = Some(vk_for(other).ok_or_else(|| {
                    format!("unrecognized key \"{}\" in hotkey \"{}\"", part, s)
                })?);
            }
        }
    }
    let vk = vk.ok_or_else(|| format!("hotkey \"{}\" has no non-modifier key", s))?;
    if modifiers == 0 {
        return Err(format!(
            "hotkey \"{}\" has no modifier — refuse to register a bare key",
            s
        ));
    }
    Ok(Hotkey {
        modifiers: modifiers | MOD_NOREPEAT,
        vk,
        display: s.to_string(),
    })
}

fn vk_for(key: &str) -> Option<u32> {
    let k = key.to_ascii_uppercase();
    if k.len() == 1 {
        let c = k.chars().next().unwrap();
        if c.is_ascii_alphabetic() {
            return Some(c as u32); // 'A'..='Z' → 0x41..=0x5A
        }
        if c.is_ascii_digit() {
            return Some(c as u32); // '0'..='9' → 0x30..=0x39
        }
    }
    if let Some(rest) = k.strip_prefix('F') {
        if let Ok(n) = rest.parse::<u32>() {
            if (1..=24).contains(&n) {
                return Some(0x70 + n - 1); // VK_F1 = 0x70
            }
        }
    }
    None
}

fn compile_matcher(raw: RawMatcher, _target_name: &str) -> Result<Matcher, String> {
    let title = raw.title.as_deref().map(TitlePattern::new);
    Ok(Matcher {
        class: raw.class,
        exe: raw.exe,
        title,
    })
}

pub fn parse_config(text: &str, _source_path: Option<PathBuf>) -> Result<Config, String> {
    let raw: RawConfig =
        basic_toml::from_str(text).map_err(|e| format!("TOML parse error: {}", e))?;
    let mut targets = Vec::with_capacity(raw.targets.len());
    let mut seen_names: Vec<String> = Vec::new();
    let mut seen_hotkeys: Vec<(u32, u32, String)> = Vec::new();
    for rt in raw.targets {
        if seen_names.iter().any(|n| n == &rt.name) {
            return Err(format!("duplicate target name: \"{}\"", rt.name));
        }
        seen_names.push(rt.name.clone());
        let hotkey = parse_hotkey(&rt.hotkey)
            .map_err(|e| format!("target \"{}\": {}", rt.name, e))?;
        if let Some(prev) = seen_hotkeys
            .iter()
            .find(|(m, k, _)| *m == hotkey.modifiers && *k == hotkey.vk)
        {
            return Err(format!(
                "hotkey \"{}\" is used by both \"{}\" and \"{}\"",
                hotkey.display, prev.2, rt.name
            ));
        }
        seen_hotkeys.push((hotkey.modifiers, hotkey.vk, rt.name.clone()));

        let matcher = compile_matcher(rt.matcher, &rt.name)?;
        let action = match rt.action.as_deref() {
            None | Some("activate") => Action::Activate,
            Some("minimize-foreground") => Action::MinimizeForeground,
            Some(other) => {
                return Err(format!(
                    "target \"{}\": unknown action \"{}\" (valid: \"activate\", \"minimize-foreground\")",
                    rt.name, other
                ));
            }
        };
        if action == Action::Activate && matcher.is_empty() && rt.launch.is_none() {
            return Err(format!(
                "target \"{}\": needs either a match predicate or a launch command",
                rt.name
            ));
        }
        targets.push(Target {
            name: rt.name,
            hotkey,
            matcher,
            launch: rt.launch,
            always_launch_if_no_window: rt.always_launch_if_no_window,
            last_hwnd: None,
            action,
        });
    }
    Ok(Config { targets })
}

/// Config search order:
///   1. --config PATH (if supplied)
///   2. %APPDATA%\eframe-jack-in\config.toml
///   3. <exe dir>\eframe-jack-in.toml
///   4. embedded default (written out to slot 2 on first run)
pub fn locate_and_load(cli_override: Option<PathBuf>) -> Result<(Config, ConfigOrigin), String> {
    if let Some(p) = cli_override {
        let text = fs::read_to_string(&p)
            .map_err(|e| format!("cannot read config \"{}\": {}", p.display(), e))?;
        let cfg = parse_config(&text, Some(p.clone()))?;
        return Ok((cfg, ConfigOrigin::Explicit(p)));
    }

    let appdata_path = appdata_config_path();
    if let Some(p) = &appdata_path {
        if p.is_file() {
            let text = fs::read_to_string(p)
                .map_err(|e| format!("cannot read config \"{}\": {}", p.display(), e))?;
            let cfg = parse_config(&text, Some(p.clone()))?;
            return Ok((cfg, ConfigOrigin::AppData(p.clone())));
        }
    }

    if let Some(p) = exe_dir_config_path() {
        if p.is_file() {
            let text = fs::read_to_string(&p)
                .map_err(|e| format!("cannot read config \"{}\": {}", p.display(), e))?;
            let cfg = parse_config(&text, Some(p.clone()))?;
            return Ok((cfg, ConfigOrigin::NextToExe(p)));
        }
    }

    // Nothing on disk. Materialise the embedded default under %APPDATA%.
    let cfg = parse_config(DEFAULT_CONFIG_STR, None)?;
    let written = if let Some(p) = appdata_path {
        if let Some(parent) = p.parent() {
            let _ = fs::create_dir_all(parent);
        }
        fs::write(&p, DEFAULT_CONFIG_STR).ok().map(|_| p)
    } else {
        None
    };
    Ok((cfg, ConfigOrigin::Default(written)))
}

pub enum ConfigOrigin {
    Explicit(PathBuf),
    AppData(PathBuf),
    NextToExe(PathBuf),
    /// Loaded from embedded default. If we managed to persist it, the path is
    /// included so we can point the user at it.
    Default(Option<PathBuf>),
}

fn appdata_config_path() -> Option<PathBuf> {
    std::env::var_os("APPDATA")
        .map(|v| Path::new(&v).join("eframe-jack-in").join("config.toml"))
}

fn exe_dir_config_path() -> Option<PathBuf> {
    let exe = std::env::current_exe().ok()?;
    Some(exe.parent()?.join("eframe-jack-in.toml"))
}
