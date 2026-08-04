#![windows_subsystem = "windows"]

mod config;
mod tray;
mod util;
mod window;

use std::path::PathBuf;
use std::process::ExitCode;

use crate::config::{Config, ConfigOrigin};

const SINGLETON_NAME: &str = "Local\\eframe-jack-in-singleton";

const USAGE: &str = "\
eframe-jack-in — Windows tray helper that binds global hotkeys to app windows.

USAGE:
    eframe-jack-in.exe [OPTIONS]

OPTIONS:
    --config <PATH>       Use a specific config file (overrides search order).
    --list-windows        Print every top-level window with class/exe/title, exit.
    --check-config        Validate the config (parse, hotkeys, launch paths), exit.
    --log <PATH>          Append diagnostic events to PATH while running.
    -h, --help            Show this text and exit.

CONFIG SEARCH ORDER:
    1. Path from --config, if given.
    2. %APPDATA%\\eframe-jack-in\\config.toml
    3. <exe dir>\\eframe-jack-in.toml
    4. Embedded default (materialised into slot 2 on first run).
";

enum Mode {
    Run,
    ListWindows,
    CheckConfig,
    Help,
}

struct Args {
    mode: Mode,
    config: Option<PathBuf>,
    log: Option<PathBuf>,
}

fn main() -> ExitCode {
    let args = match parse_args(std::env::args().skip(1).collect()) {
        Ok(a) => a,
        Err(e) => {
            util::attach_parent_console();
            eprintln!("eframe-jack-in: {}", e);
            eprintln!("{}", USAGE);
            util::message_box_error("eframe-jack-in", &e);
            return ExitCode::from(2);
        }
    };

    if let Some(p) = &args.log {
        util::set_log_path(Some(p.clone()));
    }

    match args.mode {
        Mode::Help => {
            util::attach_parent_console();
            println!("{}", USAGE);
            ExitCode::SUCCESS
        }
        Mode::ListWindows => run_list_windows(),
        Mode::CheckConfig => run_check_config(args.config),
        Mode::Run => run_tray(args.config),
    }
}

fn parse_args(mut args: Vec<String>) -> Result<Args, String> {
    let mut mode = Mode::Run;
    let mut config: Option<PathBuf> = None;
    let mut log: Option<PathBuf> = None;
    while !args.is_empty() {
        let a = args.remove(0);
        match a.as_str() {
            "-h" | "--help" | "/?" => mode = Mode::Help,
            "--list-windows" => mode = Mode::ListWindows,
            "--check-config" => mode = Mode::CheckConfig,
            "--config" => {
                let v = args
                    .first()
                    .ok_or_else(|| "--config requires a path".to_string())?
                    .clone();
                args.remove(0);
                config = Some(PathBuf::from(v));
            }
            "--log" => {
                let v = args
                    .first()
                    .ok_or_else(|| "--log requires a path".to_string())?
                    .clone();
                args.remove(0);
                log = Some(PathBuf::from(v));
            }
            other => return Err(format!("unknown argument: {}", other)),
        }
    }
    Ok(Args { mode, config, log })
}

fn run_list_windows() -> ExitCode {
    util::attach_parent_console();
    let windows = window::enumerate_windows();
    let text = window::format_window_table(&windows);
    println!("{}", text);
    let out = std::env::temp_dir().join("eframe-jack-in-windows.txt");
    if let Err(e) = std::fs::write(&out, &text) {
        eprintln!("(also tried to write {}: {})", out.display(), e);
    } else {
        println!("\n(saved to {})", out.display());
    }
    ExitCode::SUCCESS
}

fn run_check_config(override_path: Option<PathBuf>) -> ExitCode {
    util::attach_parent_console();
    let loaded = config::locate_and_load(override_path);
    let (cfg, origin) = match loaded {
        Ok(x) => x,
        Err(e) => {
            eprintln!("config error: {}", e);
            return ExitCode::from(1);
        }
    };
    match &origin {
        ConfigOrigin::Explicit(p) => println!("config: {} (explicit)", p.display()),
        ConfigOrigin::AppData(p) => println!("config: {} (APPDATA)", p.display()),
        ConfigOrigin::NextToExe(p) => println!("config: {} (exe dir)", p.display()),
        ConfigOrigin::Default(Some(p)) => println!("config: {} (default, just materialised)", p.display()),
        ConfigOrigin::Default(None) => println!("config: <embedded default> (could not materialise)"),
    }
    let windows = window::enumerate_windows();
    let mut problems = 0;
    for t in &cfg.targets {
        if t.action != config::Action::Activate {
            // Actions other than "activate" don't consult match / launch.
            let action_name = match t.action {
                config::Action::MinimizeForeground => "minimize-foreground",
                config::Action::Activate => unreachable!(),
            };
            println!("  {:<24}  hotkey={}  action={}", t.name, t.hotkey.display, action_name);
            continue;
        }
        let hits = window::match_windows(&t.matcher, &windows);
        print!(
            "  {:<24}  hotkey={}  windows_found={}",
            t.name, t.hotkey.display, hits.len()
        );
        if let Some(cmd) = &t.launch {
            match window::check_launch(cmd) {
                None => print!("  launch=ok"),
                Some(err) => {
                    problems += 1;
                    print!("  launch=MISSING ({})", err);
                }
            }
        } else if hits.is_empty() {
            problems += 1;
            print!("  launch=(none) and no windows match!");
        }
        println!();
    }
    if problems > 0 {
        println!("\n{} problem(s) detected.", problems);
        ExitCode::from(1)
    } else {
        println!("\nOK.");
        ExitCode::SUCCESS
    }
}

fn run_tray(override_path: Option<PathBuf>) -> ExitCode {
    match util::acquire_singleton(SINGLETON_NAME) {
        Ok(Some(_guard)) => {}
        Ok(None) => {
            util::message_box_warning(
                "eframe-jack-in",
                "Another instance of eframe-jack-in is already running.",
            );
            return ExitCode::SUCCESS;
        }
        Err(e) => {
            util::message_box_error("eframe-jack-in", &format!("Singleton check failed: {}", e));
            return ExitCode::from(1);
        }
    }

    let (cfg, origin) = match config::locate_and_load(override_path.clone()) {
        Ok(x) => x,
        Err(e) => {
            util::message_box_error(
                "eframe-jack-in",
                &format!("Failed to load config:\n{}", e),
            );
            return ExitCode::from(1);
        }
    };
    if let ConfigOrigin::Default(Some(p)) = &origin {
        util::message_box_warning(
            "eframe-jack-in",
            &format!(
                "No config found. A default was written to:\n{}\n\nEdit it and use \
                 \"Reload config\" from the tray menu.",
                p.display()
            ),
        );
    }
    if cfg.targets.is_empty() {
        util::message_box_error(
            "eframe-jack-in",
            "Config contains no targets. Nothing to do.",
        );
        return ExitCode::from(1);
    }

    let cfg_effective: Config = cfg;
    match tray::run(cfg_effective, override_path) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            util::message_box_error("eframe-jack-in", &format!("Fatal: {}", e));
            ExitCode::from(1)
        }
    }
}
