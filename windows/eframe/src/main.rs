extern crate systray;

extern crate inputbot;
use inputbot::*;
use KeybdKey::*;

use std::process::Command;
use std::thread;

fn rase_emacs_frame() {
    let binpath = "C:/Emacs/bin";
    let touchpath = "~/.emacs.d/touch";

    Command::new(&format!("{}/emacsclientw.exe", binpath))
        .args(&["--no-wait",
                &format!("--alternate-editor={}/{}", binpath, "runemacs.exe"),
                touchpath])
        .output()
        .expect("failed to execute process");
}

#[cfg(target_os = "windows")]
fn main() {
    let mut app;
    match systray::Application::new() {
        Ok(w) => app = w,
        Err(_) => panic!("Can't create window!"),
    }
    app.set_icon_from_file(&"emacs.ico".to_string(),).ok();
    app.add_menu_item(&"Raise Emacs frame".to_string(), |_| {
        rase_emacs_frame();
    }).ok();
    app.add_menu_separator().ok();
    app.add_menu_item(&"Quit".to_string(), |window| {
        window.quit();
    }).ok();

    thread::spawn(move || {
        EKey.bind(|| {
            if LControlKey.is_pressed() && OtherKey(0x12).is_pressed() {
                rase_emacs_frame();
            }
        });
        handle_input_events();
    });

    app.wait_for_message();
}
