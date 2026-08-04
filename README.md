[![Unlicense Public domain](https://img.shields.io/badge/license-Public_domain-green.svg)](https://wiki.creativecommons.org/wiki/Public_domain)

# eframe-jack-in

Create or popup Emacs frame immediately. Handle 2 monitors case for 
windows & frames navigation.

**See also:** [emacs-anywhere](https://github.com/zachcurry/emacs-anywhere).

## Case: popup Emacs frame to the top of windows
```
+--------------------+      +--------------------+
|                    |      |.......Emacs.......x|
|                    |      |.                  .|
|      O     O       |      |.                  .|
|         |          |  =>  |. (current-buffer) .|
|      \_____/       |      |.                  .|
|                    |      |.                  .|
|                    |      |....................|
+--------------------+      +--------------------+
          ||                          ||
       --------                    --------
```

**Linux**

Install `wmctrl`, e.g.:

```bash
sudo apt-get install wmctrl
```

Add hotkey to `eframe-jack-in\linux\switch-to-emacsclient`.

**Windows**

The quickest fallback is a shortcut to
`eframe-jack-in\windows\switch-to-emacsclient.bat` with a hotkey attached
to it. Cold-start latency can reach several seconds, though.

For a faster, more capable path, run `eframe-jack-in.exe` (a small tray
program in `windows/eframe/`) — see `windows/eframe/README-usage` /
`TASK-eframe-jack-in.md` for the design. It:

- Binds one **global hotkey per target application**, not just Emacs. Chrome,
  Edge, Slack, muCommander, Windows Terminal — each gets its own hotkey. The
  default hotkey for Emacs is `Ctrl+Alt+E`.
- Uses `RegisterHotKey` so the hotkey combination is consumed by the system
  (no leaked keystrokes into the active app) and the tray process is
  allowed to change the foreground window.
- Finds and activates the target window itself (instead of delegating to
  `emacsclientw`), cycling through multiple hits on repeat press.

Configuration lives in `%APPDATA%\eframe-jack-in\config.toml` (materialised
on first run). Diagnostic flags:

```
eframe-jack-in.exe --list-windows   # dump HWND / class / exe / title table
eframe-jack-in.exe --check-config   # validate hotkeys + launch paths
eframe-jack-in.exe --config PATH    # use a specific config file
```

Build it with `cargo build --release` in `windows/eframe/`.

Note on elevation: if the target window belongs to an elevated process and
the tray program does not, Windows UIPI will block foreground changes and
`RegisterHotKey` won't fire over an elevated foreground. Run the tray
elevated only if you need to control elevated apps.

Elisp-side buffer filter (unchanged):

```lisp
(setq eframe-omit-buffers-patterns (list "*nrepl-messages"))
```

## Case: return to the previous application window from Emacs

Use `iconify-or-deiconify-frame` as always, e.g.:

```lisp
(global-set-key (kbd "M-z") 'iconify-or-deiconify-frame)
```

## Case: popup Emacs second frame on the second monitor

```
+--------------------+  +--------------------+    +--------------------+  +--------------------+
|........Emacs......x|  |                    |    |........Emacs......x|  |........Emacs......x|
|.                  .|  |                    |    |.                  .|  |.                  .|
|.                  .|  |      O     O       |    |.                  .|  |.                  .|
|. (current-buffer) .|  |         |          | => |. (current-buffer) .|  |. (current-buffer) .|
|.    of frame 1    .|  |      \_____/       |    |.    of frame 1    .|  |.    of frame 2    .|
|.                  .|  |                    |    |.                  .|  |.                  .|
|....................|  |                    |    |....................|  |....................|
+--------------------+  +--------------------+    +--------------------+  +--------------------+
         ||                      ||                         ||                      ||
      --------                --------                   --------                --------
```
