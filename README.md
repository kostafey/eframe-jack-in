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

Actually, you can create a shortcut to
`eframe-jack-in\windows\switch-to-emacsclient.bat` and add hotkey to it,
but sometimes the response of this call can take many seconds.
To make it quick you shold run tiny `eframe-jack-in.exe` trayer program.

*chocolately note*

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
