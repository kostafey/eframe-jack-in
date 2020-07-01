;;; eframe-jack-in.el --- Get Emacs frame focus for Windows.

(defcustom eframe-omit-buffers-patterns (list)
  "List of buffer name patterns should be skiped.
Any time `eframe-next-buffer' or `eframe-previous-buffer' is called
you can skip some buffers.")

(defcustom eframe-touch-buffer-name "*touch*"
  "Buffer name with 'touch' file.")

(defun eframe-pop-emacs ()
  (interactive)
  (previous-multiframe-window)
  (when (fboundp 'eframe-reset-point)
    (eframe-reset-point)))

(defun eframe-omit-buffer-p ()
  (or (equal (buffer-name) eframe-touch-buffer-name)
      (some 'identity
            (mapcar (lambda (pattern) (cl-search pattern (buffer-name)))
                    eframe-omit-buffers-patterns))))

(defvar eframe-force-switch nil)

(defun eframe-next-buffer ()
  (interactive)
  (setq eframe-force-switch t)
  (next-buffer)
  (setq eframe-force-switch nil)
  (if (eframe-omit-buffer-p)
      (next-buffer)))

(defun eframe-previous-buffer ()
  (interactive)
  (setq eframe-force-switch t)
  (previous-buffer)
  (setq eframe-force-switch nil)
  (if (eframe-omit-buffer-p)
      (previous-buffer)))

(defun eframe-kill-buffer ()
  "Kill current buffer."
  (interactive)
  (setq eframe-force-switch t)
  (kill-buffer (current-buffer))
  (previous-buffer)
  (when (eframe-omit-buffer-p)
    (previous-buffer))
  (setq eframe-force-switch nil))

(when (eq system-type 'windows-nt)

  (defcustom eframe-touch-file "~/.emacs.d/touch"
    "Empty touch file path.")

  (defun eframe-find-touch-file ()
    (find-file eframe-touch-file)
    (rename-buffer eframe-touch-buffer-name))

  (defun eframe-touch-buffer-p (&optional buffer)
    (string= (if buffer
                 (buffer-file-name buffer)
               (buffer-file-name))
             (expand-file-name eframe-touch-file)))

  (defun eframe-back-from-touch ()
    (setq eframe-force-switch t)
    (eframe-find-touch-file)
    (eframe-previous-buffer))

  (defun eframe-icon-frame-list ()
    (-filter (lambda (f) (eq (cdr (assq 'visibility (frame-parameters f))) 'icon))
             (frame-list)))

  (setq eframe-mk t)
  (setq eframe-buffer-list-updated-p t)

  (defun eframe-window-configuration-change ()
    (when (and (eframe-touch-buffer-p)
               (not eframe-force-switch))
      (setq eframe-buffer-list-updated-p t)
      (cond ((and (or (= (length (eframe-icon-frame-list))
                         (length (frame-list)))
                      (= (length (eframe-icon-frame-list))
                         0))
                  eframe-mk)
             (eframe-back-from-touch))
            ((> (length (eframe-icon-frame-list)) 0)
             (progn
               (eframe-back-from-touch)
               (setq eframe-mk nil)
               (previous-multiframe-window)
               (eframe-back-from-touch)
               (setq eframe-mk t))))))

  (defvar-local eframe-point (point))
  (defvar-local eframe-window-start (window-start))

  (defun eframe-reset-point ()
    (setq-local eframe-point (point))
    (setq-local eframe-window-start (window-start)))

  (defun eframe-load-point ()
    (goto-char eframe-point)
    (set-window-start (selected-window) eframe-window-start))

  (add-hook 'focus-out-hook
            'eframe-reset-point)

  (defadvice iconify-or-deiconify-frame
      (around eframe-iconify-or-deiconify-frame activate)
    (eframe-reset-point)
    ad-do-it)

  (defun eframe-buffer-list-update ()
    (when (and eframe-buffer-list-updated-p
               (not eframe-force-switch)
               (not (> (length (eframe-icon-frame-list)) 1)))
      (setq eframe-buffer-list-updated-p nil)
      (eframe-load-point)))

  (add-hook 'buffer-list-update-hook
            'eframe-buffer-list-update)

  (add-hook 'window-configuration-change-hook
            'eframe-window-configuration-change))

(provide 'eframe-jack-in)
