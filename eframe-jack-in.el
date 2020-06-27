;;; eframe-jack-in.el --- Get Emacs frame focus for Windows.

(defcustom eframe-omit-buffers-patterns (list)
  "List of buffer name patterns should be skiped.
Any time `eframe-next-buffer' or `eframe-previous-buffer' is called
you can skip some buffers.")

(defcustom eframe-touch-buffer-name "*touch*"
  "Buffer name with 'touch' file.")

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

(when (eq system-type 'windows-nt)

  (defcustom eframe-touch-file "~/.emacs.d/touch"
    "Empty touch file path.")

  (defun eframe-find-touch-file ()
    (find-file eframe-touch-file)
    (rename-buffer eframe-touch-buffer-name))

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
    (when (and (string= (buffer-file-name)
                        (expand-file-name eframe-touch-file))
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

  (setq-local eframe-point 0)
  (setq-local eframe-window-start 0)

  (defun eframe-reset-point ()
    (setq eframe-point (point))
    (setq eframe-window-start (window-start)))

  (add-hook 'focus-out-hook
            'eframe-reset-point)

  (defadvice iconify-or-deiconify-frame
      (around eframe-iconify-or-deiconify-frame activate)
    (eframe-reset-point)
    ad-do-it)

  (defun eframe-buffer-list-update ()
    (when (and eframe-buffer-list-updated-p
               (string= (buffer-file-name (other-buffer))
                        (expand-file-name eframe-touch-file))
               (not eframe-force-switch)
               (not (> (length (eframe-icon-frame-list)) 1))
               (boundp 'eframe-point))
      (setq eframe-buffer-list-updated-p nil)
      (goto-char eframe-point)
      (set-window-start (selected-window) eframe-window-start)))

  (add-hook 'buffer-list-update-hook
            'eframe-buffer-list-update)

  (add-hook 'window-configuration-change-hook
            'eframe-window-configuration-change))

(defun eframe-pop-emacs ()
  (interactive)
  (previous-multiframe-window)
  (when (fboundp 'eframe-reset-point)
    (eframe-reset-point)))

(provide 'eframe-jack-in)
