;;; eframe-jack-in.el --- Get Emacs frame focus for Windows.  -*- lexical-binding: t; -*-

(require 'cl-lib)

;; Optional integration with hopper.el (see `eframe-kill-buffer').
(defvar hop-arrived-via-hop)
(declare-function hop-backward "hopper")

(defcustom eframe-omit-buffers-patterns (list)
  "List of buffer name patterns should be skiped.
Any time `eframe-next-buffer' or `eframe-previous-buffer' is called
you can skip some buffers.")

(defcustom eframe-touch-buffer-name "*touch*"
  "Buffer name with 'touch' file.")

(defun eframe-pop-emacs ()
  (interactive)
  (previous-multiframe-window))

(defun eframe-omit-buffer-p ()
  (or (equal (buffer-name) eframe-touch-buffer-name)
      (cl-some (lambda (pattern) (cl-search pattern (buffer-name)))
               eframe-omit-buffers-patterns)))

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
  "Kill current buffer.
If the buffer was entered via `hop-at-point' (see hopper.el), return to
the previous position with `hop-backward' after killing it instead of
switching to the previous buffer."
  (interactive)
  (setq eframe-force-switch t)
  (if (and (bound-and-true-p hop-arrived-via-hop)
           (fboundp 'hop-backward))
      (progn
        (kill-buffer (current-buffer))
        (hop-backward))
    (kill-buffer (current-buffer))
    (previous-buffer)
    (when (eframe-omit-buffer-p)
      (previous-buffer)))
  (setq eframe-force-switch nil))

(defun eframe-pop-buffer (mode)
  "Find first buffer with MODE major-mode and set focus or display it."
  (let ((result-buffer nil))
    (dolist (buff (buffer-list))
      (with-current-buffer buff
        (when (eq major-mode mode)
          (setq result-buffer buff))))
    (when result-buffer
      (let ((win (get-buffer-window result-buffer t)))
        (if win
            (progn
              (select-frame-set-input-focus (window-frame win))
              (set-frame-selected-window (window-frame win) win))
          (pop-to-buffer result-buffer))))
    result-buffer))

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

  (defvar eframe-mk t)

  (defun eframe-window-configuration-change ()
    (when (and (eframe-touch-buffer-p)
               (not eframe-force-switch))
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

  ;; No point and window-start restoring on the way back from the touch
  ;; buffer: `previous-buffer' brings both back from `window-prev-buffers'.
  (add-hook 'window-configuration-change-hook
            'eframe-window-configuration-change))

(provide 'eframe-jack-in)
