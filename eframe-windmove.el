;;; eframe-windmove.el --- Handle 2 monitors case for windows navigation.

(defun eframe-windmove-do-window-select (orig-fun &rest args)
  (let ((other-window (apply 'windmove-find-other-window args)))
    (if (and (null other-window)
             (> (length (frame-list)) 1))
        (let ((direction (car args))
              (f (selected-frame)))
          (if (member direction '(right left))
              (while (eq f (selected-frame))
                (cond ((equal direction 'right) (next-multiframe-window))
                      ((equal direction 'left) (previous-multiframe-window))))))
      (apply orig-fun args))))

(advice-add 'windmove-do-window-select
            :around #'eframe-windmove-do-window-select)

(provide 'eframe-windmove)
