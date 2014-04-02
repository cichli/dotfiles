(setq-default fill-column 80
              indent-tabs-mode nil)

(setq kill-whole-line t
      require-final-newline t
      sentence-end-double-space nil
      shift-select-mode nil
      x-select-enable-clipboard t)

(dolist (x '(downcase-region
             erase-buffer
             narrow-to-region
             set-goal-column))
  (put x 'disabled nil))

(delete-selection-mode 1)
(electric-indent-mode 1)
(global-subword-mode 1)

(defadvice kill-ring-save (before slick-copy activate compile)
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
