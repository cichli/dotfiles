(setq-default fill-column 80
              indent-tabs-mode nil)

(setq sentence-end-double-space nil
      shift-select-mode nil
      x-select-enable-clipboard t)

(dolist (x '(downcase-region
             erase-buffer
             narrow-to-region
             set-goal-column))
  (put x 'disabled nil))

(defadvice kill-region (before slick-cut activate compile)
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-ring-save (before slick-copy activate compile)
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; delete-selection-mode
(delete-selection-mode 1)

;; electric-indent-mode
(electric-indent-mode 1)

;; global-subword-mode
(global-subword-mode 1)
