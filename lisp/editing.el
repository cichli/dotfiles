(setq-default fill-column 80
              indent-tabs-mode nil
              sentence-end-double-space nil
              shift-select-mode nil
              undo-tree-mode t
              x-select-enable-clipboard t)

(dolist (x '(downcase-region
             erase-buffer
             narrow-to-region
             set-goal-column))
  (put x 'disabled nil))

(delete-selection-mode 1)
(electric-indent-mode 1)
(global-subword-mode 1)
