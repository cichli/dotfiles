(dolist (m '(menu-bar-mode
             scroll-bar-mode
             tool-bar-mode
             tooltip-mode))
  (funcall m -1))

(load-theme 'solarized-dark t)
(mac-toggle-fullscreen)

(set-face-attribute 'mode-line nil :underline nil)
(set-face-attribute 'mode-line-inactive nil :underline nil)

(setq-default indicate-empty-lines t
              show-trailing-whitespace t
              truncate-lines t)

(setq echo-keystrokes 0.1
      frame-title-format '(buffer-file-name "%f" ("%b"))
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      redisplay-dont-pause t
      visible-bell t)

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

(plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt)

(defalias 'yes-or-no-p 'y-or-n-p)

;; auto-revert-mode
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil
      global-auto-revert-non-file-buffers t)

;; blink-cursor-mode
(blink-cursor-mode -1)

;; column-number-mode
(column-number-mode 1)

;; font-lock-mode
(global-font-lock-mode 1)

;; fringe-mode
(fringe-mode '(1 . nil))

;; help-mode
(add-hook 'help-mode-hook 'hide-trailing-whitespace)

;; hl-line-mode
(global-hl-line-mode 1)

;; line-number-mode
(line-number-mode 1)

;; transient-mark-mode
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; winner-mode
(winner-mode 1)
