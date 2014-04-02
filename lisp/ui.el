(dolist (m '(menu-bar-mode
             scroll-bar-mode
             tool-bar-mode
             tooltip-mode))
  (funcall m -1))

(load-theme 'solarized-dark t)
(setq solarized-height-minus-1 1
      solarized-height-plus-1 1
      solarized-height-plus-2 1
      solarized-height-plus-3 1
      solarized-height-plus-4 1
      solarized-use-variable-pitch nil)

(set-face-attribute 'mode-line nil :underline nil)
(set-face-attribute 'mode-line-inactive nil :underline nil)

(mac-toggle-fullscreen)

(winner-mode 1)

(setq echo-keystrokes 0.1
      frame-title-format '(buffer-file-name "%f" ("%b"))
      redisplay-dont-pause t
      visible-bell t)

(setq inhibit-startup-echo-area-message t
      inhibit-startup-screen t)

(setq-default indicate-empty-lines t
              truncate-lines t)

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

(plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt)

(defalias 'yes-or-no-p 'y-or-n-p)

(column-number-mode 1)
(line-number-mode 1)

(fringe-mode '(1 . nil))

(setq display-time-format "%t%a %d %b %Y%t%H:%M%t"
      display-time-default-load-average nil)
(display-time-mode 1)
(display-battery-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(global-font-lock-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)

(blink-cursor-mode -1)

(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode 1)

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil
      global-auto-revert-non-file-buffers t)

(require 'rainbow-mode)
(diminish 'rainbow-mode)
(dolist (hook '(clojure-mode-hook
                css-mode-hook
                emacs-lisp-mode-hook
                html-mode-hook
                js-mode-hook
                org-mode-hook
                text-mode-hook))
  (add-hook hook 'rainbow-mode))

(require 'volatile-highlights)
(volatile-highlights-mode 1)
(set-face-inverse-video-p 'vhl/default-face t)
(diminish 'volatile-highlights-mode)
