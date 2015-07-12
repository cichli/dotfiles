(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(setq solarized-high-contrast-mode-line t
      solarized-distinct-doc-face t
      solarized-scale-org-headlines nil
      solarized-use-more-italic t
      solarized-use-variable-pitch nil)
(load-theme 'solarized-dark t)

(set-face-attribute 'default nil :font "Inconsolata")
(set-frame-font "Inconsolata" nil t)

(setq-default indicate-empty-lines t
              show-trailing-whitespace t
              truncate-lines t)

(setq echo-keystrokes 0.1
      frame-title-format '(buffer-file-name "%f" ("%b"))
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      redisplay-dont-pause t
      split-height-threshold nil
      split-width-threshold 160)

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
(add-hook 'help-mode-hook #'hide-trailing-whitespace)

;; line-number-mode
(line-number-mode 1)

;; transient-mark-mode
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; winner-mode
(winner-mode 1)
