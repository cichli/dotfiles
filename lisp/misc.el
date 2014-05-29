(setq gc-cons-threshold 67108864)

(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-charset-priority 'unicode)

(auto-compression-mode 1)

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      delete-by-moving-to-trash t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      recentf-max-saved-items 100
      recentf-save-file "~/.emacs.d/.recentf"
      version-control t)

(recentf-mode 1)

;; Yeah, about those TPS reports...
(let* ((l '(";; So I was sitting in my cubicle today, and I realized, ever since I started"
            ";; working, every single day of my life has been worse than the day before it."
            ";; So that means that every single day that you see me, that's on the worst day"
            ";; of my life."
            ";;"
            ";; What about today? Is today the worst day of your life?"
            ";;"
            ";; Yeah."
            ";;"
            ";; Wow, that's messed up."
            ""
            ""))
       (s (mapconcat 'identity l "\n")))
  (setq initial-scratch-message s))
