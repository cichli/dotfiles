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
      user-full-name "Michael Griffiths"
      user-mail-address "mikey@cich.li"
      version-control t)

(recentf-mode 1)
