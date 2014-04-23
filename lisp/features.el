;; back-button
(require 'back-button)
(back-button-mode 1)

;; bookmark+
(require 'bookmark+)
(diminish 'back-button-mode)
(setq bookmark-save-flag t
      bookmark-version-control t)

;; browse-kill-ring
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; comint
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;; company-mode
(global-company-mode)
(setq company-echo-delay 0
      company-idle-delay 0.1
      company-minimum-prefix-length 2
      company-show-numbers t
      company-tooltip-align-annotations t
      company-tooltip-limit 16)
(diminish 'company-mode " Κ")

;; dired
(setq dired-recursive-deletes 'top)

;; ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; eshell
(setq eshell-directory-name "~/.emacs.d/eshell/")
(add-hook 'eshell-mode-hook '(lambda ()
                               (company-mode -1)))

;; fancy-narrow
(fancy-narrow-mode 1)

;; git-gutter
(require 'git-gutter+)
(global-git-gutter+-mode 1)
(diminish 'git-gutter+-mode " Γ")

;; ibuffer
(require 'ibuffer)

;; ido
(setq ido-default-buffer-method 'selected-window
      ido-default-file-method 'selected-window
      ido-enable-flex-matching t
      ido-save-directory-list-file (concat user-emacs-directory ".ido.last"))

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet)

;; magit
(require 'magit)
(diminish 'magit-auto-revert-mode " Μ")

(setq magit-diff-refine-hunk t)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

;; paredit
(require 'paredit)
(diminish 'paredit-mode " Π")

;; org
(setq org-cycle-level-faces nil
      org-enforce-todo-dependencies t
      org-replace-disputed-keys t
      org-return-follows-link t
      org-src-fontify-natively t
      org-startup-indented t)

(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

;; re-builder
(setq-default reb-re-syntax 'string)

;; smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))

;; speedbar
(setq speedbar-directory-button-trim-method 'trim
      speedbar-directory-unshown-regexp "^$"
      speedbar-hide-button-brackets-flag t
      speedbar-indentation-width 2
      speedbar-show-unknown-files t
      speedbar-use-images nil
      sr-speedbar-right-side nil
      sr-speedbar-width 30
      sr-speedbar-width-x 30)

;; swoop
(require 'swoop)
(setq swoop-font-size-change: nil)

;; undo-tree
(global-undo-tree-mode 1)
(diminish 'undo-tree-mode " υ")

(dolist (fn '(undo-tree-undo undo-tree-redo))
  (defadvice fn (around keep-region activate)
    (if (use-region-p)
        (let ((m (set-marker (make-marker) (mark)))
              (p (set-marker (make-marker) (point))))
          ad-do-it
          (goto-char p)
          (set-mark m)
          (set-marker p nil)
          (set-marker m nil))
      ad-do-it)))

;; webjump
(setq webjump-sites '(("Google" .
                       [simple-query "google.com" "google.com/search?q=" ""])
                      ("Stack Overflow" .
                       [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" ""])
                      ("Wikipedia" .
                       [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
                      ("Google Groups" .
                       [simple-query "groups.google.com" "groups.google.com/groups?q=" ""])
                      ("Emacs Wiki" .
                       [simple-query "emacswiki.org" "emacswiki.org/cgi-bin/wiki/" ""])
                      ("Youtube" .
                       [simple-query "youtube.com" "youtube.com/results?search_query=" ""])))

;; yasnippet
(yas-global-mode 1)
(diminish 'yas-minor-mode " Υ")

(setq yas-prompt-functions '(yas-ido-prompt
                             yas-completing-prompt
                             yas-x-prompt
                             yas-dropdown-prompt
                             yas-no-prompt))
