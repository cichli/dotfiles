;; back-button
(require 'back-button)
(back-button-mode 1)

;; bookmark+
(require 'bookmark+)
(diminish 'back-button-mode)
(setq bmkp-bmenu-commands-file (concat user-emacs-directory "bookmarks/bmk-bmenu-commands.el")
      bmkp-bmenu-state-file (concat user-emacs-directory "bookmarks/bmk-bmenu-state.el")
      bookmark-default-file (concat user-emacs-directory "bookmarks/bookmarks")
      bookmark-save-flag t
      bookmark-version-control t)

;; browse-kill-ring
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; comint
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;; company-mode
(global-company-mode)
(setq company-echo-delay 0
      company-end-of-buffer-workaround nil
      company-idle-delay 0.1
      company-minimum-prefix-length 2
      company-show-numbers t
      company-tooltip-align-annotations t
      company-tooltip-limit 16)
(diminish 'company-mode)

;; diff-hl
(global-diff-hl-mode t)

;; dired
(setq dired-recursive-deletes 'top)

;; ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; eshell
(setq eshell-directory-name "~/.emacs.d/eshell/"
      eshell-scroll-show-maximum-output nil)
(add-hook 'eshell-mode-hook '(lambda ()
                               (company-mode -1)
                               (setq show-trailing-whitespace nil)))

;; fancy-narrow
(fancy-narrow-mode 1)
(diminish 'fancy-narrow-mode)

;; flyspell
(require 'flyspell)
(diminish 'flyspell-mode)

;; guide-key
(guide-key-mode 1)
(setq guide-key/guide-key-sequence '("C-c" "C-x" "C-z")
      guide-key/idle-delay 0.5
      guide-key/popup-window-position 'bottom
      guide-key/recursive-key-sequence-flag t)
(diminish 'guide-key-mode)

(add-hook 'popwin:after-popup-hook '(lambda ()
                                      (-when-let (buffer (get-buffer guide-key/guide-buffer-name))
                                        (with-current-buffer buffer
                                          (setq show-trailing-whitespace nil)))))

;; ibuffer
(require 'ibuffer)

;; ido
(setq ido-default-buffer-method 'selected-window
      ido-default-file-method 'selected-window
      ido-save-directory-list-file (concat user-emacs-directory ".ido.last"))

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)

;; magit
(require 'magit)
(setq magit-set-upstream-on-push t)
(diminish 'magit-auto-revert-mode)

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

;; org
(require 'org-drill)
(require 'ox-reveal)

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

;; paredit
(require 'paredit)
(diminish 'paredit-mode)

;; page-break-lines
(global-page-break-lines-mode 1)
(diminish 'page-break-lines-mode)

;; popwin
(popwin-mode t)

;; projectile
(projectile-global-mode 1)
(setq projectile-cache-file (concat user-emacs-directory "projectile/cache")
      projectile-known-projects-file (concat user-emacs-directory "projectile/bookmarks.eld"))
(diminish 'projectile-mode)

;; re-builder
(setq-default reb-re-syntax 'string)

;; smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))

;; swoop
(require 'swoop)
(setq swoop-font-size-change: nil)

;; undo-tree
(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)

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

;; which-function
(which-function-mode 1)

;; yasnippet
(yas-global-mode 1)
(diminish 'yas-minor-mode)

(setq yas-prompt-functions '(yas-ido-prompt))
