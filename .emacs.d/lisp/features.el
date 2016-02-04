;; ace-window
(require 'ace-window)
(setq aw-scope 'frame)
(set-face-attribute 'aw-leading-char-face nil :height 3.0)

;; auto-fill
(diminish 'auto-fill-function)

;; back-button
(require 'back-button)
(back-button-mode 1)

;; beacon
(beacon-mode 1)
(diminish 'beacon-mode)

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
(ansi-color-for-comint-mode-on)
(add-hook 'comint-output-filter-functions #'ansi-color-process-output)

(add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)

;; company-mode
(global-company-mode)
(setq company-idle-delay nil
      company-minimum-prefix-length 0
      company-selection-wrap-around t
      company-tooltip-align-annotations t
      company-tooltip-limit 16
      company-require-match nil)
(diminish 'company-mode)

;; diff-hl
(global-diff-hl-mode 1)
(setq diff-hl-draw-borders nil)

;; diff-mode
(add-hook 'diff-mode-hook '(lambda ()
                             (setq-local whitespace-style '(face
                                                            indentation
                                                            tabs tab-mark
                                                            spaces space-mark
                                                            newline newline-mark
                                                            space-before-tab space-after-tab))
                             (whitespace-mode 1)
                             (hide-trailing-whitespace)))

;; dired
(setq dired-recursive-deletes 'top)

;; ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; eldoc
(require 'eldoc)
(setq eldoc-idle-delay 0)

;; epa
(setq epa-armor t)

;; eshell
(setq eshell-directory-name "~/.emacs.d/eshell/"
      eshell-scroll-show-maximum-output nil)
(add-hook 'eshell-mode-hook #'hide-trailing-whitespace)

;; fancy-narrow
(fancy-narrow-mode 1)
(diminish 'fancy-narrow-mode)

;; flyspell
(require 'flyspell)
(diminish 'flyspell-mode)

;; github markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; grep
(add-hook 'grep-mode-hook #'hide-trailing-whitespace)

;; guide-key
(guide-key-mode 1)
(setq guide-key/guide-key-sequence '("C-c" "C-x" "C-z")
      guide-key/idle-delay 0.5
      guide-key/popup-window-position 'bottom
      guide-key/recursive-key-sequence-flag t)
(diminish 'guide-key-mode)

;; help+ & friends
(require 'help+)
(require 'help-fns+)
(require 'help-mode+)

;; ibuffer
(require 'ibuffer)

;; ido
(setq ido-default-buffer-method 'selected-window
      ido-default-file-method 'selected-window
      ido-save-directory-list-file (concat user-emacs-directory ".ido.last")
      ido-ubiquitous-enable-old-style-default nil)

(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)

(push '(disable exact "sql-connect") ido-ubiquitous-command-overrides)

;; iedit
(require 'iedit)

;; iflipb
(require 'iflipb)
(setq iflipb-include-more-buffers t
      iflipb-wrap-around t)

;; magit
(require 'magit)
(setq magit-branch-read-upstream-first t
      magit-completing-read-function 'magit-ido-completing-read
      magit-diff-arguments '("-C" "-M" "--no-ext-diff" "--stat")
      magit-display-buffer-function (lambda (buffer)
                                      (if magit-display-buffer-noselect
                                          (magit-display-buffer-traditional buffer)
                                        (progn
                                          (delete-other-windows)
                                          (set-window-dedicated-p nil nil)
                                          (set-window-buffer nil buffer)
                                          (get-buffer-window buffer))))
      magit-fetch-arguments '("--prune")
      magit-log-arguments '("--color" "--decorate" "--graph" "-n256" "--show-signature")
      magit-merge-arguments '("--no-ff")
      magit-rebase-arguments '("--autostash" "--preserve-merges")
      magit-revert-buffers 'silent
      magit-tag-arguments '("--annotate" "--sign"))

(diminish-major 'magit-mode-hook nil)
(diminish-major 'magit-popup-mode-hook nil)

(add-hook 'magit-popup-mode-hook #'hide-trailing-whitespace)

(magit-add-section-hook 'magit-status-sections-hook #'magit-insert-recent-commits nil t)

(magit-define-popup-switch 'magit-log-popup ?f
  "Follow only the first parent commit of merge commits"
  "--first-parent")

;; org
(require 'org-drill)
(require 'ox-reveal)

(setq org-cycle-level-faces nil
      org-enforce-todo-dependencies t
      org-replace-disputed-keys t
      org-return-follows-link t
      org-src-fontify-natively t
      org-startup-indented t)

(dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4))
  (set-face-attribute face nil :height 1))

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

;; page-break-lines
(global-page-break-lines-mode 1)
(diminish 'page-break-lines-mode)

;; paradox
(setq paradox-github-token t)
(add-hook 'paradox-menu-mode-hook #'hide-trailing-whitespace)

;; paredit
(require 'paredit)
(diminish 'paredit-mode)

;; popwin
(add-hook 'popwin:after-popup-hook #'hide-trailing-whitespace)

;; powerline
(setq-default mode-line-format
              '("%e" (:eval
                      (let* ((render (lambda (s)
                                       (when s
                                         (let ((s (replace-regexp-in-string "\s?," "" s)))
                                           (when (not (string= "" s))
                                             (concat (powerline-raw " [") (s-trim s) (powerline-raw "]")))))))
                             (lhs (list (-when-let (backend (and vc-mode buffer-file-name (vc-backend buffer-file-name)))
                                          (powerline-raw (format " [%s / %s] " backend (vc-workfile-version buffer-file-name backend))))
                                        (when (buffer-modified-p) (powerline-raw "[+] "))
                                        (when buffer-read-only (powerline-raw "[RO] "))))
                             (center (list (powerline-buffer-id)
                                           (funcall render (powerline-major-mode))
                                           (funcall render (powerline-process))
                                           (funcall render (powerline-minor-modes))))
                             (rhs (list (powerline-raw " %l,%c "))))
                        (concat (powerline-render lhs)
                                (powerline-fill-center nil (/ (powerline-width center) 2.0))
                                (powerline-render center)
                                (powerline-fill nil (powerline-width rhs))
                                (powerline-render rhs))))))

;; projectile
(projectile-global-mode 1)
(setq projectile-cache-file (concat user-emacs-directory "projectile/cache")
      projectile-known-projects-file (concat user-emacs-directory "projectile/bookmarks.eld")
      projectile-use-git-grep t)
(diminish 'projectile-mode)

;; re-builder
(setq-default reb-re-syntax 'string)

;; show-paren-mode
(show-paren-mode 1)

;; smartrep
(setq smartrep-mode-line-string-activated "[SMARTREP]")

;; smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))

;; sql
(setq sql-connection-alist '(("social-dev"
                              (sql-product 'postgres)
                              (sql-server "localhost")
                              (sql-port 5432)
                              (sql-database "social")
                              (sql-user "social"))

                             ("social-beta1"
                              (sql-product 'postgres)
                              (sql-server "localhost")
                              (sql-port 5437)
                              (sql-database "social")
                              (sql-user "social"))

                             ("social-beta2"
                              (sql-product 'postgres)
                              (sql-server "localhost")
                              (sql-port 5438)
                              (sql-database "social")
                              (sql-user "social"))

                             ("social-qa"
                              (sql-product 'postgres)
                              (sql-server "localhost")
                              (sql-port 5434)
                              (sql-database "social")
                              (sql-user "social"))
                             ("social-v1"
                              (sql-product 'postgres)
                              (sql-server "localhost")
                              (sql-port 5436)
                              (sql-database "social")
                              (sql-user "social")))
      sql-product 'postgres)

(add-hook 'sql-interactive-mode-hook #'hide-trailing-whitespace)

;; swoop
(require 'swoop)
(setq swoop-font-size-change: nil)

;; undo-tree
(global-undo-tree-mode 1)
(diminish 'undo-tree-mode)

(diminish-major 'undo-tree-visualizer-mode-hook nil)
(add-hook 'undo-tree-visualizer-mode-hook #'hide-trailing-whitespace)

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

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; visual-line
(diminish 'visual-line-mode)

;; volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode 1)
(set-face-inverse-video 'vhl/default-face t)
(diminish 'volatile-highlights-mode)

;; webjump
(setq webjump-sites '(("Google" .
                       [simple-query "google.com" "google.com/search?q=" ""])
                      ("Stack Overflow" .
                       [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" ""])
                      ("MDN" .
                       [simple-query "developer.mozilla.org" "developer.mozilla.org/en-US/search?q=" ""])
                      ("Clojars" .
                       [simple-query "clojars.org" "clojars.org/search?q=" ""])
                      ("Maven Central" .
                       [simple-query "search.maven.org" "search.maven.org/#search%7Cga%7C1%7C" ""])
                      ("Wikipedia" .
                       [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
                      ("Google Groups" .
                       [simple-query "groups.google.com" "groups.google.com/groups?q=" ""])
                      ("Emacs Wiki" .
                       [simple-query "emacswiki.org" "emacswiki.org/cgi-bin/wiki/" ""])))

;; wgrep
(require 'wgrep)
