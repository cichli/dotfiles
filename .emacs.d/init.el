(setq gc-cons-threshold 67108864)

;;,-----------------------------------------------------------------------------
;;| custom-file
;;`-----------------------------------------------------------------------------
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;;,-----------------------------------------------------------------------------
;;| packages
;;`-----------------------------------------------------------------------------
;; (package-initialize)

(require 'cask)
(cask-initialize)

(eval-when-compile
  (require 'use-package))

;;,-----------------------------------------------------------------------------
;;| editing
;;`-----------------------------------------------------------------------------
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun rename-current-buffer-file ()
  (interactive)
  (let* ((old-name (buffer-file-name))
         (_ (unless (and old-name (file-exists-p old-name))
              (error "Buffer '%s' is not visiting a file" (buffer-name))))
         (new-name (read-file-name "New name: " old-name)))
    (when (get-buffer new-name)
      (error "A buffer named '%s' already exists" new-name))
    (rename-file old-name new-name 1)
    (rename-buffer new-name)
    (set-visited-file-name new-name)
    (set-buffer-modified-p nil)
    (message "File '%s' successfully renamed to '%s'" old-name (file-name-nondirectory new-name))))

(defun delete-current-buffer-file ()
  (interactive)
  (let* ((old-name (buffer-file-name)))
    (unless (and old-name (file-exists-p old-name))
      (error "Buffer '%s' is not visiting a file" (buffer-name)))
    (when (yes-or-no-p "Are you sure you want to remove this file? ")
      (delete-file (buffer-file-name))
      (kill-buffer (current-buffer))
      (message "File '%s' successfully removed" old-name))))

(delete-selection-mode 1)
(global-subword-mode 1)

(diminish 'subword-mode)

(setq-default fill-column 80
              indent-tabs-mode nil)

(setq mode-require-final-newline 'visit-save
      require-final-newline 'visit-save
      select-enable-clipboard t
      sentence-end-double-space nil
      shift-select-mode nil)

(dolist (x '(downcase-region
             erase-buffer
             narrow-to-region
             set-goal-column))
  (put x 'disabled nil))

(bind-key "C-M-'" #'indent-buffer)
(bind-key "C-M-\"" #'indent-region)
(bind-key "M-'" #'just-one-space)

(bind-key "C-x C-r" #'rename-current-buffer-file)
(bind-key "C-x C-k" #'delete-current-buffer-file)

;;,-----------------------------------------------------------------------------
;;| locale
;;`-----------------------------------------------------------------------------

(set-charset-priority 'unicode)
(set-coding-system-priority 'utf-8)
(set-language-environment "UTF-8")

(setq locale-coding-system 'utf-8)

(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(prefer-coding-system 'utf-8)

;;,-----------------------------------------------------------------------------
;;| misc
;;`-----------------------------------------------------------------------------
(auto-compression-mode 1)

(setq directory-free-space-program "gdf"
      insert-directory-program "gls")

;;,-----------------------------------------------------------------------------
;;| backup / recentf
;;`-----------------------------------------------------------------------------
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      delete-by-moving-to-trash t
      delete-old-versions t
      make-backup-files t
      user-full-name "Michael Griffiths"
      user-mail-address "mikey@cich.li"
      vc-make-backup-files t
      version-control t)

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 100
        recentf-save-file "~/.emacs.d/.recentf"))

;;,-----------------------------------------------------------------------------
;;| ui
;;`-----------------------------------------------------------------------------
(defun diminish-major (mode alias)
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            `(lambda () (setq mode-name ,alias))))

(defun hide-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace nil))

(defun show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace t))

(blink-cursor-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(column-number-mode 1)
(global-font-lock-mode 1)
(line-number-mode 1)
(mac-auto-operator-composition-mode 1)
(winner-mode 1)

(setq-default cursor-in-non-selected-windows nil
              indicate-empty-lines t
              show-trailing-whitespace t
              truncate-lines t)

(setq echo-keystrokes 0.1
      frame-title-format '(buffer-file-name "%f" ("%b"))
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      mac-mouse-wheel-smooth-scroll nil
      mouse-wheel-follow-mouse t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(2 ((shift) . 8))
      scroll-conservatively 101
      split-height-threshold nil
      split-width-threshold 160)

(let ((font-name "Fira Code Retina 12"))
  (set-face-attribute 'default nil :font font-name)
  (set-frame-font font-name nil t))

(plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt)

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun mac-toggle-frame-fullscreen ()
  (interactive)
  (let* ((frame (selected-frame))
         (param (unless (frame-parameter frame 'fullscreen)
                  'fullscreen)))
    (set-frame-parameter frame 'fullscreen param)))

(bind-key "M-ƒ" #'mac-toggle-frame-fullscreen)

(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  (diminish 'auto-revert-mode)
  (setq auto-revert-use-notify t
        auto-revert-verbose nil
        global-auto-revert-non-file-buffers t))

(use-package solarized
  :config
  (setq solarized-distinct-doc-face t
        solarized-scale-org-headlines nil
        solarized-use-variable-pitch nil
        x-underline-at-descent-line t)

  (load-theme 'solarized-dark t)

  ;; https://github.com/bbatsov/solarized-emacs/issues/220
  (solarized-with-color-variables 'dark
    (set-face-attribute 'mode-line nil
                        :box nil
                        :overline s-line
                        :underline s-line)
    (set-face-attribute 'mode-line-inactive nil
                        :box nil
                        :overline s-line
                        :underline s-line)

    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=27830
    (set-face-attribute 'window-divider nil
                        :foreground s-line)))

(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;;,-----------------------------------------------------------------------------
;;| packages
;;`-----------------------------------------------------------------------------
(use-package abbrev
  :config
  (diminish 'abbrev-mode))

(use-package ace-jump-buffer
  :bind
  (("C-c b" . ace-jump-buffer)
   ("C-c B" . ace-jump-buffer-other-window)
   ("C-c C-S-b" . ace-jump-buffer-in-one-window)))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame)
  (set-face-attribute 'aw-leading-char-face nil :height 3.0)
  :bind
  (("M-o" . ace-window)))

(use-package advice
  :config
  (setq ad-redefinition-action 'accept))

(use-package auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (TeX-global-PDF-mode 1))

(use-package auth-source
  :config
  (setq auth-sources '("~/.authinfo.gpg")))

(use-package autodisass-java-bytecode)

(use-package avy
  :bind
  (("C-c SPC" . avy-goto-char)
   ("C-c C-SPC" . avy-goto-word-or-subword-1)
   ("C-c g" . avy-goto-line)
   ("C-c M-g" . avy-goto-line)))

(use-package avy-zap
  :bind
  (("M-z" . avy-zap-to-char-dwim)
   ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package back-button
  :defer 1
  :commands
  back-button-mode
  :config
  (back-button-mode 1)
  (diminish 'back-button-mode))

(use-package beacon
  :config
  (beacon-mode 1)
  (diminish 'beacon-mode))

(use-package bind-key)

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings)
  :bind
  (("C-c k" . browse-kill-ring)))

(use-package buffer-move
  :bind
  (("C-' f" . buf-move-right)
   ("C-' b" . buf-move-left)
   ("C-' n" . buf-move-down)
   ("C-' p" . buf-move-up)))

(use-package cask
  :config
  (add-hook 'cask-mode-hook #'enable-paredit-mode))

(use-package cider
  :defer t
  :config
  (diminish-major 'cider-repl-mode nil)
  (diminish-major 'cider-stacktrace-mode nil)
  (diminish-major 'nrepl-messages-mode nil)

  (setq cider-macroexpansion-print-metadata t
        cider-mode-line nil
        cider-pprint-fn 'puget
        cider-prompt-for-symbol nil
        cider-repl-display-help-banner nil
        cider-repl-history-file (concat user-emacs-directory ".cider-history")
        cider-repl-history-size 1000
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history t
        nrepl-log-messages t
        nrepl-message-buffer-max-size 100000000)

  ;; TODO https://github.com/bbatsov/solarized-emacs/issues/231
  (set-face-attribute 'cider-deprecated-face nil :background nil :underline "light goldenrod")

  (add-hook 'cider-inspector-mode-hook #'hide-trailing-whitespace)

  (add-hook 'cider-mode-hook #'enable-eldoc-mode)

  (add-hook 'cider-repl-mode-hook #'enable-eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'enable-clj-refactor-mode)
  (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook #'hide-trailing-whitespace))

(use-package cider-scratch
  :defer t
  :config
  (unbind-key "C-j" cider-clojure-interaction-mode-map)
  (unbind-key "<remap> <paredit-newline>" cider-clojure-interaction-mode-map))

(use-package clj-refactor
  :defer t
  :commands
  enable-clj-refactor-mode
  :config
  (setq cljr-eagerly-build-asts-on-startup nil
        cljr-eagerly-cache-macro-occurrences-on-startup nil
        cljr-favor-prefix-notation nil
        cljr-magic-requires nil)
  (defun enable-clj-refactor-mode ()
    (interactive)
    (clj-refactor-mode 1)
    (diminish 'clj-refactor-mode)
    (cljr-add-keybindings-with-prefix "C-c r")))

(use-package clojure-mode
  :config
  (diminish-major 'clojure-mode "clj")
  (add-hook 'clojure-mode-hook #'enable-clj-refactor-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (define-clojure-indent
    (for-all 1)
    (quick-check 1)
    (lazy-loop 1))
  (unbind-key "C-c SPC" clojure-mode-map))

(use-package coffee-mode
  :mode "\\.coffee\\'")

(use-package comint
  :config
  (ansi-color-for-comint-mode-on)
  (add-hook 'comint-output-filter-functions #'ansi-color-process-output)
  (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)
  (unbind-key "C-c SPC" comint-mode-map))

(use-package company
  :config
  (global-company-mode 1)
  (diminish 'company-mode)
  (setq company-idle-delay nil
        company-minimum-prefix-length 0
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-tooltip-limit 16
        company-require-match nil)
  (bind-key "C-q" #'company-show-doc-buffer company-active-map)
  :bind
  (("C-<tab>" . company-complete)))

(use-package company-auctex
  :after company
  :config
  (company-auctex-init))

(use-package compile
  :config
  (setq compile-command "gmake -k "))

(use-package conf-mode
  :config
  (unbind-key "C-c SPC" conf-mode-map))

(use-package default-text-scale
  :config
  (setq default-text-scale-mode 1))

(use-package diff-hl
  :config
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (global-diff-hl-mode 1))

(use-package diff-mode
  :config
  (add-hook 'diff-mode-hook '(lambda ()
                               (setq-local whitespace-style '(face
                                                              indentation
                                                              tabs tab-mark
                                                              spaces space-mark
                                                              newline newline-mark
                                                              space-before-tab space-after-tab))
                               (whitespace-mode 1)
                               (hide-trailing-whitespace)))
  (unbind-key "M-o" diff-mode-map))

(use-package diminish)

(use-package dired
  :config
  (setq dired-recursive-deletes 'top))

(use-package dockerfile-mode)

(use-package ediff
  :config
  (setq ediff-diff-options "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eldoc
  :commands
  enable-eldoc-mode
  :config
  (diminish 'eldoc-mode)
  (setq eldoc-idle-delay 0)

  (defun enable-eldoc-mode ()
    (interactive)
    (eldoc-mode 1)))

(use-package electric
  :bind
  (("C-j" . newline-and-indent)
   ("C-m" . electric-indent-just-newline)))

(use-package elisp-slime-nav
  :config
  (diminish 'elisp-slime-nav-mode))

(use-package emoji-cheat-sheet-plus
  :bind
  (("C-' <SPC>" . emoji-cheat-sheet-plus-insert)))

(use-package epa
  :config
  (setq epa-armor t))

(use-package expand-region
  :bind
  (("C-;" . er/expand-region)))

(use-package fancy-narrow
  :config
  (fancy-narrow-mode 1)
  (diminish 'fancy-narrow-mode))

(use-package flx-ido
  :config
  (flx-ido-mode 1))

(use-package flyspell
  :config
  (diminish 'flyspell-mode))

(use-package forge)

(use-package frame
  :config
  (setq window-divider-default-right-width 1)
  (window-divider-mode 1))

(use-package fringe
  :config
  (fringe-mode '(4 . nil)))

(use-package git-timemachine)

(use-package grep
  :config
  (add-hook 'grep-mode-hook #'hide-trailing-whitespace))

(use-package groovy-mode)

(use-package help-mode
  :config
  (add-hook 'help-mode-hook #'hide-trailing-whitespace))

(use-package ibuffer
  :config
  (setq ibuffer-default-sorting-mode 'alphabetic)
  (unbind-key "M-o" ibuffer-mode-map)
  (bind-key "C-M-o" #'ibuffer-visit-buffer-1-window ibuffer-mode-map)
  :bind
  (("C-x C-b" . ibuffer)))

(use-package ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-default-buffer-method 'selected-window
        ido-default-file-method 'selected-window
        ido-save-directory-list-file (concat user-emacs-directory ".ido.last")))

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1)
  (push 'sql-connect ido-cr+-function-blacklist))

(use-package iflipb
  :config
  (setq iflipb-include-more-buffers t
        iflipb-wrap-around t)
  :bind
  (("M-[" . iflipb-previous-buffer)
   ("M-]" . iflipb-next-buffer)))

(use-package imenu)

(use-package imenu-anywhere
  :bind
  (("C-c i" . ido-imenu-anywhere)))

(use-package isearch
  :config
  (bind-key "C-o" #'isearch-occur isearch-mode-map))

(use-package jka-compr
  :config
  (add-to-list 'jka-compr-compression-info-list
               ["\\.plist$"
                "converting text XML to binary plist"
                "plutil"
                ("-convert" "binary1" "-o" "-" "-")
                "converting binary plist to text XML"
                "plutil"
                ("-convert" "xml1" "-o" "-" "-")
                nil nil "bplist"])
  (jka-compr-update))

(use-package js
  :config
  (diminish-major 'js-mode "js")
  (add-hook 'js-mode-hook #'enable-paredit-mode)
  (bind-key "{" #'paredit-open-curly js-mode-map)
  (bind-key "}" #'paredit-close-curly js-mode-map))

(use-package lisp-mode
  :config
  (diminish-major 'emacs-lisp-mode "el")
  (setq initial-major-mode 'emacs-lisp-mode)
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package magit
  :config
  (magit-auto-revert-mode 1)
  (diminish-major 'magit-mode nil)
  (diminish-major 'magit-popup-mode nil)
  (setq magit-completing-read-function 'magit-ido-completing-read
        magit-diff-refine-hunk t
        magit-fetch-arguments '("--prune")
        magit-file-mode nil
        magit-log-arguments '("--color" "--decorate" "--graph" "-n1024")
        magit-merge-arguments '("--no-ff")
        magit-section-visibility-indicator nil
        magit-stash-arguments '("--include-untracked")
        magit-tag-arguments '("--annotate" "--sign"))
  (add-hook 'magit-popup-mode-hook #'hide-trailing-whitespace)
  (magit-define-popup-switch 'magit-log-popup ?f
    "Follow only the first parent commit of merge commits"
    "--first-parent")
  :bind
  (("C-x m" . magit-status)
   ("C-x C-m" . magit-file-popup)
   ("C-x M-m" . magit-dispatch-popup)))

(use-package magit-imerge)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package moody
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package org
  :defer t
  :config
  (setq org-enforce-todo-dependencies t
        org-src-fontify-natively t
        org-startup-indented nil))

(use-package ox-reveal
  :defer t)

(use-package page-break-lines
  :config
  (global-page-break-lines-mode 1)
  (diminish 'page-break-lines-mode))

(use-package pallet
  :config
  (pallet-mode 1))

(use-package paradox
  :defer t
  :config
  (setq paradox-column-width-package 28
        paradox-column-width-version 14
        paradox-display-download-count t
        paradox-execute-asynchronously nil
        paradox-github-token t
        paradox-use-homepage-buttons nil)
  (add-hook 'paradox-menu-mode-hook #'hide-trailing-whitespace))

(use-package paredit
  :config
  (diminish 'paredit-mode))

(use-package paren
  :config
  (show-paren-mode 1))

(use-package pcre2el)

(use-package popwin
  :config
  (add-hook 'popwin:after-popup-hook #'hide-trailing-whitespace))

(use-package projectile
  :config
  (projectile-global-mode 1)
  (diminish 'projectile-mode)
  (setq projectile-cache-file (concat user-emacs-directory "projectile/cache")
        projectile-known-projects-file (concat user-emacs-directory "projectile/bookmarks.eld")
        projectile-use-git-grep t))

(use-package rainbow-delimiters)

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

(use-package rebox
  :config
  (setq rebox-style-loop '(21 25 27))
  :bind
  (("C-M-;" . rebox-dwim)
   ("C-M-:" . rebox-cycle)))

(use-package rg
  :config
  (setq rg-custom-type-aliases '()
        rg-command-line-flags '("--max-columns 160" "--smart-case" )
        rg-group-result t
        rg-show-columns t)
  :bind
  (("C-c p s r" . rg-project)
   ("C-c r" . rg-dwim)
   ("C-c R" . rg)))

(use-package rotate
  :bind
  (("C-' l" . rotate-layout)
   ("C-' w" . rotate-window)
   ("C-' C-l" . rotate-layout)
   ("C-' C-w" . rotate-window)))

(use-package ruby-mode
  :mode ".Brewfile")

(use-package server
  :config
  (server-start))

(use-package simple
  :config
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode))

(use-package smartrep
  :config
  (setq smartrep-mode-line-string-activated "[SMARTREP]"))

(use-package smex
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  :bind
  (("M-x" . smex)
   ("C-c M-x" . smex-major-mode-commands)
   ("C-c C-c M-x" . execute-extended-command)))

(use-package sql
  :config
  (setq sql-connection-alist '(("switch"
                                (sql-product 'postgres)
                                (sql-server "localhost")
                                (sql-port 5432)
                                (sql-database "switch"))))
  (sql-set-product 'postgres)
  (add-hook 'sql-interactive-mode-hook #'hide-trailing-whitespace))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  (diminish 'undo-tree-mode)
  (diminish-major 'undo-tree-visualizer-mode nil)
  (setq undo-tree-visualizer-timestamps t)
  (add-hook 'undo-tree-visualizer-mode-hook #'hide-trailing-whitespace))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package urlenc)

(use-package volatile-highlights
  :config
  (volatile-highlights-mode 1)
  (diminish 'volatile-highlights-mode)
  (set-face-inverse-video 'vhl/default-face t))

(use-package webjump
  :config
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
                        ("NPM" .
                         [simple-query "npmjs.com" "npmjs.com/search?q=" ""])
                        ("Wikipedia" .
                         [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
                        ("Google Groups" .
                         [simple-query "groups.google.com" "groups.google.com/groups?q=" ""])
                        ("Emacs Wiki" .
                         [simple-query "emacswiki.org" "emacswiki.org/cgi-bin/wiki/" ""])))
  :bind
  (("C-x j" . webjump)))

(use-package wgrep)

(use-package which-key
  :config
  (which-key-mode 1)
  (diminish 'which-key-mode))

(use-package whitespace
  :bind
  (("C-c n" . whitespace-cleanup)
   ("C-c w" . whitespace-mode)))

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode 1)
  (diminish 'whole-line-or-region-local-mode))

(use-package winner
  :bind
  (("C-c [" . winner-undo)
   ("C-c ]" . winner-redo)))

(use-package with-editor
  :config
  (diminish 'with-editor-mode))

(use-package xref
  :config
  (setq xref-prompt-for-identifier nil)
  :bind
  (("C-." . xref-find-references)))
