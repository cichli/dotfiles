;;,-----------------------------------------------------------------------------
;;| init
;;`-----------------------------------------------------------------------------
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; (package-initialize)

(require 'cask)
(cask-initialize)

(eval-when-compile
  (require 'use-package))

(use-package benchmark-init
  :config
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

;;,-----------------------------------------------------------------------------
;;| built-ins
;;`-----------------------------------------------------------------------------
(setq-default fill-column 80
              indent-tabs-mode nil
              indicate-empty-lines t
              show-trailing-whitespace t
              truncate-lines t)

(dolist (x '(downcase-region
             erase-buffer
             narrow-to-region))
  (put x 'disabled nil))

(setq delete-by-moving-to-trash t
      echo-keystrokes 0.1
      frame-title-format '(buffer-file-name "%f" ("%b"))
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      scroll-conservatively 101
      sentence-end-double-space nil
      split-height-threshold nil
      split-width-threshold 160
      user-full-name "Michael Griffiths"
      user-mail-address "mikey@cich.li")

(defalias 'yes-or-no-p 'y-or-n-p)

;;,-----------------------------------------------------------------------------
;;| packages
;;`-----------------------------------------------------------------------------
(use-package ace-jump-buffer
  :bind
  (("C-c b" . ace-jump-buffer)
   ("C-c B" . ace-jump-buffer-other-window)
   ("C-c C-S-b" . ace-jump-buffer-in-one-window)))

(use-package ace-window
  :bind
  (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame)
  (set-face-attribute 'aw-leading-char-face nil :height 3.0))

(use-package auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (TeX-global-PDF-mode 1))

(use-package auth-source
  :defer t
  :config
  (setq auth-sources '("~/.authinfo.gpg")))

(use-package autodisass-java-bytecode)

(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  (diminish 'auto-revert-mode)
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t))

(use-package avy
  :bind
  (("C-c SPC" . avy-goto-char)
   ("C-c C-SPC" . avy-goto-word-or-subword-1)
   ("C-c g" . avy-goto-line)))

(use-package avy-zap
  :bind
  (("M-z" . avy-zap-to-char-dwim)
   ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package back-button
  :defer 1
  :config
  (back-button-mode 1)
  (diminish 'back-button-mode))

(use-package beacon
  :config
  (beacon-mode 1)
  (diminish 'beacon-mode))

(use-package bind-key)

(use-package browse-kill-ring
  :bind
  (("C-c C-b" . browse-kill-ring))
  :config
  (browse-kill-ring-default-keybindings))

(use-package buffer-move
  :bind
  (("C-' f" . buf-move-right)
   ("C-' b" . buf-move-left)
   ("C-' n" . buf-move-down)
   ("C-' p" . buf-move-up)))

(use-package cask
  :hook ((cask-mode . enable-paredit-mode)))

(use-package cider
  :defer t
  :hook ((cider-inspector-mode . hide-trailing-whitespace)
         (cider-mode . cider-company-enable-fuzzy-completion)
         (cider-repl-mode . cider-company-enable-fuzzy-completion)
         (cider-repl-mode . enable-paredit-mode)
         (cider-repl-mode . hide-trailing-whitespace))
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
  (set-face-attribute 'cider-deprecated-face nil :background nil :underline "light goldenrod"))

(use-package cider-scratch
  :defer t
  :config
  (unbind-key "C-j" cider-clojure-interaction-mode-map)
  (unbind-key "<remap> <paredit-newline>" cider-clojure-interaction-mode-map))

(use-package clj-refactor
  :defer t
  :init
  (defun enable-clj-refactor-mode ()
    (interactive)
    (clj-refactor-mode 1)
    (diminish 'clj-refactor-mode)
    (cljr-add-keybindings-with-prefix "C-c r"))
  :config
  (setq cljr-eagerly-build-asts-on-startup nil
        cljr-eagerly-cache-macro-occurrences-on-startup nil
        cljr-favor-prefix-notation nil
        cljr-magic-requires nil))

(use-package clojure-mode
  :defer t
  :hook ((clojure-mode . enable-clj-refactor-mode)
         (clojure-mode . enable-paredit-mode))
  :config
  (diminish-major 'clojure-mode "clj")
  (define-clojure-indent
    (for-all 1)
    (quick-check 1)
    (lazy-loop 1))
  (unbind-key "C-c SPC" clojure-mode-map))

(use-package coffee-mode
  :mode "\\.coffee\\'")

(use-package comint
  :defer t
  :config
  (ansi-color-for-comint-mode-on)
  (add-hook 'comint-output-filter-functions #'ansi-color-process-output)
  (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)
  (unbind-key "C-c SPC" comint-mode-map))

(use-package company
  :bind
  (("C-<tab>" . company-complete)
   :map company-active-map
   ("C-q" . company-show-doc-buffer))
  :config
  (global-company-mode 1)
  (diminish 'company-mode)
  (setq company-idle-delay nil
        company-minimum-prefix-length 0
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-tooltip-limit 16
        company-require-match nil))

(use-package company-auctex
  :after company
  :config
  (company-auctex-init))

(use-package compile
  :defer t
  :config
  (setq compile-command "gmake -k "))

(use-package conf-mode
  :defer t
  :config
  (unbind-key "C-c SPC" conf-mode-map))

(use-package crux
  :bind
  (("C-<return>" . crux-smart-open-line-above)
   ("S-<return>" . crux-smart-open-line)
   ("C-^" . crux-top-join-line)
   ("C-S-<backspace>" . crux-kill-whole-line)
   ("C-M-z" . crux-indent-defun)
   ("C-c <tab>" . crux-indent-rigidly-and-copy-to-clipboard)
   ("C-x C-u" . crux-upcase-region)
   ("C-x C-l" . crux-downcase-region)
   ("C-x M-c" . crux-capitalize-region)
   ("C-c n" . crux-cleanup-buffer-or-region)
   ("C-c k" . bury-buffer)
   ("C-c C-k" . crux-delete-file-and-buffer)
   ("C-c r" . crux-rename-file-and-buffer)
   ("C-c c" . crux-copy-file-preserve-attributes)
   ("C-c f" . crux-recentf-find-file)
   ("C-c I" . crux-find-user-init-file)
   ("C-c ," . crux-find-user-custom-file)
   ("C-c S" . crux-find-shell-init-file)
   ("C-c C-e" . crux-eval-and-replace)))

(use-package default-text-scale
  :config
  (default-text-scale-mode 1))

(use-package delsel
  :config
  (delete-selection-mode 1))

(use-package diff-hl
  :defer 1
  :config
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (global-diff-hl-mode 1))

(use-package diff-mode
  :defer t
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

(use-package diminish
  :defer t
  :init
  (defun diminish-major (mode alias)
    (add-hook (intern (concat (symbol-name mode) "-hook"))
              `(lambda () (setq mode-name ,alias)))))

(use-package dired
  :defer t
  :config
  (setq dired-recursive-deletes 'top))

(use-package dockerfile-mode
  :defer t)

(use-package ediff
  :defer t
  :config
  (setq ediff-diff-options "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eldoc
  :config
  (diminish 'eldoc-mode)
  (setq eldoc-idle-delay 0.1)
  (global-eldoc-mode 1))

(use-package electric
  :bind
  (("C-j" . newline-and-indent)
   ("C-m" . electric-indent-just-newline)))

(use-package elisp-slime-nav
  :defer t
  :config
  (diminish 'elisp-slime-nav-mode))

(use-package epa
  :defer t
  :config
  (setq epa-armor t))

(use-package expand-region
  :bind
  (("C-;" . er/expand-region)))

(use-package fancy-narrow
  :config
  (fancy-narrow-mode 1)
  (diminish 'fancy-narrow-mode))

(use-package files
  :config
  (setq backup-by-copying t
        backup-directory-alist '(("." . "~/.emacs.d/backups/"))
        delete-old-versions t
        directory-free-space-program "gdf"
        insert-directory-program "gls"
        make-backup-files t
        mode-require-final-newline 'visit-save
        require-final-newline 'visit-save
        version-control t))

(use-package flx-ido
  :config
  (flx-ido-mode 1))

(use-package flyspell
  :config
  (diminish 'flyspell-mode))

(use-package forge
  :after magit)

(use-package frame
  :config
  (set-frame-font "Fira Code Retina 12" t t)
  (blink-cursor-mode -1)
  (setq window-divider-default-right-width 1)
  (window-divider-mode 1))

(use-package fringe
  :config
  (fringe-mode '(4 . nil)))

(use-package git-timemachine
  :defer t)

(use-package grep
  :defer t
  :config
  (add-hook 'grep-mode-hook #'hide-trailing-whitespace))

(use-package help-mode
  :defer t
  :config
  (add-hook 'help-mode-hook #'hide-trailing-whitespace))

(use-package htmlize
  :defer t
  :config
  (setq htmlize-html-major-mode #'html-mode))

(use-package ibuffer
  :bind
  (("C-x C-b" . ibuffer)
   :map ibuffer-mode-map
   ("C-M-o" . ibuffer-visit-buffer-1-window))
  :config
  (setq ibuffer-default-sorting-mode 'alphabetic)
  (unbind-key "M-o" ibuffer-mode-map))

(use-package ido
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-default-buffer-method 'selected-window
        ido-default-file-method 'selected-window
        ido-save-directory-list-file (concat user-emacs-directory ".ido.last")))

(use-package ido-completing-read+
  :defer 1
  :config
  (ido-ubiquitous-mode 1)
  (push 'sql-connect ido-cr+-function-blacklist))

(use-package iflipb
  :bind
  (("M-[" . iflipb-previous-buffer)
   ("M-]" . iflipb-next-buffer))
  :config
  (setq iflipb-include-more-buffers t
        iflipb-wrap-around t))

(use-package imenu
  :defer t)

(use-package imenu-anywhere
  :bind
  (("C-c i" . ido-imenu-anywhere)))

(use-package isearch
  :bind
  (:map isearch-mode-map
        ("C-o" . isearch-occur)))

(use-package jka-compr
  :defer t
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
  :bind
  (:map js-mode-map
        ("{" . paredit-open-curly)
        ("}" . paredit-close-curly))
  :hook ((js-mode . enable-paredit-mode))
  :config
  (diminish-major 'js-mode "js"))

(use-package lisp-mode
  :hook ((emacs-lisp-mode . elisp-slime-nav-mode)
         (emacs-lisp-mode . enable-paredit-mode))
  :config
  (diminish-major 'emacs-lisp-mode "el")
  (setq initial-major-mode 'emacs-lisp-mode))

(use-package locate
  :defer t
  :config
  (setq locate-command "mdfind"))

(use-package mac-win
  :init
  (defun mac-toggle-frame-fullscreen ()
    (interactive)
    (let* ((frame (selected-frame))
           (param (unless (frame-parameter frame 'fullscreen)
                    'fullscreen)))
      (set-frame-parameter frame 'fullscreen param)))
  :bind
  (("M-ƒ" . mac-toggle-frame-fullscreen))
  :config
  (setq mac-mouse-wheel-smooth-scroll nil)
  (mac-auto-operator-composition-mode 1))

(use-package magit
  :bind
  (("C-x m" . magit-status)
   ("C-x C-m" . magit-file-popup)
   ("C-x M-m" . magit-dispatch-popup)
   :map magit-mode-map
   ("C-S-<tab>" . magit-section-cycle-diffs))
  :hook ((magit-popup-mode . hide-trailing-whitespace))
  :config
  (magit-auto-revert-mode 1)
  (diminish-major 'magit-mode nil)
  (diminish-major 'magit-popup-mode nil)
  (setq magit-completing-read-function 'magit-ido-completing-read
        magit-diff-refine-hunk t
        magit-fetch-arguments '("--prune")
        magit-log-arguments '("--color" "--decorate" "--graph" "-n1024")
        magit-merge-arguments '("--no-ff")
        magit-section-visibility-indicator nil
        magit-stash-arguments '("--include-untracked")
        magit-tag-arguments '("--annotate" "--sign"))
  (global-magit-file-mode -1)
  (magit-define-popup-switch 'magit-log-popup ?f
    "Follow only the first parent commit of merge commits"
    "--first-parent"))

(use-package magit-imerge
  :after magit)

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode 1))

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
  (setq org-enforce-todo-dependencies t))

(use-package ox-reveal
  :after org)

(use-package page-break-lines
  :config
  (global-page-break-lines-mode 1)
  (diminish 'page-break-lines-mode))

(use-package pallet
  :defer 1
  :config
  (pallet-mode 1))

(use-package paradox
  :defer t
  :hook ((paradox-menu-mode . hide-trailing-whitespace))
  :config
  (setq paradox-column-width-package 28
        paradox-column-width-version 14
        paradox-display-download-count t
        paradox-execute-asynchronously nil
        paradox-github-token t
        paradox-use-homepage-buttons nil))

(use-package paredit
  :defer t
  :config
  (diminish 'paredit-mode))

(use-package paren
  :config
  (show-paren-mode 1))

(use-package pcre2el
  :config
  (rxt-global-mode 1))

(use-package projectile
  :defer 1
  :config
  (projectile-global-mode 1)
  (diminish 'projectile-mode)
  (setq projectile-cache-file (concat user-emacs-directory "projectile/cache")
        projectile-known-projects-file (concat user-emacs-directory "projectile/bookmarks.eld")
        projectile-use-git-grep t))

(use-package re-builder
  :defer t
  :config
  (setq reb-re-syntax 'string))

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 100
        recentf-save-file "~/.emacs.d/.recentf"))

(use-package rg
  :bind
  (("C-c p s r" . rg-project)
   ("C-c r" . rg-dwim)
   ("C-c R" . rg))
  :config
  (setq rg-custom-type-aliases '()
        rg-command-line-flags '("--max-columns 160" "--smart-case" )
        rg-group-result t
        rg-show-columns t))

(use-package rotate
  :bind
  (("C-' l" . rotate-layout)
   ("C-' w" . rotate-window)
   ("C-' C-l" . rotate-layout)
   ("C-' C-w" . rotate-window)))

(use-package ruby-mode
  :mode ".Brewfile")

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package server
  :config
  (server-start))

(use-package simple
  :bind
  ("M-'" . just-one-space)
  :config
  (put #'set-goal-column 'disabled nil)
  (plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt)
  (setq shift-select-mode nil)
  (column-number-mode 1)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode))

(use-package smartrep
  :config
  (unbind-key "C-q")
  (smartrep-define-key global-map "C-q"
    '(("n" . (scroll-other-window 1))
      ("p" . (scroll-other-window -1))
      ("N" . 'scroll-other-window)
      ("P" . (scroll-other-window '-))
      ("a" . (beginning-of-buffer-other-window 0))
      ("e" . (end-of-buffer-other-window 0)))))

(use-package smex
  :bind
  (("M-x" . smex)
   ("C-c M-x" . smex-major-mode-commands)
   ("C-c C-c M-x" . execute-extended-command))
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))

(use-package solarized
  :config
  (setq solarized-distinct-doc-face t
        solarized-scale-org-headlines nil
        solarized-use-variable-pitch nil
        x-underline-at-descent-line t)

  (load-theme 'solarized-light t)

  ;; https://github.com/bbatsov/solarized-emacs/issues/220
  (solarized-with-color-variables 'light
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

(use-package sql
  :defer t
  :hook ((sql-interactive-mode . hide-trailing-whitespace))
  :config
  (setq sql-connection-alist '(("switch"
                                (sql-product 'postgres)
                                (sql-server "localhost")
                                (sql-port 5432)
                                (sql-database "switch"))))
  (sql-set-product 'postgres))

(use-package subword
  :config
  (global-subword-mode 1)
  (diminish 'subword-mode))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package tooltip
  :config
  (tooltip-mode -1))

(use-package undo-tree
  :demand t
  :hook ((undo-tree-visualizer-mode . hide-trailing-whitespace))
  :config
  (global-undo-tree-mode 1)
  (diminish 'undo-tree-mode)
  (diminish-major 'undo-tree-visualizer-mode nil)
  (setq undo-tree-visualizer-timestamps t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package urlenc
  :defer t)

(use-package vc-hooks
  :defer t
  :config
  (setq vc-make-backup-files t))

(use-package volatile-highlights
  :config
  (volatile-highlights-mode 1)
  (diminish 'volatile-highlights-mode)
  (set-face-inverse-video 'vhl/default-face t))

(use-package webjump
  :bind
  (("C-x j" . webjump))
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
                         [simple-query "emacswiki.org" "emacswiki.org/cgi-bin/wiki/" ""]))))

(use-package wgrep
  :defer t)

(use-package which-key
  :config
  (which-key-mode 1)
  (diminish 'which-key-mode))

(use-package whitespace
  :init
  (defun hide-trailing-whitespace ()
    (interactive)
    (setq show-trailing-whitespace nil))

  (defun show-trailing-whitespace ()
    (interactive)
    (setq show-trailing-whitespace t))
  :bind
  (("C-c w" . whitespace-mode)))

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode 1)
  (diminish 'whole-line-or-region-local-mode))

(use-package winner
  :demand t
  :bind
  (("C-c [" . winner-undo)
   ("C-c ]" . winner-redo))
  :config
  (winner-mode 1))

(use-package with-editor
  :defer t
  :config
  (diminish 'with-editor-mode))

(use-package xref
  :bind
  (("C-." . xref-find-references))
  :config
  (setq xref-prompt-for-identifier nil))
