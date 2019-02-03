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
  :demand t
  :hook ((after-init . benchmark-init/deactivate)))

;;,-----------------------------------------------------------------------------
;;| built-ins
;;`-----------------------------------------------------------------------------
(setq-default fill-column 80
              indent-tabs-mode nil
              indicate-empty-lines t
              truncate-lines t)

(dolist (x '(downcase-region
             erase-buffer
             narrow-to-region))
  (put x 'disabled nil))

(setq create-lockfiles nil
      delete-by-moving-to-trash t
      echo-keystrokes 0.01
      frame-title-format '(:eval (if (buffer-file-name)
                                     (abbreviate-file-name (buffer-file-name))
                                   "%b"))
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
(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package amx
  :defer t)

(use-package auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (TeX-global-PDF-mode 1))

(use-package auth-source
  :defer t)

(use-package autodisass-java-bytecode)

(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t))

(use-package avy
  :bind (("C-' a" . avy-goto-char)
         ("C-' s" . avy-goto-char-2)
         ("C-' d" . avy-goto-char-timer)
         ("C-' f" . avy-goto-word-or-subword-1)
         ("C-' g" . avy-goto-line)
         ("C-' r" . avy-resume)))

(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package back-button
  :defer 1
  :config
  (back-button-mode 1))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package bind-key)

(use-package browse-kill-ring
  :bind (("C-c C-M-k" . browse-kill-ring))
  :config
  (browse-kill-ring-default-keybindings))

(use-package cask
  :hook ((cask-mode . enable-paredit-mode)))

(use-package cider
  :bind (("C-c l" . (lambda ()
                      (interactive)
                      (find-file "~/.lein/profiles.clj"))))
  :hook ((cider-mode . cider-company-enable-fuzzy-completion)
         (cider-repl-mode . cider-company-enable-fuzzy-completion)
         (cider-repl-mode . enable-paredit-mode))
  :config
  (setq cider-pprint-fn 'puget
        cider-prompt-for-symbol nil
        cider-repl-display-help-banner nil
        cider-repl-history-file (concat user-emacs-directory ".cider-history")
        cider-repl-history-size 1000
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-wrap-history t
        nrepl-log-messages t))

(use-package cider-scratch
  :defer t
  :config
  (unbind-key "C-j" cider-clojure-interaction-mode-map)
  (unbind-key "<remap> <paredit-newline>" cider-clojure-interaction-mode-map))

(use-package clojure-mode
  :hook ((clojure-mode . enable-paredit-mode))
  :config
  (define-clojure-indent
    (quick-check 1)))

(use-package coffee-mode
  :mode "\\.coffee\\'")

(use-package comint
  :defer t
  :config
  (ansi-color-for-comint-mode-on)
  (add-hook 'comint-output-filter-functions #'ansi-color-process-output)
  (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m))

(use-package company
  :bind (("C-<tab>" . company-complete)
         :map company-active-map
         ("C-q" . company-show-doc-buffer))
  :config
  (global-company-mode 1)
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

(use-package company-flx
  :after company
  :config
  (company-flx-mode 1))

(use-package compile
  :defer t
  :config
  (setq compile-command "gmake -k "))

(use-package counsel
  :init
  (unbind-key "C-\\")
  :bind (("C-\\ f" . counsel-find-library)
         ("C-\\ g" . counsel-git-grep)
         ("C-\\ G" . counsel-git-log)
         ("C-\\ j" . counsel-bookmark)
         ("C-\\ l" . counsel-locate)
         ("C-\\ r" . counsel-rg)
         ("C-\\ u" . counsel-unicode-char)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable))
  :config
  (unbind-key "C-'" counsel-ag-map)
  (unbind-key "C-'" counsel-grep-map)
  (bind-key "C-' '" #'swiper-avy counsel-ag-map)
  (bind-key "C-' '" #'swiper-avy counsel-grep-map)
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable
        counsel-locate-cmd #'counsel-locate-cmd-mdfind
        counsel-grep-post-action-hook '(recenter)
        counsel-rg-base-command (string-join '("rg"
                                               "--color never"
                                               "--hidden"
                                               "--line-number"
                                               "--max-columns 160"
                                               "--no-heading"
                                               "--smart-case"
                                               "%s .")
                                             " "))
  (counsel-mode 1))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

(use-package counsel-tramp
  :bind (("C-\\ s" . counsel-tramp))
  :hook ((counsel-tramp-pre-command . (lambda ()
                                        (projectile-mode -1)))
         (counsel-tramp-quit-hook . (lambda ()
                                      (projectile-mode +1)))))

(use-package crux
  :bind (("C-<return>" . crux-smart-open-line-above)
         ("S-<return>" . crux-smart-open-line)
         ("C-^" . crux-top-join-line)
         ("C-S-<backspace>" . crux-kill-whole-line)
         ("C-c <tab>" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-x C-u" . crux-upcase-region)
         ("C-x C-l" . crux-downcase-region)
         ("C-x M-c" . crux-capitalize-region)
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
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1))

(use-package diff-mode
  :defer t
  :config
  (unbind-key "M-o" diff-mode-map))

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
  (setq eldoc-idle-delay 0.1)
  (global-eldoc-mode 1))

(use-package electric
  :bind (("C-j" . newline-and-indent)
         ("C-m" . electric-indent-just-newline)))

(use-package elisp-slime-nav
  :defer t)

(use-package epa
  :defer t
  :config
  (setq epa-armor t))

(use-package expand-region
  :bind (("C-;" . er/expand-region)))

(use-package eyebrowse
  :config
  (eyebrowse-mode 1))

(use-package fancy-narrow
  :config
  (fancy-narrow-mode 1))

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

(use-package flx
  :defer t)

(use-package forge
  :after magit)

(use-package frame
  :config
  (set-frame-font "Menlo 12" t t)
  (blink-cursor-mode -1)
  (setq window-divider-default-right-width 1)
  (window-divider-mode 1))

(use-package fringe
  :config
  (fringe-mode '(4 . nil)))

(use-package git-timemachine
  :defer t)

(use-package gitattributes-mode
  :defer t)

(use-package gitconfig-mode
  :defer t)

(use-package gitignore-mode
  :mode ".gitignore_global")

(use-package grep
  :defer t)

(use-package help-mode
  :defer t)

(use-package helpful
  :bind (("C-h C" . helpful-command)
         ("C-h F" . helpful-function)
         ("C-h k" . helpful-key)))

(use-package htmlize
  :defer t
  :config
  (setq htmlize-html-major-mode #'html-mode))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("C-M-o" . ibuffer-visit-buffer-1-window))
  :config
  (setq ibuffer-default-sorting-mode 'alphabetic)
  (unbind-key "M-o" ibuffer-mode-map))

(use-package iflipb
  :bind (("M-[" . iflipb-previous-buffer)
         ("M-]" . iflipb-next-buffer))
  :config
  (setq iflipb-include-more-buffers t
        iflipb-wrap-around t))

(use-package imenu
  :defer t)

(use-package imenu-anywhere
  :bind (("C-\\ i" . imenu-anywhere)))

(use-package isearch
  :bind (:map isearch-mode-map
              ("C-o" . isearch-occur)))

(use-package ivy
  :demand t
  :bind (("C-c C-r" . ivy-resume))
  :config
  (unbind-key "C-'" ivy-minibuffer-map)
  (bind-key "C-' '" #'ivy-avy ivy-minibuffer-map)
  (setq ivy-count-format "(%d/%d) "
        ivy-format-function 'ivy-format-function-arrow
        ivy-height 20
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((swiper . ivy--regex)
                                (counsel-ag . ivy--regex)
                                (t . ivy--regex-fuzzy))
        ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'abbrev
        ivy-wrap t)
  (ivy-mode 1))

(use-package ivy-hydra
  :after ivy
  :init
  (unbind-key "M-o" ivy-minibuffer-map)
  :bind (:map ivy-minibuffer-map
              ("M-O" . ivy-dispatching-done-hydra)))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package ivy-xref
  :after ivy
  :config
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

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
  :bind (:map js-mode-map
              ("{" . paredit-open-curly)
              ("}" . paredit-close-curly))
  :hook ((js-mode . enable-paredit-mode)))

(use-package lisp-mode
  :hook ((emacs-lisp-mode . elisp-slime-nav-mode)
         (emacs-lisp-mode . enable-paredit-mode))
  :config
  (setq initial-major-mode 'emacs-lisp-mode))

(use-package mac-win
  :init
  (defun mac-toggle-frame-fullscreen ()
    (interactive)
    (let* ((frame (selected-frame))
           (param (unless (frame-parameter frame 'fullscreen)
                    'fullscreen)))
      (set-frame-parameter frame 'fullscreen param)))
  :bind (("M-ƒ" . mac-toggle-frame-fullscreen))
  :config
  (setq mac-mouse-wheel-smooth-scroll nil)
  (mac-auto-operator-composition-mode 1))

(use-package magit
  :bind (("C-c m" . (lambda ()
                      (interactive)
                      (magit-status "~/.dotfiles")))
         ("C-x m" . magit-status)
         ("C-x C-m" . magit-file-popup)
         ("C-x M-m" . magit-dispatch-popup)
         :map magit-mode-map
         ("C-S-<tab>" . magit-section-cycle-diffs))
  :config
  (magit-auto-revert-mode 1)
  (setq magit-diff-refine-hunk t
        magit-log-arguments '("--color" "--decorate" "--graph" "-n1024")
        magit-merge-arguments '("--no-ff")
        magit-section-initial-visibility-alist '(((issue pullreq) . hide)
                                                 ([file * status] . hide)
                                                 ([* status] . show))
        magit-section-visibility-indicator '("..." . true)
        magit-stash-arguments '("--include-untracked")
        magit-tag-arguments '("--annotate" "--sign"))
  (global-magit-file-mode -1)
  (magit-define-popup-switch 'magit-log-popup ?f
    "Follow only the first parent commit of merge commits"
    "--first-parent"))

(use-package magit-imerge
  :after magit)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)))

(use-package minions
  :config
  (setq minions-mode-line-lighter "...")
  (minions-mode 1))

(use-package mode-line-bell
  :config
  (mode-line-bell-mode 1))

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
  (global-page-break-lines-mode 1))

(use-package paradox
  :defer t
  :config
  (setq paradox-column-width-package 28
        paradox-column-width-version 14
        paradox-display-download-count t
        paradox-execute-asynchronously t
        paradox-github-token t))

(use-package paredit
  :defer t)

(use-package paren
  :config
  (setq show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t)
  (show-paren-mode 1))

(use-package pcre2el
  :config
  (rxt-global-mode 1))

(use-package prog-mode
  :hook ((prog-mode . (lambda ()
                        (setq show-trailing-whitespace t)))))

(use-package projectile
  :defer 1
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (projectile-mode 1)
  (setq projectile-cache-file (concat user-emacs-directory "projectile/cache")
        projectile-known-projects-file (concat user-emacs-directory "projectile/bookmarks.eld")
        projectile-use-git-grep t))

(use-package rainbow-mode
  :config
  (rainbow-mode 1))

(use-package re-builder
  :defer t
  :config
  (setq reb-re-syntax 'string))

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 100
        recentf-save-file "~/.emacs.d/.recentf"))

(use-package rotate
  :bind (("C-' l" . rotate-layout)
         ("C-' w" . rotate-window)))

(use-package ruby-mode
  :mode ".Brewfile")

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package server
  :config
  (server-start))

(use-package simple
  :demand t
  :bind ("M-'" . just-one-space)
  :hook ((eval-expression-minibuffer-setup . enable-paredit-mode))
  :config
  (put #'set-goal-column 'disabled nil)
  (plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt)
  (setq shift-select-mode nil)
  (column-number-mode 1))

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

(use-package subword
  :config
  (global-subword-mode 1))

(use-package swiper
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-S-s" . swiper-all))
  :config
  (unbind-key "C-'" swiper-map)
  (bind-key "C-' '" #'swiper-avy swiper-map)
  (setq swiper-action-recenter t
        swiper-goto-start-of-match t))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package tooltip
  :config
  (tooltip-mode -1))

(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh")
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil)))

(use-package undo-tree
  :demand t
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-timestamps t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package urlenc
  :defer t)

(use-package vc-hooks
  :defer t
  :config
  (setq vc-make-backup-files t
        vc-follow-symlinks t))

(use-package volatile-highlights
  :config
  (volatile-highlights-mode 1)
  (set-face-inverse-video 'vhl/default-face t))

(use-package webjump
  :bind (("C-\\ w" . webjump))
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
  (which-key-mode 1))

(use-package whitespace
  :init
  :bind (("C-c w" . global-whitespace-mode)
         ("C-c n" . whitespace-cleanup)))

(use-package winner
  :demand t
  :bind (("C-c [" . winner-undo)
         ("C-c ]" . winner-redo))
  :config
  (winner-mode 1))

(use-package with-editor
  :defer t)

(use-package xref
  :bind (("C-." . xref-find-references))
  :config
  (setq xref-prompt-for-identifier nil))
