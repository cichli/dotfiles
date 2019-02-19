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
  :hook   ((after-init . benchmark-init/deactivate)))

;;,-----------------------------------------------------------------------------
;;| built-ins
;;`-----------------------------------------------------------------------------
(setq-default fill-column 80
              indent-tabs-mode nil
              indicate-empty-lines t
              truncate-lines t)

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

(dolist (x '(downcase-region erase-buffer narrow-to-region))
  (put x 'disabled nil))

(defalias 'yes-or-no-p 'y-or-n-p)

;;,-----------------------------------------------------------------------------
;;| packages
;;`-----------------------------------------------------------------------------
(use-package ace-window
  :bind   (("M-o" . ace-window)))

(use-package amx)

(use-package autodisass-java-bytecode)

(use-package autorevert
  :config (progn
            (global-auto-revert-mode +1)
            (setq auto-revert-verbose nil
                  global-auto-revert-non-file-buffers t)))

(use-package avy
  :bind   (("C-' a" . avy-goto-char)
           ("C-' s" . avy-goto-char-2)
           ("C-' d" . avy-goto-char-timer)
           ("C-' f" . avy-goto-word-or-subword-1)
           ("C-' g" . avy-goto-line)
           ("C-' r" . avy-resume)))

(use-package avy-zap
  :bind   (("M-z" . avy-zap-to-char-dwim)
           ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package back-button
  :defer  1
  :config (back-button-mode +1))

(use-package beacon
  :config (progn
            (setq beacon-blink-when-focused t)
            (beacon-mode +1)))

(use-package browse-kill-ring
  :bind   (("C-c C-M-k" . browse-kill-ring))
  :config (browse-kill-ring-default-keybindings))

(use-package cask
  :hook   ((cask-mode . enable-paredit-mode)))

(use-package cider
  :hook   ((cider-mode . cider-company-enable-fuzzy-completion)
           (cider-repl-mode . (lambda ()
                                (setq-local beacon-blink-when-window-scrolls nil)))
           (cider-repl-mode . cider-company-enable-fuzzy-completion)
           (cider-repl-mode . enable-paredit-mode))
  :config (setq cider-print-fn 'fipp
                cider-prompt-for-symbol nil
                cider-repl-display-help-banner nil
                cider-repl-history-file (concat user-emacs-directory ".cider-history")
                cider-repl-history-size 1000
                cider-repl-pop-to-buffer-on-connect nil
                cider-repl-wrap-history t))

(use-package cider-scratch
  :after  cider
  :config (progn
            (unbind-key "C-j" cider-clojure-interaction-mode-map)
            (unbind-key "<remap> <paredit-newline>" cider-clojure-interaction-mode-map)))

(use-package clojure-mode
  :bind   (("C-c l" . (lambda ()
                        (interactive)
                        (find-file-other-window "~/.lein/profiles.clj"))))
  :hook   ((clojure-mode . enable-paredit-mode))
  :config (progn
            (setq clojure-toplevel-inside-comment-form t)
            (define-clojure-indent
              (quick-check 1))))

(use-package comint
  :config (progn
            (ansi-color-for-comint-mode-on)
            (add-hook 'comint-output-filter-functions #'ansi-color-process-output)
            (add-hook 'comint-output-filter-functions #'comint-strip-ctrl-m)))

(use-package company
  :bind   (("C-<tab>" . company-complete)
           :map
           company-active-map
           ("C-q" . company-show-doc-buffer))
  :config (progn
            (global-company-mode +1)
            (setq company-idle-delay nil
                  company-minimum-prefix-length 0
                  company-selection-wrap-around t
                  company-tooltip-align-annotations t
                  company-tooltip-limit 16
                  company-require-match nil)))

(use-package company-auctex
  :after  (company tex)
  :config (company-auctex-init))

(use-package company-flx
  :after  (company flx)
  :config (company-flx-mode +1))

(use-package compile
  :config (setq compile-command "gmake -k "))

(use-package counsel
  :init   (unbind-key "C-\\")
  :bind   (("C-\\ f" . counsel-find-library)
           ("C-\\ g" . counsel-git-grep)
           ("C-\\ G" . counsel-git-log)
           ("C-\\ j" . counsel-bookmark)
           ("C-\\ l" . counsel-locate)
           ("C-\\ r" . counsel-rg)
           ("C-\\ u" . counsel-unicode-char)
           ("C-h f"  . counsel-describe-function)
           ("C-h v"  . counsel-describe-variable))
  :config (progn
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
            (counsel-mode +1)))

(use-package counsel-projectile
  :after  (counsel projectile)
  :config (counsel-projectile-mode +1))

(use-package counsel-tramp
  :bind   (("C-\\ s" . counsel-tramp))
  :hook   ((counsel-tramp-pre-command . (lambda ()
                                          (projectile-mode -1)))
           (counsel-tramp-quit-hook . (lambda ()
                                        (projectile-mode +1)))))

(use-package crux
  :bind   (("C-<return>"      . crux-smart-open-line-above)
           ("S-<return>"      . crux-smart-open-line)
           ("C-^"             . crux-top-join-line)
           ("C-S-<backspace>" . crux-kill-whole-line)
           ("C-c <tab>"       . crux-indent-rigidly-and-copy-to-clipboard)
           ("C-x C-u"         . crux-upcase-region)
           ("C-x C-l"         . crux-downcase-region)
           ("C-x M-c"         . crux-capitalize-region)
           ("C-c k"           . bury-buffer)
           ("C-c M-k"         . crux-delete-file-and-buffer)
           ("C-c r"           . crux-rename-file-and-buffer)
           ("C-c c"           . crux-copy-file-preserve-attributes)
           ("C-c f"           . crux-recentf-find-file)
           ("C-c I"           . crux-find-user-init-file)
           ("C-c ,"           . crux-find-user-custom-file)
           ("C-c S"           . crux-find-shell-init-file)
           ("C-c C-e"         . crux-eval-and-replace)))

(use-package default-text-scale
  :config (default-text-scale-mode +1))

(use-package delsel
  :config (delete-selection-mode 1))

(use-package diff-hl
  :hook   ((magit-post-refresh . diff-hl-magit-post-refresh))
  :config (global-diff-hl-mode +1))

(use-package diff-mode
  :config (unbind-key "M-o" diff-mode-map))

(use-package dired
  :config (setq dired-recursive-deletes 'top))

(use-package dockerfile-mode)

(use-package ediff
  :config (setq ediff-diff-options "-w"
                ediff-split-window-function 'split-window-horizontally
                ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eldoc
  :config (progn
            (setq eldoc-idle-delay 0.1)
            (global-eldoc-mode +1)))

(use-package elisp-slime-nav)

(use-package epa
  :config (setq epa-armor t))

(use-package expand-region
  :bind   (("C-;" . er/expand-region)))

(use-package eyebrowse
  :config (progn
            (setq eyebrowse-mode-line-separator " "
                  eyebrowse-wrap-around t)
            (eyebrowse-mode +1)))

(use-package fancy-narrow
  :config (fancy-narrow-mode +1))

(use-package files
  :config (setq backup-by-copying t
                backup-directory-alist '(("." . "~/.emacs.d/backups/"))
                delete-old-versions t
                directory-free-space-program "gdf"
                insert-directory-program "gls"
                make-backup-files t
                mode-require-final-newline 'visit-save
                require-final-newline 'visit-save
                version-control t))

(use-package flx)

(use-package forge
  :after  magit)

(use-package frame
  :config (progn
            (set-frame-font "Menlo 12" t t)
            (blink-cursor-mode -1)
            (setq window-divider-default-right-width 1)
            (window-divider-mode +1)))

(use-package fringe
  :config (fringe-mode '(4 . nil)))

(use-package git-timemachine)

(use-package gitattributes-mode)

(use-package gitconfig-mode)

(use-package gitignore-mode
  :mode   ".gitignore_global")

(use-package helpful
  :bind   (("C-h C" . helpful-command)
           ("C-h F" . helpful-function)
           ("C-h k" . helpful-key)))

(use-package htmlize
  :config (setq htmlize-html-major-mode #'html-mode))

(use-package ibuffer
  :bind   (("C-x C-b" . ibuffer)
           :map
           ibuffer-mode-map
           ("C-M-o" . ibuffer-visit-buffer-1-window))
  :config (progn
            (setq ibuffer-default-sorting-mode 'alphabetic)
            (unbind-key "M-o" ibuffer-mode-map)))

(use-package imenu-anywhere
  :bind   (("C-\\ i" . ivy-imenu-anywhere)))

(use-package ivy
  :demand t
  :bind   (("C-c C-r" . ivy-resume))
  :config (progn
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
            (ivy-mode +1)))

(use-package ivy-hydra
  :after  ivy
  :bind   (:map
           ivy-minibuffer-map
           ("M-O" . ivy-dispatching-done-hydra))
  :config (unbind-key "M-o" ivy-minibuffer-map))

(use-package ivy-rich
  :after  ivy
  :config (ivy-rich-mode +1))

(use-package ivy-xref
  :after  (ivy xref)
  :config (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package jka-compr
  :config (progn
            (add-to-list 'jka-compr-compression-info-list
                         ["\\.plist$"
                          "converting text XML to binary plist"
                          "plutil"
                          ("-convert" "binary1" "-o" "-" "-")
                          "converting binary plist to text XML"
                          "plutil"
                          ("-convert" "xml1" "-o" "-" "-")
                          nil nil "bplist"])
            (jka-compr-update)))

(use-package lisp-mode
  :hook   ((emacs-lisp-mode . elisp-slime-nav-mode)
           (emacs-lisp-mode . enable-paredit-mode))
  :config (setq initial-major-mode 'emacs-lisp-mode))

(use-package mac-win
  :init   (defun mac-toggle-frame-fullscreen ()
            (interactive)
            (let* ((frame (selected-frame))
                   (param (unless (frame-parameter frame 'fullscreen)
                            'fullscreen)))
              (set-frame-parameter frame 'fullscreen param)))
  :bind   (("M-ƒ" . mac-toggle-frame-fullscreen))
  :config (progn
            (setq mac-mouse-wheel-smooth-scroll nil)
            (mac-auto-operator-composition-mode +1)))

(use-package magit
  :bind   (("C-c m"   . (lambda ()
                          (interactive)
                          (magit-status "~/.dotfiles")))
           ("C-x m"   . magit-status)
           ("C-x C-m" . magit-file-dispatch)
           :map
           magit-mode-map
           ("C-S-<tab>" . magit-section-cycle-diffs))
  :config (progn
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
            (magit-auto-revert-mode +1)))

(use-package magit-imerge
  :after  magit)

(use-package markdown-mode
  :mode   (("\\.md\\'" . gfm-mode)))

(use-package minions
  :config (progn
            (setq minions-mode-line-lighter "...")
            (minions-mode +1)))

(use-package mode-line-bell
  :config (mode-line-bell-mode +1))

(use-package moody
  :config (progn
            (moody-replace-mode-line-buffer-identification)
            (moody-replace-vc-mode)))

(use-package org
  :config (setq org-enforce-todo-dependencies t))

(use-package page-break-lines
  :config (global-page-break-lines-mode +1))

(use-package paradox
  :config (setq paradox-column-width-package 28
                paradox-column-width-version 14
                paradox-display-download-count t
                paradox-execute-asynchronously t
                paradox-github-token t))

(use-package paren
  :config (progn
            (setq show-paren-when-point-in-periphery t
                  show-paren-when-point-inside-paren t)
            (show-paren-mode +1)))

(use-package pcre2el
  :config (rxt-global-mode +1))

(use-package prog-mode
  :hook   ((prog-mode . (lambda ()
                          (setq show-trailing-whitespace t)))))

(use-package projectile
  :demand t
  :bind   (:map
           projectile-mode-map
           ("C-c p" . projectile-command-map))
  :config (progn
            (projectile-mode +1)
            (setq projectile-use-git-grep t)))

(use-package rainbow-mode
  :config (rainbow-mode +1))

(use-package re-builder
  :config (setq reb-re-syntax 'string))

(use-package recentf
  :config (progn
            (recentf-mode +1)
            (setq recentf-max-saved-items 100
                  recentf-save-file "~/.emacs.d/.recentf")))

(use-package rotate
  :after  smartrep
  :config (smartrep-define-key global-map "C-'"
            '(("l" . rotate-layout)
              ("w" . rotate-window))))

(use-package ruby-mode
  :mode   ".Brewfile")

(use-package scroll-bar
  :config (scroll-bar-mode -1))

(use-package server
  :config (server-start))

(use-package simple
  :bind   ("M-'" . just-one-space)
  :hook   ((eval-expression-minibuffer-setup . enable-paredit-mode))
  :config (progn
            (put 'set-goal-column 'disabled nil)
            (plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt)
            (setq shift-select-mode nil)
            (column-number-mode +1)))

(use-package smartrep
  :config (progn
            (unbind-key "C-q")
            (smartrep-define-key global-map "C-q"
              '(("n" . (scroll-other-window 1))
                ("p" . (scroll-other-window -1))
                ("N" . scroll-other-window)
                ("P" . (scroll-other-window '-))
                ("a" . (beginning-of-buffer-other-window 0))
                ("e" . (end-of-buffer-other-window 0))))))

(use-package solarized
  :config (progn
            (setq solarized-distinct-doc-face t
                  solarized-scale-org-headlines nil
                  solarized-use-variable-pitch nil
                  x-underline-at-descent-line t)
            (load-theme 'solarized-dark t)
            (solarized-with-color-variables 'dark
              ;; https://github.com/bbatsov/solarized-emacs/issues/220
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
                                  :foreground s-line))))

(use-package subword
  :config (global-subword-mode +1))

(use-package swiper
  :bind   (("C-s"   . counsel-grep-or-swiper)
           ("C-S-s" . swiper-all))
  :config (progn
            (unbind-key "C-'" swiper-map)
            (bind-key "C-' '" #'swiper-avy swiper-map)
            (setq swiper-action-recenter t
                  swiper-goto-start-of-match t)))

(use-package tex
  :mode   (("\\.tex\\'" . TeX-latex-mode))
  :config (progn
            (setq font-latex-fontify-sectioning 'color)
            (TeX-global-PDF-mode +1)))

(use-package text-mode
  :hook   ((text-mode . (lambda ()
                          (setq show-trailing-whitespace t)))))

(use-package tool-bar
  :config (tool-bar-mode -1))

(use-package tooltip
  :config (tooltip-mode -1))

(use-package tramp
  :config (progn
            (setq tramp-default-method "ssh")
            (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))))

(use-package undo-tree
  :config (progn
            (global-undo-tree-mode +1)
            (setq undo-tree-visualizer-timestamps t)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package urlenc)

(use-package vc-hooks
  :config (setq vc-make-backup-files t
                vc-follow-symlinks t))

(use-package volatile-highlights
  :config (progn
            (volatile-highlights-mode +1)
            (set-face-inverse-video 'vhl/default-face t)))

(use-package webjump
  :bind   (("C-\\ w" . webjump))
  :config (setq webjump-sites '(("Clojars"           . [simple-query "clojars.org" "clojars.org/search?q=" ""])
                                ("Emacs Wiki"        . [simple-query "emacswiki.org" "www.emacswiki.org/cgi-bin/wiki.pl?search=" "&dosearch=1"])
                                ("GitHub"            . [simple-query "github.com" "github.com/search?q=" ""])
                                ("Google"            . [simple-query "google.com" "google.com/search?q=" ""])
                                ("Google Groups"     . [simple-query "groups.google.com" "groups.google.com/groups?q=" ""])
                                ("Maven Central"     . [simple-query "search.maven.org" "search.maven.org/#search%7Cga%7C1%7C" ""])
                                ("MDN"               . [simple-query "developer.mozilla.org" "developer.mozilla.org/en-US/search?q=" ""])
                                ("NPM"               . [simple-query "npmjs.com" "npmjs.com/search?q=" ""])
                                ("Oracle"            . [simple-query "docs.oracle.com" "docs.oracle.com/apps/search/search.jsp?q=" ""])
                                ("Oracle JDK11"      . [simple-query "docs.oracle.com/en/java/javase/11/" "docs.oracle.com/apps/search/search.jsp?category=java&product=en/java/javase/11&q=" ""])
                                ("Oxford Dictionary" . [simple-query "oxforddictionaries.com/definition/" "en.oxforddictionaries.com/search?utf8=✓&filter=dictionary&query=" ""])
                                ("Oxford Thesaurus"  . [simple-query "oxforddictionaries.com/thesaurus/" "en.oxforddictionaries.com/search?utf8=✓&filter=thesaurus&query=" ""])
                                ("Stack Overflow"    . [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" ""])
                                ("Wikipedia"         . [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""]))))

(use-package wgrep)

(use-package which-key
  :config (which-key-mode +1))

(use-package whitespace
  :bind   (("C-c w" . global-whitespace-mode)
           ("C-c n" . whitespace-cleanup)))

(use-package winner
  :after  smartrep
  :config (progn
            (smartrep-define-key global-map "C-'"
              '(("[" . winner-undo)
                ("]" . winner-redo)))
            (winner-mode +1)))

(use-package with-editor
  :config (shell-command-with-editor-mode +1))

(use-package xref
  :bind   (("C-." . xref-find-references))
  :config (setq xref-prompt-for-identifier nil))

(use-package yaml-mode)
