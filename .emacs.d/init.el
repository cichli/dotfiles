#!/usr/bin/env emacs --script

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-find-executable "gfind"
      straight-use-package-by-default t)

(straight-use-package 'use-package)

(defmacro use-feature (name &rest args)
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

(use-package benchmark-init
  :demand t
  :hook   ((after-init . benchmark-init/deactivate)))



(use-package ace-link
  :bind*  (("C-' l" . ace-link))
  :config (ace-link-setup-default))

(use-package ace-window
  :config (setq aw-scope 'visible))

(use-package amx)

(use-package autodisass-java-bytecode)

(use-package autorevert
  :config (progn
            (setq auto-revert-avoid-polling t
                  auto-revert-interval 0.1
                  auto-revert-verbose nil
                  global-auto-revert-non-file-buffers t)
            (global-auto-revert-mode +1)))

(use-package avy
  :bind*  (("C-' a" . avy-goto-char)
           ("C-' s" . avy-goto-char-2)
           ("C-' d" . avy-goto-char-timer)
           ("C-' f" . avy-goto-word-or-subword-1)
           ("C-' g" . avy-goto-line)
           ("C-' h" . avy-resume)
           ("C-' j" . avy-goto-symbol-1)
           ("C-' k" . avy-goto-whitespace-end))
  :config (setq avy-style 'de-bruijn))

(use-package beacon
  :config (progn
            (setq beacon-blink-when-focused t)
            (beacon-mode +1)))

(use-package bookmark
  :config (setq bookmark-save-flag 1))

(use-package browse-kill-ring
  :bind   (("C-c C-M-k" . browse-kill-ring))
  :config (setq browse-kill-ring-highlight-current-entry t
                browse-kill-ring-highlight-inserted-item t))

(use-package cider
  :hook   ((cider-mode . cider-company-enable-fuzzy-completion)
           (cider-repl-mode . (lambda ()
                                (setq-local beacon-blink-when-window-scrolls nil)))
           (cider-repl-mode . cider-company-enable-fuzzy-completion)
           (cider-repl-mode . enable-paredit-mode))
  :config (setq cider-print-fn 'fipp
                cider-prompt-for-symbol nil
                cider-redirect-server-output-to-repl nil
                cider-repl-display-help-banner nil
                cider-repl-history-file (expand-file-name ".cider-history" user-emacs-directory)
                cider-repl-history-size 1000
                cider-repl-pop-to-buffer-on-connect nil
                cider-repl-wrap-history t
                nrepl-use-ssh-fallback-for-remote-hosts t))

(use-feature cider-scratch
  :config (setq cider-clojure-interaction-mode-map clojure-mode-map))

(use-package clojure-mode
  :hook   ((clojure-mode . enable-paredit-mode))
  :config (progn
            (setq clojure-toplevel-inside-comment-form t)
            (define-clojure-indent
              (quick-check 1))))

(use-feature comint
  :config (progn
            (add-hook 'comint-output-filter-functions #'ansi-color-process-output)
            (ansi-color-for-comint-mode-on)))

(use-package company
  :after  (counsel)
  :bind   (("C-\\ C-\\" . counsel-company))
  :config (progn
            (setq company-frontends '(company-preview-if-just-one-frontend))
            (global-company-mode +1)))

(use-package company-auctex
  :config (company-auctex-init))

(use-package company-terraform
  :config (company-terraform-init))

(use-package compile
  :config (setq compile-command "gmake -k "))

(use-package counsel
  :init   (unbind-key "C-\\")
  :bind   (("C-M-s"  . counsel-grep)
           ("C-\\ f" . counsel-find-library)
           ("C-\\ F" . counsel-faces)
           ("C-\\ g" . counsel-git-grep)
           ("C-\\ G" . counsel-git-log)
           ("C-\\ j" . counsel-bookmark)
           ("C-\\ l" . counsel-locate)
           ("C-\\ m" . counsel-mark-ring)
           ("C-\\ p" . counsel-package)
           ("C-\\ r" . counsel-rg)
           ("C-\\ u" . counsel-unicode-char))
  :config (progn
            (unbind-key "<remap> <pop-to-mark-command>" counsel-mode-map)
            (unbind-key "C-'" counsel-ag-map)
            (unbind-key "C-'" counsel-grep-map)
            (bind-key "C-' '" #'swiper-avy counsel-ag-map)
            (bind-key "C-' '" #'swiper-avy counsel-grep-map)
            (setq counsel-describe-function-function #'helpful-callable
                  counsel-describe-variable-function #'helpful-variable
                  counsel-locate-cmd #'counsel-locate-cmd-mdfind
                  counsel-mode-override-describe-bindings t
                  counsel-grep-post-action-hook '(recenter)
                  counsel-rg-base-command '("rg"
                                            "--color" "never"
                                            "--hidden"
                                            "--line-number"
                                            "--max-columns" "240"
                                            "--no-heading"
                                            "--smart-case"
                                            "--with-filename"
                                            "%s"))
            (ivy-configure 'counsel-yank-pop :height 10)
            (ivy-configure 'counsel-git-log :height 6)
            (counsel-mode +1)))

(use-package counsel-projectile
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
           ("C-c M-u"         . crux-upcase-region)
           ("C-c M-l"         . crux-downcase-region)
           ("C-c M-c"         . crux-capitalize-region)
           ("C-c k"           . bury-buffer)
           ("C-c C-k"         . crux-delete-file-and-buffer)
           ("C-c r"           . crux-rename-file-and-buffer)
           ("C-c c"           . crux-copy-file-preserve-attributes)
           ("C-c C-e"         . crux-eval-and-replace)))

(use-feature cus-edit
  :config (progn
            (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
            (load custom-file)))

(use-package default-text-scale
  :config (default-text-scale-mode +1))

(use-package delsel
  :config (delete-selection-mode +1))

(use-package diff-hl
  :after  magit
  :hook   ((magit-post-refresh . diff-hl-magit-post-refresh)
           (magit-pre-refresh . diff-hl-magit-pre-refresh))
  :config (global-diff-hl-mode +1))

(use-package dockerfile-mode)

(use-package ediff
  :config (setq ediff-diff-options "-w"
                ediff-patch-program "gpatch"
                ediff-split-window-function 'split-window-horizontally
                ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eldoc
  :config (progn
            (setq eldoc-idle-delay 0.1)
            (global-eldoc-mode +1)))

(use-package emacs
  :config (progn
            (setq-default fill-column 80
                          indent-tabs-mode nil
                          indicate-empty-lines t
                          truncate-lines t)
            (setq create-lockfiles nil
                  delete-by-moving-to-trash t
                  echo-keystrokes 0.01
                  enable-recursive-minibuffers t
                  frame-title-format '(:eval (if (buffer-file-name)
                                                 (abbreviate-file-name (buffer-file-name))
                                               "%b"))
                  inhibit-startup-echo-area-message t
                  inhibit-startup-screen t
                  mac-frame-tabbing nil
                  read-process-output-max (* 1024 1024)
                  scroll-conservatively 101
                  sentence-end-double-space nil
                  split-height-threshold nil
                  split-width-threshold 160)
            (dolist (x '(downcase-region erase-buffer narrow-to-region upcase-region))
              (put x 'disabled nil))
            (defalias 'yes-or-no-p 'y-or-n-p)))

(use-package epa
  :config (setq epa-armor t))

(use-package expand-region
  :bind   (("C-;" . er/expand-region)))

(use-package eyebrowse
  :init   (setq eyebrowse-keymap-prefix (kbd "C-' w"))
  :config (progn
            (setq eyebrowse-mode-line-separator " "
                  eyebrowse-wrap-around t)
            (eyebrowse-mode +1)))

(use-feature files
  :config (setq backup-by-copying t
                backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
                delete-old-versions t
                directory-free-space-program "gdf"
                insert-directory-program "gls"
                make-backup-files t
                require-final-newline t
                version-control t))

(use-package forge)

(use-feature frame
  :config (progn
            (set-frame-font "Menlo 14" t t)
            (blink-cursor-mode -1)
            (setq window-divider-default-right-width 1)
            (window-divider-mode +1)))

(use-feature fringe
  :config (fringe-mode '(4 . nil)))

(use-package git-commit
  :hook   ((git-commit-mode . (lambda ()
                                  (setq-local fill-column 80)))))

(use-package git-modes)

(use-package git-timemachine)

(use-package helpful
  :bind   (("C-c C-d" . helpful-at-point)
           ("C-h C"   . helpful-command)
           ("C-h F"   . helpful-function)
           ("C-h k"   . helpful-key)
           ("C-h o"   . helpful-symbol)))

(use-package htmlize
  :config (setq htmlize-html-major-mode #'html-mode))

(use-package ibuffer
  :bind   (("C-x C-b" . ibuffer))
  :config (setq ibuffer-default-sorting-mode 'alphabetic))

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
                  ivy-use-virtual-buffers t
                  ivy-virtual-abbreviate 'abbreviate
                  ivy-wrap t)
            (ivy-mode +1)))

(use-package ivy-hydra)

(use-package ivy-rich
  :config (ivy-rich-mode +1))

(use-package ivy-xref
  :config (setq ivy-xref-use-file-path t
                xref-show-xrefs-function #'ivy-xref-show-xrefs))

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

(use-feature lisp-mode
  :hook   ((emacs-lisp-mode . enable-paredit-mode))
  :config (setq initial-major-mode 'emacs-lisp-mode))

(use-feature mac-win
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
  :bind   (("C-x m"   . magit-status)
           ("C-x C-m" . magit-file-dispatch)
           :map magit-mode-map
           ("C-S-<tab>" . magit-section-cycle-diffs))
  :config (setq magit-diff-refine-hunk t
                magit-section-initial-visibility-alist '(((issue pullreq) . hide)
                                                         ([file * status] . hide)
                                                         ([* status] . show))
                magit-section-visibility-indicator '("..." . true)))

(use-package magit-imerge)

(use-package markdown-mode
  :mode   (("\\.md\\'" . gfm-mode)))

(use-package mb-depth
  :config (minibuffer-depth-indicate-mode +1))

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

(use-package page-break-lines
  :config (global-page-break-lines-mode +1))

(use-package paredit)

(use-package paren
  :config (show-paren-mode +1))

(use-feature prog-mode
  :hook   ((prog-mode . (lambda ()
                          (setq show-trailing-whitespace t)))))

(use-package projectile
  :config (progn
            (bind-key "C-c p" #'projectile-command-map projectile-mode-map)
            (setq projectile-completion-system 'ivy
                  projectile-use-git-grep t)
            (projectile-mode +1)))

(use-package rainbow-mode
  :config (rainbow-mode +1))

(use-package re-builder
  :config (setq reb-re-syntax 'string))

(use-package recentf
  :config (progn
            (setq recentf-max-saved-items 100
                  recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
            (recentf-mode +1)))

(use-package rotate)

(use-package ruby-mode
  :mode   ".Brewfile")

(use-feature scroll-bar
  :config (scroll-bar-mode -1))

(use-package server
  :config (server-start))

(use-feature simple
  :bind   ("M-'" . just-one-space)
  :hook   ((eval-expression-minibuffer-setup . enable-paredit-mode))
  :config (progn
            (setq shift-select-mode nil)
            (put 'set-goal-column 'disabled nil)
            (column-number-mode +1)))

(use-package smartrep
  :after  (ace-window rotate winner)
  :config (progn
            (smartrep-define-key override-global-map "C-' w"
              '(("o"   . ace-window)
                ("l"   . rotate-layout)
                ("w"   . rotate-window)
                ("C-_" . winner-undo)
                ("M-_" . winner-redo)))
            (smartrep-define-key override-global-map "C-' w s"
              '(("n" . (scroll-other-window +1))
                ("p" . (scroll-other-window -1))
                ("N" . scroll-other-window)
                ("P" . (scroll-other-window '-))
                ("a" . (beginning-of-buffer-other-window 0))
                ("e" . (end-of-buffer-other-window 0))))
            (smartrep-define-key override-global-map "C-c"
              '(("C-SPC" . pop-to-mark-command)))
            (smartrep-define-key override-global-map "C-x"
              '(("o"     . other-window)
                ("C-SPC" . pop-global-mark)
                ("]"     . forward-page)
                ("["     . backward-page)))))

(use-package solarized-theme
  :config (progn
            (setq solarized-distinct-doc-face t
                  solarized-use-variable-pitch nil
                  x-underline-at-descent-line t)
            (load-theme 'solarized-light t)
            (require 'solarized-palettes)
            (solarized-with-color-variables 'light 'solarized-light solarized-light-color-palette-alist
              '(;; https://github.com/bbatsov/solarized-emacs/issues/220
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
                                    :foreground s-line)))))

(use-package subword
  :config (global-subword-mode +1))

(use-package swiper
  :bind   (("C-s"   . swiper-isearch)
           ("C-r"   . swiper-isearch-backward)
           ("C-S-s" . swiper-all))
  :config (progn
            (unbind-key "C-'" swiper-map)
            (bind-key "C-' '" #'swiper-avy swiper-map)
            (setq swiper-action-recenter t
                  swiper-goto-start-of-match t)))

(use-package terraform-mode)

(use-feature tex
  :mode   (("\\.tex\\'" . TeX-latex-mode))
  :config (progn
            (setq font-latex-fontify-sectioning 'color)
            (TeX-global-PDF-mode +1)))

(use-feature text-mode
  :hook   ((text-mode . (lambda ()
                          (setq show-trailing-whitespace t)))
           (text-mode . auto-fill-mode)))

(use-feature tool-bar
  :config (tool-bar-mode -1))

(use-feature tooltip
  :config (tooltip-mode -1))

(use-package tramp
  :config (progn
            (setq tramp-default-method "ssh")
            (add-to-list 'backup-directory-alist `(,tramp-file-name-regexp . nil))))

(use-package undo-tree
  :config (progn
            (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree"))
                  undo-tree-visualizer-timestamps t)
            (global-undo-tree-mode +1)))

(use-feature uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package urlenc)

(use-feature vc-hooks
  :config (setq vc-make-backup-files t
                vc-follow-symlinks t))

(use-package volatile-highlights
  :config (volatile-highlights-mode +1))

(use-package wgrep)

(use-package which-key
  :config (which-key-mode +1))

(use-package whitespace
  :bind   (("C-c w" . global-whitespace-mode)
           ("C-c n" . whitespace-cleanup)))

(use-package winner
  :config (progn
            (setq winner-dont-bind-my-keys t)
            (winner-mode +1)))

(use-package with-editor
  :config (shell-command-with-editor-mode +1))

(use-package xref
  :bind   (("C-." . xref-find-references))
  :config (setq xref-prompt-for-identifier nil))

(use-package yaml-mode)
