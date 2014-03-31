(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(require 'use-package)

(dolist (c '(misc
             defuns
             ui
             editing
             features
             clojure
             elisp
             custom
             keys))
  (load (concat user-emacs-directory "lisp/" (symbol-name c))))

(server-start)
