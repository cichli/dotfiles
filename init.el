(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(require 'use-package)

(dolist (c '(custom
             misc
             defuns
             ui
             editing
             features
             clojure
             elisp
             keys))
  (load (concat user-emacs-directory "lisp/" (symbol-name c))))

(server-start)
