(require 'cask)
(cask-initialize)

(pallet-mode t)

(setq custom-file "~/.emacs.d/lisp/custom.el")

(dolist (c '(custom
             misc
             defuns
             editing
             features
             clojure
             elisp
             ui
             keys))
  (load (concat user-emacs-directory "lisp/" (symbol-name c))))

(server-start)
