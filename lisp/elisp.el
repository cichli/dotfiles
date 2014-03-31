(setq initial-major-mode 'emacs-lisp-mode)

(dolist (fn '(eldoc-mode
              enable-paredit-mode
              rainbow-delimiters-mode))
  (add-hook 'emacs-lisp-mode-hook fn))

(diminish-major 'emacs-lisp-mode-hook "el")
(diminish 'eldoc-mode)
