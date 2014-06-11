(require 'elisp-slime-nav)

(setq initial-major-mode 'emacs-lisp-mode)

(dolist (fn '(eldoc-mode
              elisp-slime-nav-mode
              rainbow-delimiters-mode))
  (add-hook 'emacs-lisp-mode-hook fn))

(diminish-major 'emacs-lisp-mode-hook "el")
(diminish 'eldoc-mode)
(diminish 'elisp-slime-nav-mode)
