(require 'elisp-slime-nav)

(setq initial-major-mode 'emacs-lisp-mode)

(dolist (fn '(eldoc-mode
              elisp-slime-nav-mode
              rainbow-delimiters-mode))
  (add-hook 'emacs-lisp-mode-hook fn))

(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

(diminish-major 'emacs-lisp-mode-hook "el")
(diminish 'eldoc-mode)
(diminish 'elisp-slime-nav-mode)
