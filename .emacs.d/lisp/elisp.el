(require 'elisp-slime-nav)

(setq initial-major-mode 'emacs-lisp-mode)

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

(diminish-major 'emacs-lisp-mode-hook "el")
(diminish 'eldoc-mode)
(diminish 'elisp-slime-nav-mode)
