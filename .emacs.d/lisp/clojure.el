(require 'cider)

(setq cider-auto-select-error-buffer t
      cider-mode-line nil
      cider-prompt-for-project-on-connect nil
      cider-prompt-for-symbol nil
      cider-repl-history-file (concat user-emacs-directory ".cider-history")
      cider-repl-history-size 1000
      cider-repl-use-clojure-font-lock t
      cider-repl-use-pretty-printing t
      cider-repl-wrap-history t
      cider-show-error-buffer 'always
      nrepl-buffer-name-show-port t
      nrepl-log-messages t)

(setq cljr-sort-comparator 'cljr--semantic-comparator)

(defun enable-clj-refactor-mode ()
  (interactive)
  (clj-refactor-mode 1)
  (diminish 'clj-refactor-mode)
  (cljr-add-keybindings-with-prefix "C-c r"))

(add-hook 'clojure-mode-hook #'enable-clj-refactor-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(add-hook 'cider-inspector-mode-hook #'hide-trailing-whitespace)

(add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook #'hide-trailing-whitespace)

(diminish-major 'clojure-mode-hook "clj")
(diminish-major 'cider-repl-mode-hook nil)
(diminish-major 'cider-stacktrace-mode-hook nil)
(diminish-major 'nrepl-messages-mode-hook nil)
