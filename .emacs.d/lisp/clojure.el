(require 'cider)

(setq cider-auto-select-error-buffer t
      cider-interactive-eval-result-prefix ";; => "
      cider-mode-line nil
      cider-show-error-buffer 'always
      cider-repl-history-file (concat user-emacs-directory ".cider-history")
      cider-repl-history-size 1000
      cider-repl-result-prefix ";; => "
      cider-repl-use-clojure-font-lock t
      cider-repl-use-pretty-printing t
      cider-repl-wrap-history t
      nrepl-buffer-name-show-port t
      nrepl-log-messages t)

(defun enable-clj-refactor-mode ()
  (interactive)
  (clj-refactor-mode 1)
  (diminish 'clj-refactor-mode)
  (cljr-add-keybindings-with-prefix "C-c r"))

(add-hook 'clojure-mode-hook #'enable-clj-refactor-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(add-hook 'cider-inspector-mode-hook #'hide-trailing-whitespace)

(add-hook 'cider-repl-mode-hook #'hide-trailing-whitespace)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)

(sp-local-pair 'clojure-mode "'" nil :actions nil)
(sp-local-pair 'cider-repl-mode "'" nil :actions nil)

(diminish-major 'clojure-mode-hook "clj")
