(require 'cider)

(setq cider-auto-select-error-buffer t
      cider-interactive-eval-result-prefix ";; => "
      cider-repl-history-file (concat user-emacs-directory ".cider-history")
      cider-repl-history-size 1000
      cider-repl-popup-stacktraces t
      cider-repl-result-prefix ";; => "
      cider-repl-wrap-history t
      nrepl-buffer-name-show-port t
      nrepl-log-messages t)

(defun enable-clj-refactor-mode ()
  (interactive)
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(dolist (hook '(cider-repl-mode-hook
                clojure-mode-hook))
  (add-hook hook 'enable-clj-refactor-mode)
  (add-hook hook 'enable-paredit-mode)
  (add-hook hook 'rainbow-delimiters-mode-enable))

(add-hook 'cider-repl-mode-hook '(lambda ()
                                   (setq show-trailing-whitespace nil)))

(diminish-major 'clojure-mode-hook "clj")
