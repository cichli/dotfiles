(require 'cider)
(setq cider-auto-select-error-buffer t
      cider-interactive-eval-result-prefix ";; => "
      cider-repl-history-file (concat user-emacs-directory ".cider-history")
      cider-repl-history-size 1000
      cider-repl-popup-stacktraces t
      cider-repl-result-prefix ";; => "
      cider-repl-use-clojure-font-lock t
      cider-repl-wrap-history t
      cider-stacktrace-default-filters nil
      nrepl-buffer-name-show-port t)

(defun enable-clj-refactor-mode ()
  (interactive)
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(defun cider-start-cljs-repl ()
  (interactive)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(cemerick.austin.repls/cljs-repl (cemerick.austin/exec-env))")
    (cider-repl-return)))

(dolist (hook '(cider-repl-mode-hook
                clojure-mode-hook))
  (add-hook hook 'enable-clj-refactor-mode)
  (add-hook hook 'enable-paredit-mode)
  (add-hook hook 'rainbow-delimiters-mode-enable))

(font-lock-add-keywords 'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                                         (0 (progn (compose-region (match-beginning 1)
                                                                   (match-end 1) "λ")
                                                   nil)))))
(font-lock-add-keywords 'clojure-mode `(("\\(#\\)("
                                         (0 (progn (compose-region (match-beginning 1)
                                                                   (match-end 1) "ƒ")
                                                   nil)))))
(font-lock-add-keywords 'clojure-mode `(("\\(#\\){"
                                         (0 (progn (compose-region (match-beginning 1)
                                                                   (match-end 1) "∈")
                                                   nil)))))

(diminish-major 'clojure-mode-hook "clj")
