(require 'cider)

(setq cider-auto-select-error-buffer t
      cider-repl-pop-to-buffer-on-connect nil
      cider-mode-line nil
      cider-pprint-fn 'puget
      cider-prompt-for-symbol nil
      cider-repl-history-file (concat user-emacs-directory ".cider-history")
      cider-repl-history-size 1000
      cider-repl-use-clojure-font-lock t
      cider-repl-use-pretty-printing t
      cider-repl-wrap-history t
      cider-show-error-buffer 'always
      nrepl-buffer-name-show-port t
      nrepl-log-messages t)

(setq cljr-eagerly-build-asts-on-startup nil
      cljr-favor-prefix-notation nil
      cljr-sort-comparator (lambda (s1 s2)
                             (cl-flet* ((extract-segments (s) (s-split "\\." s))
                                        (shared-segments (s) (->> (extract-segments (cljr--extract-sexp-content s))
                                                                  (mapcar* #'string= (extract-segments (clojure-find-ns)))
                                                                  (seq-take-while #'identity))))
                               (let ((shared-length-s1 (length (shared-segments s1)))
                                     (shared-length-s2 (length (shared-segments s2))))
                                 (if (/= shared-length-s1 shared-length-s2)
                                     (> shared-length-s1 shared-length-s2)
                                   (cljr--string-natural-comparator s1 s2))))))

(defun enable-clj-refactor-mode ()
  (interactive)
  (clj-refactor-mode 1)
  (diminish 'clj-refactor-mode)
  (cljr-add-keybindings-with-prefix "C-c r"))

(add-hook 'clojure-mode-hook #'enable-clj-refactor-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(add-hook 'cider-inspector-mode-hook #'hide-trailing-whitespace)

(add-hook 'cider-repl-mode-hook #'enable-clj-refactor-mode)
(add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook #'hide-trailing-whitespace)

(diminish-major 'clojure-mode-hook "clj")
(diminish-major 'cider-repl-mode-hook nil)
(diminish-major 'cider-stacktrace-mode-hook nil)
(diminish-major 'nrepl-messages-mode-hook nil)

(set-register ?f "(do (require 'figwheel-sidecar.repl-api) (figwheel-sidecar.repl-api/cljs-repl))")
(set-register ?r "(user/cljs-repl user/foo-system)")

(define-clojure-indent
  (for-all 1)
  (quick-check 1))
