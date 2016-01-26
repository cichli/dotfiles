(provide 'keybindings)

(global-set-key (kbd "M-ƒ") #'toggle-frame-fullscreen)

;; defuns
(global-set-key (kbd "C-M-'") #'indent-buffer)
(global-set-key (kbd "C-M-\"") #'indent-region)

(global-set-key (kbd "M-'") #'just-one-space)

(global-set-key (kbd "C-x C-r") #'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") #'delete-current-buffer-file)

;; ace-jump-buffer
(global-set-key (kbd "C-c b") #'ace-jump-buffer)
(global-set-key (kbd "C-c B") #'ace-jump-buffer-other-window)
(global-set-key (kbd "C-c C-S-b") #'ace-jump-buffer-in-one-window)

;; ace-window
(global-set-key (kbd "M-o") #'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; avy
(global-set-key (kbd "C-c SPC") #'avy-goto-char)
(global-set-key (kbd "C-c C-SPC") #'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-c M-g") #'avy-goto-line)

;; avy-zap
(global-set-key (kbd "M-z") #'avy-zap-to-char-dwim)
(global-set-key (kbd "M-Z") #'avy-zap-up-to-char-dwim)

;; browse-kill-ring
(global-set-key (kbd "C-c k") #'browse-kill-ring)

;; buffer-move
(global-set-key (kbd "C-' f") #'buf-move-right)
(global-set-key (kbd "C-' b") #'buf-move-left)
(global-set-key (kbd "C-' n") #'buf-move-down)
(global-set-key (kbd "C-' p") #'buf-move-up)

;; CIDER
(define-key cider-mode-map (kbd "C-c C-b") #'cider-eval-buffer)

(define-key cider-repl-mode-map "{" #'paredit-open-curly)
(define-key cider-repl-mode-map "}" #'paredit-close-curly)

;; clojure-mode
(define-key clojure-mode-map (kbd "C-c SPC") nil)

(dolist (keymap (list cider-mode-map cider-repl-mode-map))
  (define-key keymap (kbd "C-c C-q") #'cider-quit)
  (define-key keymap (kbd "C-c M-q") #'cider-restart)
  (define-key keymap (kbd "C-c C-j") #'cider-create-sibling-cljs-repl))

;; comint-mode
(define-key comint-mode-map (kbd "C-c SPC") nil)

;; company-mode
(global-set-key (kbd "C-<tab>") #'company-complete)

(define-key company-mode-map (kbd "C-q") #'company-show-doc-buffer)

;; conf-mode
(require 'conf-mode)
(define-key conf-mode-map (kbd "C-c SPC") nil)

;; diff-mode
(define-key diff-mode-map (kbd "M-o") nil)

;; electric-indent
(global-set-key (kbd "C-j") #'newline-and-indent)
(global-set-key (kbd "C-m") #'electric-indent-just-newline)

;; eshell
(add-hook 'eshell-mode-hook '(lambda () (define-key eshell-mode-map (kbd "C-c SPC") nil)))

;; expand-region
(global-set-key (kbd "C-;") #'er/expand-region)
(global-set-key (kbd "C-M-;") #'mark-sexp)

;; ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

(define-key ibuffer-mode-map (kbd "M-o") nil)
(define-key ibuffer-mode-map (kbd "C-M-o") #'ibuffer-visit-buffer-1-window)

;; iedit
(global-unset-key (kbd "C-'"))
(global-set-key (kbd "C-M-;") #'iedit-mode)

(define-key iedit-mode-keymap (kbd "C-'") nil)
(define-key iedit-mode-keymap (kbd "C-M-'") #'iedit-toggle-unmatched-lines-visible)

;; iflipb
(global-set-key (kbd "M-[") #'iflipb-previous-buffer)
(global-set-key (kbd "M-]") #'iflipb-next-buffer)

;; imenu
(global-set-key (kbd "C-.") #'ido-imenu-anywhere)

;; isearch
(define-key isearch-mode-map (kbd "C-o") #'isearch-occur)

;; jump-char
(global-set-key (kbd "C-<") #'jump-char-backward)
(global-set-key (kbd "C->") #'jump-char-forward)

;; magit
(global-set-key (kbd "C-x m") #'magit-status)
(global-set-key (kbd "C-x l") #'magit-log-popup)
(global-set-key (kbd "<backtab>") (kbd "<s-tab>"))

;; rotate
(global-set-key (kbd "C-' l") #'rotate-layout)
(global-set-key (kbd "C-' w") #'rotate-window)
(global-set-key (kbd "C-' C-l") #'rotate-layout)
(global-set-key (kbd "C-' C-w") #'rotate-window)

;; smex
(global-set-key (kbd "M-x") #'smex)
(global-set-key (kbd "C-c M-x") #'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") #'execute-extended-command)

;; swoop
(global-set-key (kbd "C-c C-s") #'swoop)
(global-set-key (kbd "C-c C-a") #'swoop-multi)
(global-set-key (kbd "C-c C-M-s") #'swoop-pcre-regexp)
(global-set-key (kbd "C-c C-S-s") #'swoop-back-to-last-position)

;; webjump
(global-set-key (kbd "C-x g") #'webjump)

;; whitespace
(global-set-key (kbd "C-c n") #'whitespace-cleanup)
(global-set-key (kbd "C-c w") #'whitespace-mode)

;; winner
(define-key winner-mode-map (kbd "C-c [") #'winner-undo)
(define-key winner-mode-map (kbd "C-c ]") #'winner-redo)

;; zoom-frm
(global-set-key (kbd "C-x C-0") #'zoom-frm-unzoom)
(global-set-key (kbd "C-x C--") #'zoom-frm-out)
(global-set-key (kbd "C-x C-=") #'zoom-frm-in)
(global-set-key (kbd "C-x C-+") #'zoom-frm-in)
