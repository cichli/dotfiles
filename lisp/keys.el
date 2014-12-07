(provide 'keybindings)

;; defuns
(global-set-key (kbd "C-c f") 'mac-toggle-fullscreen)

(global-set-key (kbd "C-M-'") 'indent-buffer)

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; ace-jump-buffer
(global-set-key (kbd "C-c b") 'ace-jump-buffer)
(global-set-key (kbd "C-c B") 'ace-jump-buffer-other-window)
(global-set-key (kbd "C-c C-S-b") 'ace-jump-buffer-in-one-window)

;; ace-jump-mode
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; ace-window
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; browse-kill-ring
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;; buffer-move
(global-set-key (kbd "C-' f") 'buf-move-right)
(global-set-key (kbd "C-' b") 'buf-move-left)
(global-set-key (kbd "C-' n") 'buf-move-down)
(global-set-key (kbd "C-' p") 'buf-move-up)

;; CIDER
(define-key cider-mode-map (kbd "C-c C-b") 'cider-eval-buffer)
(define-key cider-mode-map (kbd "C-c C-q") 'cider-quit)
(define-key cider-mode-map (kbd "C-c M-q") 'cider-restart)

(define-key cider-repl-mode-map (kbd "C-c C-q") 'cider-quit)
(define-key cider-repl-mode-map (kbd "C-c M-q") 'cider-restart)

;; company-mode
(global-set-key (kbd "C-<tab>") 'company-complete)

(define-key company-mode-map (kbd "C-q") 'company-show-doc-buffer)

;; expand-region
(global-set-key (kbd "C-;") 'er/expand-region)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(define-key ibuffer-mode-map (kbd "M-o") nil)
(define-key ibuffer-mode-map (kbd "C-M-o") 'ibuffer-visit-buffer-1-window)

;; iedit
(global-unset-key (kbd "C-'"))
(global-set-key (kbd "C-M-;") 'iedit-mode)

(define-key iedit-mode-keymap (kbd "C-'") nil)
(define-key iedit-mode-keymap (kbd "C-M-'") 'iedit-toggle-unmatched-lines-visible)

;; iflipb
(global-set-key (kbd "M-[") 'iflipb-previous-buffer)
(global-set-key (kbd "M-]") 'iflipb-next-buffer)

;; imenu
(global-set-key (kbd "C-.") 'ido-imenu-anywhere)

;; isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; jump-char
(global-set-key (kbd "C-<") 'jump-char-backward)
(global-set-key (kbd "C->") 'jump-char-forward)

;; magit
(global-set-key (kbd "C-x m") 'magit-status)
(global-set-key (kbd "C-x l") 'magit-log-popup)

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;; phi-search
(global-set-key (kbd "C-s") 'phi-search)
(global-set-key (kbd "C-r") 'phi-search-backward)
(global-set-key (kbd "M-%") 'phi-replace-query)

;; popwin
(global-set-key (kbd "C-z") popwin:keymap)

;; rotate
(global-set-key (kbd "C-' l") 'rotate-layout)
(global-set-key (kbd "C-' w") 'rotate-window)

;; smartparens
(sp-use-paredit-bindings)

(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)
(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c M-x") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; swoop
(global-set-key (kbd "C-c C-s") 'swoop)
(global-set-key (kbd "C-c C-a") 'swoop-multi)
(global-set-key (kbd "C-c C-M-s") 'swoop-pcre-regexp)
(global-set-key (kbd "C-c C-S-s") 'swoop-back-to-last-position)

;; webjump
(global-set-key (kbd "C-x g") 'webjump)

;; whitespace
(global-set-key (kbd "C-c n") 'whitespace-cleanup)
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; winner
(define-key winner-mode-map (kbd "C-c [") 'winner-undo)
(define-key winner-mode-map (kbd "C-c ]") 'winner-redo)
