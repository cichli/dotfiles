(provide 'keybindings)

;; defuns
(global-set-key (kbd "C-M-'") 'indent-buffer)
(global-set-key (kbd "C-M-\"") 'indent-region)

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; ace-jump-buffer
(global-set-key (kbd "C-c b") 'ace-jump-buffer)
(global-set-key (kbd "C-c B") 'ace-jump-buffer-other-window)
(global-set-key (kbd "C-c C-S-b") 'ace-jump-buffer-in-one-window)

;; ace-jump-mode
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode)
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

;; diff-mode
(define-key diff-mode-map (kbd "M-o") nil)

;; electric-indent
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "C-m") 'electric-indent-just-newline)

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

;; popwin
(global-set-key (kbd "C-z") popwin:keymap)

;; rotate
(global-set-key (kbd "C-' l") 'rotate-layout)
(global-set-key (kbd "C-' w") 'rotate-window)
(global-set-key (kbd "C-' C-l") 'rotate-layout)
(global-set-key (kbd "C-' C-w") 'rotate-window)

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

;; zoom-frm
(global-set-key (kbd "C-x C-0") 'zoom-frm-unzoom)
(global-set-key (kbd "C-x C--") 'zoom-frm-out)
(global-set-key (kbd "C-x C-=") 'zoom-frm-in)
(global-set-key (kbd "C-x C-+") 'zoom-frm-in)
