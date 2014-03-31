(provide 'keybindings)

(global-set-key (kbd "C-c f") 'mac-toggle-fullscreen)

(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)
(global-set-key (kbd "C-M-'") 'indent-buffer)

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(global-set-key (kbd "C-c b") 'ace-jump-buffer)

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(global-set-key (kbd "C-c k") 'browse-kill-ring)

(define-key company-mode-map (kbd "C-,") 'company-show-doc-buffer)

(global-set-key (kbd "TAB") 'complete-or-indent)

(global-set-key (kbd "C-;") 'er/expand-region)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(define-key ibuffer-mode-map (kbd "M-o") nil)
(define-key ibuffer-mode-map (kbd "C-M-o") 'ibuffer-visit-buffer-1-window)

(global-set-key (kbd "C-.") 'ido-imenu-anywhere)

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(global-set-key (kbd "C-x m") 'magit-status)
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c M-x") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)

(global-set-key (kbd "C-x g") 'webjump)

(global-set-key (kbd "C-c n") 'whitespace-cleanup)
(global-set-key (kbd "C-c w") 'whitespace-mode)

(define-key winner-mode-map (kbd "C-c [") 'winner-undo)
(define-key winner-mode-map (kbd "C-c ]") 'winner-redo)

(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(global-set-key (kbd "<backtab>") 'yas-insert-snippet)
