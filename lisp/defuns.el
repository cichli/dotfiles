(defun mac-toggle-fullscreen ()
  (interactive)
  (if (not (eq (frame-parameter (selected-frame) 'fullscreen) 'fullboth))
      (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
    (set-frame-parameter (selected-frame) 'fullscreen nil)))

(defun diminish-major (hook alias)
  (add-hook hook `(lambda () (setq mode-name ,alias))))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun hide-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(defun show-trailing-whitespace ()
  (setq show-trailing-whitespace t))
