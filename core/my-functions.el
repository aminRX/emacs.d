;;; my-functions.el --- Misc functions
;;; Commentary:
;; Functions

;;; Code:
;; ask before exiting
(defun confirm-exit-emacs ()
  "Ask for confirmation before exiting Emacs."
  (interactive)
  (if (y-or-n-p "Are you sure you want to exit? ")
      (save-buffers-kill-emacs)))

(defun confirm-erase-buffer ()
  "Ask for confirmation before erase the current buffer."
  (interactive)
  (if (y-or-n-p "Are you sure you want to erase the current buffer? ")
      (erase-buffer)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (if (y-or-n-p "Are you sure you want to erase the current buffer? ")
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))))

(defun my-upgrade-all ()
  "Upgrades all packages."
  (interactive)
  (epl-refresh)
  (epl-upgrade))

(defun untabify-buffer ()
  "Remove all tabs from the buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Indent the buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))
(setq make-backup-files nil)

;; https://github.com/jaypei/emacs-neotree/issues/149

(defun neotree-project-root-dir-or-current-dir ()
  "Open NeoTree using the project root, using projectile, or the
current buffer directory."
  (interactive)
  (let ((project-dir (ignore-errors (projectile-project-root)))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
            (neotree-dir project-dir))
        (if file-name
            (neotree-find file-name))))))


(provide 'my-functions)

;;; my-functions.el ends here
