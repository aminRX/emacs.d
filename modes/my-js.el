;;; my-js.el --- All about JS
;;; Commentary:
;; Configure everything about JS

;;; Code:
(require-packages '(js2-mode tern company-tern))

(require 'js2-mode)
;; (autoload 'js2-mode "js" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js2-mode-hook 'flow-minor-enable-automatically)
(add-hook 'js2-mode-hook 'flow-minor-mode)

(setq-default js2-basic-offset 2)
(setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "process"))
(setq js-switch-indent-offset 2)
(setq-default js2-strict-trailing-comma-warning nil)

(defun setup-js2 ()
  ;; electric-layout-mode doesn't play nice with smartparens
  (setq-local electric-layout-rules '((?\; . after)))
  (js2-imenu-extras-mode +1)
  (tern-mode t)
  (unless (member 'company-tern 'company-backends)
    (add-to-list 'company-backends 'company-tern)))

(add-hook 'js2-mode-hook 'setup-js2)
(add-hook 'js2-jsx-mode-hook 'setup-js2)

(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
(setq typescript-indent-level 2)


(provide 'my-js)

;;; my-js.el ends here
