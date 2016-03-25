;;; my-js.el --- All about JS
;;; Commentary:
;; Configure everything about JS

;;; Code:

(require-packages '(js2-mode tern company-tern))

(require 'js2-mode)
(autoload 'js2-mode "js" nil t)
(push '("\\.js$" . js2-mode) auto-mode-alist)
(push '("\\.json$" . js2-mode) auto-mode-alist)
(push '("\\.jsx$" . js2-jsx-mode) auto-mode-alist)

(setq-default js2-basic-offset 2)

(defun setup-tern ()
  (js2-imenu-extras-mode +1)
  (tern-mode t))

(defun setup-company ()
  (unless (member 'company-tern 'company-backends)
    (add-to-list 'company-backends 'company-tern)))

(add-hook 'js2-mode-hook 'setup-tern)
(add-hook 'js2-mode-hook 'setup-company)
(add-hook 'js2-jsx-mode-hook 'setup-tern)
(add-hook 'js2-jsx-mode-hook 'setup-company)

(require 'smartparens-config)
(sp-local-pair 'js2-mode "'" nil :unless '(sp-point-before-same-p))
(add-hook 'js2-mode-hook #'smartparens-mode)

(provide 'my-js)

;;; my-js.el ends here
