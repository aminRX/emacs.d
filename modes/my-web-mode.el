;;; my-web-mode.el --- Web mode
;;; Commentary:
;; Web mode for templated html

;;; Code:

(require-packages '(web-mode sass-mode rainbow-mode react-snippets coffee-mode sass-mode))

(require 'web-mode)
(require 'rainbow-mode)
(require 'coffee-mode)
(require 'emmet-mode)
;; Rainbow mode
(add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))
(add-hook 'html-mode-hook (lambda () (rainbow-mode 1)))
(add-hook 'sass-mode-hook (lambda () (rainbow-mode 1)))
(diminish 'rainbow-mode)

;; Web mode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  "JSX."
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; Coffeescript
(custom-set-variables '(coffee-tab-width 2))

;; What to load with what
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss" . sass-mode))
;; Emmet
(add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(provide 'my-web-mode)

;; my-web-mode.el ends here
