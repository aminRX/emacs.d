;;; init.el --- Emacs configuration
;;; Commentary:
;; Load everything up.

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl)
(require 'package)

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar root-dir (file-name-directory load-file-name))
(defvar core-dir (expand-file-name "core" root-dir))
(defvar modes-dir (expand-file-name  "modes" root-dir))
(defvar vendor-dir (expand-file-name "vendor" root-dir))
(defvar savefile-dir (expand-file-name "savefile" root-dir))
(defvar backup-dir (expand-file-name "backup" root-dir))

;; Copied from prelude.
(defun add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (add-subfolders-to-load-path name)))))

(add-to-list 'load-path core-dir)
(add-to-list 'load-path modes-dir)
(add-to-list 'load-path vendor-dir)
(add-to-list 'load-path core-dir)
(add-subfolders-to-load-path vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold (* 1024 1024 50))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Core configuration
(require 'my-packages)
(require 'my-functions)
(require 'my-editor)
(require 'my-key-bindings)
(when (equal system-type 'darwin)
  (require 'my-osx))

;; Modes configuration
(require 'my-ido)
(require 'my-magit)
(require 'my-ruby)
(require 'my-lisp)
(require 'my-js)
(require 'my-scheme)
(require 'my-rust)
(require 'my-text)
(require 'my-web-mode)
(require 'my-modes)
(require 'my-helm)
(require 'my-evil)

;; Load UI after everything else.
(require 'my-ui)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(package-selected-packages
   (quote
    (evil-escape zenburn-theme yari yaml-mode writegood-mode web-mode volatile-highlights typescript-mode tss spacemacs-theme spaceline solarized-theme smex smartparens slime scss-mode sass-mode rust-mode ruby-tools restclient react-snippets rbenv rainbow-mode rainbow-delimiters projectile-rails paredit noctilux-theme nlinum neotree move-text monokai-theme moe-theme markdown-mode magit leuven-theme js2-refactor iedit ido-vertical-mode ido-ubiquitous helm-projectile helm-descbinds helm-ag geiser flycheck-rust flx-ido expand-region exec-path-from-shell evil emmet-mode discover-my-major dired+ diminish diff-hl dash-at-point company-tern color-theme-sanityinc-tomorrow coffee-mode browse-kill-ring anzu ample-theme ag ace-window ace-jump-mode ace-jump-buffer))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
