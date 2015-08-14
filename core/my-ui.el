;;; my-ui.el --- UI
;;; Commentary:
;; UI.

;;; Code:

;; Truncate lines
(setq-default truncate-lines t)

;; apply syntax highlighting to all buffers
(global-font-lock-mode t)

;; No splash screen
(setq inhibit-startup-message t)

;; show column number in bar
(column-number-mode t)

;; show marks as selections
(setq transient-mark-mode t)

;; highlight matching parens
(show-paren-mode t)

;; blink cursor
(blink-cursor-mode -1)

;; force new frames into existing window
(setq ns-pop-up-frames nil)

;; no bell
(setq ring-bell-function 'ignore)

;; highlight incremental search
(defconst search-highlight t)
(defconst query-replace-highlight t)

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Theme and fonts
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Font

(provide 'my-ui)

;;; my-ui.el ends here
