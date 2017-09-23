;;; my-.evil --- All about evil
;;; Commentary:
;; Configure everything about evil

;;; Code:
(require-packages '(evil))
;; evil
(require 'evil)
(evil-mode 1)

(define-key evil-insert-state-map "\C-a" 'move-beginning-of-line)
(define-key evil-normal-state-map "\C-e" 'evil-end-of-line)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-visual-state-map "\C-e" 'evil-end-of-line)
(define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
(define-key evil-normal-state-map "\C-f" 'evil-forward-char)
(define-key evil-insert-state-map "\C-f" 'evil-forward-char)
(define-key evil-visual-state-map "\C-f" 'evil-forward-char)
(define-key evil-normal-state-map "\C-b" 'evil-backward-char)
(define-key evil-insert-state-map "\C-b" 'evil-backward-char)
(define-key evil-visual-state-map "\C-b" 'evil-backward-char)
(define-key evil-normal-state-map "\C-d" 'evil-delete-char)
(define-key evil-insert-state-map "\C-d" 'evil-delete-char)
(define-key evil-visual-state-map "\C-d" 'evil-delete-char)
(define-key evil-normal-state-map "\C-n" 'evil-next-line)
(define-key evil-insert-state-map "\C-n" 'evil-next-line)
(define-key evil-visual-state-map "\C-n" 'evil-next-line)
(define-key evil-normal-state-map "\C-p" 'evil-previous-line)
(define-key evil-insert-state-map "\C-p" 'evil-previous-line)
(define-key evil-visual-state-map "\C-p" 'evil-previous-line)
(define-key evil-normal-state-map "\C-w" 'evil-delete)
(define-key evil-insert-state-map "\C-w" 'evil-delete)
(define-key evil-visual-state-map "\C-w" 'evil-delete)
(define-key evil-normal-state-map "\C-y" 'yank)
(define-key evil-insert-state-map "\C-y" 'yank)
(define-key evil-visual-state-map "\C-y" 'yank)
(define-key evil-normal-state-map "\C-k" 'kill-line)
(define-key evil-insert-state-map "\C-k" 'kill-line)
(define-key evil-visual-state-map "\C-k" 'kill-line)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map  "kj" 'evil-normal-state)
;; Max time delay between two key presses to be considered a key chord
(setq key-chord-two-keys-delay 0.1) ; default 0.1

;; Max time delay between two presses of the same key to be considered a key chord.
;; Should normally be a little longer than `key-chord-two-keys-delay'.
(setq key-chord-one-key-delay 0.2) ; default 0.2

;; neotree key bindings
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

;; Evil machit
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; Evil leader
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

;; Navigation
(evil-leader/set-key
  "jj" 'avy-goto-char
  "jl" 'avy-goto-line
  "jw" 'avy-goto-word-1)

;; Files manipulations
(evil-leader/set-key
  "ff" 'helm-find-files
  "fj" 'dired-jump
  "fs" 'save-buffer
  "fS" 'save-some-buffers
  "ft" 'neotree-toggle)

;; buffer manipulation
(evil-leader/set-key
  "bb" 'helm-mini
  "bB" 'helm-buffers-list
  "bd" 'kill-this-buffer
  "bd" 'kill-buffer
  "be" 'confirm-erase-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bm" 'kill-other-buffers
  "br" 'revert-buffer)

(evil-leader/set-key
  "pf" 'helm-projectile
  "pt" 'neotree-projectile-action)

;; git manipulation
(evil-leader/set-key
  "gs" 'magit-status)

(evil-leader/set-key
  "hm" 'discover-my-major)

(evil-leader/set-key
  "ss" 'helm-swoop
  "sS" 'helm-multi-swoop
  "s C-s" 'helm-multi-swoop-all)

(evil-leader/set-key
  "'" 'shell)

;; Visual Expand-region
(eval-after-load "evil" '(setq expand-region-contract-fast-key "V"))
(evil-leader/set-key "v" 'er/expand-region)

;; quit emacs.
(evil-leader/set-key
  "qq" 'confirm-exit-emacs)

(provide 'my-evil)

;;; my-evil.el ends here
