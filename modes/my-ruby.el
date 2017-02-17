;;; my-ruby.el --- All about Ruby
;;; Commentary:
;; Configure everything about Ruby

;;; Code:

(require-packages '(rbenv ruby-tools projectile-rails inf-ruby yari))

;; Ruby
(require 'ruby-mode)
(push '("Gemfile" . ruby-mode) auto-mode-alist)
(push '("Gemfile.lock" . ruby-mode) auto-mode-alist)
(push '("Rakefile" . ruby-mode) auto-mode-alist)
(push '("Capfile" . ruby-mode) auto-mode-alist)
(push '("\\.rake" . ruby-mode) auto-mode-alist)
(push '("\\.gemspec" . ruby-mode) auto-mode-alist)
(setq ruby-insert-encoding-magic-comment nil)
(projectile-rails-global-mode)
(define-key 'help-command (kbd "R") 'yari)

;; Ruby Version Manager
(require 'rbenv)
(rbenv-use-global)

;; Ruby tools
(require 'ruby-tools)
(diminish 'ruby-tools-mode)

;; Ruby projects
(add-hook 'projectile-mode-hook 'projectile-rails-on)


(provide 'my-ruby)

;;; my-ruby.el ends here
