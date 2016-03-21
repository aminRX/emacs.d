;;; my-osx.el --- Configuration for OSX
;;; Commentary:
;; OSX

;;; Code:

(require-packages '(exec-path-from-shell))

;; Turn cmd key into meta
(setq mac-command-modifier 'control)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(provide 'my-osx)

;;; my-osx.el ends here
