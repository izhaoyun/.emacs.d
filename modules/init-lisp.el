;;; -*- lexical-binding: t; -*-

;; automatically compile emacs lisp libraries.
;; @github: tarsius/auto-compile
(use-package auto-compile
  :init
  (setq load-prefer-newer t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  :config
  ;; to display the buffer use command `auto-compile-display-log'.
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t))

(use-package lispy)

(provide 'init-lisp)
