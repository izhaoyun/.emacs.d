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

;; make it easy to navigate and edit lisp code.
;; @github: abo-abo/lispy
(use-package lispy
  :config
  (dolist (hook '(emacs-lisp-mode-hook
                  scheme-mode-hook))
    (add-hook hook (lambda () (lispy-mode 1))))

  ;; use lispy in the minibuffer during `eval-expression'.
  (defun conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy))



(provide 'init-lisp)
