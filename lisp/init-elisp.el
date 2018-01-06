;;; init-elisp.el --- Lisp configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; @github: tarsius/auto-compile
(use-package auto-compile
  :init
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  :config
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  )

;; @github: abo-abo/lispy
(use-package lispy
  :diminish lispy-mode
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  scheme-mode-hook))
    (add-hook hook (lambda () (lispy-mode 1))))
  ;; enable lispy for eval-expression
;;;###autoload
  (defun conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
  )

(provide 'init-elisp)
;;; init-elisp.el ends here
