;;; init-lisp.el --- Lisp configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; @github: tarsius/auto-compile
(use-package auto-compile
  :init
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t)
  )

;; @github: abo-abo/lispy
(use-package lispy
  :diminish lispy-mode
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  scheme-mode-hook))
    (add-hook hook (lambda () (lispy-mode 1))))
  ;; enable lispy for eval-expression
  (defun conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
  )

;; @github: haskell/haskell-mode
(use-package haskell-mode-autoloads
  :ensure haskell-mode
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . haskell-mode)
         ("\\.hsc\\'" . haskell-mode)
         ("\\.cpphs\\'" . haskell-mode)
         ("\\.c2hs\\'" . haskell-mode))
  :bind (:map haskell-mode-map
              ("C-c C-," . haskell-mode-format-imports))
  )

(provide 'init-lisp)
;;; init-lisp.el ends here
