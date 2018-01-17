;;; -*- lexical-binding: t -*-

(use-package auto-compile
  :hook ((emacs-lisp-mode . auto-compile-on-load-mode)
         (emacs-lisp-mode . auto-compile-on-save-mode))
  :init
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t)
  )

(use-package lispy
  :ensure lispy
  :diminish lispy-mode
  :hook (emacs-lisp-mode . lispy-mode)
  )

(provide 'init-lisp)
;;; init-lisp.el ends here
