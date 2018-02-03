;;; -*- lexical-binding: t -*-

(use-package async-bytecomp
  :defer t
  :ensure async
  :hook (emacs-lisp-mode . async-bytecomp-package-mode)
  )

(use-package auto-compile
  :defer t
  :hook ((emacs-lisp-mode . auto-compile-on-load-mode)
         (emacs-lisp-mode . auto-compile-on-save-mode))
  :init
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t)
  )

(use-package lispy
  :defer t
  :hook (emacs-lisp-mode . lispy-mode)
  )

(provide 'init-lisp)
;;; init-lisp.el ends here
