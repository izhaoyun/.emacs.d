;;; -*- lexical-binding: t -*-

(use-package async-bytecomp
  :defer t
  :ensure async
  :init
  (async-bytecomp-package-mode 1)
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
  :diminish lispy-mode
  :hook (emacs-lisp-mode . lispy-mode)
  )

(provide 'init-lisp)
;;; init-lisp.el ends here
