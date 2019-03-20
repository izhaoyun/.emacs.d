;;; -*- lexical-binding: t -*-

(use-package async-bytecomp
  :ensure async
  :hook
  (emacs-lisp-mode . async-bytecomp-package-mode)
  )

(use-package auto-compile
  :hook
  ((emacs-lisp-mode . auto-compile-on-load-mode)
   (emacs-lisp-mode . auto-compile-on-save-mode))
  :init
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t)
  )

(use-package lispy
  :disabled
  :diminish lispy-mode
  :hook
  (emacs-lisp-mode . lispy-mode)
  )

(provide 'init-lisp)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init-lisp.el ends here
