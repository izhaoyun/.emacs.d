;;; -*- lexical-binding: t -*-

(use-package async-bytecomp
  :ensure async
  :hook
  (emacs-lisp-mode . async-bytecomp-package-mode)
  )

(use-package lispy
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
