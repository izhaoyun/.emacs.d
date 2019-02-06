;;; -*- lexical-binding: t -*-

(use-package eshell-bookmark
  :hook
  (eshell-mode . eshell-bookmark-setup)
  )

(use-package systemd)

(provide 'init-shell)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init-shell.el ends here
