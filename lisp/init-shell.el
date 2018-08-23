;;; -*- lexical-binding: t -*-

(use-package eshell-bookmark
  :hook
  (eshell-mode . eshell-bookmark-setup)
  )

(use-package systemd)

(provide 'init-shell)
