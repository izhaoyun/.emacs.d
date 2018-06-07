;;; -*- lexical-binding: t -*-

(use-package eshell-bookmark
  :defer t
  :hook (eshell-mode . eshell-bookmark-setup)
  )

(use-package systemd
  :defer t
  )

(provide 'init-shell)
