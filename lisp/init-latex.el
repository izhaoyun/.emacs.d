;;; -*- lexical-binding: t -*-

(use-package auctex-autoloads
  :ensure auctex
  :defer t
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :init
  (setq-default TeX-engine 'xetex)
  )

(use-package company-auctex
  :after (company latex)
  :defer t
  )

(provide 'init-latex)
