;;; -*- lexical-binding: t; -*-

(use-package auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

(use-package reftex
  :commands turn-on-reftex
  :init
  (setq reftex-plug-into-AUCTeX t))

(use-package preview
  :commands LaTeX-preview-setup)

(provide 'init-latex)
