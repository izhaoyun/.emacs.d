;;; init-latex --- LaTeX Settings ----*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(use-package auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  :config
  (setq-default TeX-auto-save t)
  (setq-default TeX-parse-self t)
  (setq-default TeX-master nil))

(use-package reftex
  :commands turn-on-reftex
  :init
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

(use-package preview
  :commands LaTeX-preview-setup)

(provide 'init-latex)
;;; init-latex.el ends here
