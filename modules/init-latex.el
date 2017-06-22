;;; init-latex --- LaTeX Settings ----*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; TeX
(use-package tex-mode
  :commands latex-mode
  :config
  ;; AUCTeX
  (use-package tex
    :ensure auctex
    :commands TeX-PDF-mode
    :config
    (setq-default TeX-engine 'xelatex)
    (setq-default TeX-master nil)
    (setq-default TeX-auto-save t)
    (setq-default TeX-parse-self t))

  (TeX-PDF-mode 1)
  (outline-minor-mode 1))

;; BibTeX
(use-package bibtex
  :commands bibtex-mode
  :config
  (setq bibtex-maintain-sorted-entries t))

(provide 'init-latex)
;;; init-latex.el ends here
