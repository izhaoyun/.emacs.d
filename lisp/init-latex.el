;;; -*- lexical-binding: t -*-

(use-package auctex
  :load-path "site-lisp/auctex-12.1"
  :defer t
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :preface
  (defun my-latex-hook ()
    (company-mode t)
    (set (make-local-variable 'company-backends)
         '(company-capf company-yasnippet))
    )

  (defun latex/init-company-auctex ()
    (use-package company-auctex
      :defer t
      :init
      (push 'company-auctex company-backends)
      )
    )
  :hook ((TeX-latex-mode . latex/init-company-auctex)
         (TeX-latex-mode . my-latex-hook))
  :init
  (setq TeX-auto-save t
        TeX-parse-self t)

  (setq-default TeX-master nil
                TeX-engine 'xetex
                TeX-command-extra-options "-shell-escape")
  (setq LaTeX-section-hook
        '(LaTeX-section-heading LaTeX-section-title LaTeX-section-toc LaTeX-section-section LaTeX-section-label)
        LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %(file-line-error) %(extraopts) %S%(PDFout)")))
  )

(use-package preview-latex
  :load-path "site-lisp/auctex-12.1"
  :defer t
  )

(provide 'init-latex)
