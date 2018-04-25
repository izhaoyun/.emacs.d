;;; -*- lexical-binding: t -*-

(use-package tex
  :load-path "site-lisp/auctex"
  :defer t
  :preface
  (defun my-latex-hook ()
    (company-mode t)
    (set (make-local-variable 'company-backends)
         '(company-capf company-yasnippet))
    (use-package company-auctex
      :defer t
      :init
      (push 'company-auctex company-backends)
      )
    )
  :hook ((LaTeX-mode . my-latex-hook)
         (LaTeX-mode . TeX-PDF-mode)
         (LaTeX-mode . auto-fill-mode))
  :init
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil
                TeX-engine 'xetex
                TeX-command-extra-options "-shell-escape")
  (setq LaTeX-command-style
        '(("" "%(PDF)%(latex) -shell-escape %(file-line-error) %(extraopts) %S%(PDFout)")))
  )

(use-package latex
  :load-path "site-lisp/auctex"
  :defer t
  :init
  (setq LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label))
  )

(use-package preview-latex
  :load-path "site-lisp/auctex"
  :defer t
  )

(use-package reftex
  :ensure nil
  :defer t
  :hook (LaTeX-mode . turn-on-reftex)
  )

(provide 'init-tex)
