;;; -*- lexical-binding: t -*-

(use-package tex
  :load-path "site-lisp/auctex"
  :defer t
  :preface
  (defun latex/init-company ()
    (set (make-local-variable 'company-backends)
         '(company-capf
           company-yasnippet))
    ;; (company-mode t)

    (use-package company-auctex
      :after company
      :defer t
      :commands (company-auctex-init)
      :init
      (company-auctex-init)
      )
    )
  :hook ((LaTeX-mode . latex/init-company)
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
