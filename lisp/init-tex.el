;;; -*- lexical-binding: t -*-

(use-package auctex
  :load-path "site-lisp/auctex"
  :defer t
  :preface
  (defun tex/init-company ()
    (set (make-local-variable 'company-backends)
         '(company-capf
           company-yasnippet))
    (company-mode)

    (use-package company-auctex
      :after company
      :defer t
      :hook (company-mode . company-auctex-init)
      )
    )
  :hook ((LaTeX-mode . tex/init-company)
         (LaTeX-mode . TeX-PDF-mode)
         (LaTeX-mode . auto-fill-mode))
  :init
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label))
  (setq-default TeX-master nil
                TeX-engine 'xetex
                TeX-command-extra-options "-shell-escape")
  (setq LaTeX-command-style
        '(("" "%(PDF)%(latex) -shell-escape %(file-line-error) %(extraopts) %S%(PDFout)")))
  :config
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
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

(use-package pdf-tools
  :defer t
  :hook (pdf-view-mode . (lambda () (cua-mode 0)))
  :bind
  (:map pdf-view-mode-map
        ("C-s" . isearch-forward)
        ("D" . pdf-annot-delete))
  :init
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-resize-factor 1.1)
  :config
  (setq pdf-annot-activate-created-annotations t)
  )

(provide 'init-tex)
