(defconst my/latex-packages
  '(auctex)
  )

(install-packages my/latex-packages)

(use-package latex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  )

(provide 'init-latex)
