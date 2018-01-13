;;; init-python --- Python Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  )

;; @github: proofit404/anaconda-mode
(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  )

;; @github: proofit404/company-anaconda
(use-package company-anaconda
  :after company
  :init
  (push 'company-anaconda company-backends)
  )

;; @github: naiquevin/sphinx-doc.el
(use-package sphinx-doc
  :hook (python-mode . sphinx-doc-mode)
  )

(use-package ansible)

(provide 'init-python)
;;; init-python.el ends here
