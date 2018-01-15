;;; -*- lexical-binding: t -*-

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  )

(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  )

(use-package company-anaconda
  :after company
  :init
  (push 'company-anaconda company-backends)
  )

(use-package sphinx-doc
  :hook (python-mode . sphinx-doc-mode)
  )

(use-package ansible)

(provide 'init-python)
;;; init-python.el ends here
