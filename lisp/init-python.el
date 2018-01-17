;;; -*- lexical-binding: t -*-

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :preface
  (defun my-python-hook ()
    (use-package company-anaconda
      :init
      (push 'company-anaconda company-backends)
      )
    (use-package virtualenvwrapper)
    )
  :hook (python-mode . my-python-hook)
  )

(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  )

(use-package sphinx-doc
  :hook (python-mode . sphinx-doc-mode)
  )

(provide 'init-python)
;;; init-python.el ends here
