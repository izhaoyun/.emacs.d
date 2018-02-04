;;; -*- lexical-binding: t -*-

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :preface
  (defun my-python-hook ()
    (company-mode t)
    (set (make-local-variable 'company-backends) '(company-yasnippet))

    (use-package company-anaconda
      :defer t
      :init
      (push 'company-anaconda company-backends)
      )
    )
  :hook ((python-mode . my-python-hook))
  )

(use-package anaconda-mode
  :defer t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  )

(use-package pyvenv
  :defer t
  )

(use-package sphinx-doc
  :defer t
  :hook (python-mode . sphinx-doc-mode)
  )

(provide 'init-python)
;;; init-python.el ends here
