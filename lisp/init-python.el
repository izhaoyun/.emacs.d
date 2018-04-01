;;; -*- lexical-binding: t -*-

(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :preface
  (defun my-python-hook ()
    (company-mode t)
    (set (make-local-variable 'company-backends)
         '(company-capf company-yasnippet))

    (use-package company-anaconda
      :defer t
      :init
      (push 'company-anaconda company-backends)
      )

    (flycheck-mode t)
    )

  :hook ((python-mode . my-python-hook))
  :init
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i")
  )

(use-package anaconda-mode
  :defer 4
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  )

(use-package pyvenv
  :defer 5
  :hook (python-mode . pyvenv-mode)
  )

(use-package sphinx-doc
  :defer 6
  :diminish sphinx-doc-mode
  :hook (python-mode . sphinx-doc-mode)
  )

(use-package importmagic
  :defer 7
  :diminish importmagic-mode
  :hook (python-mode . importmagic-mode)
  :init
  (add-to-list 'ivy-ignore-buffers "\\*epc con")
  )

(use-package pip-requirements
  :defer t
  )

(use-package py-autopep8
  :defer t
  :hook (python-mode . py-autopep8-enable-on-save)
  )

(use-package cython-mode
  :defer t
  )

(provide 'init-python)
;;; init-python.el ends here
