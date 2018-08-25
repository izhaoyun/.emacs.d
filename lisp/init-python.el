;;; -*- lexical-binding: t -*-

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :preface
  (defun py/init-company ()
    (set (make-local-variable 'company-backends)
         '(company-capf company-yasnippet))
    (company-mode t)
    )

  :hook ((python-mode . py/init-company))
  :init
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i")
  )

(use-package anaconda-mode
  :hook
  ((python-mode . anaconda-mode)
   (python-mode . anaconda-eldoc-mode))
  )

(use-package company-anaconda
  :after company
  :init
  (push 'company-anaconda company-backends)
  )

(use-package pyvenv
  :hook
  (python-mode . pyvenv-mode)
  )

(use-package pyenv-mode
  :hook
  (python-mode . pyenv-mode)
  :commands
  (pyenv-mode-set
   pyenv-mode-unset
   pyenv-mode-versions)
  :init
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))

  (add-hook 'projectile-after-switch-project-hook
            'projectile-pyenv-mode-set)
  )

(use-package sphinx-doc
  :diminish sphinx-doc-mode
  :hook
  (python-mode . sphinx-doc-mode)
  )

(use-package importmagic
  :diminish importmagic-mode
  :hook
  (python-mode . importmagic-mode)
  :init
  (add-to-list 'ivy-ignore-buffers "\\*epc con")
  )

(use-package pip-requirements)

(use-package py-autopep8
  :hook
  (python-mode . py-autopep8-enable-on-save)
  )

(use-package cython-mode)

(provide 'init-python)
;;; init-python.el ends here
