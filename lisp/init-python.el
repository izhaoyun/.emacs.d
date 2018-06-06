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
  :defer t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  )

(use-package company-anaconda
  :defer t
  :after (company anaconda-mode)
  :init
  (push 'company-anaconda company-backends)
  )

(use-package pyvenv
  :defer t
  :hook (python-mode . pyvenv-mode)
  )

(use-package pyenv-mode
  :hook (python-mode . pyenv-mode)
  :commands (pyenv-mode-set pyenv-mode-unset pyenv-mode-versions)
  :init
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))

  (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)
  )

(use-package sphinx-doc
  :defer t
  :diminish sphinx-doc-mode
  :hook (python-mode . sphinx-doc-mode)
  )

(use-package importmagic
  :defer t
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
