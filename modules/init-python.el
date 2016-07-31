(defconst my/python-packages
  '(py-yapf
    company-jedi
    pyvenv))

(install-packages my/python-packages)

(defvar python-indent-guess-indent-offset nil)

(use-package py-yapf
  :config
  (add-hook 'python-mode-hook 'py-yapf-enable-on-save))

(use-package company-jedi
  :init
  (add-to-list 'company-backends 'company-jedi))

(use-package pyvenv
  :init
  (add-hook 'python-mode-hook 'pyvenv-mode))

(provide 'init-python)
