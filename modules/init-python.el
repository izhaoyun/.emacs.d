;;; -*- lexical-binding: t; -*-

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :preface
  (defun zhao/python-mode-defaults ()
    "Defaults for Python programming."
    (subword-mode 1)
    (eldoc-mode 1))
  :init
  (add-hook 'python-mode-hook 'zhao/python-mode-defaults))

;; github: proofit404/anaconda-mode
(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  :config
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  ;; github: proofit404/company-anaconda
  (use-package company-anaconda
    :after company
    :init
    (add-to-list 'company-backends 'company-anaconda)))

;; github: proofit404/pyenv-mode
(use-package pyenv-mode
  :init
  (add-hook 'python-mode-hook #'pyenv-mode))

(provide 'init-python)
