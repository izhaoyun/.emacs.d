;;; init-python --- Python packages configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :preface
  (defun zhao/python-mode-defaults ()
    "Defaults for Python programming."
    (subword-mode 1)
    (eldoc-mode 1))
  :init
  (add-hook 'python-mode-hook 'zhao/python-mode-defaults)
  :config
  (setq python-shell-completion-native-disabled-interpreters
        '("pypy" "python")))

;; @github: proofit404/anaconda-mode
(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  :config
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  ;; @github: proofit404/company-anaconda
  (use-package company-anaconda
    :after company
    :init
    (add-to-list 'company-backends 'company-anaconda)))

;; @github: proofit404/pyenv-mode
(use-package pyenv-mode
  :init
  (add-hook 'python-mode-hook #'pyenv-mode))

;; @github: Wilfred/pip-requirements.el
(use-package pip-requirements)

(provide 'init-python)
;;; init-python.el ends here
