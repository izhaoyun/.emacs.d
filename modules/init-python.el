;;; init-python --- Python packages configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :preface
  (defun zhao/python-mode-defaults ()
    "Defaults for Python programming."
    (diminish 'subword-mode)
    (subword-mode 1)
    (eldoc-mode 1))
  :init
  (add-hook 'python-mode-hook 'zhao/python-mode-defaults)
  :config
  ;; (setq python-shell-completion-native-disabled-interpreters
  ;; '("pypy" "python"))
  )

;; @github: proofit404/anaconda-mode
(use-package anaconda-mode
  :diminish anaconda-mode
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

(use-package virtualenvwrapper
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(use-package auto-virtualenvwrapper
  :init
  (add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
  (add-hook 'projectile-after-switch-project-hook
            #'auto-virtualenvwrapper-activate))

(provide 'init-python)
;;; init-python.el ends here
