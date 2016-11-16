(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  )

(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  :config
  ;; company-anaconda
  (eval-after-load "company"
	'(add-to-list 'company-backends 'company-anaconda))
  )

(use-package py-yapf
  :init
  (add-hook 'python-mode-hook 'py-yapf-enable-on-save)
  )

(use-package flycheck-pyflakes
  :after flycheck
  :init
  (add-to-list 'flycheck-disabled-checkers 'python-pylint)
  (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  )

(use-package pip-requirements
  :mode (("\\.pip\\'" . pip-requirements-mode)
		 ("requirements\.txt\\'" . pip-requirements-mode)
		 ("test-requirements\.txt\'" . pip-requirements-mode))
  :init
  (add-hook 'python-mode-hook 'pip-requirements-auto-complete-setup)
  )

(provide 'init-python)
