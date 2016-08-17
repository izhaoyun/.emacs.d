(defconst my/python-packages
  '(python-mode
	pyvenv
	jedi
	py-yapf
	flycheck-pyflakes
	pip-requirements
	)
  )

(install-packages my/python-packages)

(use-package python-mode
  :init
  :config
  (setq-default py-shell-name "ipython")
  (setq-default py-which-bufname "IPython")
  (setq py-python-command-args '("--gui=wx", "--pylab=wx" "--colors" "Linux"))
  (setq py-smart-indentation t)
  )

(use-package jedi
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  :config
  (setq jedi:complete-on-dot t)
  )

(defun python/init-pyvenv ()
  (use-package pyvenv
	:init
	(add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python)
	)
  )
(add-hook 'python-mode-hook 'python/init-pyvenv)

;; setup py-yapf
(add-hook 'python-mode-hook 'py-yapf-enable-on-save)

(use-package flycheck-pyflakes)

(use-package pip-requirements
  :mode (("\\.pip\'" . pip-requirements-mode)
		 ("requirements\.txt\'" . pip-requirements-mode)
		 ("test-requirements\.txt\'" . pip-requirements-mode))
  :init
  (add-hook 'python-mode-hook 'pip-requirements-auto-complete-setup)
  )

(provide 'init-python)
