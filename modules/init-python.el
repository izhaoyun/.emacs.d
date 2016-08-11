(defconst my/python-packages
  '(pythonic
	pyvenv
	company-jedi
	py-yapf
	flycheck-pyflakes
	)
  )

(install-packages my/python-packages)

(use-package python
  :init
  :config
  (setq python-indent-guess-indent-offset nil)
  )

(defun python/init-company-jedi ()
  (use-package company-jedi
	:init
	(add-hook 'company-backends 'company-jedi)
	)
  )
(add-hook 'python-mode-hook 'python/init-company-jedi)

(defun python/init-pyvenv ()
  (use-package pyvenv
	:init
	(add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python)
	)
  )
(add-hook 'python-mode-hook 'python/init-pyvenv)

;; setup py-yapf
(add-hook 'python-mode-hook 'py-yapf-enable-on-save)

;;;###autoload
(defun python/init-flycheck-pyflakes ()
  (use-package flycheck-pyflakes)
  )
(add-hook 'python-mode-hook 'python/init-flycheck-pyflakes)

(provide 'init-python)
