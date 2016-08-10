(defconst my/python-packages
  '(elpy
	)
  )

(install-packages my/python-packages)

(use-package python
  :init
  (add-hook 'python-mode-hook
			'(lambda ()
			   (flycheck-mode -1)))
  :config
  (setq python-indent-guess-indent-offset nil)
  )

(use-package elpy
  :init
  (add-hook 'python-mode-hook 'elpy-enable)
  )

(provide 'init-python)
