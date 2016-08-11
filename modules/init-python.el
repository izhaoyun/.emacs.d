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

(defun python/init-elpy ()
  (use-package elpy
	:init
	(elpy-enable)
	)
  )
(add-hook 'python-mode-hook 'python/init-elpy)


(provide 'init-python)
