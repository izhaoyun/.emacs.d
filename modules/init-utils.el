(defconst my/utils-package
  '(sunrise-commander
	async
	)
  )

(install-packages my/utils-package)

(use-package sunrise-commander
  :commands (sunrise)
  :bind ("C-x t c" . sunrise)
  )

(use-package async-bytecomp
  :ensure async
  :config
  (async-bytecomp-package-mode)
  )

(provide 'init-utils)
;;; init-utils.el ends here.
