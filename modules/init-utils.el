(defconst my/utils-package
  '(restclient
    sunrise-commander
    async
    ))

(install-packages my/utils-package)

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

(use-package sunrise-commander
  :commands (sunrise)
  :bind ("C-x t c" . sunrise))

(use-package async-bytecomp
  :ensure async
  :init
  (async-bytecomp-package-mode))


(provide 'init-utils)
;;; init-utils.el ends here.
