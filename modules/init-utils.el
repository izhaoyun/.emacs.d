(defconst my/utils-package
  '(restclient
    sunrise-commander
    ))

(install-packages my/utils-package)

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

(use-package sunrise-commander
  :commands (sunrise)
  :bind ("C-x t c" . sunrise))

(provide 'init-utils)
;;; init-utils.el ends here.
