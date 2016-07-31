(defconst my/makefile-packages
  '(cmake-mode
    cmake-font-lock))

(install-packages my/makefile-packages)

(use-package cmake-mode
  :mode (("CMakeLists.txt\\'" . cmake-mode)
         ("\\.cmake\\'"       . cmake-mode))
  :config
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
  )


(provide 'init-makefile)
