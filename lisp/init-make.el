(use-package cmake-mode
  :mode (("CMakeLists.txt\\'" . cmake-mode)
         ("\\.cmake\\'"       . cmake-mode))
  )

(use-package cmake-font-lock
  :init
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
  )

(use-package helm-make
  :defer t
  :init
  (setq helm-make-completion-method 'ivy)
  )

(provide 'init-make)
