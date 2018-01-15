;;; -*- lexical-binding: t -*-

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :preface
  (defun init-cmake-hook ()
    (setq-local company-backends
                '(company-dabbrev-code company-keywords company-cmake))
    (company-mode 1)
    )
  :init
  (add-hook 'cmake-mode-hook 'init-cmake-hook)
  )

(use-package cmake-font-lock
  :defer t
  :hook (cmake-mode . cmake-font-lock-activate)
  )

(provide 'init-makefile)
;;; init-makefile.el ends here
