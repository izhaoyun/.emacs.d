;;; -*- lexical-binding: t -*-

(use-package go-mode-autoloads
  :ensure go-mode
  :mode ("\\.go\\'" . go-mode)
  :preface
  (defun my-go-mode-hook ()
    (use-package company-go
      :init
      (push 'company-go company-backends)
      (company-mode 1)
      )
    (add-hook 'before-save-hook 'gofmt-before-save)
    )
  :hook (go-mode . my-go-mode-hook)
  )

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup)
  )

(provide 'init-go)
;;; init-go.el ends here
