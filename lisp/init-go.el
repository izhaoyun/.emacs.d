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
  :defer t
  :hook (go-mode . go-eldoc-setup)
  )

(use-package go-dlv
  :disabled
  )

(use-package go-guru
  :defer t
  :hook (go-mode . go-guru-hl-identifier-mode)
  )

(provide 'init-go)
