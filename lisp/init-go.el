;;; -*- lexical-binding: t -*-

(use-package go-mode-autoloads
  :defer t
  :ensure go-mode
  :mode ("\\.go\\'" . go-mode)
  :preface
  (defun my-go-mode-hook ()
    (company-mode t)
    (set (make-local-variable 'company-backends) '(company-capf company-yasnippet))
    )

  (defun go/init-company-go ()
    (use-package company-go
      :defer t
      :init
      (push 'company-go company-backends)
      )
    )
  :hook ((go-mode . go/init-company-go)
         (go-mode . my-go-mode-hook)
         (go-mode . flycheck-mode))
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

(use-package go-eldoc
  :defer t
  :hook (go-mode . go-eldoc-setup)
  )

(use-package go-dlv
  :disabled
  :defer t
  )

(use-package go-guru
  :defer t
  :hook (go-mode . go-guru-hl-identifier-mode)
  )

(use-package flycheck-gometalinter
  :defer t
  :hook (go-mode . flycheck-gometalinter-setup)
  )

(provide 'init-go)
