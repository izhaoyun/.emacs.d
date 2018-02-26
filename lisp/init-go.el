;;; -*- lexical-binding: t -*-

(use-package go-mode-autoloads
  :defer t
  :ensure go-mode
  :mode ("\\.go\\'" . go-mode)
  :preface
  (defun my-go-mode-hook ()
    (setq my_gopath_bin (concat (getenv "HOME") "/go/bin:"))
    (setenv "PATH" (concat my_gopath_bin (getenv "PATH")))

    (company-mode t)
    (set (make-local-variable 'company-backends) '(company-capf company-yasnippet))

    (use-package company-go
      :defer t
      :init
      (push 'company-go company-backends)
      )
    )
  :hook ((go-mode . my-go-mode-hook)
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
