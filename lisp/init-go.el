;;; -*- lexical-binding: t -*-

(use-package go-mode-autoloads
  :defer t
  :ensure go-mode
  :mode ("\\.go\\'" . go-mode)
  :preface
  (defun my-go-mode-hook ()
    (company-mode t)
    (set (make-local-variable 'company-backends)
         '(company-capf company-yasnippet))

    (push 'company-go company-backends)
    )
  :hook ((go-mode . my-go-mode-hook)
         (go-mode . flycheck-mode))
  :init
  (setenv "GOPATH" "~/go")
  (setenv "PATH" (concat "~/go/bin" ":" (getenv "PATH")))
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

(use-package go-eldoc
  :defer 15
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
