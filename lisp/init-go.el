;;; -*- lexical-binding: t -*-

(use-package go-mode-autoloads
  :ensure go-mode
  :defer t
  :mode ("\\.go\\'" . go-mode)
  :preface
  (defun go/init-company ()
    (set (make-local-variable 'company-backends)
         '(company-capf
           company-yasnippet))
    (company-mode)

    (use-package company-go
      :after company
      :init
      (push 'company-go company-backends)
      )
    )
  :commands (gofmt-before-save)
  :hook ((go-mode . go/init-company)
         (go-mode . flycheck-mode)
         (go-mode . (lambda ()
                      (add-hook 'before-save-hook 'gofmt-before-save))))
  )

(use-package go-eldoc
  :defer t
  :diminish eldoc-mode
  :hook (go-mode . go-eldoc-setup)
  )

(use-package go-guru
  :defer t
  :hook (go-mode . go-guru-hl-identifier-mode)
  )

(use-package go-playground
  :defer t
  )

(use-package gorepl-mode
  :defer t
  :hook (go-mode . gorepl-mode)
  )

(use-package flycheck-gometalinter
  :after flycheck
  :defer t
  :hook (go-mode . flycheck-gometalinter-setup)
  )

(use-package dockerfile-mode
  :defer t
  :mode ("Dockerfile\\'" . dockerfile-mode)
  )

(use-package docker-tramp
  :defer t
  )

(use-package docker-compose-mode
  :defer t
  )

(use-package docker
  :defer t
  )

(provide 'init-go)
