;;; -*- lexical-binding: t -*-

(use-package go-mode-autoloads
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
  (setenv "GOPATH" (concat (getenv "HOME") "/go"))
  (setenv "PATH" (concat (concat (getenv "GOPATH") "/bin") ":" (getenv "PATH")))
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

(use-package go-eldoc
  :defer t
  :diminish eldoc-mode
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

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode)
  )

(provide 'init-go)
