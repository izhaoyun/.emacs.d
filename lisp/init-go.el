;;; -*- lexical-binding: t -*-

(use-package go-mode-autoloads
  :mode ("\\.go\\'" . go-mode)
  :preface
  (defun init-go-mode-hook ()
    (use-package company-go
      :init
      (setq-local company-backends
                  '(company-dabbrev-code company-keywords company-go))
      (company-mode 1)
      )
    )
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'init-go-mode-hook)
  )


(use-package company-go
  :after company
  :init
  (push 'company-go company-backends)
  )

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup)
  )

(provide 'init-go)
;;; init-go.el ends here
