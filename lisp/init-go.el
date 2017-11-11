;;; init-go --- Go Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package go-mode-autoloads
  :ensure go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  ;; (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

(use-package company-go
  :config
  (push 'company-go company-backends)
  )

(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setupt)
  )

(provide 'init-go)
;;; init-go.el ends here
