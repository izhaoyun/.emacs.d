;;; init-go --- Go Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

(use-package company-go
  :config
  (push 'company-go company-backends)
  )

(provide 'init-go)
;;; init-go.el ends here
