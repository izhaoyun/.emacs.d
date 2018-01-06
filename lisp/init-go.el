;;; init-go --- Go Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; @github: dominikh/go-mode.el
(use-package go-mode-autoloads
  :ensure go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  ;; (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

;; https://github.com/nsf/gocode/tree/master/emacs-company
(use-package company-go
  :config
  (push 'company-go company-backends)
  )

;; @github: syohex/emacs-go-eldoc
(use-package go-eldoc
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  )

(provide 'init-go)
;;; init-go.el ends here
