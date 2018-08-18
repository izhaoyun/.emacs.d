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

  (defun go/setup-env-var (&optional gopath)
    (unless gopath (setq gopath (concat (getenv "HOME") "/go")))
    (setenv "GOPATH" gopath)
    (setenv "PATH" (concat (getenv "PATH") ":" (concat gopath "/bin")))
    (setq exec-path (append exec-path (list (concat gopath "/bin"))))
    )
  :commands (gofmt-before-save)
  :bind
  (:map go-mode-map
        ;; ("C-c C-a" . go-import-add)
        ("C-c C-d" . godef-describe)
        ("C-c C-j" . godef-jump)
        ("C-x 4 C-c C-j" . godef-jump-other-window)
        ("C-c C-f a" . go-goto-arguments)
        ("C-c C-f d" . go-goto-docstring)
        ("C-c C-f f" . go-goto-function)
        ("C-c C-f i" . go-goto-imports)
        ("C-c C-f m" . go-goto-method-receiver)
        ("C-c C-f n" . go-goto-function-name)
        ("C-c C-f r" . go-goto-return-values)
        )
  :hook
  ((go-mode . go/init-company)
   ;; (go-mode . flycheck-mode)
   (go-mode . (lambda ()
                (add-hook 'before-save-hook 'gofmt-before-save))))
  :init
  (go/setup-env-var)
  )

(use-package go-eldoc
  :after go-mode
  :defer t
  :diminish eldoc-mode
  :hook (go-mode . go-eldoc-setup)
  )

(use-package go-guru
  :after go-mode
  :defer t
  :hook (go-mode . go-guru-hl-identifier-mode)
  )

(use-package go-rename
  :after go-mode
  :defer t
  )

(use-package go-playground
  :after go-mode
  :defer t
  )

(use-package go-dlv
  :after go-mode
  :defer t
  )

(use-package gorepl-mode
  :defer t
  :hook (go-mode . gorepl-mode)
  )

(use-package flycheck-gometalinter
  :disabled
  :after flycheck
  :defer t
  :hook (go-mode . flycheck-gometalinter-setup)
  )

(provide 'init-go)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init-go.el ends here
