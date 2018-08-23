;;; -*- lexical-binding: t -*-

(use-package go-mode
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

  (defun go/init-snippets ()
    (use-package go-snippets
      :after yasnippet
      )
    )

  (defun go/setup-env-var (&optional gopath)
    (unless gopath (setq gopath (concat (getenv "HOME") "/go")))
    (setenv "GOPATH" gopath)
    (setenv "PATH" (concat (getenv "PATH") ":" (concat gopath "/bin")))
    (setq exec-path (append exec-path (list (concat gopath "/bin"))))
    )

  (defun go/init-flycheck ()
    (use-package flycheck-gometalinter
      :init
      (flycheck-gometalinter-setup)
      )
    (flycheck-mode t)
    )
  :commands (gofmt-before-save)
  :hook
  ((go-mode . hs-minor-mode)
   (go-mode . go/init-snippets)
   (go-mode . go/init-company)
   (go-mode . go/init-flycheck)
   (go-mode . go/setup-env-var)
   (go-mode . (lambda ()
                (add-hook 'before-save-hook 'gofmt-before-save)))
   )
  )

(use-package go-dlv)

(use-package go-gen-test)

(use-package go-eldoc
  :hook
  (go-mode . go-eldoc-setup)
  )

(use-package go-guru
  :hook
  (go-mode . go-guru-hl-identifier-mode)
  )

(use-package go-rename
  :bind
  (:map go-mode-map
        ("C-c t n" . go-rename))
  )

(use-package go-tag
  :bind
  (:map go-mode-map
        ("C-c t a" . go-tag-add)
        ("C-c t r" . go-tag-remove))
  )

(use-package go-direx
  :bind
  (:map go-mode-map
        ("C-c t o" . go-direx-pop-to-buffer)
        ("C-c t u" . go-direx-switch-to-buffer))
  )

(use-package go-imports
  :bind
  (:map go-mode-map
        ("C-c t i" . go-imports-insert-import)
        ("C-c t l" . go-imports-reload-packages-list))
  )

(use-package go-fill-struct
  :bind
  (:map go-mode-map
        ("C-c t f" . go-fill-struct))
  )

(use-package gotest
  :bind
  (:map go-mode-map
        ("C-c t c" . go-test-current-file)
        ("C-c t t" . go-test-current-test)
        ("C-c t p" . go-test-current-project)
        ("C-c t b" . go-test-current-benchmark)
        ("C-c t x" . go-run))
  )

(use-package go-playground)

(use-package gorepl-mode
  :hook
  (go-mode . gorepl-mode)
  )

(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode)
  )

(provide 'init-go)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init-go.el ends here
