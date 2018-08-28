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

  (defun go/setup-env-var (&optional gopath)
    (unless gopath (setq gopath (concat (getenv "HOME") "/go")))
    (setenv "GOPATH" gopath)
    (setenv "PATH" (concat (getenv "PATH") ":" (concat gopath "/bin")))
    (setq exec-path (append exec-path (list (concat gopath "/bin"))))
    )
  :commands (gofmt-before-save)
  :hook
  ((go-mode . hs-minor-mode)
   (go-mode . flycheck-mode)
   (go-mode . go/init-company)
   (go-mode . go/setup-env-var)
   (go-mode . (lambda ()
                (add-hook 'before-save-hook 'gofmt-before-save)))
   )
  :bind
  (:map go-mode-map
        ("C-c g a" . go-imports-insert-import)
        ("C-c g p" . go-direx-pop-to-buffer)
        ("C-c g b" . go-direx-switch-to-buffer)
        ("C-c g i" . go-impl)
        ("C-c g f" . go-fill-struct)
        ("C-c g r" . go-rename)
        ("C-c g l" . go-imports-reload-packages-list)
        ("C-c g t" . go-tag-add)
        ("C-c g v" . go-tag-remove)
        ("C-c t g" . go-gen-test-dwim)
        ("C-c t a" . go-gen-test-all)
        ("C-c t e" . go-gen-test-exported)
        ("C-c t f" . go-test-current-file)
        ("C-c t t" . go-test-current-test)
        ("C-c t p" . go-test-current-project)
        ("C-c t b" . go-test-current-benchmark)
        ("C-c t x" . go-run))
  )

(use-package go-snippets
  :after yasnippets
  :config
  (go-snippets-initialize)
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
