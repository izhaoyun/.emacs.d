;;; -*- lexical-binding: t -*-

(use-package async-bytecomp
  :defer t
  :ensure async
  :hook (emacs-lisp-mode . async-bytecomp-package-mode)
  )

(use-package auto-compile
  :defer t
  :hook ((emacs-lisp-mode . auto-compile-on-load-mode)
         (emacs-lisp-mode . auto-compile-on-save-mode))
  :init
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t)
  )

(use-package lispy
  :defer t
  :hook (emacs-lisp-mode . lispy-mode)
  )

(use-package erlang-start
  :ensure erlang
  :preface
  (defun erlang/init-company ()
    (set (make-local-variable 'company-backends)
         '(company-etags
           company-capf
           company-yasnippet))
    ;; (company-mode t)
    )
  :hook (erlang-mode . erlang/init-company)
  )

(use-package company-erlang
  :after (company erlang-start)
  :defer t
  :hook (erlang-mode . company-erlang-init)
  )

(use-package ivy-erlang-complete
  :after (ivy company-erlang)
  :defer t
  :hook ((erlang-mode . ivy-erlang-complete-init)
         (after-save . ivy-erlang-complete-reparse))
  )

(use-package haskell-mode-autoloads
  :ensure haskell-mode
  :defer t
  :preface
  (defun haskell/init-company ()
    (set (make-local-variable 'company-backends)
         '(company-capf
           company-dabbrev-code
           company-yasnippet))
    ;; (company-mode t)
    )
  :hook ((haskell-mode . haskell/init-company)
         (haskell-mode . subword-mode))
  :bind (:map haskell-mode-map
              ("C-c C-," . haskell-mode-format-imports)
              ("C-c C-n" . haskell-navigate-imports))
  )

(provide 'init-lisp)
;;; init-lisp.el ends here
