;;; -*- lexical-binding: t -*-

(use-package erlang-start
  :ensure erlang
  )

(use-package company-erlang
  :defer t
  :hook (erlang-mode . company-erlang-init)
  )

(use-package ivy-erlang-complete
  :defer t
  :hook ((erlang-mode . ivy-erlang-complete-init)
         (after-save . ivy-erlang-complete-reparse))
  )

(provide 'init-erlang)
