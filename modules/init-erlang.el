(use-package erlang-start
  :mode (("\\.erl?$" . erlang-mode)
         ("\\.hrl?$" . erlang-mode))
  :ensure erlang
  :init
  (setq erlang-root-dir "/usr/lib/erlang")
  (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
  :config
  (use-package erlang-flymake)
  )

(provide 'init-erlang)
