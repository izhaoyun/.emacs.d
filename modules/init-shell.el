(defconst my/shell-packages
  '(systemd)
  )

(install-packages my/shell-packages)

(use-package sh-script
  :mode (("\\.zsh$" . shell-script-mode) ; use shell-script-mode for .zsh files
		 )
  )

(use-package systemd)

(provide 'init-shell)
