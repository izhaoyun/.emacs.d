(defconst my/shell-packages
  '(systemd)
  )

(install-packages my/shell-packages)

(use-package sh-script
  :mode (("\\.zsh$" . shell-script-mode)
		 )
  :init
  (defvar sh-script-initialized nil)
  (defun initialize-sh-script ()
    (unless sh-script-initialized
      (setq sh-script-initialized t)
      (info-lookup-add-help :mode 'shell-script-mode
                            :regexp ".*"
                            :doc-spec
                            '(("(bash)Index"))))
	)

  (add-hook 'shell-mode-hook 'initialize-sh-script)
  )


(use-package systemd)

(provide 'init-shell)
