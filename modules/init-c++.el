(defconst my/c++-packages
  '(company-c-headers
	irony
	company-irony
	flycheck-irony
	google-c-style
	flycheck-google-cpplint
	)
  )

(install-packages my/c++-packages)

(use-package cc-mode
  :mode ("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
  :preface
  (defun my/init-hs-minor-mode ()
	(hs-minor-mode 1)
	(diminish 'hs-minor-mode)
	(setq-default indent-tabs-mode nil)
	)
  :config
  (define-key c-mode-map  [(tab)] 'company-complete)
  (define-key c++-mode-map  [(tab)] 'company-complete)
  (setq-default c-basic-offset 4)
  ;; (setq-default c-default-style "linux")
  (add-hook 'c-mode-common-hook 'my/init-hs-minor-mode)
  )

(use-package irony
  :commands (irony-mode)
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  :config
  (defun my-irony-mode-hook ()
	(define-key irony-mode-map [remap completion-at-point]
	  'irony-completion-at-point-async)
	(define-key irony-mode-map [remap complete-symbol]
	  'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (use-package company-irony
	:init
	(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
	:config
	(add-to-list 'company-backends 'company-irony)
	)

  (use-package flycheck-irony
	:init
	(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
	)
  )

(use-package company-c-headers
  :config
  (eval-after-load 'company
	'(add-to-list 'company-backends 'company-c-headers))
  )

(use-package google-c-style
  :defer t
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)
  )

(use-package flycheck-google-cpplint
  :commands (flycheck-add-next-checker)
  :after flycheck
  :config
  (flycheck-add-next-checker 'c/c++-cppcheck
							 '(warning . c/c++-googlelint))
  (setq flycheck-googlelint-verbose "3")
  (setq flycheck-googlelint-filter "-whitespace,+whitespace/braces")
  (setq flycheck-googlelint-root "project/src")
  (setq flycheck-googlelint-linelength "120")
  )

(provide 'init-c++)
;;; init-c++.el ends here
