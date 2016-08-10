(defconst my/c++-packages
  '(company-c-headers
	irony
	company-irony
	flycheck-irony
	)
  )

(install-packages my/c++-packages)

(use-package cc-mode
  :mode ("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
  :preface
  (defun my/init-hs-minor-mode ()
	(hs-minor-mode 1)
	(diminish 'hs-minor-mode)
	(setq indent-tabs-mode nil)
	)
  :config
  (setq-default c-basic-offset 4)
  (setq-default c-default-style "linux")
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
  )

(use-package company-irony
  :commands (company-irony-setup-begin-commands)
  :init
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  :config
  (eval-after-load 'company
	'(add-to-list 'company-backends 'company-irony))
  )

(use-package company-c-headers
  :config
  (eval-after-load 'company
	'(add-to-list 'company-backends 'company-c-headers))
  )

(provide 'init-c++)
;; init-c++.el ends here.
