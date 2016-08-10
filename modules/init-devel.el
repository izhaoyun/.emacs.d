(defconst my/devel-packages
  '(company
	company-quickhelp
	yasnippet
	projectile
	comment-dwim-2
	aggressive-indent
	magit
	flycheck
	highlight-indentation
	clean-aindent-mode
	ws-butler
	)
  )

(install-packages my/devel-packages)

(defun my/prog-mode-hook ()
  (setq-default case-fold-search nil)
  (setq-default tab-width 4)
  (setq tab-always-indent 'complete)
  (electric-indent-mode -1)
  )
(add-hook 'prog-mode-hook 'my/prog-mode-hook)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(use-package whitespace
  :defer t
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup)
  )

(use-package ws-butler
  :diminish ws-butler-mode
  :defer t
  :config
  (ws-butler-global-mode)
  )

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-global-mode)
  :init
  (yas-global-mode 1)
  :init
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  )

(use-package company
  :diminish company-mode
  :commands (company-mode
			 company-yasnippet)
  :bind (("C-<tab>" . company-yasnippet))
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 20)
  (setq company-backends (delete 'company-semantic company-backends))

  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defun company-mode/backend-with-yas (backend)
	(if (and (listp backend) (member 'company-yasnippet backend))
		backend
	  (append (if (consp backend) backend (list backend))
			  '(:with company-yasnippet))
	  )
	)
  (setq company-backends
		(mapcar #'company-mode/backend-with-yas company-backends))
  ;; solving conflicts in company and yasnippet
  (defun check-expansion ()
	(save-excursion
	  (if (looking-at "\\_>") t
		(backward-char 1)
		(if (looking-at "\\.") t
		  (backward-char 1)
		  (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
	(let ((yas-fallback-behavior 'return-nil))
	  (yas-expand)))

  (defun tab-indent-or-complete ()
	(interactive)
	(if (minibufferp)
		(minibuffer-complete)
	  (if (or (not yas-minor-mode)
			  (null (do-yas-expand)))
		  (if (check-expansion)
			  (company-complete-common)
			(indent-for-tab-command)))))

  (global-set-key [tab] 'tab-indent-or-complete)
  )

(use-package company-quickhelp
  :commands company-quickhelp-mode
  :after company
  :bind
  (:map company-active-map
	("M-h" . company-quickhelp-manual-begin))
  :init
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay nil))

(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

(use-package projectile
  :diminish projectile-mode
  :commands (projectile-global-mode)
  :init
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy))

(use-package eldoc
  :diminish eldoc-mode
  :commands eldoc-mode
  :config
  (setq eldoc-minor-mode-string ""))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :defer t
  :commands (global-aggressive-indent-mode)
  :config
  (global-aggressive-indent-mode 1)
  )

(use-package magit
  :defer t
  :bind ("C-x t g" . magit-status)
  )

(use-package highlight-indentation
  :diminish highlight-indentation-mode
  )

(use-package clean-aindent-mode
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode)
  )

(use-package flycheck
  :commands (flycheck-mode)
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  )

(provide 'init-devel)
