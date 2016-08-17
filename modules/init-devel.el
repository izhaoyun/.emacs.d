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
	stickyfunc-enhance
	)
  )

(install-packages my/devel-packages)

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
  ;; :diminish company-mode
  :commands (company-mode
			 company-yasnippet)
  :bind (("C-<tab>" . company-yasnippet))
  :init
  (progn
	(setq company-global-modes '(not python-mode pip-requirements-mode))
	(global-company-mode))
  ;; (add-hook 'prog-mode-hook 'company-mode)
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

  (defun check-expansion ()
	(save-excursion
	  (if (looking-at "\\_>") t
		(backward-char 1)
		(if (looking-at "\\.") t
		  (backward-char 1)
		  (if (looking-at "->") t nil))))
	)

  (defun do-yas-expand ()
	(let ((yas-fallback-behavior 'return-nil))
	  (yas-expand))
	)

  (defun tab-indent-or-complete ()
	(interactive)
	(cond
	 ((minibufferp)
	  (minibuffer-complete))
	 (t
	  (indent-for-tab-command)
	  (if (or (not yas-minor-mode)
			  (null (do-yas-expand)))
		  (if (check-expansion)
			  (progn
				(company-manual-begin)
				(if (null company-candidates)
					(progn
					  (company-abort)
					  (indent-for-tab-command))))))))
	)

  (defun tab-complete-or-next-field ()
	(interactive)
	(if (or (not yas-minor-mode)
			(null (do-yas-expand)))
		(if company-candidates
			(company-complete-selection)
		  (if (check-expansion)
			  (progn
				(company-manual-begin)
				(if (null company-candidates)
					(progn
					  (company-abort)
					  (yas-next-field))))
			(yas-next-field))))
	)

  (defun expand-snippet-or-complete-selection ()
	(interactive)
	(if (or (not yas-minor-mode)
			(null (do-yas-expand))
			(company-abort))
		(company-complete-selection))
	)

  (defun abort-company-or-yas ()
	(interactive)
	(if (null company-candidates)
		(yas-abort-snippet)
	  (company-abort))
	)

  (global-set-key [tab] 'tab-indent-or-complete)
  (global-set-key (kbd "TAB") 'tab-indent-or-complete)
  (global-set-key [(control return)] 'company-complete-common)
  (define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
  (define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection)
  (define-key yas-minor-mode-map [tab] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-keymap [tab] 'tab-complete-or-next-field)
  (define-key yas-keymap (kbd "TAB") 'tab-complete-or-next-field)
  (define-key yas-keymap [(control tab)] 'yas-next-field)
  (define-key yas-keymap (kbd "C-g") 'abort-company-or-yas)

  (use-package company-quickhelp
	:bind
	(:map company-active-map
		  ("M-h" . company-quickhelp-manual-begin))
	:init
	(company-quickhelp-mode 1)
	:config
	(setq company-quickhelp-delay nil)
	)
  )

(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2)
  )

(use-package projectile
  :diminish projectile-mode
  :commands (projectile-global-mode)
  :init
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  :config
  (setq projectile-enable-caching t)
  )

(use-package eldoc
  :diminish eldoc-mode
  :commands eldoc-mode
  :config
  (setq eldoc-minor-mode-string ""))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :init
  ;; when `global-aggressive-indent-mode' is enabled it will not
  ;; affect major modes listed in `aggressive-indent-excluded-modes'.
  (global-aggressive-indent-mode 1)
  )

(use-package magit
  :bind (("C-x t g" . magit-status))
  )

(use-package highlight-indentation
  :diminish (highlight-indentation-mode
			 highlight-indentation-current-column-mode)
  :init
  (dolist (hook '(python-mode-hook ruby-mode-hook))
	(add-hook hook #'highlight-indentation-mode)
	(add-hook hook #'highlight-indentation-current-column-mode))
  :config
  (set-face-background 'highlight-indentation-face "#e3e3d3")
  (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
  )

(use-package clean-aindent-mode
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode)
  )

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  ;; check the buffer only when it was saved.
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  )

(use-package stickyfunc-enhance
  :init
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (semantic-mode 1)
  )

(provide 'init-devel)
