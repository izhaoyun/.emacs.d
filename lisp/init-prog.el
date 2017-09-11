;; @github: bbatsov/projectile
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode 1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (setq projectile-enable-idle-timer t)
  (setq projectile-find-dir-includes-top-level t)
  (setq projectile-switch-project-action #'projectile-dired)
  ;; ignored directories
  (add-to-list 'projectile-globally-ignored-directories "man")
  (add-to-list 'projectile-globally-ignored-directories "bin")
  (add-to-list 'projectile-globally-ignored-directories "doxygen")
  )

;; @github: company-mode/company-mode
(use-package company
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  ;; (add-hook 'prog-mode-hook #'company-mode)
  :config
  ;; (setq company-global-modes
  ;;       '(not python-mode pip-requirements-mode))
  (setq company-show-numbers t)
  (setq company-backends (delete 'company-semantic company-backends))
  )

;; @github: expez/company-quickhelp
(use-package company-quickhelp
  :bind (:map company-active-map
              ("M-h" . company-quickhelp-manual-begin))
  :config
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay nil)
  )

;; @github: remyferre/comment-dwim-2
(use-package comment-dwim-2
  :bind (("M-;" . comment-dwim-2))
  )

;; @github: DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
  (setq highlight-indent-guides-auto-odd-face-perc 15)
  (setq highlight-indent-guides-auto-even-face-perc 15)
  (setq highlight-indent-guides-auto-character-face-perc 20)
  )

;; @github: pmarinov/clean-aindent-mode
(use-package clean-aindent-mode
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode)
  :config
  (setq clean-aindent-is-simple-indent t)
  )

;; @github: jscheid/dtrt-indent
(use-package dtrt-indent
  :diminish dtrt-indent-mode
  :init
  (dtrt-indent-mode 1)
  :config
  (setq dtrt-indent-verbosity 0)
  )

;; @github: Malabarba/aggressive-indent-mode
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :init
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)

  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line)))))
  )

;; @github: Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

;; @github: magit/magit
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  )

;; @github: joaotavora/yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-reload-all)
  :init
  ;; @github: AndreaCrotti/yasnippet-snippets
  (use-package yasnippet-sinppets)

  (yas-global-mode 1)
  )

(provide 'init-prog)
