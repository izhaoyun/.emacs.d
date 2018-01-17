;;; -*- lexical-binding: t -*-

(use-package projectile
  :diminish (projectile-mode)
  :init
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-find-dir-includes-top-level t
        projectile-switch-project-action #'projectile-dired)
  )

(use-package counsel-projectile
  :ensure t
  :commands (counsel-projectile-mode)
  :init
  (counsel-projectile-mode)
  )

(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :init
  (setq company-tooltip-limit 20
        company-idle-delay .3
        company-echo-delay 0
        company-show-numbers t)
  :config
  (setq company-begin-commands '(self-insert-command))
  )

(use-package company-quickhelp
  :if window-system
  :defer t
  :bind (:map company-active-map
              ("M-h" . company-quickhelp-manual-begin))
  :hook (company-mode . company-quickhelp-mode)
  :init
  (setq company-quickhelp-delay nil)
  )

(use-package comment-dwim-2
  :defer t
  :bind (("M-;" . comment-dwim-2))
  )

(use-package highlight-indent-guides
  :if window-system
  :defer t
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|
        highlight-indent-guides-auto-odd-face-perc 15
        highlight-indent-guides-auto-even-face-perc 15
        highlight-indent-guides-auto-character-face-perc 20)
  )

(use-package clean-aindent-mode
  :defer t
  :hook prog-mode
  :init
  (setq clean-aindent-is-simple-indent t)
  )

(use-package dtrt-indent
  :defer t
  :diminish dtrt-indent-mode
  :hook (prog-mode . dtrt-indent-mode)
  :init
  (setq dtrt-indent-verbosity 0)
  )

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook ((emacs-lisp-mode cmake-mode) . aggressive-indent-mode)
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line)))))
  )

(use-package rainbow-delimiters
  :defer t
  :hook ((prog-mode cmake-mode) . rainbow-delimiters-mode)
  )

(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  )

(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  )

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :hook ((prog-mode cmake-mode) . eldoc-mode)
  )

(use-package hideshow
  :defer t
  :diminish hs-minor-mode
  :hook ((prog-mode) . hs-minor-mode)
  )

(provide 'init-prog)
