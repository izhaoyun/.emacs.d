;;; -*- lexical-binding: t -*-

(use-package projectile
  :defer t
  :diminish (projectile-mode)
  :hook (projectile-mode . recentf-mode)
  :init
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-find-dir-includes-top-level t
        projectile-switch-project-action #'projectile-dired)
  )

(use-package counsel-projectile
  :defer t
  :init
  (counsel-projectile-mode)
  )

(use-package yasnippet
  :defer t
  :init
  (yas-global-mode 1)
  )

(use-package company
  :defer t
  :hook ((emacs-lisp-mode) . company-mode)
  :init
  (setq company-tooltip-limit 20
        company-idle-delay .3
        company-echo-delay 0
        company-show-numbers t)
  :bind ("C-c y" . company-yasnippet)
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
  :defer t
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
  :ensure rainbow-delimiters
  :defer t
  :hook ((prog-mode cmake-mode) . rainbow-delimiters-mode)
  )

(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :init
  (setq magit-completing-read-function 'ivy-completing-read)
  )

(use-package flycheck
  :defer t
  ;; :diminish flycheck-mode
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  )

(use-package hideshow
  :defer t
  :diminish hs-minor-mode
  :hook ((prog-mode cmake-mode) . hs-minor-mode)
  )

(use-package diff-hl
  :defer t
  :hook ((prog-mode vc-dir-mode) . turn-on-diff-hl-mode)
  )

(use-package smartparens-config
  :disabled
  :ensure smartparens
  :defer t
  ;; :hook (minibuffer-setup . turn-on-smartparens-strict-mode)
  :preface
  (defhydra smartparens-hydra ()
    "Smartparens"
    ("d" sp-down-sexp "Down")
    ("e" sp-up-sexp "Up")
    ("u" sp-backward-up-sexp "Up")
    ("a" sp-backward-down-sexp "Down")
    ("f" sp-forward-sexp "Forward")
    ("b" sp-backward-sexp "Backward")
    ("k" sp-kill-sexp "Kill" :color blue)
    ("q" nil "Quit" :color blue)
    )
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-a" . sp-backward-down-sexp)
              ("C-S-d" . sp-beginning-of-sexp)
              ("C-S-a" . sp-end-of-sexp)
              ("C-M-e" . sp-up-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-t" . sp-transpose-sexp)
              ("C-M-n" . sp-forward-hybrid-sexp)
              ("C-M-p" . sp-backward-hybrid-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("M-<delete>" . sp-unwrap-sexp)
              ("M-<backspace>" . sp-backward-unwrap-sexp)
              ("M-F" . sp-forward-symbol)
              ("M-B" . sp-backward-symbol)
              ("C-M-s" . smartparens-hydra/body)
              )
  :init
  )

(provide 'init-prog)
