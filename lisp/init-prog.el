;;; -*- lexical-binding: t -*-

(use-package counsel-projectile
  :defer t
  :init
  (counsel-projectile-mode)
  )

(use-package yasnippet
  :defer t
  :bind (:map yas-minor-mode-map
              ("<f2>" . yas-expand)
              ("C-<f2>" . yas-insert-snippet))
  :hook (after-init . yas-global-mode)
  )

(use-package yasnippet-snippets
  :after yasnippet
  :defer t
  )

(use-package company
  :defer t
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :init
  (setq company-tooltip-limit 20
        company-idle-delay .3
        company-echo-delay 0
        company-show-numbers t)
  :bind ("C-c y" . company-yasnippet)
  ;; :config
  ;; (setq company-begin-commands '(self-insert-command))
  )

(use-package company-quickhelp
  :if window-system
  :defer t
  :after company
  :bind
  (:map company-active-map
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
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  )

(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  )

(use-package hideshow
  :defer t
  :diminish hs-minor-mode
  )

(use-package diff-hl
  :defer 15
  :hook ((prog-mode vc-dir-mode) . turn-on-diff-hl-mode)
  )

(use-package counsel-etags
  :defer t
  :init
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  :config
  (progn
    ;; counsel-etags-ignore-directories does NOT support wildcast
    (add-to-list 'counsel-etags-ignore-directories "build_clang")
    (add-to-list 'counsel-etags-ignore-directories "build_clang")
    ;; counsel-etags-ignore-filenames supports wildcast
    (add-to-list 'counsel-etags-ignore-filenames "TAGS")
    (add-to-list 'counsel-etags-ignore-filenames "*.json"))
  ;; Setup auto update now
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))
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
  :bind
  (:map smartparens-mode-map
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
  )

(provide 'init-prog)
