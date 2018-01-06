;; @github: bbatsov/projectile
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode 1)
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
  :config
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .3)
  (setq company-echo-delay 0)
  (setq company-show-numbers t)
  ;; start autocompletion only after typing
  (setq company-begin-commands '(self-insert-command))
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
  :if window-system
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
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)

  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line)))))
  )

;; @github: Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :config
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
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  ;; @github: AndreaCrotti/yasnippet-snippets
  (use-package yasnippet-snippets)

;;;###autoload
  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil))))
    )

;;;###autoload
  (defun do-yas-expand ()
    (let ((yas-fallback-behavior 'return-nil))
      (yas-expand))
    )

;;;###autoload
  (defun tab-indent-or-complete ()
    (interactive)
    (cond ((minibufferp)
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
                           (indent-for-tab-command))
                       ))
                 ))
           ))
    )

;;;###autoload
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

;;;###autoload
  (defun expand-snippet-or-complete-selection ()
    (interactive)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand))
            (company-abort))
        (company-complete-selection))
    )

;;;###autoload
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
  )

;; @github: flycheck/flycheck
(use-package flycheck
  :diminish flycheck-mode
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))

  ;; @github: flycheck/flycheck-pos-tip
  (use-package flycheck-pos-tip
    :init
    (flycheck-pos-tip-mode)
    )
  )

(provide 'init-prog)
