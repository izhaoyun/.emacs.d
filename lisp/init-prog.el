;;; init-prog.el --- Programming Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; @github: bbatsov/projectile
(use-package projectile
  :diminish (projectile-mode)
  :init
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-enable-idle-timer t
        projectile-find-dir-includes-top-level t
        projectile-switch-project-action #'projectile-dired)
  )

;; @github: ericdanan/counsel-projectile
(use-package counsel-projectile
  :init
  (counsel-projectile-mode)
  )

;; @github: company-mode/company-mode
(use-package company
  :diminish company-mode
  :hook ((prog-mode cmake-mode) . company-mode)
  :init
  (setq company-tooltip-limit 20
        company-idle-delay .3
        company-echo-delay 0
        company-show-numbers t)
  :config
  (setq company-backends (delete 'company-semantic company-backends)
        company-begin-commands '(self-insert-command))
  )

;; @github: expez/company-quickhelp
(use-package company-quickhelp
  :if window-system
  :defer t
  :bind (:map company-active-map
              ("M-h" . company-quickhelp-manual-begin))
  :hook (company-mode . company-quickhelp-mode)
  :init
  (setq company-quickhelp-delay nil)
  )

;; @github: joaotavora/yasnippet
(use-package yasnippet
  :disabled
  :diminish yas-minor-mode
  :commands (yas-minor-mode
             yas-expand-from-trigger-key
             yas-expand
             yas-next-field
             yas-abort-snippet)
  :preface
  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil))))
    )
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'cmake-mode-hook #'yas-minor-mode)

  (defun do-yas-expand ()
    (let ((yas-fallback-behavior 'return-nil))
      (yas-expand))
    )

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

  :config
  (define-key yas-minor-mode-map [tab] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (define-key yas-keymap [tab] 'tab-complete-or-next-field)
  (define-key yas-keymap (kbd "TAB") 'tab-complete-or-next-field)
  (define-key yas-keymap [(control tab)] 'yas-next-field)
  (define-key yas-keymap (kbd "C-g") 'abort-company-or-yas)
  )

;; @github: remyferre/comment-dwim-2
(use-package comment-dwim-2
  :defer t
  :bind (("M-;" . comment-dwim-2))
  )

;; @github: DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :defer t
  :if window-system
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|
        highlight-indent-guides-auto-odd-face-perc 15
        highlight-indent-guides-auto-even-face-perc 15
        highlight-indent-guides-auto-character-face-perc 20)
  )

;; @github: pmarinov/clean-aindent-mode
(use-package clean-aindent-mode
  :defer t
  :hook prog-mode
  :init
  (setq clean-aindent-is-simple-indent t)
  )

;; @github: jscheid/dtrt-indent
(use-package dtrt-indent
  :defer t
  :diminish dtrt-indent-mode
  :hook (prog-mode . dtrt-indent-mode)
  :init
  (setq dtrt-indent-verbosity 0)
  )

;; @github: Malabarba/aggressive-indent-mode
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook ((prog-mode cmake-mode) . aggressive-indent-mode)
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line)))))
  )

;; @github: Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;; @github: magit/magit
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  )

;; @github: flycheck/flycheck
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
