;;; setup-core --- Packages bla bla ... -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; swipper
(use-package counsel
  :init
  (use-package ivy
    :init
    (ivy-mode 1)
    (diminish 'ivy-mode)
    :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-wrap t)
    (add-to-list 'ivy-ignore-buffers "\\*epc con"))
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("C-x C-b" . counsel-bookmark)
         ("C-c f" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-c h" . counsel-locate)
         ("C-c m" . counsel-imenu)
         :map help-map
         ("b" . counsel-descbinds)
         ("f" . counsel-describe-function)
         ("v" . counsel-describe-variable)
         ("s" . counsel-info-lookup-symbol)
         ("u" . counsel-unicode-char)
         :map ivy-minibuffer-map
         ("C-c o" . ivy-occur)
         :map read-expression-map
         ("C-r" . counsel-expression-history))
  :config
  (setq counsel-find-file-at-point t)
  (use-package ivy-hydra))

;; select a window to switch to.
;; @github: abo-abo/ace-window
(use-package ace-window
  :bind ("M-p" . ace-window)
  :config
  (setq aw-dispatch-always t))

;; jump to visible text using a char-based decision tree.
;; @github: abo-abo/avy
(use-package avy
  :init
  (avy-setup-default)
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)))

;; display the key bindings following currently entered incomplete
;; command in a popup.
;; @github: justbur/emacs-which-key
(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.7)
  (which-key-setup-side-window-right-bottom))

;; @github: company-mode/company-mode
(use-package company
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  ;; (setq company-global-modes
  ;;       '(not python-mode pip-requirements-mode))
  (setq company-show-numbers t)
  ;; @github:
  (use-package company-quickhelp
    :bind (:map company-active-map
                ("M-h" . company-quickhelp-manual-begin))
    :config
    (company-quickhelp-mode 1)
    (setq company-quickhelp-delay nil)))

;; @github: joaotavora/yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  :config
  ;; https://emacs.stackexchange.com/questions/7908/how-to-make-yasnippet-and-company-work-nicer
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
    (cond
     ((minibufferp)
      (minibuffer-complete))
     (t (indent-for-tab-command)
        (if (or (not yas-minor-mode)
                (null (do-yas-expand)))
            (if (check-expansion)
                (progn
                  (company-manual-begin)
                  (if (null company-candidates)
                      (progn
                        (company-abort)
                        (indent-for-tab-command)))))))))

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
            (yas-next-field)))))

  (defun expand-snippet-or-complete-section ()
    (interactive)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand))
            (company-abort))
        (company-complete-selection)))

  (defun abort-company-or-yas ()
    (interactive)
    (if (null company-candidates)
        (yas-abort-snippet)
      (company-abort)))

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
  (define-key yas-keymap (kbd "C-g") 'abort-company-or-yas))

(use-package hippie-exp
  :bind  ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          ;; try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-complete-file-name-partially
          try-complete-file-name
          ;; try-expand-all-abbrevs
          ;; try-expand-list
          ;; try-expand-line
          )))

;; @github: magit/magit
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

;; @github: pidu/git-timemachine
(use-package git-timemachine
  :defer t
  :bind (("C-x t" . git-timemachine-toggle)))

(use-package projectile
  :init
  (projectile-mode 1)
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (setq projectile-enable-idle-timer t)
  (setq projectile-find-dir-includes-top-level t)
  (setq projectile-switch-project-action #'projectile-dired)
  ;; ignored directories
  (add-to-list 'projectile-globally-ignored-directories "man")
  (add-to-list 'projectile-globally-ignored-directories "bin")
  (add-to-list 'projectile-globally-ignored-directories "doxygen"))

;; @github: flycheck/flycheck
(use-package flycheck
  :diminish flycheck-mode
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))

  ;; @github: flycheck/flycheck-pos-tip
  (use-package flycheck-pos-tip
    :after flycheck
    :init
    (flycheck-pos-tip-mode))
  )

(provide 'setup-core)
;;; setup-core.el ends here
