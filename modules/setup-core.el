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
    (setq ivy-wrap t))
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
  (setq counsel-find-file-at-point t))

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
  (setq company-global-modes
        '(not python-mode pip-requirements-mode))
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
  (yas-global-mode 1))

(use-package hippie-exp
  :bind  ("M-/" . hippie-expand))

;; @github: magit/magit
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

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
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

;; @github: flycheck/flycheck-pos-tip
(use-package flycheck-pos-tip
  :after flycheck
  :init
  (flycheck-pos-tip-mode))

;; @github: tuhdo/semantic-stickyfunc-enhance
(use-package stickyfunc-enhance
  :init
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (semantic-mode 1))

(use-package annotate)

(provide 'setup-core)
;;; setup-core.el ends here
