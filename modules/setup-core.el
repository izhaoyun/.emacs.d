;;; -*- lexical-binding: t; -*-

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
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-c C-r" . ivy-resume)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("C-x C-b" . counsel-bookmark)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-c l" . counsel-locate)
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
  :init
  (setq which-key-idle-delay 0.5)
  (which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom))

;; @github: company-mode/company-mode
(use-package company
  :init
  ;; (setq company-global-modes
  ;;       '(not python-mode web-mode))
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-show-numbers t)
  ;;
  (use-package company-quickhelp
    :bind (:map company-active-map
                ("M-h" . company-quickhelp-manual-begin))
    :config
    (company-quickhelp-mode 1)
    (setq company-quickhelp-delay nil)))

;; @github: joaotavora/yasnippet
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config)

(use-package hippie-exp
  :bind  ("M-/" . hippie-expand))

;; @github: magit/magit
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

(provide 'setup-core)
