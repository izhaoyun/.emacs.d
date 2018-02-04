;;; -*- lexical-binding: t -*-

(fset 'yes-or-no-p 'y-or-n-p)

(use-package cl-lib)

(use-package hydra
  :defer t
  )

(use-package counsel
  :ensure counsel
  :diminish ivy-mode
  :defer t
  :init
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-display-style 'fancy
        ivy-wrap t)
  (ivy-mode 1)
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("C-s" . counsel-grep-or-swiper)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("C-x C-b" . counsel-bookmark)
         ("C-c m" . counsel-imenu)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("M-y" . counsel-yank-pop)
         ("C-c f" . counsel-git-log))
  :bind (:map help-map
              ("b" . counsel-descbinds)
              ("f" . counsel-describe-function)
              ("l" . counsel-find-library)
              ("v" . counsel-describe-variable)
              ("s" . counsel-info-lookup-symbol)
              ("u" . counsel-unicode-char))
  :bind (:map ivy-minibuffer-map
              ("C-r" . counsel-minibuf-history)
              ("C-c o" . ivy-occur))
  :bind (:map minibuffer-local-map
              ("C-r" . counsel-minibuf-history))
  :config
  (setq counsel-find-file-at-point t)
  )

(use-package avy
  :defer t
  :commands (avy-setup-default)
  :init
  (avy-setup-default)
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g c" . avy-goto-char)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  )

(use-package avy-zap
  :defer t
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim))
  )

(use-package ace-link
  :defer t
  :init
  (ace-link-setup-default)
  )

(use-package window-numbering
  :defer t
  :commands (window-numbering-mode)
  :init
  (window-numbering-mode)
  )

(use-package highlight-symbol
  :defer t
  :bind (("C-<f3>" . highlight-symbol)
         ("<f3>" . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-query-replace))
  )

(use-package which-key
  :defer t
  :diminish which-key-mode
  :commands (which-key-setup-side-window-right-bottom
             which-key-mode)
  :init
  (progn
    (setq which-key-idle-delay 0.8)
    (which-key-mode)
    (which-key-setup-side-window-right-bottom)
    )
  )

(use-package ws-butler
  :defer t
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode)
  )

(use-package expand-region
  :if window-system
  :defer t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region))
  )

(use-package multiple-cursors
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this))
  )

(use-package undo-tree
  :diminish undo-tree-mode
  :hook ((prog-mode cmake-mode org-mode) . undo-tree-mode)
  :init
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  )

(use-package popwin
  :defer t
  :bind-keymap ("C-z" . popwin:keymap)
  :commands (popwin-mode)
  :init
  (popwin-mode 1)
  :config
  (push '(Man-mode :stick t :height 20)
        popwin:special-display-config)
  (push '(" *undo-tree*" :width 0.3 :position right)
        popwin:special-display-config)
  (push '(compilation-mode :noselect t)
        popwin:special-display-config)
  (push "*Shell Command Output*"
        popwin:special-display-config)
  (push '("*magit-commit*" :noselect t :height 40 :width 80 :stick t)
        popwin:special-display-config)
  )

(use-package dired
  :ensure nil
  :defer t
  :init
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always)
  )

(use-package dired-async
  :ensure async
  :defer t
  :hook (dired-mode . dired-async-mode)
  )

(use-package hippie-exp
  :ensure nil
  :defer t
  :bind ("M-/" . hippie-expand)
  )

(use-package cheatsheet
  :disabled
  :defer t
  )

(use-package paren
  :ensure nil
  :defer 8
  :hook (prog-mode . show-paren-mode)
  )

(use-package winner
  :ensure nil
  :defer 10
  :commands (winner-mode)
  :init
  (winner-mode 1)
  )

(use-package tramp
  :ensure nil
  :defer t
  )

(provide 'setup-editor)
;;; setup-editor.el ends here
