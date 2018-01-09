;;; setup-editor.el --- basic settings for the editor. -*- lexical-binding: t; -*-

;;; Commentary:

;; This file provides basic settings for the editor.

;;; Code:

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

;; github: abo-abo/swiper
(use-package counsel
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-display-style 'fancy)
  (setq ivy-wrap t)

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

;; @github: abo-abo/avy
(use-package avy
  :defer 5
  :commands (avy-setup-default)
  :init
  (avy-setup-default)
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  )

;; @github: abo-abo/ace-link
(use-package ace-link
  :defer 10
  :config
  (ace-link-setup-default)
  )

;; @github: nschum/window-numbering.el
(use-package window-numbering
  :defer 8
  :commands (window-numbering-mode)
  :init
  (window-numbering-mode)
  )

;; @github: nschum/highlight-symbol.el
(use-package highlight-symbol
  :defer t
  :bind (("C-<f3>" . highlight-symbol)
         ("<f3>" . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-query-replace))
  )

;; @github: justbur/emacs-which-key
(use-package which-key
  :defer t
  :diminish which-key-mode
  :commands (which-key-setup-side-window-right-bottom
             which-key-mode)
  :init
  (progn
    (which-key-mode)
    (which-key-setup-side-window-right-bottom)
    )
  :config
  (setq which-key-idle-delay 0.8)
  )

;; @github: lewang/ws-butler
(use-package ws-butler
  :defer 8
  :diminish ws-butler-mode
  :commands (ws-butler-mode)
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  )

;; @github: magnars/expand-region.el
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region))
  )

;; http://www.dr-qubit.org/Emacs_Undo_Tree_package.html
(use-package undo-tree
  :diminish undo-tree-mode
  :commands (global-undo-tree-mode)
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  )

;; @github: m2ym/popwin-el
(use-package popwin
  :defer t
  :bind-keymap ("C-z" . popwin:keymap)
  :config
  (popwin-mode 1)
  ;; M-x man
  (push '(Man-mode :stick t :height 20)
        popwin:special-display-config)
  ;; undo-tree
  (push '(" *undo-tree*" :width 0.3 :position right)
        popwin:special-display-config)
  ;; M-x compile
  (push '(compilation-mode :noselect t)
        popwin:special-display-config)
  ;; M-!
  (push "*Shell Command Output*"
        popwin:special-display-config)
  ;; magit
  (push '("*magit-commit*" :noselect t :height 40 :width 80 :stick t)
        popwin:special-display-config)
  )

(use-package dired
  :defer t
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  )

(use-package hippie-exp
  :commands (hippie-expand)
  :bind ("M-/" . hippie-expand)
  )

(provide 'setup-editor)
;;; setup-editor.el ends here
