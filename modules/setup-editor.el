;;; -*- lexical-binding: t; -*-

;; disable default statup message.
(setq inhibit-startup-screen t)

(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(menu-bar-mode -1)
(setq visible-bell t)

(global-font-lock-mode 1)
(global-linum-mode 1)
(global-hl-line-mode 1)
(setq make-backup-files nil)

;; tabs, spaces, lines and parenthesis
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
;; Sentences end with one space
(setq sentence-end-double-space nil)

;; os clipboard integration.
(setq select-enable-clipboard t)
(setq select-enable-primary t)
(setq mouse-drag-copy-region t)

(set-language-environment "UTF-8")

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)

(use-package paren
  :init
  (show-paren-mode 1)
  :config
  (setq blink-matching-paren-distance nil)
  ;; highlight text between parens.
  (setq show-paren-style 'expression))

(defalias 'yes-or-no-p 'y-or-n-p)

;; automatic and manual symbol highlighting for emacs.
;; @github: nschum/highlight-symbol.el
(use-package highlight-symbol
  :defer t
  :bind (("C-<f3>" . highlight-symbol)
         ("<f3>"   . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-query-replace)))

;; trim spaces from end of line.
;; @github: lewang/ws-butler
(use-package ws-butler
  :defer t
  :diminish ws-butler-mode
  :config
  (ws-butler-global-mode))

(use-package whitespace
  :defer t
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup))

;; a replacement for the emacs' built-in command `comment-dwim'.
;; @github: remyferre/comment-dwim-2
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; treat undo history as a tree.
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t))

;; expand region increases the selected region by semantic units.
;; @github: magnars/expand-region.el
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;; automatic resize windows by golden ratio.
;; @github: roman/golden-ratio.el
(use-package golden-ratio
  :diminish golden-ratio-mode
  :config
  (golden-ratio-mode 1))

;; a popup window manager.
;; @github: m2ym/popwin-el
(use-package popwin
  :defer t
  :config
  (popwin-mode 1)
  (global-set-key (kbd "C-z") popwin:keymap))

(provide 'setup-editor)
