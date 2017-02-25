;;; -*- lexical-binding: t; -*-

;; disable statup screen
(setq inhibit-startup-screen t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode 1)
(setq make-backup-files nil)

;; automatic and manual symbol highlighting for Emacs.
;; @github: nschum/highlight-symbol.el
(use-package highlight-symbol
  :bind (("C-<f3>" . highlight-symbol)
         ("<f3>"   . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-query-replace)))

;; trim spaces from end of line.
;; @github: lewang/ws-butler
(use-package ws-butler
  :diminish ws-butler-mode
  :init
  (ws-butler-global-mode))

;; a replacement for the Emacs' built-in command `comment-dwim'.
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

(provide 'setup-editor)
