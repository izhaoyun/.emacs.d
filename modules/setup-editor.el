;;; -*- lexical-binding: t; -*-
(setq inhibit-startup-screen t)

;; automatic and manual symbol highlighting for Emacs.
;; @github: nschum/highlight-symbol.el
(use-package highlight-symbol
  :bind (("C-<f3>" . highlight-symbol)
         ("<f3>"   . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-query-replace)))

;; trim spaces from end of line
;; @github: lewang/ws-butler
(use-package ws-butler
  :diminish ws-butler-mode
  :init
  (ws-butler-global-mode))

;; a replacement for the Emacs' built-in command `comment-dwim'
;; @github: remyferre/comment-dwim-2
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; numbered window shortcuts for emacs
;; @github: nschum/window-numbering.el
(use-package window-numbering)

(provide 'setup-editor)
