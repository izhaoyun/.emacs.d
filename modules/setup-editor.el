;;; setup-editor --- basic settings -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defalias 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)

;; automatic and manual symbol highlighting for emacs.
;; @github: nschum/highlight-symbol.el
(use-package highlight-symbol
  :defer t
  :bind (("C-<f3>" . highlight-symbol)
         ("<f3>"   . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-query-replace)))

;; @github: lewang/ws-butler
(use-package ws-butler
  :defer t
  :diminish ws-butler-mode
  :init
  (ws-butler-global-mode))

;; @github: remyferre/comment-dwim-2
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; http://www.dr-qubit.org/Emacs_Undo_Tree_package.html
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t))

;; @github: magnars/expand-region.el
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;; @github: m2ym/popwin-el
(use-package popwin
  :bind-keymap ("C-z" . popwin:keymap)
  :commands (popwin-mode)
  :init
  (popwin-mode 1)
  :config
  (push '(Man-mode :stick t :height 20) popwin:special-display-config)
  (push '(" *undo-tree*" :width 0.3 :position right)
        popwin:special-display-config))

;; @github: malabarba/aggressive-indent-mode
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)

  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;; @github: DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
  (setq highlight-indent-guides-auto-odd-face-perc 15)
  (setq highlight-indent-guides-auto-even-face-perc 15)
  (setq highlight-indent-guides-auto-character-face-perc 20))

(use-package clean-aindent-mode
  :config
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

(use-package dtrt-indent
  :diminish (dtrt-indent-mode)
  :init
  (dtrt-indent-mode 1)
  :config
  (setq dtrt-indent-verbosity 0))

;; @github: fourier/ztree
(use-package ztree)

;; @github: Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook #'rainbow-delimiters-mode))

;; @github: magnars/multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; @github: magit/with-editor
(use-package with-editor
  :init
  (add-hook 'shell-mode-hook 'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor))

(provide 'setup-editor)
;;; setup-editor.el ends here
