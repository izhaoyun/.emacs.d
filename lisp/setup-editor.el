;;; setup-editor --- basic settings -*- lexical-binding: t; -*-

;;; Commentary:

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
  (use-package ivy
    :diminish ivy-mode
    :init
    (ivy-mode 1)
    :config
    (setq ivy-use-virtual-buffers t
          ivy-count-format "(%d/%d) "
          ivy-display-style 'fancy
          ivy-wrap t)
    :bind (("C-c r" . ivy-resume))
    :bind (:map ivy-minibuffer-map
                ("C-c o" . ivy-occur))
    )
  :bind (("C-s" . counsel-grep-or-swiper)
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
              ("C-r" . counsel-expression-history))
  :config
  (setq counsel-find-file-at-point t)
  )

;; @github: abo-abo/avy
(use-package avy
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
  :init
  (ace-link-setup-default)
  :config
  (eval-after-load "org"
    '(progn (define-key org-mode-map (kbd "M-o") 'ace-link-org)))
  )

;; @github: abo-abo/ace-window
(use-package ace-window
  :bind (("M-p" . ace-window))
  :config
  (setq aw-dispatch-always t)
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
  :diminish which-key-mode
  :init
  (which-key-setup-side-window-right-bottom)
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.8)
  )

;; @github: lewang/ws-butler
(use-package ws-butler
  :diminish ws-butler-mode
  :init
  (add-hook 'prog-mode-hook 'ws-butler-mode)
  (add-hook 'text-mode 'ws-butler-mode)
  (add-hook 'fundamental-mode 'ws-butler-mode)
  )

(use-package whitespace
  :init
  (add-hook 'before-save-hook #'whitespace-cleanup)
  )

;; @github: magnars/expand-region.el
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region))
  )

;; http://www.dr-qubit.org/Emacs_Undo_Tree_package.html
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  )

;; @github: m2ym/popwin-el
(use-package popwin
  :bind-keymap ("C-z" . popwin:keymap)
  :commands (popwin-mode)
  :init
  (popwin-mode 1)
  :config
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

;; @github: cute-jumper/ace-pinyin
(use-package ace-pinyin
  :diminish ace-pinyin-mode
  :init
  (ace-pinyin-global-mode 1)
  )

(provide 'setup-editor)
;; setup-editor.el ends here
