;;; -*- lexical-binding: t -*-

(fset 'yes-or-no-p 'y-or-n-p)

(use-package ivy
  :defer t
  :diminish ivy-mode
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("C-c C-r" . ivy-resume))
  :bind (:map ivy-minibuffer-map
              ("C-c o" . ivy-occur)
              ("<tab>" . ivy-alt-done)
              ("C-i"   . ivy-partial-or-done)
              ("C-r"   . ivy-previous-line-or-history)
              ("M-r"   . ivy-reverse-i-search)
              ("<return>" . ivy-alt-done))
  :bind (:map ivy-switch-buffer-map
              ("C-k" . ivy-switch-buffer-kill))
  :init
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-display-style 'fancy
        ivy-use-selectable-prompt t
        ivy-wrap t)
  :init
  (ivy-mode 1)
  )

(use-package swiper
  :defer t
  :bind (("C-c u a" . swiper-all)
         ("C-r" . swiper))
  :bind (:map swiper-map
              ("M-%" . swiper-query-replace)
              ("C-." . swiper-avy)
              ("M-c" . swiper-mc))
  )

(use-package counsel
  :defer t
  :bind (("C-s" . counsel-grep-or-swiper)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("C-x C-b" . counsel-bookmark)
         ("C-c c" . counsel-org-capture)
         ("C-c m" . counsel-imenu)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-c r" . counsel-rg)
         ("C-x l" . counsel-locate)
         ("C-x p" . counsel-mark-ring)
         ("M-y" . counsel-yank-pop)
         ("C-c f" . counsel-git-log)
         ("M-s d" . counsel-dired-jump)
         ("M-s f" . counsel-file-jump))
  :bind (:map help-map
              ("b" . counsel-descbinds)
              ("f" . counsel-describe-function)
              ("l" . counsel-find-library)
              ("v" . counsel-describe-variable)
              ("s" . counsel-info-lookup-symbol)
              ("u" . counsel-unicode-char))
  :bind (:map minibuffer-local-map
              ("C-r" . counsel-minibuf-history))
  :init
  (setq counsel-find-file-at-point t)
  ;; use `rg' instead of `grep'
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
  )

(use-package hydra
  :defer t
  )

(use-package ivy-hydra
  :defer t
  )

(use-package avy
  :defer t
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g c" . avy-goto-char)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)
         ("C-c C-j" . avy-resume))
  :init
  (avy-setup-default)
  )

(use-package avy-zap
  :defer 6
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim))
  )

(use-package ace-link
  :defer 6
  :init
  (ace-link-setup-default)
  )

(use-package window-numbering
  :defer t
  :init
  (window-numbering-mode)
  )

(use-package highlight-symbol
  :defer 7
  :bind (("C-<f3>" . highlight-symbol)
         ("<f3>" . highlight-symbol-next)
         ("S-<f3>" . highlight-symbol-prev)
         ("M-<f3>" . highlight-symbol-query-replace))
  )

(use-package which-key
  :defer t
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.8)
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  )

(use-package ws-butler
  :defer 8
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode)
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  )

(use-package expand-region
  :if window-system
  :defer t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region))
  )

(use-package undo-tree
  :defer 8
  :diminish undo-tree-mode
  :hook ((prog-mode cmake-mode org-mode) . undo-tree-mode)
  :bind (("C-z" . undo)
         ("C-S-z" . redo))
  :init
  (defalias 'redo 'undo-tree-redo)
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  )

(use-package dired-async
  :ensure async
  :defer t
  :hook (dired-mode . dired-async-mode)
  :init
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always)
  )

(use-package hippie-exp
  :ensure nil
  :defer t
  :bind ("M-/" . hippie-expand)
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

(use-package show-marks
  ;; :disabled
  :defer t
  :bind (("C-s-<right>" . forward-mark)
         ("C-s-<left>" . backward-mark)
         ("C-s-<up>" . show-marks))
  )

(use-package server
  :ensure nil
  :defer t
  :commands (server-running-p)
  :init
  (unless (server-running-p)
    (server-start))
  )

(use-package flyspell
  :disabled
  :diminish flyspell-mode
  :defer 18
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :init
  ;; settings for ispell
  (setq ispell-dictionary "en_US")
  )

(provide 'setup-editor)
