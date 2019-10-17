(use-package ivy
  :diminish ivy-mode
  :bind
  (("C-x b"   . ivy-switch-buffer)
   ("C-c C-r" . ivy-resume))
  :bind
  (:map ivy-minibuffer-map
        ("C-c o" . ivy-occur)
        ("<tab>" . ivy-alt-done)
        ("C-i"   . ivy-partial-or-done)
        ("C-r"   . ivy-previous-line-or-history)
        ("M-r"   . ivy-reverse-i-search)
        ("<return>" . ivy-alt-done))
  :init
  (setq ivy-count-format "(%d/%d) "
        ivy-display-style 'fancy
        ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t
        ivy-wrap t)
  (ivy-mode 1)
  :config
  (ivy-set-occur 'swiper 'swiper-occur)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
  )

(use-package swiper
  :after ivy
  :demand t
  :bind
  (("C-c u a" . swiper-all)
   ("C-r" . swiper))
  :bind
  (:map swiper-map
        ("M-q" . swiper-query-replace)
        ("C-l" . swiper-recenter-top-bottom)
        ("C-'" . swiper-avy)
        ("M-c" . swiper-mc))
  )

(use-package counsel
  :after swiper
  :bind
  (("C-s" . counsel-grep-or-swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-x r b" . counsel-bookmark)
   ("C-x r d" . counsel-bookmarked-directory)
   ("C-x C-b" . counsel-ibuffer)
   ("C-c c" . counsel-org-capture)
   ("<f2> m" . counsel-imenu)
   ("<f2> f" . counsel-git)
   ("<f2> g" . counsel-git-grep)
   ("<f2> s" . counsel-stash)
   ("<f2> b" . counsel-switch-to-shell-buffer)
   ("<f2> a" . counsel-ag)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)
   ("C-x m" . counsel-mark-ring)
   ("M-y" . counsel-yank-pop)
   ("C-c f" . counsel-git-log)
   ("M-s d" . counsel-dired-jump)
   ("M-s f" . counsel-file-jump)
   ("C-h a" . counsel-apropos)
   ("C-h g" . counsel-info-lookup-symbol)
   ("C-h u" . counsel-unicode-char)
   ("C-h l" . counsel-find-library)
   ("C-h L" . counsel-load-library)
   ("C-h b" . counsel-descbinds)
   ("C-h w" . woman))
  :bind
  (:map minibuffer-local-map
        ("C-r" . counsel-minibuf-history))
  :init
  (setq counsel-find-file-at-point t)
  )

(use-package avy
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ("M-g c" . avy-goto-char)
   ("M-g f" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)
   ("C-c C-j" . avy-resume))
  :init
  (avy-setup-default)
  )

(use-package ace-pinyin
  :after avy
  :diminish ace-pinyin-mode
  )

(use-package ace-window
  :after avy
  :bind
  ("M-o" . ace-window)
  )

(use-package avy-zap
  :after avy
  :bind
  (("M-z" . avy-zap-to-char-dwim)
   ("M-Z" . avy-zap-up-to-char-dwim))
  )

(use-package ace-link
  :after avy
  :init
  (ace-link-setup-default)
  )

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :init
  (volatile-highlights-mode)
  )

(use-package highlight-symbol
  :if window-system
  :bind
  (("C-<f3>" . highlight-symbol)
   ("<f3>"   . highlight-symbol-next)
   ("S-<f3>" . highlight-symbol-prev)
   ("M-<f3>" . highlight-symbol-query-replace))
  )

(use-package which-key
  :diminish which-key-mode
  :hook
  (after-init . which-key-mode)
  :init
  (which-key-setup-side-window-right-bottom)
  )

(use-package ws-butler
  :diminish ws-butler-mode
  :hook
  (prog-mode . ws-butler-mode)
  )

(use-package expand-region
  ;; :if window-system
  :bind
  (("<f2> =" . er/expand-region)
   ("<f2> -" . er/contract-region))
  )

(use-package undo-tree
  :diminish undo-tree-mode
  :hook
  ((prog-mode cmake-mode org-mode) . undo-tree-mode)
  :bind
  (("<f2> z" . undo)
   ("<f2> c" . redo))
  :init
  (defalias 'redo 'undo-tree-redo)
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  )

(use-package dired-async
  :ensure async
  :hook
  (dired-mode . dired-async-mode)
  )

(use-package hippie-exp
  :ensure nil
  :bind
  ("M-/" . hippie-expand)
  )

(use-package paren
  :ensure nil
  :hook
  (prog-mode . show-paren-mode)
  )

(use-package winner
  :ensure nil
  :hook
  (after-init . winner-mode)
  )

(use-package diminish)

(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  :custom
  ((initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
   (dashboard-items '((recents . 5)
                      (projects . 5)
                      (bookmarks . 5)
                      (agenda . 5)
                      (registers . 5)))
   (dashboard-footer "Happy coding!"))
  )

(provide 'init-editor)
