;; -*- lexical-binding: t; -*-

(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  )

(use-package counsel-projectile
  :after (projectile counsel)
  :init
  (counsel-projectile-mode)
  )

(use-package company
  :diminish company-mode
  :bind
  ("C-c y" . company-yasnippet)
  :hook
  (after-init . global-company-mode)
  :custom
  ((company-tooltip-limit 20)
   (company-idle-delay .3)
   (company-echo-delay 0)
   (company-show-numbers t))
  )

(use-package company-quickhelp
  ;; :if window-system
  :requires company
  :bind
  (:map company-active-map
        ("M-h" . company-quickhelp-manual-begin))
  :hook
  (company-mode . company-quickhelp-mode)
  :custom
  (company-quickhelp-delay nil)
  )

(use-package yasnippet
  :diminish yas-minor-mode
  :bind
  (:map yas-minor-mode-map
        ("<f2> x" . yas-expand)
        ("<f2> i" . yas-insert-snippet))
  :hook
  (after-init . yas-global-mode)
  )

(use-package yasnippet-snippets
  :requires yasnippet
  )

(use-package flycheck
  :diminish flycheck-mode
  :hook
  ((python-mode c-mode c++-mode go-mode) . flycheck-mode)
  )

(use-package lsp-mode
  :hook
  ((python-mode c-mode c++-mode) . lsp-deferred)
  :custom
  ((lsp-prefer-flymake nil))
  )

(use-package lsp-ui
  :requires lsp-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ("C-c C-l ." . lsp-ui-peek-find-definitions)
        ("C-c C-l ?" . lsp-ui-peek-find-references)
        ("C-c C-l r" . lsp-rename)
        ("C-c C-l i" . lsp-ui-peek-find-implementation)
        )
  :custom
  ((lsp-ui-doc-enable t)
   (lsp-ui-flycheck-enable t))
  )

(use-package lsp-ui-imenu
  :ensure lsp-ui
  )

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  )

(use-package company-lsp
  :requires (company lsp)
  :config
  (push 'company-lsp company-backends)
  )

(use-package dap-mode
  :hook
  ((go-mode . dap-mode)
   (go-mode . dap-ui-mode))
  )

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  )

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  )

(use-package comment-dwim-2
  :bind
  ("M-;" . comment-dwim-2)
  )

(use-package highlight-indent-guides
  :if window-system
  :diminish highlight-indent-guides-mode
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :custom
  ((highlight-indent-guides-method 'character)
   (highlight-indent-guides-character ?\|)
   (highlight-indent-guides-auto-odd-face-perc 15)
   (highlight-indent-guides-auto-even-face-perc 15)
   (highlight-indent-guides-auto-character-face-perc 20))
  )

(use-package clean-aindent-mode
  :hook prog-mode
  :custom
  (clean-aindent-is-simple-indent t)
  )

(use-package dtrt-indent
  :diminish dtrt-indent-mode
  :hook
  (prog-mode . dtrt-indent-mode)
  :custom
  (dtrt-indent-verbosity 0)
  )

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook
  ((emacs-lisp-mode cmake-mode) . aggressive-indent-mode)
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line)))))
  )

(use-package rainbow-delimiters
  :hook
  ((prog-mode cmake-mode) . rainbow-delimiters-mode)
  )

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  )

(use-package dumb-jump
  :bind
  (("<f2> o" . dumb-jump-go-other-window))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cc-mode
  :demand t
  )

(use-package google-c-style
  :hook
  (((c-mode c++-mode) . google-set-c-style)
   ((c-mode c++-mode) . google-make-newline-indent))
  )

(use-package modern-cpp-font-lock
  :diminish modern-cpp-font-lock-mode
  :hook
  ((c-mode c++-mode) . modern-c++-font-lock-mode)
  )

(use-package clang-format
  :after cc-mode
  :bind
  (:map c-mode-base-map
        ("C-c u b" . clang-format-buffer)
        ("C-c u r" . clang-format-region))
  )

(use-package disaster
  :after cc-mode
  :bind
  (:map c-mode-base-map
        ("C-c u d" . disaster))
  )

(use-package elf-mode
  :defer t
  )

(use-package demangle-mode
  :defer t
  )

(use-package cmake-mode
  :mode
  (("CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode))
  )

(use-package cmake-font-lock
  :after cmake-mode
  :hook
  (cmake-mode . cmake-font-lock-activate)
  )

(use-package helm-make
  :custom
  (helm-make-completion-method 'ivy)
  )

(use-package dap-lldb
  :ensure dap-mode
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package go-mode
  :mode
  (("\\.go\\'" . go-mode)
   ("go\\.mod\\'" . go-dot-mod-mode))
  :hook
  ((before-save . gofmt-before-save))
  )

(use-package company-go
  :requires (company go-mode)
  :init
  (push 'company-go company-backends)
  )

(use-package dap-go
  :ensure dap-mode
  :after go-mode
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dap-python
  :ensure dap-mode
  :after python-mode
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lispy
  :diminish lispy-mode
  :hook
  (emacs-lisp-mode . lispy-mode)
  :config
  (unbind-key "M-o" lispy-mode-map)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-develop)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
