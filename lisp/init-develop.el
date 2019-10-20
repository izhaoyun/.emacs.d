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

(use-package lsp-mode
  :hook
  ((python-mode c-mode c++-mode) . lsp-deferred)
  )

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        )
  )

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  )

(use-package company-lsp
  :after (company lsp)
  :config
  (push 'company-lsp company-backends)
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
  :if window-system
  :after company
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
  :after yasnippet
  )

(use-package flycheck
  :diminish flycheck-mode
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
  :hook
  (cmake-mode . cmake-font-lock-activate)
  )

(use-package helm-make
  :custom
  (helm-make-completion-method 'ivy)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'init-develop)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
