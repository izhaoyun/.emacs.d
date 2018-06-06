;;; -*- lexical-binding: t -*-

(use-package cc-mode
  :mode (("\\.h\\'" . c++-mode))
  :preface
  (defun cc/init-company ()
    (set (make-local-variable 'company-backends)
         '(company-clang
           company-gtags
           company-etags
           company-capf
           company-yasnippet))
    ;; (company-mode t)

    (use-package company-c-headers
      :after company
      :defer t
      :init
      (push 'company-c-headers company-backends)
      )

    (use-package company-irony
      :after company
      :defer t
      )

    (use-package company-irony-c-headers
      :after company
      :defer t
      )

    (push '(company-irony-c-headers company-irony)
          company-backends)
    )

  :bind
  (:map c-mode-base-map
        ("C-c h c" . hs-toggle-hiding)
        ("C-c h b" . hs-hide-block)
        ("C-c h s" . hs-show-block)
        ("C-c h a" . hs-hide-all)
        ("C-c h d" . hs-show-all)
        ("C-c h l" . hs-hide-level))
  :hook (((c-mode c++-mode) . cc/init-company)
         ((c-mode c++-mode) . which-function-mode)
         ((c-mode c++-mode) . eldoc-mode)
         ((c-mode c++-mode) . hs-minor-mode)
         )
  :init
  (setq gdb-show-main t
        gdb-many-windows t)
  )

(use-package google-c-style
  :defer t
  :hook (((c++-mode c-mode) . google-set-c-style)
         ((c++-mode c-mode) . google-make-newline-indent))
  )

(use-package modern-cpp-font-lock
  :defer t
  :diminish modern-c++-font-lock-mode
  :hook ((c-mode c++-mode) . modern-c++-font-lock-mode)
  )

(use-package ggtags
  :defer t
  :bind
  (:map ggtags-mode-map
        ("C-c g s" . ggtags-find-other-symbol)
        ("C-c g h" . ggtags-view-tag-history)
        ("C-c g r" . ggtags-find-reference)
        ("C-c g f" . ggtags-find-file)
        ("C-c g c" . ggtags-create-tags)
        ("C-c g u" . ggtags-update-tags)
        ("C-c g e" . ggtags-save-project-settings)
        ("C-c <"   . ggtags-prev-mark)
        ("C-c >"   . ggtags-next-mark)
        ("C-c M-j" . ggtags-visit-project-root)
        ("M-,"     . pop-tag-mark))
  :hook ((c-mode c++-mode) . ggtags-mode)
  :init
  (setq large-file-warning-threshold nil)
  :config
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
  (setq-local hippie-expand-try-functions-list
              (cons 'ggtags-try-complete-tag hippie-expand-try-functions-list))
  )

(use-package irony
  :defer t
  :hook (((c-mode c++-mode) . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options))
  ;; :commands (counsel-irony)
  :bind (:map irony-mode-map
              ([remap completion-at-point] . counsel-irony)
              ([remap complete-symbol] . counsel-irony))
  )

(use-package irony-eldoc
  :after irony
  :defer t
  :diminish eldoc-mode
  :hook (irony-mode . irony-eldoc)
  )

(use-package flycheck-irony
  :after (irony flycheck)
  :defer t
  :hook (flycheck-mode . flycheck-irony-setup)
  )

(use-package clang-format
  :defer t
  :bind
  (:map c-mode-base-map
        ("C-c u b" . clang-format-buffer)
        ("C-c u i" . clang-format-region))
  )

(use-package disaster
  :defer t
  :bind
  (:map c-mode-base-map
        ("C-c d" . disaster))
  )

(use-package cquery
  :disabled
  :defer t
  :preface
  (defun cc/cquery-enable ()
    (condition-case nil
        (lsp-cquery-enable)
      (user-error nil)))
  :hook ((c++-mode c-mode) . cc/cquery-enable)
  :init
  (setq cquery-executable "/usr/bin/cquery")
  ;; Log file
  (setq cquery-extra-args '("--log-file=/tmp/cq.log"))
  ;; Cache directory, both relative and absolute paths are supported
  (setq cquery-cache-dir ".cquery_cached_index")
  ;; Initialization options
  (setq cquery-extra-init-params '(:cacheFormat "msgpack"))
  )

(use-package demangle-mode :defer t)

(use-package elf-mode :defer t)

(use-package cmake-mode
  :defer t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :preface
  (defun init-cmake-hook ()
    (setq-local company-backends
                '(company-dabbrev-code
                  company-keywords
                  company-cmake))
    (company-mode 1)
    )
  :hook ((cmake-mode . init-cmake-hook)
         (cmake-mode . cmake-font-lock-activate))
  )

(use-package helm-make
  :defer t
  :init
  (setq helm-make-completion-method 'ivy)
  )

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode)
  :preface
  (defun my-lua-mode-hook ()
    (set (make-local-variable 'company-backends)
         '(company-etags
           company-capf
           company-yasnippet
           company-dabbrev-code
           company-keywords))
    (company-mode 1)

    (use-package company-lua
      :defer t
      :init
      (push 'company-lua company-backends)
      )
    )
  :hook (lua-mode . my-lua-mode-hook)
  )

(provide 'init-cc)
