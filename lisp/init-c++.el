;;; -*- lexical-binding: t -*-

(use-package cc-mode
  :defer t
  :mode (("\\.h\\'" . c++-mode))
  :preface
  (defun my-cc-hook ()
    (setq gdb-show-main t
          gdb-many-windows t)
    )

  (defun cc/init-company ()
    (company-mode t)
    (set (make-local-variable 'company-backends)
         '(company-clang company-gtags company-etags company-capf company-yasnippet))

    (use-package company-c-headers
      :defer t
      :init
      (push 'company-c-headers company-backends)
      )

    (use-package company-irony
      :defer t
      :init
      (push 'company-irony company-backends)
      )

    (use-package company-irony-c-headers
      :defer t
      :init
      (push 'company-irony-c-headers company-backends)
      )
    )

  (defun cc/init-counsel ()
    (use-package counsel-etags
      :defer t
      :init
      (setq tags-revert-without-query t
            large-file-warning-threshold nil)
      :config
      ;; counsel-etags-ignore-directories does NOT support wildcast
      (add-to-list 'counsel-etags-ignore-directories "build_clang")
      (add-to-list 'counsel-etags-ignore-directories "build_clang")
      ;; counsel-etags-ignore-filenames supports wildcast
      (add-to-list 'counsel-etags-ignore-filenames "TAGS")
      (add-to-list 'counsel-etags-ignore-filenames "*.json")
      )
    )

  :bind (:map c-mode-base-map
              ("C-c h c" . hs-toggle-hiding)
              ("C-c h b" . hs-hide-block)
              ("C-c h s" . hs-show-block)
              ("C-c h a" . hs-hide-all)
              ("C-c h d" . hs-show-all)
              ("C-c h l" . hs-hide-level))
  :hook (((c-mode c++-mode) . cc/init-counsel)
         ((c-mode c++-mode) . cc/init-company)
         ((c-mode c++-mode) . my-cc-hook)
         ((c-mode c++-mode) . which-function-mode)
         ((c-mode c++-mode) . eldoc-mode))
  )

(use-package google-c-style
  :defer t
  :hook (((c++-mode c-mode) . google-set-c-style)
         ((c++-mode c-mode) . google-make-newline-indent))
  )

(use-package ggtags
  :defer t
  :bind (:map ggtags-mode-map
              ("C-c g s" . ggtags-find-other-symbol)
              ("C-c g h" . ggtags-view-tag-history)
              ("C-c g r" . ggtags-find-reference)
              ("C-c g f" . ggtags-find-file)
              ("C-c g c" . ggtags-create-tags)
              ("C-c g u" . ggtags-update-tags)
              ("C-c g e" . ggtags-save-project-settings)
              ("C-c <" . ggtags-prev-mark)
              ("C-c >" . ggtags-next-mark)
              ("C-c M-j" . ggtags-visit-project-root)
              ("M-,"   . pop-tag-mark)
              ;; counsel-etags
              ("C-c g g" . counsel-etags-grep)
              ("C-c g p" . counsel-etags-grep-symbol-at-point)
              ;; call-graph
              ("C-c g l" . call-graph)
              )
  :hook ((c-mode c++-mode) . ggtags-mode)
  :config
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
  (setq-local hippie-expand-try-functions-list
              (cons 'ggtags-try-complete-tag hippie-expand-try-functions-list))
  )

(use-package xcscope
  :defer 15
  :hook ((c-mode c++-mode) . cscope-setup)
  :init
  (setq cscope-program "gtags-cscope")
  )

(use-package irony
  :defer t
  :hook ((c++-mode c-mode) . irony-mode)
  :preface
  (defun cc/my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
    (define-key irony-mode-map [remap complete-symbol] 'counsel-irony)
    )
  :hook ((irony-mode . cc/my-irony-mode-hook)
         (irony-mode . irony-cdb-autosetup-compile-options))
  )

(use-package flycheck-irony
  :defer t
  :hook (irony-mode . flycheck-irony-setup)
  )

(use-package irony-eldoc
  :defer t
  :hook (irony-mode . irony-eldoc)
  )

(use-package stickyfunc-enhance
  :defer 12
  :hook ((c-mode c++-mode) . semantic-mode)
  :init
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  )

(use-package function-args
  :defer 12
  :hook ((c-mode c++-mode) . fa-config-default)
  )

(use-package demangle-mode
  :disabled
  )

(use-package elf-mode
  :disabled
  )

(use-package call-graph
  :disabled
  )

(provide 'init-c++)
