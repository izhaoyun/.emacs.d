;;; -*- lexical-binding: t -*-

(use-package cc-mode
  :defer t
  :mode (("\\.h\\'" . c++-mode))
  :preface
  (defun cc/init-gdb ()
    (setq gdb-show-main t
          gdb-many-windows t)
    )

  (defun cc/init-misc ()
    (diminish 'eldoc-mode)
    (diminish 'modern-c++-font-lock-mode)

    (use-package clang-format :defer t)
    (use-package disaster :defer t)
    )

  (defun cc/init-company ()
    (company-mode t)
    (set (make-local-variable 'company-backends)
         '(company-clang company-gtags company-etags company-capf company-yasnippet))

    (push 'company-c-headers company-backends)

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

  (defun cc/init-flycheck ()
    (flycheck-mode t)
    (use-package flycheck-pkg-config
      :defer t
      )
    )

  :bind (:map c-mode-base-map
              ("C-c h c" . hs-toggle-hiding)
              ("C-c h b" . hs-hide-block)
              ("C-c h s" . hs-show-block)
              ("C-c h a" . hs-hide-all)
              ("C-c h d" . hs-show-all)
              ("C-c h l" . hs-hide-level)
              ("C-c d" . disaster))
  :hook (((c-mode c++-mode) . cc/init-company)
         ((c-mode c++-mode) . cc/init-gdb)
         ((c-mode c++-mode) . cc/init-misc)
         ((c-mode c++-mode) . cc/init-flycheck)
         ((c-mode c++-mode) . which-function-mode)
         ((c-mode c++-mode) . modern-c++-font-lock-mode)
         ((c-mode c++-mode) . eldoc-mode)
         )
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
              ("M-,"   . pop-tag-mark))
  :hook ((c-mode c++-mode) . ggtags-mode)
  :init
  (setq-local large-file-warning-threshold nil)
  :config
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
  (setq-local hippie-expand-try-functions-list
              (cons 'ggtags-try-complete-tag hippie-expand-try-functions-list))
  )

(use-package irony
  :defer t
  :hook ((c++-mode c-mode) . irony-mode)
  :preface
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
    (define-key irony-mode-map [remap complete-symbol] 'counsel-irony)
    )
  :hook ((irony-mode . my-irony-mode-hook)
         (irony-mode . irony-cdb-autosetup-compile-options)
         (irony-mode . irony-eldoc)
         (irony-mode . flycheck-irony-setup))
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
                '(company-dabbrev-code company-keywords company-cmake))
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
  :defer t
  :mode ("\\.lua$" . lua-mode)
  :preface
  (defun my-lua-mode-hook ()
    (set (make-local-variable 'company-backends)
         '(company-etags company-capf company-yasnippet company-dabbrev-code company-keywords))
    (company-mode 1)

    (use-package company-lua
      :defer t
      :init
      (push 'company-lua company-backends)
      )
    )
  :hook (lua-mode . my-lua-mode-hook)
  )

(provide 'init-c++)
