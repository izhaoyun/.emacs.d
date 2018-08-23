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
    (company-mode)

    (use-package company-c-headers
      :after company
      :init
      (push 'company-c-headers company-backends)
      )

    (use-package company-irony
      :after company
      )

    (use-package company-irony-c-headers
      :after company
      )

    (push '(company-irony-c-headers company-irony)
          company-backends)
    )
  :hook
  (((c-mode c++-mode) . cc/init-company)
   ((c-mode c++-mode) . which-function-mode)
   ((c-mode c++-mode) . turn-on-eldoc-mode)
   ((c-mode c++-mode) . hs-minor-mode))
  :init
  (setq gdb-show-main t
        gdb-many-windows t)
  )

(use-package google-c-style
  :hook
  (((c++-mode c-mode) . google-set-c-style)
   ((c++-mode c-mode) . google-make-newline-indent))
  )

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook
  ((c-mode c++-mode) . modern-c++-font-lock-mode)
  )

(use-package ggtags
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
  :hook
  ((c-mode c++-mode) . ggtags-mode)
  :init
  (setq large-file-warning-threshold nil)
  :config
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
  (setq-local hippie-expand-try-functions-list
              (cons 'ggtags-try-complete-tag hippie-expand-try-functions-list))
  )

(use-package counsel-gtags
  :hook
  ((c-mode c++-mode) . counsel-gtags-mode)
  :bind
  (:map counsel-gtags-mode-map
        ("M-s g" . counsel-gtags-find-definition)
        ("M-s r" . counsel-gtags-find-reference)
        ("M-s s" . counsel-gtags-find-symbol)
        ("M-s b" . counsel-gtags-go-backward))
  )

(use-package irony
  :hook
  (((c-mode c++-mode) . irony-mode)
   (irony-mode . irony-cdb-autosetup-compile-options))
  :bind
  (:map irony-mode-map
        ([remap completion-at-point] . counsel-irony)
        ([remap complete-symbol] . counsel-irony))
  )

(use-package irony-eldoc
  :hook (irony-mode . irony-eldoc)
  )

(use-package flycheck-irony
  :hook (flycheck-mode . flycheck-irony-setup)
  )

(use-package flycheck-clang-analyzer
  :hook ((c-mode c++-mode) . flycheck-clang-analyzer-setup)
  )

(use-package clang-format
  :bind
  (:map c-mode-base-map
        ("C-c u b" . clang-format-buffer)
        ("C-c u i" . clang-format-region))
  )

(use-package disaster
  :bind
  (:map c-mode-base-map
        ("C-c d" . disaster))
  )

(use-package demangle-mode)

(use-package elf-mode)

(use-package cmake-mode
  :mode
  (("CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode))
  :preface
  (defun cmake/init-company ()
    (setq-local company-backends
                '(company-dabbrev-code
                  company-keywords
                  company-cmake))
    (company-mode 1)
    )
  :hook
  ((cmake-mode . cmake/init-company)
   (cmake-mode . cmake-font-lock-activate))
  )

(use-package helm-make
  :init
  (setq helm-make-completion-method 'ivy)
  )

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode)
  :preface
  (defun lua/init-company ()
    (set (make-local-variable 'company-backends)
         '(company-etags
           company-capf
           company-yasnippet
           company-dabbrev-code
           company-keywords))
    (company-mode)

    (use-package company-lua
      :after company
      :init
      (push 'company-lua company-backends)
      )
    )
  :hook
  (lua-mode . lua/init-company)
  )

(provide 'init-cc)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init-cc.el ends here
