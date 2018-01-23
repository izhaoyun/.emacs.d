;;; -*- lexical-binding: t -*-

(use-package cc-mode
  :mode (("\\.h\\'" . c++-mode))
  :preface
  (defun my-cc-hook ()
    ;; setup gdb
    (setq gdb-show-main t
          gdb-many-windows t)
    ;; setup company-mode
    (setq-local company-backends (delete 'company-semantic company-backends))
    (company-mode 1)

    (use-package company-c-headers
      :init
      (push 'company-c-headers company-backends)
      )
    )
  :bind (:map c-mode-base-map
              ("C-c h c" . hs-toggle-hiding)
              ("C-c h b" . hs-hide-block)
              ("C-c h s" . hs-show-block)
              ("C-c h a" . hs-hide-all)
              ("C-c h d" . hs-show-all)
              ("C-c h l" . hs-hide-level))
  :hook ((c-mode-common . my-cc-hook)
         (c-mode-common . which-function-mode))
  )

(use-package google-c-style
  :defer t
  :hook ((c-mode-common . google-set-c-style)
         (c-mode-common . google-make-newline-indent))
  )

(use-package ggtags
  ;; :diminish ggtags-mode
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
  :hook (c-mode-common . ggtags-mode)
  :config
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
  (setq-local hippie-expand-try-functions-list
              (cons 'ggtags-try-complete-tag hippie-expand-try-functions-list))
  )

(use-package irony
  :defer t
  :hook (c-mode-common . irony-mode)
  :config
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
    (define-key irony-mode-map [remap complete-symbol] 'counsel-irony)
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
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

(use-package rtags
  :defer t
  :hook (((c-mode c++-mode) . rtags-start-process-unless-running)
         ((c-mode c++-mode) . rtags-enable-standard-keybindings))
  :init
  (setq rtags-display-result-backend 'ivy)
  )

(use-package cmake-ide
  :defer t
  :hook ((c-mode c++-mode) . cmake-ide-setup)
  )

(provide 'init-c++)
;;; init-c++.el ends here
