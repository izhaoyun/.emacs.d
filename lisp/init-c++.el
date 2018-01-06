;;; init-c++ --- C++ Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package find-file
  :init
  (setq-default ff-always-in-other-window t)
  )

(use-package cc-mode
  :mode (("\\.h\\'" . c++-mode))
  :commands (eldoc-mode)
  :config
  (add-hook 'c-mode-common-hook 'which-function-mode)
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  (add-hook 'c-mode-common-hook #'eldoc-mode)
  ;; @github: https://github.com/google/styleguide/blob/gh-pages/google-c-style.el
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)

  ;; @github: randomphrase/company-c-headers
  (push 'company-c-headers company-backends)

  :bind (:map c-mode-base-map
              ("C-c t" . ff-find-other-file)
              ("C-c h c" . hs-toggle-hiding)
              ("C-c h b" . hs-hide-block)
              ("C-c h s" . hs-show-block)
              ("C-c h a" . hs-hide-all)
              ("C-c h d" . hs-show-all)
              ("C-c h l" . hs-hide-level))
  )

;; @github: leoliu/ggtags
(use-package ggtags
  :commands (ggtags-eldoc-function)
  :init
  (dolist (hook '(c++-mode-hook c-mode-hook))
    (add-hook hook (lambda () (ggtags-mode 1))))
  :commands (ggtags-find-other-symbol
             ggtags-view-tag-history
             ggtags-find-reference
             ggtags-find-file
             ggtags-create-tags
             ggtags-update-tags)
  :bind (:map ggtags-mode-map
              ("C-c g s" . ggtags-find-other-symbol)
              ("C-c g h" . ggtags-view-tag-history)
              ("C-c g r" . ggtags-find-reference)
              ("C-c g f" . ggtags-find-file)
              ("C-c g c" . ggtags-create-tags)
              ("C-c g u" . ggtags-update-tags)
              ("C-c <" . ggtags-prev-mark)
              ("C-c >" . ggtags-next-mark)
              ("M-,"   . pop-tag-mark))
  :config
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
  (setq-local hippie-expand-try-functions-list
              (cons 'ggtags-try-complete-tag hippie-expand-try-functions-list))
  )

;; @github: Sarcasm/irony-mode
(use-package irony
  :diminish irony-mode
  :init
  (dolist (hook '(c++-mode-hook c-mode-hook objc-mode-hook))
    (add-hook hook (lambda () (irony-mode 1))))
  :config
;;;###autoload
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
    (define-key irony-mode-map [remap complete-symbol] 'counsel-irony)
    )
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  ;; @github: hotpxl/company-irony-c-headers
  (push 'company-irony-c-headers company-backends)
  ;; @github: Sarcasm/company-irony
  (push 'company-irony company-backends)
  ;; @github: Sarcasm/flycheck-irony
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
  )

(use-package cmake-mode
  :mode (("CMakeLists.txt\\'" . cmake-mode)
         ("\\.cmake\\'"       . cmake-mode))
  :config
  ;; cmake-font-lock
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
  )

(provide 'init-c++)
;;; init-c++.el ends here
