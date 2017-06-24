;;; init-c++ --- C/C++ Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; ----- C/C++ Related Packages -----
(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode))
  :config
  (setq-default c-basic-offset 4)
  ;; source code completion using clang
  (eval-after-load "company"
    '(progn
       (setq company-backends (delete 'company-semantic company-backends))
       (define-key c-mode-map [(tab)] 'company-complete)
       (define-key c++-mode-map [(tab)] 'company-complete)))
  ;; enable mode line to display current function.
  (add-hook 'c-mode-common-hook 'which-function-mode)
  ;; display function interface in the minibuffer.
  (add-hook 'c-mode-common-hook 'semantic-idle-summary-mode)
  ;; hs-minor-mode
  (add-hook 'c-mode-common-hook 'hs-minor-mode))

(use-package sr-speedbar
  :bind
  ;; NB: 's' is the 'Win' key instead of the 'shift' key.
  ("s-<f3>" . sr-speedbar-toggle)
  :config
  (setq speedbar-show-unknown-files t))

;; @github: abo-abo/function-args
(use-package function-args
  :init
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook #'fa-config-default)))

;; @github: leoliu/ggtags
(use-package gtags
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode)
                (ggtags-mode 1))))
  :commands (ggtags-find-other-symbol
             ggtags-view-tag-history
             ggtags-find-reference
             ggtags-find-file
             ggtags-create-tags
             ggtags-update-tags)
  :config
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
  (setq-local hippie-expand-try-functions-list
              (cons 'ggtags-try-complete-tag hippie-expand-try-functions-list)))

;; @github: Sarcasm/irony-mode
(use-package irony
  :init
  (dolist (hook '(c++-mode-hook c-mode-hook objc-mode-hook))
    (add-hook hook (lambda () (irony-mode 1))))
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function.
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  ;; @github: hotpxl/company-irony-c-headers
  (use-package company-irony-c-headers)

  ;; @github: Sarcasm/company-irony
  (use-package company-irony)

  (eval-after-load 'company
    '(add-to-list
      'company-backends '(company-irony-c-headers company-irony))))


;; @github: Sarcasm/flycheck-irony
(use-package flycheck-irony
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; @github: randomphrase/company-c-headers
(use-package company-c-headers
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))

;; @github: abo-abo/elf-mode
(use-package elf-mode
  :mode ("\\.\\(?:a\\|so\\)\\'" . elf-mode))

;; ----- Makefile Related Packages -----
(use-package cmake-mode
  :mode (("CMakeLists.txt\\'" . cmake-mode)
         ("\\.cmake\\'"       . cmake-mode))
  :config
  (use-package cmake-font-lock
    :init
    (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)))

(provide 'init-c++)
;;; init-c++.el ends here
