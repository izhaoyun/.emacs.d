;;; init-c++ --- C/C++ Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; ----- C/C++ Related Packages -----
(use-package cc-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  :config
  (setq-default c-basic-offset 4)
  ;; enable mode line to display current function.
  (add-hook 'c-mode-common-hook 'which-function-mode)
  ;; hs-minor-mode
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  ;; eldoc-mode
  (add-hook 'c-mode-common-hook #'eldoc-mode)

  (eval-after-load 'company
    '(setq company-backends (delete 'company-semantic company-backends))))

;; @github: leoliu/ggtags
(use-package ggtags
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
  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

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
      'company-backends '(company-irony-c-headers company-irony)))

  ;; @github: Sarcasm/flycheck-irony
  (use-package flycheck-irony
    :after flycheck
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  )

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
