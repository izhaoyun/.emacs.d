;;; init-c++ --- C++ Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package cc-mode
  :mode (("\\.h\\'" . c++-mode))
  :commands (eldoc-mode)
  :init
  (add-hook 'c-mode-common-hook 'init-c-c++/find-file)
  (add-hook 'c-mode-common-hook 'which-function-mode)
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  ;; @github: https://github.com/google/styleguide/blob/gh-pages/google-c-style.el
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)
  (add-hook 'c-mode-common-hook 'init-c-c++/setup-gdb)
  :config
  (add-hook 'c-mode-common-hook #'eldoc-mode)
  :bind (:map c-mode-base-map
              ("C-c t" . ff-find-other-file)
              ("C-c h c" . hs-toggle-hiding)
              ("C-c h b" . hs-hide-block)
              ("C-c h s" . hs-show-block)
              ("C-c h a" . hs-hide-all)
              ("C-c h d" . hs-show-all)
              ("C-c h l" . hs-hide-level))
  )

;;;###autoload
(defun init-c-c++/find-file ()
  (use-package find-file
    :init
    (setq-default ff-always-in-other-window t)
    )
  )

;;;###autoload
(defun init-c-c++/setup-gdb ()
  (use-package gdb-mi
    :init
    (setq gdb-many-windows t)
    (setq gdb-show-main t)
    )
  )

;;;###autoload
(defun init-c-c++/setup-rtags ()
  ;; @github: Andersbakken/rtags
  (use-package rtags
    :commands (rtags-start-process-unless-running
               rtags-enable-standard-keybindings)
    :init
    (rtags-start-process-unless-running)
    (rtags-enable-standard-keybindings c-mode-base-map)
    )
  ;; @github: Andersbakken/rtags
  (use-package ivy-rtags
    :init
    (setq rtags-display-result-backend 'ivy)
    )
  )
(add-hook 'c-mode-common-hook 'init-c-c++/setup-rtags)

;;;###autoload
(defun init-c-c++/setup-company-c-headers ()
  ;; @github: randomphrase/company-c-headers
  (use-package company-c-headers
    :after company
    :init
    (push 'company-c-headers company-backends)
    )
  )
(add-hook 'c-mode-common-hook 'init-c-c++/setup-company-c-headers)

;;;###autoload
(defun init-c-c++/setup-ggtags ()
  ;; @github: leoliu/ggtags
  (use-package ggtags
    :commands (ggtags-eldoc-function
               ggtags-find-other-symbol
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
    :init
    (ggtags-mode t)
    :config
    (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
    (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
    (setq-local hippie-expand-try-functions-list
                (cons 'ggtags-try-complete-tag hippie-expand-try-functions-list))
    (use-package company-gtags
      :after company
      :init
      (push 'company-gtags company-backends)
      )
    )
  )
(add-hook 'c-mode-common-hook 'init-c-c++/setup-ggtags)

;;;###autoload
(defun init-c-c++/setup-irony ()
  ;; @github: Sarcasm/irony-mode
  (use-package irony
    :init
    (irony-mode t)
    :config
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
      (define-key irony-mode-map [remap complete-symbol] 'counsel-irony)
      )
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    )
  ;; @github: hotpxl/company-irony-c-headers
  (use-package company-irony-c-headers
    :after company
    :init
    (push 'company-irony-c-headers company-backends)
    )
  ;; @github: Sarcasm/company-irony
  (use-package company-irony
    :after company
    :init
    (push 'company-irony company-backends)
    )
  )
(add-hook 'c-mode-common-hook 'init-c-c++/setup-irony)

;; https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :init
  ;; @github: Lindydancer/cmake-font-lock
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
  (use-package company-cmake
    :after company
    :init
    (push 'company-cmake company-backends)
    )
  )

(provide 'init-c++)
;;; init-c++.el ends here
