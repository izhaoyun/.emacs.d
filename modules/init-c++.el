(defconst my/c++-packages
  '(company-c-headers
    irony
    company-irony
    flycheck-irony
    google-c-style
    flycheck-google-cpplint
    irony-eldoc
    rtags
    ggtags
    )
  )

(install-packages my/c++-packages)

(use-package cc-mode
  :mode ("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
  :preface
  (defun my/init-hs-minor-mode ()
    (hs-minor-mode 1)
    (diminish 'hs-minor-mode)
    )
  :config
  (defun my/c-mode-common-hook ()
    (setq-default indent-tabs-mode nil)
    (define-key c-mode-map  [(tab)] 'company-complete)
    (define-key c++-mode-map  [(tab)] 'company-complete)

	(setq-default indent-tabs-mode nil)
    )
  (use-package google-c-style
    :init
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    (add-hook 'c-mode-common-hook 'google-make-newline-indent)
    )
  (add-hook 'c-mode-common-hook 'my/init-hs-minor-mode)
  (add-hook 'c-mode-common-hook 'my/c-mode-common-hook)
  )

(use-package irony
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (setq irony-additional-clang-options '("-std=c++11"))
  :config
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async)
    )
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (use-package company-irony
    :init
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
    :config
    (add-to-list 'company-backends 'company-irony)
    )

  (use-package flycheck-irony
    :init
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
    )

  (use-package irony-eldoc
    :init
    (add-hook 'irony-mode-hook 'irony-eldoc)
    )
  )

(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers)
  )

(defun c++/init-flycheck-google-cpplint ()
  (use-package flycheck-google-cpplint
    :after flycheck
    :init
    (setq flycheck-googlelint-verbose "3")
    (setq flycheck-googlelint-filter "-whitespace,+whitespace/braces")
    (setq flycheck-googlelint-root "project/src")
    (setq flycheck-googlelint-linelength "120")
    :config
    (flycheck-add-next-checker 'c/c++-cppcheck
                               '(warning . c/c++-googlelint))
    )
  )
(add-hook 'c-mode-common-hook 'c++/init-flycheck-google-cpplint)

(use-package rtags
  :defer t
  :init
  :config
  ;; TODO: fix the process to initialize rtags.
  (rtags-enable-standard-keybindings c-mode-base-map)
  )

(use-package ggtags
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1))))
  :commands (ggtags-find-other-symbol)
  :config
  (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  (setq-local hippie-expand-try-functions-list
              (cons 'ggtags-try-complete-tag hippie-expand-try-functions-list))

  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
  )

(provide 'init-c++)
;;; init-c++.el ends here
