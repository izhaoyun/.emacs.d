(defconst my/c++-packages
  '(company-c-headers
    rtags
    irony
    company-irony
    flycheck-irony
    ggtags
    cmake-ide
    ))

(install-packages my/c++-packages)

(use-package cc-mode
  :mode ("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
  :preface
  (defun my/init-hs-minor-mode ()
    (hs-minor-mode 1)
    (diminish 'hs-minor-mode)
    )
  :init
  (setq indent-tabs-mode nil)
  :config
  (setq-default c-basic-offset 4)
  (setq-default c-default-style "linux")
  (add-hook 'c-mode-common-hook 'my/init-hs-minor-mode)  
  )

(use-package irony
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

(use-package company-irony
  :init
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))
  )

(defun my/init-rtags ()
  (interactive)

  (use-package rtags
    :defer t
    :commands (rtags-start-process-unless-running)
    :init
    (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
    :config
    (setq rtags-completions-enabled t)
    (rtags-enable-standard-keybindings c-mode-base-map)

    (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
    (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
    (define-key c-mode-base-map (kbd "M-;") 'rtags-find-file)
    (define-key c-mode-base-map (kbd "C-.") 'rtags-find-symbol)
    (define-key c-mode-base-map (kbd "C-,") 'rtags-find-references)
    (define-key c-mode-base-map (kbd "C-<") 'rtags-find-virtuals-at-point)
    (define-key c-mode-base-map (kbd "M-i") 'rtags-imenu)
    )

  (use-package company-rtags
    :defer t
    :ensure rtags
    :config
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-rtags))
    )
  )

(defun my/init-company-c-headers ()
  (use-package company-c-headers
    :config
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-c-headers))
    )
  )
(add-hook 'c-mode-common-hook 'my/init-company-c-headers)

(provide 'init-c++)
;; init-c++.el ends here.
