(defconst my/c++-packages
  '(company-c-headers
    google-c-style
    function-args
    rtags
    irony
    company-irony
    flycheck-irony
    ggtags
    ))

(install-packages my/c++-packages)

(use-package cc-mode
  :mode ("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
  :preface
  (defun my/c++-mode-hook ()
    (setq indent-tabs-mode nil)
    (setq tab-width 4)
    ;; (google-set-c-style)
    ;; (google-make-newline-indent)
    )
  :init
  (add-hook 'c-mode-common-hook 'my/c++-mode-hook)
  :config
  ;; HideShow
  (add-hook 'c-mode-common-hook
	    '(lambda ()
	       (hs-minor-mode 1)
	       (diminish 'hs-minor-mode)))
  )

(use-package irony
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  :config
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
  (require 'rtags)
  (require 'company-rtags)

  (rtags-start-process-unless-running)
  
  (setq rtags-completions-enabled t)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-rtags))

  (rtags-enable-standard-keybindings c-mode-base-map)

  (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
  (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
  (define-key c-mode-base-map (kbd "M-;") 'rtags-find-file)
  (define-key c-mode-base-map (kbd "C-.") 'rtags-find-symbol)
  (define-key c-mode-base-map (kbd "C-,") 'rtags-find-references)
  (define-key c-mode-base-map (kbd "C-<") 'rtags-find-virtuals-at-point)
  (define-key c-mode-base-map (kbd "M-i") 'rtags-imenu)
  )
(add-hook 'c-mode-common-hook 'my/init-rtags)


(defun my/init-company-c-headers ()
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-c-headers))
  )

(provide 'init-c++)
;; init-c++.el ends here.
