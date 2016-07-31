(defconst my/c++-packages
  '(company-c-headers
    google-c-style
    function-args
    rtags
    irony
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


(provide 'init-c++)
;; init-c++.el ends here.
