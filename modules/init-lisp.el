(use-package lispy
  :diminish lispy-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode)

  (defun conditionally-enable-lispy ()
	(when (eq this-command 'eval-expression)
	  (lispy-mode 1))
	)
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
  )

(provide 'init-lisp)
