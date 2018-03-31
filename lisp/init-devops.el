;;; -*- lexical-binding: t -*-

(use-package dockerfile-mode
  :defer t
  :mode ("Dockerfile\\'" . dockerfile-mode)
  )

(use-package ansible
  :defer t
  :load-path "site-lisp/ansible"
  :init
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
  )

(provide 'init-devops)
