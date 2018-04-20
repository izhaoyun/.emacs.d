;;; -*- lexical-binding: t -*-

(use-package dockerfile-mode
  :defer t
  :mode ("Dockerfile\\'" . dockerfile-mode)
  )

(use-package ansible
  :defer t
  )

(provide 'init-devops)
