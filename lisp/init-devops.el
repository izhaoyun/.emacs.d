;;; -*- lexical-binding: t -*-

(use-package counsel-tramp
  :defer t
  )

(use-package dockerfile-mode
  :defer t
  :mode ("Dockerfile\\'" . dockerfile-mode)
  )

(use-package docker-tramp
  :disabled
  :defer t
  )

(use-package docker-compose-mode
  :defer t
  :mode ("docker-compose[^/]*\\.yml\\'" . docker-compose-mode)
  )

(use-package docker
  :defer t
  )

(use-package ansible
  :defer t
  :preface
  (defun ansible/init-company ()
    (set (make-local-variable 'company-backends)
         '(company-capf
           company-yasnippet))
    (company-mode)

    (use-package company-ansible
      :after (ansible company)
      :defer t
      :init
      (push 'company-ansible company-backends)
      )
    )
  :hook (ansible . ansible/init-company)
  )

(use-package vagrant
  :defer t
  )

(provide 'init-devops)
