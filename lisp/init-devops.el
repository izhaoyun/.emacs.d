;;; -*- lexical-binding: t -*-

(use-package counsel-tramp)

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode)
  )

(use-package docker-tramp
  :disabled
  )

(use-package docker-compose-mode
  :mode ("docker-compose[^/]*\\.yml\\'" . docker-compose-mode)
  )

(use-package docker)

(use-package ansible
  :preface
  (defun ansible/init-company ()
    (set (make-local-variable 'company-backends)
         '(company-capf
           company-yasnippet))
    (company-mode)

    (use-package company-ansible
      :after company
      :init
      (push 'company-ansible company-backends)
      )
    )
  :hook
  (ansible . ansible/init-company)
  )

(use-package ansible-vault)

(use-package vagrant)

(provide 'init-devops)
