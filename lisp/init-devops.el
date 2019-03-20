;;; -*- lexical-binding: t -*-

(use-package counsel-tramp)

(use-package docker)

(use-package docker-tramp)

(use-package dockerfile-mode)

(use-package docker-compose-mode)

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

(provide 'init-devops)
