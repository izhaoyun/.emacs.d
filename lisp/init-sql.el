(use-package sql
  :ensure nil
  :defer t
  :config
  (sql-set-product-feature
   'mysql :prompt-regexp "^\\(MariaDB\\|MySQL\\) \\[[_a-zA-Z()]*\\]> ")
  )

(use-package edbi
  :defer t
  )

(use-package edbi-minor-mode
  :defer t
  :hook (sql-mode . edbi-minor-mode)
  )

(use-package edbi-database-url
  :defer t
  )

(provide 'init-sql)
