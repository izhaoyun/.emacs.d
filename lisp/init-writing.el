(use-package org
  :ensure org-plus-contrib
  :pin org
)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  )


(provide 'init-writing)
