;;; init-org --- Org-mode Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org\\'" . org-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-iswitch)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config

  (use-package ob-ditaa
    :config
    (add-to-list 'org-babel-load-languages '(ditaa . t) t)
    (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar"))

  (use-package ob-plantuml
    :config
    (add-to-list 'org-babel-load-languages '(plantuml . t) t)
    (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar"))

  (use-package org-bullets
    :init
    (org-bullets-mode 1))

  )

(use-package plantuml-mode
  :mode (("\\.plantuml\\'" . plantuml-mode))
  :config
  (setq plantuml-jar-path "/opt/plantuml/plantuml.jar"))

(use-package graphviz-dot-mode
  :mode (("\\.diag\\'"      . graphviz-dot-mode)
         ("\\.blockdiag\\'" . graphviz-dot-mode)
         ("\\.nwdiag\\'"    . graphviz-dot-mode)
         ("\\.rackdiag\\'"  . graphviz-dot-mode)
         ("\\.dot\\'"       . graphviz-dot-mode)
         ("\\.gv\\'"        . graphviz-dot-mode)))

(provide 'init-org)
;;; init-org.el ends here
