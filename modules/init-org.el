;;; init-org --- Org-mode Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-iswitch)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)))

(use-package org-bullets
  :after org
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; @github: skuro/plantuml-mode
(use-package plantuml-mode
  :mode (("\\.plantuml\\'" . plantuml-mode))
  :config
  (setq plantuml-jar-path "/opt/plantuml/plantuml.jar")
  ;; @github: alexmurray/flycheck-plantuml
  (use-package flycheck-plantuml
    :after flycheck
    :init
    (flycheck-plantuml-setup)))

(use-package graphviz-dot-mode
  :mode (("\\.diag\\'"      . graphviz-dot-mode)
         ("\\.blockdiag\\'" . graphviz-dot-mode)
         ("\\.nwdiag\\'"    . graphviz-dot-mode)
         ("\\.rackdiag\\'"  . graphviz-dot-mode)
         ("\\.dot\\'"       . graphviz-dot-mode)
         ("\\.gv\\'"        . graphviz-dot-mode)))

(provide 'init-org)
;;; init-org.el ends here
