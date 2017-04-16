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
  :init

  ;; Hooks
  (add-hook 'org-mode-hook #'turn-on-auto-fill)
  (add-hook 'org-mode-hook #'turn-on-font-lock)

  ;; Misc
  (use-package org-bullets
    :disabled t
    :ensure org-plus-contrib
    :init
    (add-hook 'org-mode-hook 'org-bullets-mode))

  :config
  ;; Babel
  ;; -----------------------------------------------------------------------
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)

  (setq org-confirm-babel-evaluate nil)

  (use-package ob-ditaa
    :init
    (add-to-list 'org-babel-load-languages '(ditaa . t) t)
    (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar"))

  (use-package ob-plantuml
    :init
    (add-to-list 'org-babel-load-languages '(plantuml . t) t)
    (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar"))

  ;; @github: zweifisch/ob-http
  (use-package ob-http
    :init
    (add-to-list 'org-babel-load-languages '(http . t) t))

  ;; Exporting
  ;; -----------------------------------------------------------------------
  (setq org-export-default-language "zh-CN")

  (use-package ox-md)
  (use-package ox-gfm)
  (use-package ox-latex
    :config
    (setq org-latex-pdf-process
          '("xelatex -escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -escape -interaction nonstopmode -output-directory %o %f"))
    ;; setup org latex packages list
    (add-to-list 'org-latex-packages-alist '("" "ctex"))
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (add-to-list 'org-latex-packages-alist '("" "color"))
    (add-to-list 'org-latex-packages-alist '("" "geometry"))
    (add-to-list 'org-latex-packages-alist '("" "tabularx"))
    (add-to-list 'org-latex-packages-alist '("" "tabu"))
    (add-to-list 'org-latex-packages-alist '("" "fancyhdr"))
    (add-to-list 'org-latex-packages-alist '("" "natbib"))
    (add-to-list 'org-latex-packages-alist '("" "titlesec"))
    ;; fontify source code
    (setq org-latex-listings 'minted)
    ;; map languages to their minted language counterpart.
    (add-to-list 'org-latex-minted-langs '(python "python"))
    (add-to-list 'org-latex-minted-langs '(sql "sql"))
    (add-to-list 'org-latex-minted-langs '(go "go"))
    ;; setup default minted options
    (setq org-latex-minted-options '(("frame" "single")
                                     ("breaklines" "")))
    )
  )

(use-package plantuml-mode
  :mode (("\\.plantuml\\'" . plantuml-mode))
  :config
  (setq plantuml-jar-path "/opt/plantuml/plantuml.jar"))

(use-package graphviz-dot-mode
  :mode (("\\.dot\\'" . graphviz-dot-mode)))

(provide 'init-org)
;;; init-org.el ends here
