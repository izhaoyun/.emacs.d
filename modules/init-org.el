;;; -*- lexical-binding: t; -*-

(use-package org
  :mode (("\\.org$" . org-mode))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-iswitch)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :commands (transient-mark-mode)
  :init
  (use-package htmlize)
  (transient-mark-mode 1)

  (setq org-emphasis-regexp-components
        (list (concat " \t('\"{"            "[:nonascii:]")
              (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
              " \t\r\n,\"'"
              "."
              1))

  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.pdf\\'" . "evince %s")))
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  :config
  (use-package ox
    :init
    (setq org-export-default-language "zh-CN")
    :config
    (use-package ox-beamer)

    (use-package ox-html
      :config
      (defadvice org-html-paragraph (before org-html-paragraph-advice
                                            (paragraph contents info) activate)
        "Join consecutive Chinese lines into a single long line
without unwanted space when exporting org-mode to html."
        (let* ((origin-contents (ad-get-arg 1))
               (fix-regexp "[[:multibyte:]]")
               (fixed-contents
                (replace-regexp-in-string
                 (concat
                  "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))

          (ad-set-arg 1 fixed-contents))))

    (use-package ox-latex
      :init
      (setq org-latex-listings 'minted)
      (setq org-latex-minted-options '(("frame" "single")
                                       ("breaklines" "")))
      (setq org-latex-pdf-process
            '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
              "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
              "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

      (add-to-list 'org-latex-packages-alist '("" "ctex"))
      (add-to-list 'org-latex-packages-alist '("" "minted"))
      (add-to-list 'org-latex-packages-alist '("" "color"))
      (add-to-list 'org-latex-packages-alist '("" "geometry"))
      (add-to-list 'org-latex-packages-alist '("" "tabularx"))
      (add-to-list 'org-latex-packages-alist '("" "tabu"))
      (add-to-list 'org-latex-packages-alist '("" "fancyhdr"))
      (add-to-list 'org-latex-packages-alist '("" "natbib"))
      (add-to-list 'org-latex-packages-alist '("" "titlesec"))))
  (use-package ob
    :init
    (setq org-confirm-babel-evaluate nil)
    :config
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

    (use-package ob-plantuml
      :config
      (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar"))

    (use-package ob-gnuplot
      :init
      (use-package gnuplot
        :mode ("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode)))

    (use-package ob-dot
      :init
      (use-package graphviz-dot-mode
        :mode ("\\.dot\\'" . graphviz-dot-mode)))

    (use-package ob-ditaa
      :init
      (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar"))

    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((C          . t)
                                   (awk        . t)
                                   (dot        . t)
                                   (sed        . t)
                                   (sql        . t)
                                   (calc       . t)
                                   (ruby       . t)
                                   (ditaa      . t)
                                   (latex      . t)
                                   (shell      . t)
                                   (python     . t)
                                   (plantuml   . t)
                                   (emacs-lisp . t)
                                   (gnuplot    . t))))

  (use-package org-bullets
    :init
    (add-hook 'org-mode-hook 'org-bullets-mode)
    (setq org-bullets-bullet-list '("◉" "◎" "⚫" "○" "►" "◇"))))

(provide 'init-org)
