;;; -*- lexical-binding: t -*-

(use-package org
  :defer t
  :ensure org-plus-contrib
  :mode (("\\.org\'" . org-mode))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb))
  :bind (:map org-mode-map
              ("M-o" . ace-link-org))
  :init
  (setq org-image-actual-width nil
        org-catch-invisible-edits 'smart
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-hide-emphasis-markers t
        org-url-hexify-p nil)
  (setq org-footnote-auto-adjust t)
  ;; https://emacs-china.org/t/org-mode/597/11
  (setq org-emphasis-regexp-components
        (list (concat " \t('\"{"            "[:nonascii:]")
              (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
              " \t\r\n,\"'"
              "."
              1))
  :config
  ;; https://emacs-china.org/t/org-mode/597/6
  (setq org-match-substring-regexp
        (concat
         ;; info: (org) Subscripts and superscripts
         "\\([0-9a-zA-Zα-γΑ-Ω]\\)\\([_^]\\)\\("
         "\\(?:" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
         "\\|"
         "\\(?:" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
         "\\|"
         "\\(?:\\*\\|[+-]?[[:alnum:].,\\]*[[:alnum:]]\\)\\)"))

  (add-to-list 'org-latex-packages-alist '("" "ctex"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("" "geometry"))
  (add-to-list 'org-latex-packages-alist '("" "tabularx"))
  (add-to-list 'org-latex-packages-alist '("" "tabu"))
  (add-to-list 'org-latex-packages-alist '("" "fancyhdr"))
  (add-to-list 'org-latex-packages-alist '("" "natbib"))
  (add-to-list 'org-latex-packages-alist '("" "titlesec"))
  )

(use-package ob
  :defer t
  :ensure org-plus-contrib
  :commands (org-babel-do-load-languages)
  :init
  (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"
        org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C          . t)
     (awk        . t)
     (dot        . t)
     (sed        . t)
     (sql        . t)
     ;; (http       . t)
     (ditaa      . t)
     (shell      . t)
     (gnuplot    . t)
     (python     . t)
     (plantuml   . t)
     (emacs-lisp . t)))
  :config
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  )

(use-package ox
  :defer t
  :ensure org-plus-contrib
  :preface
  (defun clear-single-linebreak-in-cjk-string (string)
    (let* ((regexp "\\([\u4E00-\u9FA5]\\)\n\\([\u4E00-\u9FA5]\\)")
           (start (string-match regexp string)))
      (while start
        (setq string (replace-match "\\1\\2" nil nil string)
              start (string-match regexp string start))))
    string)
  (use-package htmlize :ensure t)
  :init
  (setq org-export-with-toc nil
        org-export-default-language "zh-CN"
        org-html-doctype "html5"
        org-html-html5-fancy t
        org-latex-compiler "xelatex"
        org-latex-listings 'minted
        org-latex-minted-options '(("breaklines" "")
                                   ("frame" "single")))
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  :config
  (defun ox-html-clear-single-linebreak-for-cjk (string backend info)
    (when (org-export-derived-backend-p backend 'html)
      (clear-single-linebreak-in-cjk-string string))
    )
  (add-to-list 'org-export-filter-final-output-functions
               'ox-html-clear-single-linebreak-for-cjk)
  )

(use-package toc-org
  :defer t
  :hook (org-mode . toc-org-enable)
  )

(use-package plantuml-mode
  :mode ("\\.plantuml\\'")
  )

(use-package graphviz-dot-mode
  :mode ("\\.dot\\'" "\\.gv\\'")
  )

(provide 'init-org)
;;; init-org.el ends here
