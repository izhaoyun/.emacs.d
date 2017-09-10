;;; init-org --- Org-mode Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package org
  :mode
  (("\\.org$" . org-mode)
   ("\\.txt$" . txt-mode))
  :bind
  (("C-c a" . org-agenda)
   ("C-c b" . org-iswitch)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :init
  (setq-default org-catch-invisible-edits 'smart)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  ;; 必须在Org被加载之前执行
  ;; 参考地址：https://emacs-china.org/t/org-mode/597/11
  (setq org-emphasis-regexp-components
        ;; markup 记号前后允许中文
        (list (concat " \t('\"{"            "[:nonascii:]")
              (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
              " \t\r\n,\"'"
              "."
              1)
        )

  :config
  (setq org-footnote-auto-adjust t)
  (setq org-hide-emphasis-markers t)
  (setq org-match-substring-regexp
        (concat
         ;; 限制上标和下标的匹配范围，org 中对其的介绍见：(org) Subscripts and superscripts
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

(use-package ob-C)
(use-package ob-awk)
(use-package ob-dot)
(use-package ob-sed)
(use-package ob-sql)
(use-package ob-ruby)
(use-package ob-shell)
(use-package ob-python)
(use-package ob-emacs-lisp)

(use-package ob-ditaa
  :ensure org
  :init
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar")
  )

(use-package ob-http)

(use-package ob
  :ensure org
  :init
  (use-package ob-plantuml
    :ensure org
    :init
    (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
    )

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C          . t)
     (awk        . t)
     (dot        . t)
     (sed        . t)
     (sql        . t)
     (ruby       . t)
     (http       . t)
     (ditaa      . t)
     (shell      . t)
     (python     . t)
     (plantuml   . t)
     (emacs-lisp . t)))
  )

(use-package ox-beamer :ensure org)

(use-package ox-gfm :ensure org-plus-contrib)

(use-package ox-html
  :ensure org
  :init
  (use-package htmlize)
  :config
  (setq org-html-html5-fancy t
        org-html-indent t
        org-html-doctype "html5")
  )

(use-package ox-latex
  :ensure org
  :init
  (setq org-latex-listings 'minted)
  (setq org-latex-minted-options '(("frame"      "single")
                                   ("breaklines" "")))
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  )

(use-package ox
  :ensure org
  :preface
  (defun clear-single-linebreak-in-cjk-string (string)
    "clear single line-break between cjk characters that is
usually soft line-breaks"
    (let* ((regexp "\\([\u4E00-\u9FA5]\\)\n\\([\u4E00-\u9FA5]\\)")
           (start (string-match regexp string)))
      (while start
        (setq string (replace-match "\\1\\2" nil nil string)
              start (string-match regexp string start))))
    string)
  :init
  (setq org-export-default-language "zh-CN"
        org-latex-compiler "xelatex")
  :config
  (defun ox-html-clear-single-linebreak-for-cjk (string backend info)
    (when (org-export-derived-backend-p backend 'html)
      (clear-single-linebreak-in-cjk-string string))
    )
  (add-to-list 'org-export-filter-final-output-functions
               'ox-html-clear-single-linebreak-for-cjk)
  )

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "markdown")
  )

(provide 'init-org)
