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

  ;; 必须在Org被加载之前执行
  ;; 参考地址：https://emacs-china.org/t/org-mode/597/11
  (setq org-emphasis-regexp-components
        ;; markup 记号前后允许中文
        (list (concat " \t('\"{"            "[:nonascii:]")
              (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
              " \t\r\n,\"'"
              "."
              1))

  :config
  (setq org-image-actual-width nil)
  ;; 隐藏**，~~，++等符号
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
    (use-package plantuml-mode
      :mode (("\\.plantuml\\'" . plantuml-mode))
      :config
      (setq plantuml-jar-path "/opt/plantuml/plantuml.jar"))
    (add-to-list 'org-babel-load-languages '(plantuml . t) t)
    (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar"))

  ;; @github: zweifisch/ob-http
  (use-package ob-http
    :init
    (add-to-list 'org-babel-load-languages '(http . t) t))

  ;; Exporting
  ;; -----------------------------------------------------------------------
  (setq org-export-default-language "zh-CN")

  (use-package ox
    :init
    (defun clear-single-linebreak-in-cjk-string (string)
      "clear single line-break between cjk characters that is usually soft line-breaks"
      (let* ((regexp "\\([\u4E00-\u9FA5]\\)\n\\([\u4E00-\u9FA5]\\)")
             (start (string-match regexp string)))
        (while start
          (setq string (replace-match "\\1\\2" nil nil string)
                start (string-match regexp string start))))
      string)
    :config
    (defun ox-html-clear-single-linebreak-for-cjk (string backend info)
      (when (org-export-derived-backend-p backend 'html)
        (clear-single-linebreak-in-cjk-string string)))

    (add-to-list 'org-export-filter-final-output-functions
                 'ox-html-clear-single-linebreak-for-cjk)
    )

  (use-package ox-md)
  (use-package ox-gfm)
  (use-package ox-latex
    :init
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

(use-package graphviz-dot-mode
  :mode (("\\.dot\\'" . graphviz-dot-mode)))

(provide 'init-org)
;;; init-org.el ends here
