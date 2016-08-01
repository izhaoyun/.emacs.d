(defconst my/org-packages
  '(org
    org-plus-contrib
    htmlize))

(install-packages my/org-packages)


(eval-and-compile
  (defun my/init-org-bullets ()
    (use-package org-bullets
      :ensure org-plus-contrib
      :init
      (org-bullets-mode 1))
    )

  (defun my/init-org-babel ()
    (use-package ob-C)
    (use-package ob-awk)
    (use-package ob-dot)
    (use-package ob-sed)
    (use-package ob-sql)
    (use-package ob-ruby)
    (use-package ob-shell)
    (use-package ob-python)
    (use-package ob-emacs-lisp)
    (use-package ob-plantuml
      :config
      (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar"))
    (use-package ob-ditaa
      :config
      (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_9.jar"))

    (use-package ob
      :config
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((C          . t)
	 (awk        . t)
	 (dot        . t)
	 (sed        . t)
	 (sql        . t)
	 (ruby       . t)
	 (ditaa      . t)
	 (shell      . t)
	 (python     . t)
	 (plantuml   . t)
	 (emacs-lisp . t)))))

  (defun my/init-org-export ()
    (use-package htmlize)
    ;; export to markdown files.
    (use-package ox-beamer)
    (use-package ox-gfm :ensure org-plus-contrib)

    (use-package ox-html)
    (use-package ox-latex
      :config
      (setq org-latex-listings 'minted)
      (setq org-latex-minted-options
	    '(("frame"      "single")
	      ("breaklines" "")))
      (setq org-latex-pdf-process
	    '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	      "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	      "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
    )

  (defun clear-single-linebreak-in-cjk-string (string)
    (let* ((regexp "\\([\u4E00-\u9FA5]\\)\n\\([\u4E00-\u9FA5]\\)")
           (start (string-match regexp string)))
      (while start
        (setq string (replace-match "\\1\\2" nil nil string)
              start (string-match regexp string start))))
    string)

  (defun ox-html-clear-single-linebreak-for-cjk (string backend info)
    (when (org-export-derived-backend-p backend 'html)
      (clear-single-linebreak-in-cjk-string string)))
  )

(use-package ox
  :init
  (setq org-export-default-language "zh-CN")
  (setq org-latex-compiler "xelatex")
  :config
  (add-to-list 'org-export-filter-final-output-functions
	       'ox-html-clear-single-linebreak-for-cjk))

(use-package org
  :mode (("\\.org$" . org-mode)
	 ("\\.txt$" . txt-mode))
  :bind (("C-c a" . org-agenda)
	 ("C-c b" . org-iswitch)
	 ("C-c c" . org-capture)
	 ("C-c l" . org-store-link))
  :init

  :config
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  (add-to-list 'org-latex-packages-alist '("" "ctex"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("" "geometry"))
  (add-to-list 'org-latex-packages-alist '("" "tabularx"))
  (add-to-list 'org-latex-packages-alist '("" "tabu"))
  (add-to-list 'org-latex-packages-alist '("" "fancyhdr"))
  (add-to-list 'org-latex-packages-alist '("" "natbib"))
  (add-to-list 'org-latex-packages-alist '("" "titlesec"))
  
  (add-hook 'org-mode-hook 'my/init-org-babel)
  (add-hook 'org-mode-hook 'my/init-org-export)
  (add-hook 'org-mode-hook 'my/init-org-bullets))

(provide 'init-org)
