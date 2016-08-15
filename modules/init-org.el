(defconst my/org-packages
  '(org
	org-bullets
    org-plus-contrib
	ob-ipython
	ob-http
    htmlize
	gnuplot
	graphviz-dot-mode
	)
  )

(install-packages my/org-packages)

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode)
  )

(use-package ob
  :config
  (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  (use-package ob-plantuml
  	:config
  	(setq org-plantuml-jar-path
		  (shell-command-to-string "locate plantuml.jar"))
  	)

  (use-package ob-gnuplot
	:init
	(use-package gnuplot
	  :mode ("\\.\\(gp\\|gnuplot\\)$" . gnuplot-mode)
	  )
	)

  (use-package ob-dot
	:init
	(use-package graphviz-dot-mode
	  :mode ("\\.dot\\'" . graphviz-dot-mode)
	  )
	)

  (use-package ob-ditaa
	:config
	(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_9.jar")
	)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C          . t)					; C, C++, D
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
	 (http       . t)
	 (gnuplot    . t)
	 )
   )
  )

;;;###autoload
(defun my/init-org-export ()

  (require 'ox-beamer)
  (require 'ox-gfm)

  (require 'htmlize)
  (use-package ox-html)

  (use-package ox-latex
	:config
	(setq org-latex-listings 'minted)
	(setq org-latex-minted-options '(("frame"      "single")
									 ("breaklines" "")))
	(setq org-latex-pdf-process
		  '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
			"xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
			"xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
  )

(use-package ox
  :preface
  (defun clear-single-linebreak-in-cjk-string (string)
    (let* ((regexp "\\([\u4E00-\u9FA5]\\)\n\\([\u4E00-\u9FA5]\\)")
           (start (string-match regexp string)))
      (while start
        (setq string (replace-match "\\1\\2" nil nil string)
              start (string-match regexp string start))))
    string)
  :commands (org-export-derived-backend-p)
  :init
  (defun ox-html-clear-single-linebreak-for-cjk (string backend info)
    (when (org-export-derived-backend-p backend 'html)
      (clear-single-linebreak-in-cjk-string string))
	)
  :config
  (setq org-export-default-language "zh-CN")
  ;; (setq org-latex-compiler "xelatex")
  (add-to-list 'org-export-filter-final-output-functions
			   'ox-html-clear-single-linebreak-for-cjk)
  )

(use-package org
  :mode (("\\.org$" . org-mode))
  :bind (("C-c a" . org-agenda)
		 ("C-c b" . org-iswitch)
		 ("C-c c" . org-capture)
		 ("C-c l" . org-store-link))
  :config
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)

  (add-to-list 'org-latex-packages-alist '("" "ctex"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("" "geometry"))
  (add-to-list 'org-latex-packages-alist '("" "tabularx"))
  (add-to-list 'org-latex-packages-alist '("" "tabu"))
  (add-to-list 'org-latex-packages-alist '("" "fancyhdr"))
  (add-to-list 'org-latex-packages-alist '("" "natbib"))
  (add-to-list 'org-latex-packages-alist '("" "titlesec"))

  (add-hook 'org-mode-hook 'my/init-org-export)
  )

(provide 'init-org)
