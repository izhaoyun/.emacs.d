;;; -*- lexical-binding: t -*-

(use-package org
  :defer t
  :load-path "site-lisp/org-mode/lisp"
  :mode (("\\.org\'" . org-mode))
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   :map org-mode-map
   ("M-o" . ace-link-org))
  :init
  (setq org-image-actual-width nil
        org-catch-invisible-edits 'smart
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-hide-emphasis-markers t
        org-url-hexify-p nil
        org-startup-with-inline-images t
        org-footnote-auto-adjust t)

  ;; https://emacs-china.org/t/org-mode/597/11
  (setq org-emphasis-regexp-components
        (list (concat " \t('\"{"            "[:nonascii:]")
              (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
              " \t\r\n,\"'"
              "."
              1))
  (setq org-highlight-latex-and-related
        '(latex script entities))
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
         "\\(?:\\*\\|[+-]?[[:alnum:].,\\]*[[:alnum:]]\\)\\)")
        )

  (add-to-list 'org-latex-packages-alist '("" "ctex"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("" "geometry"))
  (add-to-list 'org-latex-packages-alist '("" "tabularx"))
  (add-to-list 'org-latex-packages-alist '("" "tabu"))
  (add-to-list 'org-latex-packages-alist '("" "fancyhdr"))
  (add-to-list 'org-latex-packages-alist '("" "natbib"))
  (add-to-list 'org-latex-packages-alist '("" "titlesec"))
  (add-to-list 'org-latex-packages-alist '("" "tikz"))
  )

(use-package ob
  :defer t
  :load-path "site-lisp/org-mode/lisp"
  :init
  (setq org-preview-latex-default-process 'imagemagick
        org-babel-uppercase-example-markers t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk        . t)
     (C          . t)
     (ditaa      . t)
     (dot        . t)
     (emacs-lisp . t)
     (gnuplot    . t)
     (go         . t)
     (http       . t)
     (latex      . t)
     (plantuml   . t)
     (python     . t)
     (sed        . t)
     (shell      . t)
     (sql        . t)))
  :config
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  )

(use-package ob-ditaa
  :defer t
  :load-path "site-lisp/org-mode/lisp"
  )

(use-package ob-plantuml
  :defer t
  :load-path "site-lisp/org-mode/lisp"
  :init
  (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  )

(use-package ob-latex
  :defer t
  :load-path "site-lisp/org-mode/lisp"
  :init
  (add-to-list 'org-babel-default-header-args:latex
               '(:imagemagick . "yes"))
  (add-to-list 'org-babel-default-header-args:latex
               '(:iminoptions . "-density 600"))
  (add-to-list 'org-babel-default-header-args:latex
               '(:imoutoptions . "-geometry 400"))
  )

(use-package ob-http
  :defer t
  )

(use-package ob-go
  :defer t
  )

(use-package ox
  :defer t
  :load-path "site-lisp/org-mode/lisp"
  :preface
  (defun clear-single-linebreak-in-cjk-string (string)
    (let* ((regexp "\\([\u4E00-\u9FA5]\\)\n\\([\u4E00-\u9FA5]\\)")
           (start (string-match regexp string)))
      (while start
        (setq string (replace-match "\\1\\2" nil nil string)
              start (string-match regexp string start))))
    string)
  :init
  (setq org-export-with-toc nil
        org-export-default-language "zh-CN"
        org-export-time-stamp-file nil)
  )

(use-package ox-html
  :defer t
  :load-path "site-lisp/org-mode/lisp"
  :init
  (use-package htmlize
    :defer t
    )

  (setq org-html-validation-link nil
        org-html-doctype "html5"
        org-html-html5-fancy t)
  :config
  (defun ox-html-clear-single-linebreak-for-cjk (string backend info)
    (when (org-export-derived-backend-p backend 'html)
      (clear-single-linebreak-in-cjk-string string))
    )
  (add-to-list 'org-export-filter-final-output-functions
               'ox-html-clear-single-linebreak-for-cjk)
  )

(use-package ox-latex
  :defer t
  :load-path "site-lisp/org-mode/lisp"
  :init
  (setq org-latex-compiler "xelatex"
        org-latex-listings 'minted
        org-latex-minted-options '(("breaklines" "")
                                   ("frame" "single"))
        )
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  )

(use-package org-pdfview
  :after (org  pdf-tools)
  :defer t
  )

(use-package pdf-tools
  :defer t
  :hook (pdf-view-mode . (lambda () (cua-mode 0)))
  :bind
  (:map pdf-view-mode-map
        ("\\" . hydra-pdftools/body)
        ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
        ("g"  . pdf-view-first-page)
        ("G"  . pdf-view-last-page)
        ("l"  . image-forward-hscroll)
        ("h"  . image-backward-hscroll)
        ("j"  . pdf-view-next-page)
        ("k"  . pdf-view-previous-page)
        ("e"  . pdf-view-goto-page)
        ("u"  . pdf-view-revert-buffer)
        ("al" . pdf-annot-list-annotations)
        ("ad" . pdf-annot-delete)
        ("aa" . pdf-annot-attachment-dired)
        ("am" . pdf-annot-add-markup-annotation)
        ("at" . pdf-annot-add-text-annotation)
        ("y"  . pdf-view-kill-ring-save)
        ("i"  . pdf-misc-display-metadata)
        ("s"  . pdf-occur)
        ("b"  . pdf-view-set-slice-from-bounding-box)
        ("r"  . pdf-view-reset-slice))
  :init
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-resize-factor 1.1)
  :config
  (setq pdf-annot-activate-created-annotations t)
  )

(defhydra hydra-pdftools (:color blue :hint nil)
  "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤   [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
  ("\\" hydra-master/body "back")
  ("<ESC>" nil "quit")
  ("al" pdf-annot-list-annotations)
  ("ad" pdf-annot-delete)
  ("aa" pdf-annot-attachment-dired)
  ("am" pdf-annot-add-markup-annotation)
  ("at" pdf-annot-add-text-annotation)
  ("y"  pdf-view-kill-ring-save)
  ("+" pdf-view-enlarge :color red)
  ("-" pdf-view-shrink :color red)
  ("0" pdf-view-scale-reset)
  ("H" pdf-view-fit-height-to-window)
  ("W" pdf-view-fit-width-to-window)
  ("P" pdf-view-fit-page-to-window)
  ("n" pdf-view-next-page-command :color red)
  ("p" pdf-view-previous-page-command :color red)
  ("d" pdf-view-dark-minor-mode)
  ("b" pdf-view-set-slice-from-bounding-box)
  ("r" pdf-view-reset-slice)
  ("g" pdf-view-first-page)
  ("G" pdf-view-last-page)
  ("e" pdf-view-goto-page)
  ("o" pdf-outline)
  ("s" pdf-occur)
  ("i" pdf-misc-display-metadata)
  ("u" pdf-view-revert-buffer)
  ("F" pdf-links-action-perfom)
  ("f" pdf-links-isearch-link)
  ("B" pdf-history-backward :color red)
  ("N" pdf-history-forward :color red)
  ("l" image-forward-hscroll :color red)
  ("h" image-backward-hscroll :color red)
  )

(use-package toc-org
  :defer t
  :hook (org-mode . toc-org-enable)
  )

(use-package plantuml-mode
  :defer t
  :mode ("\\.plantuml\\'")
  :init
  (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  )

(use-package graphviz-dot-mode
  :defer t
  :mode ("\\.dot\\'" "\\.gv\\'")
  )

(provide 'init-org)
;;; init-org.el ends here
