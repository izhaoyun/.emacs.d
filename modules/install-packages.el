;;; install-packages --- Installed Packages Lists -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar my/packages '(ivy
                      swiper
                      hydra
                      counsel
                      ace-window
                      avy
                      use-package
                      diminish
                      bind-key
                      hungry-delete
                      popwin
                      which-key
                      sr-speedbar
                      ace-pinyin
                      undo-tree
                      ws-butler
                      expand-region
                      flycheck
                      flycheck-pos-tip
                      ztree
                      stickyfunc-enhance
                      diff-hl
                      ;; --- parentheses ---
                      rainbow-delimiters
                      smartparens
                      ;; --- indent ---
                      dtrt-indent
                      aggressive-indent
                      clean-aindent-mode
                      highlight-indent-guides
                      highlight-symbol
                      comment-dwim-2
                      projectile
                      ;; --- version control ---
                      magit git-commit
                      company
                      company-quickhelp
                      yasnippet
                      ;; --- org ---
                      org
                      org-bullets
                      htmlize
                      gnuplot-mode
                      plantuml-mode
                      ;; --- lisp ---
                      auto-compile
                      lispy
                      ;; --- c/c++ ---
                      cmake-ide
                      ggtags
                      irony
                      company-irony
                      flycheck-irony
                      company-c-headers
                      function-args
                      google-c-style
                      ;; --- shell ---
                      ;; --- makefile ---
                      cmake-mode
                      cmake-font-lock
                      ;; --- python ---
                      python
                      anaconda-mode
                      company-anaconda
                      pyenv-mode
                      pip-requirements
                      virtualenvwrapper
                      ;; --- erlang ---
                      ivy-erlang-complete
                      company-erlang
                      ;; --- go ---
                      go-mode
                      company-go
                      ;; --- latex ---
                      auctex
                      company-auctex
                      ;; --- sql ---
                      edbi
                      company-edbi
                      ;; --- web ---
                      web-mode
                      js2-mode
                      yaml-mode))

(setq package-selected-packages my/packages)

(eval-when-compile
  (require 'cl))

(defun my/packages-installed-p ()
  (loop for pkg in my/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(provide 'install-packages)
;;; install-packages.el ends here
