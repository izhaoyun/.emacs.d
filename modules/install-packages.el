;;; -*- lexical-binding: t; -*-
(defvar my/packages '(ivy
                      swiper
                      hydra
                      use-package
                      diminish
                      counsel
                      bind-key
                      ace-window
                      hungry-delete
                      popwin
                      which-key
                      golden-ratio
                      avy
                      ace-pinyin
                      undo-tree
                      ws-butler
                      expand-region
                      aggressive-indent
                      highlight-symbol
                      comment-dwim-2
                      projectile
                      magit
                      git-commit
                      company
                      company-quickhelp
                      yasnippet
                      ;; --- org ---
                      org
                      org-plus-contrib
                      htmlize
                      ;; --- lisp ---
                      auto-compile
                      lispy
                      ;; --- c/c++ ---
                      ggtags
                      irony
                      ;; --- shell ---
                      ;; python
                      ;; ruby
                      ;; erlang
                      ;; --- latex ---
                      auctex
                      ;; --- web ---
                      js2-mode))

(setq package-selected-packages my/packages)

(require 'cl)

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
