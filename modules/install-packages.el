;;; -*- lexical-binding: t; -*-
(defvar my/packages '(ivy swiper hydra counsel ace-window avy
                          use-package diminish bind-key
                          hungry-delete
                          popwin
                          which-key
                          ace-pinyin
                          undo-tree
                          ws-butler
                          expand-region
                          ;; --- indent ---
                          aggressive-indent
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
                          org org-plus-contrib htmlize gnuplot-mode
                          ;; --- lisp ---
                          auto-compile lispy
                          ;; --- c/c++ ---
                          ggtags irony
                          ;; --- shell ---
                          ;; --- makefile ---
                          cmake-mode cmake-font-lock
                          ;; --- python ---
                          ;; --- erlang ---
                          ;; --- latex ---
                          auctex
                          ;; --- web ---
                          js2-mode))

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
