(defvar my/packages '( ;; utils
                      counsel
                      ivy
                      swiper
                      hydra
                      ace-window
                      hungry-delete
                      which-key
                      auto-compile
                      golden-ratio
                      avy
                      ace-pinyin
                      undo-tree
                      ws-butler
                      expand-region
                      aggressive-indent
                      highlight-symbol
                      comment-dwim-2
                      ;; project
                      projectile
                      ;; --- version control ---
                      magit
                      git-commit
                      ;;
                      company
                      ;;--- org ---
                      org
                      org-plus-contrib
                      htmlize
                      ;; lisp
                      lispy
                      ;; c/c++
                      ggtags
                      irony
                      ;; shell
                      ;; python
                      ;; ruby
                      ;; erlang
                      ;; latex
                      auctex))

(setq package-selected-packages my/packages)

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
