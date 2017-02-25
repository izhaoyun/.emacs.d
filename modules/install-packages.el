(defvar my/packages
  '(
    ;; utils
    counsel
    ivy
    swiper
    hydra
    which-key
    window-numbering
    auto-compile
    ;; edit
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
    ;; version control
    magit
    git-commit
    company
    ;; languages
    ;;--------------------
    ;; org
    org
    org-plus-contrib
    htmlize
    ;;--------------------
    ;; lisp
    lispy
    ;;--------------------
    ;; c/c++
    ggtags
    irony
    ;;--------------------
    ;; shell
    ;;--------------------
    ;; python
    ;;--------------------
    ;; ruby
    ;;--------------------
    ;; erlang
    ;;--------------------
    ;; latex
    auctex
    ;;--------------------
    )
  )
;;(eval-after-load 'package
;;  '(setq package-selected-packages my/packages))

(defun install-packages-from-list (pkgs-list)
  (dolist (pkg pkgs-list)
    (unless (package-installed-p pkg)
      (package-install pkg))))
(install-packages-from-list my/packages)

(provide 'install-packages)
