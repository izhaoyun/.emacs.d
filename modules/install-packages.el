(defvar my/packages
  '(
    ;; utils
    counsel
    ivy
    swiper
    hydra
    which-key
    window-number
    ;; edit
    avy
    ace-pinyin
    undo-tree
    ws-butler
    expand-region
    aggressive-indent
    ;; project
    projectile
    ;; version control
    magit
    ;;
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
    ))
(eval-after-load 'package
  '(setq package-selected-packages my/packages))

(defun install-packages-from-list (pkgs-list)
  (dolist (pkg pkgs-list)
    (unless (package-installed-p pkg)
      (package-install pkg))))
(install-packages-from-list my/packages)

(provide 'install-packages)
