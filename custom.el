;;; custom.el --- Customization -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(before-save-hook (quote (whitespace-cleanup)))
 '(column-number-mode t)
 '(gc-cons-threshold 104857600)
 '(gdb-many-windows t)
 '(gdb-show-main t)
 '(gdb-speedbar-auto-raise t)
 '(global-auto-revert-mode t)
 '(global-linum-mode nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(package-archives
   (quote
    (("melpa" . "https://elpa.emacs-china.org/melpa/")
     ("org" . "https://elpa.emacs-china.org/org/")
     ("gnu" . "http://elpa.emacs-china.org/gnu/"))))
 '(package-enable-at-startup nil)
 '(package-selected-packages
   (quote
    (multiple-cursors calfw git-gutter rainbow-delimiters smartparens cmake-ide sr-speedbar ztree yaml-mode ws-butler which-key web-mode virtualenvwrapper use-package undo-tree stickyfunc-enhance python pyenv-mode projectile popwin pip-requirements org-plus-contrib org-bullets org magit lispy js2-mode jinja2-mode hungry-delete htmlize highlight-symbol highlight-indent-guides gnuplot-mode ggtags function-args flycheck-pos-tip flycheck-plantuml flycheck-irony expand-region dtrt-indent diff-hl company-quickhelp company-irony company-go company-erlang company-edbi company-c-headers company-auctex company-anaconda comment-dwim-2 cmake-font-lock clean-aindent-mode auto-compile aggressive-indent ace-pinyin)))
 '(prog-mode-hook
   (quote
    (eldoc-mode rainbow-delimiters-mode clean-aindent-mode highlight-indent-guides-mode linum-mode)))
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; custom.el ends here
