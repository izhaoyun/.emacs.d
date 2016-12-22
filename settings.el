(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(after-save-hook (quote (recompile-elisp-file)))
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(before-save-hook
   (quote
	(copyright-update time-stamp delete-trailing-whitespace)))
 '(case-fold-search nil)
 '(column-number-mode t)
 '(electric-indent-mode nil)
 '(gc-cons-threshold 125829120)
 '(ibuffer-case-fold-search t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
	(auto-complete rtags ox-gfm ace-window avy hydra swiper ivy cmake-mode cmake-ide cmake-font-lock google-c-style company-c-headers irony company-irony-c-headers company-irony flycheck-irony cpputils-cmake ggtags irony-eldoc flycheck-google-cpplint org-bullets htmlize org-plus-contrib plantuml-mode ob-http ob-ipython graphviz-dot-mode gnuplot company-anaconda anaconda-mode pip-requirements py-yapf flycheck-pyflakes inf-ruby robe yari company-inf-ruby ruby-tools web-mode systemd puppet-mode smartparens projectile yasnippet undo-tree which-key haskell-mode yaml-mode ws-butler window-numbering stickyfunc-enhance restclient rainbow-delimiters markdown-preview-eww markdown-mode magit lispy levenshtein impatient-mode highlight-symbol highlight-indentation expand-region erlang dtrt-indent counsel company-quickhelp comment-dwim-2 clean-aindent-mode bundler auctex aggressive-indent ace-pinyin 4clojure)))
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(select-enable-primary t)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(tab-always-indent (quote complete))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "red")))))
