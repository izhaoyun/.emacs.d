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
	(company-anaconda cmake-ide web-mode systemd puppet-mode robe pip-requirements py-yapf smartparens projectile yasnippet undo-tree which-key anaconda-mode anaconda haskell-mode yari yaml-mode ws-butler window-numbering sunrise-commander stickyfunc-enhance ruby-tools restclient rainbow-delimiters org-plus-contrib org-bullets ob-ipython ob-http mysql-to-org markdown-preview-eww markdown-mode magit lispy levenshtein irony-eldoc impatient-mode highlight-symbol highlight-indentation graphviz-dot-mode google-c-style gnuplot ggtags flycheck-pyflakes flycheck-irony flycheck-google-cpplint expand-region erlang dtrt-indent cpputils-cmake counsel company-quickhelp company-irony-c-headers company-irony company-inf-ruby company-c-headers comment-dwim-2 cmake-font-lock clean-aindent-mode bundler auctex aggressive-indent ace-pinyin 4clojure)))
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
