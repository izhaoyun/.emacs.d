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
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-paren-style (quote expression))
 '(tab-always-indent (quote complete))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(winner-mode t)
 '(x-select-enable-primary t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
