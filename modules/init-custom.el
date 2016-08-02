(setq inhibit-startup-message t)
(when window-system
  (menu-bar-mode   -1)
  (tooltip-mode    -1)
  (tool-bar-mode   -1)
  (scroll-bar-mode -1))

(global-font-lock-mode 1)

(column-number-mode 1)

;; backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(defalias 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a buffer name
;; (if the buffer isn 't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(provide 'init-custom)
