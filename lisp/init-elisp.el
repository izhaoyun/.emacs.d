;;; init-elisp.el --- Lisp configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; @github: tarsius/auto-compile
(use-package auto-compile
  :init
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  )

;; @github: abo-abo/lispy
(use-package lispy
  :defer t
  :diminish lispy-mode
  :commands (lispy-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  )

(provide 'init-elisp)
;;; init-elisp.el ends here
