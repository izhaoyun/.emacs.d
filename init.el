;;; -*- lexical-binding: t -*-

;; load custom.el
(progn
  (setq custom-file
        (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file 'noerror))
)

;; install use-package
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'bind-key)

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-verbose t)

(require 'use-package)

(use-package auto-compile
  :hook
  ((emacs-lisp-mode . auto-compile-on-load-mode)
   (emacs-lisp-mode . auto-compile-on-save-mode))
  :init
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t)
  )

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

(require 'init-editor)
(require 'init-develop)
(require 'init-writing)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
