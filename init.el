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
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

(require 'init-org)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
