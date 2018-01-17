;;; -*- lexical-binding: t -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(require 'package)
(setq load-prefer-newer t
      package-enable-at-startup nil)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish)
  )
(package-install-selected-packages)
(require 'use-package)
(require 'diminish)
(require 'bind-key)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'setup-editor)
(require 'init-prog)
(require 'init-lisp)
(require 'init-makefile)
(require 'init-c++)
(require 'init-org)
(require 'init-python)
(require 'init-go)
(require 'init-web)
;; (require 'init-erlang)

;;; init.el ends here
