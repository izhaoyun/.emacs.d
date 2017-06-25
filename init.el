;;; init --- all magic things start from here -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(require 'package)
(package-initialize)

;;; use `use-package' to manage package configuration.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'diminish)
(require 'bind-key)
(require 'use-package)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'install-packages)
(require 'setup-editor)
(require 'setup-core)

;; --- language modules ---
(require 'init-lisp)
(require 'init-c++)
(require 'init-org)
(require 'init-python)
(require 'init-web)
(require 'init-ruby)
(require 'init-latex)

;;; init.el ends here
