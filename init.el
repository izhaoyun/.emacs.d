(require 'package)
(package-initialize)
(setq package-archives '(("melpa" . "https://elpa.emacs-china.org/melpa/")
			 ("org"   . "https://elpa.emacs-china.org/org/")
			 ("gnu"   . "http://elpa.emacs-china.org/gnu/")))
(setq package-enable-at-startup nil)

;;; use `use-package' to manage package configuration.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'diminish)
(require 'bind-key)
(require 'use-package)
(setq use-package-always-ensure t)

(setq load-prefer-newer t)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'setup-editor)
(require 'install-packages)
(require 'setup-core)
