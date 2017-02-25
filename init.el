;;; -*- lexical-binding: t; -*-
(require 'package)
(setq load-prefer-newer t)
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
;; (setq use-package-always-ensure t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'install-packages)
(require 'setup-editor)
(require 'setup-core)
(require 'init-lisp)
(require 'init-latex)
