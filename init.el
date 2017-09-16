;;; init --- all magic things start from here -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(require 'package)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(require 'diminish)
(require 'bind-key)
(require 'use-package)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'setup-editor)
(require 'init-prog)
(require 'init-elisp)
(require 'init-c++)
(require 'init-org)
(require 'init-python)
(require 'init-go)
(require 'init-erlang)
(require 'init-haskell)
(require 'hydra-keys)

;;; init.el ends here
