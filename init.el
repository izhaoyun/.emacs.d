;;; -*- lexical-binding: t -*-

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(setq load-prefer-newer t
      package-enable-at-startup nil)
(autoload 'package-initialize "package")

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents)
  (package-install 'use-package)
  )

(use-package use-package
  :init
  (setq use-package-always-ensure t)
  ;; (setq use-package-verbose t)
  )

(use-package diminish
  :defer t
  )

(use-package bind-key
  :defer t
  )

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))
(require 'setup-editor)
(require 'init-prog)
(require 'init-lisp)
(require 'init-cc)
(require 'init-org)
(require 'init-python)
(require 'init-go)
(require 'init-web)
(require 'init-tex)
(require 'init-shell)
(require 'init-devops)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
