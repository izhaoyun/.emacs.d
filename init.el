;;; -*- lexical-binding: t -*-

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(add-to-list
 'load-path
 (expand-file-name "lisp" user-emacs-directory))

(require 'setup-packages)
(require 'setup-editor)
(require 'init-lisp)
(require 'init-prog)
(require 'init-cc)
(require 'init-org)
(require 'init-go)
(require 'init-python)
(require 'init-tex)
(require 'init-shell)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
