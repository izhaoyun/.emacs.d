;;; -*- lexical-binding: t -*-

(setq load-prefer-newer t
      package-enable-at-startup nil)
(autoload 'package-initialize "package")
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents)
  (when (boundp 'package-selected-packages)
    (package-install-selected-packages)
    )
  )

(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  )

(require 'bind-key)
(require 'diminish)

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-verbose t)

(eval-when-compile
  (require 'use-package)
  )

(use-package auto-compile
  :hook
  ((emacs-lisp-mode . auto-compile-on-load-mode)
   (emacs-lisp-mode . auto-compile-on-save-mode))
  :init
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t)
  )

(use-package delight)

(provide 'setup-packages)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; setup-packages.el ends here
