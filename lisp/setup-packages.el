;;; -*- lexical-binding: t -*-

(setq package-enable-at-startup nil)
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

(require 'use-package)

(use-package system-packages
  :disabled
  :init
  (setq system-packages-use-sudo t
        system-packages-package-manager 'apt)
  )

(use-package use-package-ensure-system-package
  :disabled
  :after (system-packages)
  )

(use-package use-package-chords
  :disabled
  :init
  (key-chord-mode 1)
  )

(use-package auto-package-update
  :disabled
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t
        auto-package-update-prompt-before-update t
        auto-package-update-interval 14)
  (auto-package-update-maybe)
  )

(use-package delight)

(provide 'setup-packages)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; setup-packages.el ends here
