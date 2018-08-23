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

(use-package use-package
  :init
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-verbose t)
  )

(use-package diminish)

(use-package bind-key)

(use-package system-packages
  :init
  (setq system-packages-use-sudo t
        system-packages-package-manager 'apt)
  )

(use-package use-package-ensure-system-package
  :after (system-packages)
  )

(use-package use-package-chords
  :init
  (key-chord-mode 1)
  )

(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t
        auto-package-update-prompt-before-update t
        auto-package-update-interval 14)
  (auto-package-update-maybe)
  )

(provide 'setup-packages)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; setup-packages.el ends here
