;;; install-packages --- Installed Packages Lists -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;(require 'cl-lib)

(defun my/packages-installed-p ()
  (loop for pkg in package-selected-packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg package-selected-packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(provide 'install-packages)
;;; install-packages.el ends here
