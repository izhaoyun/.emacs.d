(setq gc-cons-threshold 125829120)

(defconst emacs-start-time (current-time))

(eval-and-compile
  (require 'package)
  (add-to-list 'package-archives '("melpa"  . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("org"    . "http://orgmode.org/elpa/"))
  (add-to-list 'package-archives '("popkit" . "https://elpa.popkit.org/packages/"))
  (add-to-list 'package-archives '("SC"     . "http://joseito.republika.pl/sunrise-commander/"))
  (package-initialize)

  ;; get `use-package' installed.
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package)
  (require 'diminish)
  (require 'bind-key)
  )

(eval-and-compile
  (setq load-prefer-newer t)
;;;###autoload
  (defun recompile-elisp-file ()
    (interactive)
    (when (and buffer-file-name (string-match "\\.el" buffer-file-name))
      (let ((byte-file (concat buffer-file-name "\\.elc")))
	(if (or (not (file-exists-p byte-file))
		(file-newer-than-file-p buffer-file-name byte-file))
	    (byte-compile-file buffer-file-name)))))
  (add-hook 'after-save-hook #'recompile-elisp-file)
  )

(eval-and-compile
  (defun install-packages (packages-list)
    (dolist (package packages-list)
      (unless (package-installed-p package)
	(package-install package)
	)
      )
    )
  )

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "modules" user-emacs-directory)))

(require 'init-custom)
(require 'init-starter-kit)
(require 'init-devel)

(require 'init-c++)
(require 'init-python)
(require 'init-ruby)
(require 'init-org)

(require 'init-utils)

(when window-system
  (add-hook 'after-init-hook
	    `(lambda ()
	       (let ((elapsed (float-time (time-subtract (current-time)
							 emacs-start-time))))
		 (message "Loading %s...done (%.3fs) [after-init]"
			  ,load-file-name elapsed)))
	    t))
