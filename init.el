(require 'package)
(setq package-archives '(("melpa"  . "http://melpa.org/packages/")
						 ("org"    . "http://orgmode.org/elpa/")
						 ("SC"     . "http://joseito.republika.pl/sunrise-commander/")))
(package-initialize)
(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'diminish)
(require 'bind-key)
(require 'use-package)

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

(eval-and-compile
  (defun install-packages (packages-list)
	(dolist (package packages-list)
	  (unless (package-installed-p package)
		(package-install package)))
	)
  )

(setq custom-file (expand-file-name "settings.el" user-emacs-directory))
(load custom-file)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'init-starter-kit)
(require 'init-devel)
(require 'init-utils)

(require 'init-org)
(require 'init-c++)
(require 'init-python)
(require 'init-ruby)
(require 'init-makefile)
(require 'init-erlang)
(require 'init-latex)
(require 'init-markdown)
(require 'init-shell)
(require 'init-web)
