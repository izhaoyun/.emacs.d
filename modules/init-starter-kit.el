(defconst my/starter-kit-packages
  '(ivy
	swiper
	counsel
	hydra
	avy
	ace-window
	lispy
	which-key
	hi-lock
	expand-region
	rainbow-delimiters
	undo-tree
	iedit
	highlight-symbol
	ace-pinyin
	chinese-fonts-setup
	)
  )

(install-packages my/starter-kit-packages)

(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "RET") 'newline-and-indent)

(use-package hydra)

(use-package counsel
  :commands (ivy-mode)
  :preface
  (defun ivy-dired ()
	(interactive)
	(if ivy--directory
	(ivy-quit-and-run
	 (dired ivy--directory)
	 (when (re-search-forward
		(regexp-quote
		 (substring ivy--current 0 -1)) nil t)
	   (goto-char (match-beginning 0))))
	  (user-error
	   "Not completing files currently")))
  :bind (("C-s" . counsel-grep-or-swiper)
	 ("M-x" . counsel-M-x)
	 ("M-y" . counsel-yank-pop)
	 ("C-r" . ivy-resume)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x r b" . counsel-bookmark)
	 ("C-c s a" . counsel-ag)
	 ("C-c s f" . counsel-git)
	 ("C-c s i" . counsel-imenu)
	 ("C-c s p" . counsel-git-grep)
	 ("C-c s l" . counsel-locate)
	 ("C-c s t" . counsel-tmm)
	 ("C-c s r" . counsel-load-library)
	 ("C-c s n" . counsel-linux-app)
	 ("C-c u"   . swiper-all)
	 ("C-c v"   . ivy-push-view)
	 ("C-c V"   . ivy-pop-view))
  :bind (:map help-map
		  ("b" . counsel-descbinds)
		  ("f" . counsel-describe-function)
		  ("v" . counsel-describe-variable)
		  ("s" . counsel-info-lookup-symbol)
		  ("u" . counsel-unicode-char))
  :bind (:map ivy-minibuffer-map
		  ("C-:" . ivy-dired)
		  ("C-c o" . ivy-occur))
  :init
  (use-package ivy
	:init
	(ivy-mode 1)
	(diminish 'ivy-mode)
	:config
	(setq ivy-use-virtual-buffers t)
	(setq ivy-display-style 'fancy)
	(setq ivy-count-format "(%d/%d) ")
	)
  :config
  (setq counsel-find-file-at-point t)
  )

(use-package avy
  :bind (("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-char-2)
	 ("M-g f" . avy-goto-line)
	 ("M-g w" . avy-goto-word-1)
	 ("M-g e" . avy-goto-word-0)
	 ("M-g c" . avy-goto-conditional)
	 ("M-g p" . avy-goto-paren))
  :init
  (avy-setup-default)
  (eval-and-compile
	;; Jumping to conditionals in Elisp
	(defun avy-goto-conditional ()
	  (interactive)
	  (avy--generic-jump "\\s(\\(if\\|cond\\|when\\|unless\\)\\b" nil 'pre))
	(defun avy-goto-paren ()
	  (interactive)
	  (avy--generic-jump "(" nil 'pre))
	)
  :config
  (advice-add 'swiper :before 'avy-push-mark))

(use-package ace-window
  :bind ("M-p" . ace-window))

(use-package iedit
  :commands (iedit-mode)
  :bind ("C-c i e" . iedit-mode)
  )

(use-package lispy
  :diminish lispy-mode
  :init
  ;; enable lispy automatically for emacs-lisp-mode
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
  ;; enable lispy for eval-expression
  (defun conditionally-enable-lispy ()
	(when (eq this-command 'eval-expression)
	  (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
  )

(use-package which-key
  :diminish which-key-mode
  :commands (which-key-mode
			 which-key-setup-side-window-right-bottom)
  :init
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  )

(use-package highlight-symbol
  :bind (("C-<f3>" . highlight-symbol)
	 ("<f3>"   . highlight-symbol-next)
	 ("S-<f3>" . highlight-symbol-prev)
	 ("M-<f3>" . highlight-symbol-query-replace))
  )

(use-package expand-region
  :bind (("C-=" . er/expand-region)
		 ("C--" . er/contract-region))
  )

(use-package paren
  :init
  (show-paren-mode)
  :config
  (setq show-paren-style 'expression)
  )

(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-saved-filter-groups
	'("default"
	  ("Commands" (or (mode . shell-mode)
			  (mode . eshell-mode)
			  (mode . term-mode)
			  (mode . compilation-mode)))
	  ("C++" (or (mode . c-mode)
			 (mode . c++-mode)))
	  ("Magit" (or (mode . magit-status-mode)
			   (mode . magit-log-mode)
			   (name . "^\\*magit")
			   (name . "git-monitor")))
	  ("Emacs" (or (mame . "^\\*scratch\\*$")
			   (name . "^\\*Messages\\*$" )
			   (name . "^\\*\\(Customize\\|Hellp\\)" )
			   (name . "\\*\\(Echo\\|Minibuf\\)" )))
	  ("lisp" (mode . emacs-lisp-mode))))
  )

(use-package winner
  :if (not noninteractive)
  :defer 5
  :config
  (winner-mode 1))

(use-package recentf
  :defer t
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (recentf-mode)
  )

(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :commands (global-undo-tree-mode)
  :bind (("C-z"   . undo)
	 ("C-S-z" . undo-tree-redo))
  :config
  (progn
	(global-undo-tree-mode)
	(setq undo-tree-visualizer-diff t)
	(setq undo-tree-visualizer-timestamps t))
  )

(use-package hippie-exp
  :commands (hippie-expand)
  :bind ("M-/" . hippie-expand))

(use-package ace-pinyin
  :diminish ace-pinyin-mode
  :after avy
  :config
  (ace-pinyin-global-mode)
  )

(use-package chinese-fonts-setup
  :config
  (setq cfs-profiles
	'("program" "org-mode" "read-book"))
  (setq cfs--current-profile "program")
  (setq cfs--profiles-steps (quote (("program" . 3))))
  )

(use-package flyspell
  :diminish flyspell-mode
  :commands (flyspell-mode
		 flyspell-prog-mode)
  :init
  ;; if (aspell installed) { use aspell}
  ;; else if (hunspell installed) { use hunspell }
  ;; whatever spell checker I use, I always use English dictionary
  ;; I prefer use aspell because:
  ;; 1. aspell is older
  ;; 2. looks Kevin Atkinson still get some road map for aspell:
  ;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
  (defun flyspell-detect-ispell-args (&optional run-together)
	"if RUN-TOGETHER is true, spell check the CamelCase words."
	(let (args)
	  (cond
	   ((string-match  "aspell$" ispell-program-name)
	;; Force the English dictionary for aspell
	;; Support Camel Case spelling check (tested with aspell 0.6)
	(setq args (list "--sug-mode=ultra" "--lang=en_US"))
	(if run-together
		(setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
	   ((string-match "hunspell$" ispell-program-name)
	;; Force the English dictionary for hunspell
	(setq args "-d en_US")))
	  args))

  (cond
   ((executable-find "aspell")
	;; you may also need `ispell-extra-args'
	(setq ispell-program-name "aspell"))
   ((executable-find "hunspell")
	(setq ispell-program-name "hunspell")

	;; Please note that `ispell-local-dictionary` itself will be passed to hunspell cli with "-d"
	;; it's also used as the key to lookup ispell-local-dictionary-alist
	;; if we use different dictionary
	(setq ispell-local-dictionary "en_US")
	(setq ispell-local-dictionary-alist
	  '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
   (t (setq ispell-program-name nil)))

  ;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
  ;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
  ;; Please note when you use hunspell, ispell-extra-args will NOT be used.
  ;; Hack ispell-local-dictionary-alist instead.
  (setq-default ispell-extra-args (flyspell-detect-ispell-args t))
  ;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
  (defadvice ispell-word (around my-ispell-word activate)
	(let ((old-ispell-extra-args ispell-extra-args))
	  (ispell-kill-ispell t)
	  (setq ispell-extra-args (flyspell-detect-ispell-args))
	  ad-do-it
	  (setq ispell-extra-args old-ispell-extra-args)
	  (ispell-kill-ispell t)
	  ))

  (defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
	(let ((old-ispell-extra-args ispell-extra-args))
	  (ispell-kill-ispell t)
	  ;; use emacs original arguments
	  (setq ispell-extra-args (flyspell-detect-ispell-args))
	  ad-do-it
	  ;; restore our own ispell arguments
	  (setq ispell-extra-args old-ispell-extra-args)
	  (ispell-kill-ispell t)
	  ))

  (defun text-mode-hook-setup ()
	;; Turn off RUN-TOGETHER option when spell check text-mode
	(setq-local ispell-extra-args (flyspell-detect-ispell-args)))
  (add-hook 'text-mode-hook 'text-mode-hook-setup)

  :config
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  )



(provide 'init-starter-kit)
;; init-starter-kit.el ends here.
