(defconst my/ruby-packages
  '(inf-ruby
	robe
	yari
	ruby-tools
	puppet-mode
	bundler
	company-inf-ruby
	)
  )

(install-packages my/ruby-packages)

(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode)
		 ("\\.rake$" . ruby-mode)
		 ("\\.gemspec$" . ruby-mode))
  :interpreter "ruby"
  )

(defun ruby/init-inf-ruby ()
  (progn
	(use-package inf-ruby
	  :init
	  (inf-ruby-minor-mode 1)
	  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
	  )
	(use-package company-inf-ruby)
	)
  )
(add-hook 'ruby-mode-hook 'ruby/init-inf-ruby)

(defun ruby/init-robe ()
  (use-package robe
	:init
	(robe-mode)
	:config
	(push 'company-robe company-backends)
	)
  )
(add-hook 'ruby-mode-hook 'ruby/init-robe)

(use-package puppet-mode
  :mode ("\\.pp$" . puppet-mode)
  )

(provide 'init-ruby)
