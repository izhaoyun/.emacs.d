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

(use-package inf-ruby
  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  :config
  (use-package company-inf-ruby
	:config

	)
  )

(use-package robe
  :init
  (add-hook 'ruby-mode-hook 'robe-mode)
  :config
  (eval-after-load 'company
	'(push 'company-robe company-backends))
  )

(use-package puppet-mode
  :mode ("\\.pp$" . puppet-mode)
  )

(provide 'init-ruby)
