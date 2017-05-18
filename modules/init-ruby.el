;;; init-ruby --- Ruby packages configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package ruby-mode
  :ensure inf-ruby
  :mode ("\\.rb\\'" . ruby-mode)
  :interpreter ("ruby" . ruby-mode)
  :config
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

;; @github: voxpupuli/puppet-mode
(use-package puppet-mode)

(provide 'init-ruby)
;;; init-ruby.el ends here
