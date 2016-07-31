;;;###autoload
(defun my/ruby-hook ()
  (setq tab-width 2)
  (add-hook 'write-file-functions
	    (lambda ()
	      (save-excursion
		(untabify (point-min) (point-max))
		(delete-trailing-whitespace))))
  )
(add-hook 'ruby-mode-hook 'my/ruby-hook)

(provide 'init-ruby)
