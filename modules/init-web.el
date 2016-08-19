(defconst my/web-packages
  '(web-mode
	impatient-mode
	)
  )

(install-packages my/web-packages)

(use-package web-mode
  :mode (("\\.phtml\\'"		. web-mode)
		 ("\\.tpl\\.php\\'" . web-mode)
		 ("\\.[agj]sp\\'"	. web-mode)
		 ("\\.as[cp]x\\'"	. web-mode)
		 ("\\.erb\\'"		. web-mode)
		 ("\\.mustache\\'"	. web-mode)
		 ("\\.djhtml\\'"	. web-mode)
		 ("\\.html?\\'"		. web-mode))
  :config
  ;; indentation
  (defun web-mode/indentation-hook ()
	(setq web-mode-markup-indent-offset 2)
	(setq web-mode-css-indent-offset 2)
	(setq web-mode-code-indent-offset 2)
	)
  (add-hook 'web-mode-hook 'web-mode/indentation-hook)

  (setq web-mode-ac-sources-alist
		'(("css" .  (ac-source-css-property))
		  ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  )

(use-package impatient-mode)

(provide 'init-web)
