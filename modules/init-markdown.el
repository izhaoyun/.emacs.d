(use-package markdown-mode
  :commands (markdown-mode
			 gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode))
  )

(defun my/init-markdown-preview-eww ()
  (use-package markdown-preview-eww)
  )
(add-hook 'markdown-mode-hook 'my/init-markdown-preview-eww)
(add-hook 'gfm-mode-hook 'my/init-markdown-preview-eww)

(provide 'init-markdown)
