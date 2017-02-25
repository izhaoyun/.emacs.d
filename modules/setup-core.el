;;; -*- lexical-binding: t; -*-

;; swipper
(use-package counsel
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
  :bind (("C-s" . counsel-grep-or-swiper)
	 ("C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("M-y" . counsel-yank-pop)
	 ("C-x C-f" . counsel-find-file)
	 ("C-x C-r" . counsel-recentf)
	 ("C-x C-b" . counsel-bookmark)))

(provide 'setup-core)
