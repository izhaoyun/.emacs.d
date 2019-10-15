;; configuration for making backup files
;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(defvar my-backup-directory
  (concat user-emacs-directory "backups"))
(if (not (file-exists-p my-backup-directory))
    (make-directory my-backup-directory t))
(setq backup-directory-alist '(("." . my-backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

(use-package ivy
  :bind
  (("C-x b"   . ivy-switch-buffer)
   ("C-c C-r" . ivy-resume))
  :bind
  (:map ivy-minibuffer-map
        ("C-c o" . ivy-occur)
        ("<tab>" . ivy-alt-done)
        ("C-i"   . ivy-partial-or-done)
        ("C-r"   . ivy-previous-line-or-history)
        ("M-r"   . ivy-reverse-i-search)
        ("<return>" . ivy-alt-done))
  :init
  (setq ivy-count-format "(%d/%d) "
        ivy-display-style 'fancy
        ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t
        ivy-wrap t)
  (ivy-mode 1)
  :config
  (ivy-set-occur 'swiper 'swiper-occur)
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
  )

(use-package swiper
  :bind
  (("C-c u a" . swiper-all)
   ("C-r" . swiper))
  :bind
  (:map swiper-map
        ("M-q" . swiper-query-replace)
        ("C-l" . swiper-recenter-top-bottom)
        ("C-'" . swiper-avy)
        ("M-c" . swiper-mc))
  )

(use-package counsel
  :bind
  (("C-s" . counsel-grep-or-swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-x r b" . counsel-bookmark)
   ("C-x r d" . counsel-bookmarked-directory)
   ("C-x C-b" . counsel-ibuffer)
   ("C-c c" . counsel-org-capture)
   ("<f2> m" . counsel-imenu)
   ("<f2> f" . counsel-git)
   ("<f2> g" . counsel-git-grep)
   ("<f2> s" . counsel-stash)
   ("<f2> b" . counsel-switch-to-shell-buffer)
   ("<f2> a" . counsel-ag)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)
   ("C-x m" . counsel-mark-ring)
   ("M-y" . counsel-yank-pop)
   ("C-c f" . counsel-git-log)
   ("M-s d" . counsel-dired-jump)
   ("M-s f" . counsel-file-jump)
   ("C-h a" . counsel-apropos)
   ("C-h g" . counsel-info-lookup-symbol)
   ("C-h u" . counsel-unicode-char)
   ("C-h l" . counsel-find-library)
   ("C-h L" . counsel-load-library)
   ("C-h b" . counsel-descbinds)
   ("C-h w" . woman))
  :bind
  (:map minibuffer-local-map
        ("C-r" . counsel-minibuf-history))
  :init
  (setq counsel-find-file-at-point t)
  )

(use-package avy
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-char-2))
  :init
  (avy-setup-default)
  )

(provide 'init-editor)
