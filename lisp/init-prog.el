;;; -*- lexical-binding: t -*-

(use-package counsel-projectile
  :defer t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (counsel-projectile-mode)
  )

(use-package yasnippet
  :defer t
  :bind
  (:map yas-minor-mode-map
        ("<f2> x" . yas-expand)
        ("<f2> i" . yas-insert-snippet))
  :hook
  (after-init . yas-global-mode)
  )

(use-package yasnippet-snippets
  :after yasnippet
  :defer t
  )

(use-package company
  :defer t
  :diminish company-mode
  :bind
  ("C-c y" . company-yasnippet)
  :init
  (setq company-tooltip-limit 20
        company-idle-delay .3
        company-echo-delay 0
        company-show-numbers t)
  ;; :config
  ;; (setq company-begin-commands '(self-insert-command))
  )

(use-package company-quickhelp
  :if window-system
  :defer t
  :after company
  :bind
  (:map company-active-map
        ("M-h" . company-quickhelp-manual-begin))
  :hook
  (company-mode . company-quickhelp-mode)
  :init
  (setq company-quickhelp-delay nil)
  )

(use-package comment-dwim-2
  :defer t
  :bind
  ("M-;" . comment-dwim-2)
  )

(use-package highlight-indent-guides
  :defer t
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|
        highlight-indent-guides-auto-odd-face-perc 15
        highlight-indent-guides-auto-even-face-perc 15
        highlight-indent-guides-auto-character-face-perc 20)
  )

(use-package clean-aindent-mode
  :defer t
  :hook prog-mode
  :init
  (setq clean-aindent-is-simple-indent t)
  )

(use-package dtrt-indent
  :defer t
  :diminish dtrt-indent-mode
  :hook
  (prog-mode . dtrt-indent-mode)
  :init
  (setq dtrt-indent-verbosity 0)
  )

(use-package aggressive-indent
  :defer t
  :diminish aggressive-indent-mode
  :hook
  ((emacs-lisp-mode cmake-mode) . aggressive-indent-mode)
  :config
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode)
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line)))))
  )

(use-package rainbow-delimiters
  :defer t
  :hook
  ((prog-mode cmake-mode) . rainbow-delimiters-mode)
  )

(use-package magit
  :defer t
  :bind
  (("C-x g" . magit-status))
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  )

(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  )

(use-package hideshow
  :defer t
  :diminish hs-minor-mode
  )

(use-package diff-hl
  :defer 15
  :hook
  ((prog-mode vc-dir-mode) . turn-on-diff-hl-mode)
  )

(use-package counsel-etags
  :defer t
  :init
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  :config
  (progn
    ;; counsel-etags-ignore-directories does NOT support wildcast
    (add-to-list 'counsel-etags-ignore-directories "build_clang")
    (add-to-list 'counsel-etags-ignore-directories "build_clang")
    ;; counsel-etags-ignore-filenames supports wildcast
    (add-to-list 'counsel-etags-ignore-filenames "TAGS")
    (add-to-list 'counsel-etags-ignore-filenames "*.json"))
  ;; Setup auto update now
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))
  )

(use-package eldoc-overlay
  :defer t
  :bind
  ("<f1> j" . eldoc-overlay-toggle)
  :init
  (diminish 'eldoc-mode)
  )

(use-package inline-docs
  :disabled
  :defer t
  :init
  (setq eldoc-message-function #'inline-docs)
  )

(provide 'init-prog)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;;  init-prog.el ends here
