(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-completion-system 'ivy)
  )

(use-package counsel-projectile
  :init
  (counsel-projectile-mode)
  )

(use-package lsp-mode
  :commands lsp
  :hook
  ((python-mode c-mode c++-mode) . lsp)
  )

(use-package company
  :diminish company-mode
  :bind
  ("C-c y" . company-yasnippet)
  :hook
  (after-init . global-company-mode)
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
  :bind
  ("M-;" . comment-dwim-2)
  )

(use-package highlight-indent-guides
  :if window-system
  :diminish highlight-indent-guides-mode
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
  :hook prog-mode
  :init
  (setq clean-aindent-is-simple-indent t)
  )

(use-package dtrt-indent
  :diminish dtrt-indent-mode
  :hook
  (prog-mode . dtrt-indent-mode)
  :init
  (setq dtrt-indent-verbosity 0)
  )

(use-package aggressive-indent
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
  :hook
  ((prog-mode cmake-mode) . rainbow-delimiters-mode)
  )

(use-package magit
  :bind
  (("C-x g" . magit-status))
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  )

(provide 'init-develop)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
