;; @github: haskell/haskell-mode
(use-package haskell-mode-autoloads
  :ensure haskell-mode
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . haskell-mode)
         ("\\.hsc\\'" . haskell-mode)
         ("\\.cpphs\\'" . haskell-mode)
         ("\\.c2hs\\'" . haskell-mode))
  :bind (:map haskell-mode-map
              ("C-c C-," . haskell-mode-format-imports))
  )

(provide 'init-haskell)
;;; init-haskell.el ends here
