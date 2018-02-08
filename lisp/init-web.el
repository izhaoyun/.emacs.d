;;; -*- lexical-binding: t -*-

(use-package web-mode
  :defer t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ;; associate a content type
         ("\\.api\\'" . web-mode)
         ("/some/react/path/.*\\.js[x]?\\'" . web-mode))
  :init
  ;; associate an engine
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\.")))
  ;; associate a content type
  (setq web-mode-content-types-alist
        '(("json" . "/some/path/.*\\.api\\'")
          ("xml"  . "/other/path/.*\\.api\\'")
          ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))

  ;; indentation
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)

  ;; comments
  (setq web-mode-comment-style 2)

  ;; left padding
  (setq web-mode-style-padding 1
        web-mode-script-padding 1
        web-mode-block-padding 0)

  (setq web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t
        web-mode-enable-block-face t
        web-mode-enable-part-face t
        web-mode-enable-comment-keywords t
        web-mode-enable-heredoc-fontification t
        web-mode-enable-current-element-highlight t)
  )

(use-package yaml-mode
  :defer t
  :mode ("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
  :bind (:map yaml-mode-map
              ("C-m" . newline-and-indent))
  )

(use-package js2-mode
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode)
  )

(use-package css-mode
  :disabled
  :defer t
  :init
  (push 'company-css company-backends)
  )

(provide 'init-web)
;;; init-web.el ends here
