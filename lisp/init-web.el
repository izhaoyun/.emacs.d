;;; init-web --- html/css/javascript related stuff -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package web-mode
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
  :config
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
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)

  ;; comments
  (setq web-mode-comment-style 2)

  ;; left padding
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0)

  ;; enable css colorization
  (setq web-mode-enable-css-colorization t)
  ;; enable auto-paring
  (setq web-mode-enable-auto-pairing t)

  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-comment-keywords t)
  (setq web-mode-enable-heredoc-fontification t)
  ;; highlight current html element
  (setq web-mode-enable-current-element-highlight t))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package markdown-mode)

(provide 'init-web)
;;; init-web.el ends here
