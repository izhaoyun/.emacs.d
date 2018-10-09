;;; -*- lexical-binding: t -*-

(use-package web-mode
  :mode
  (("\\.html?\\'" . web-mode)
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

(use-package htmlize)

(use-package yaml-mode
  :mode ("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)
  :bind
  (:map yaml-mode-map
        ("C-m" . newline-and-indent))
  )

(use-package flycheck-yamllint
  :after (flycheck yaml-mode)
  :hook (yaml-mode . flycheck-yamllint-setup)
  )

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode)
  )

(use-package css-mode
  :preface
  (defun css/init-company ()
    (set (make-local-variable 'company-backends)
         '(company-css
           company-etags
           company-capf
           company-yasnippet))
    (company-mode)
    )
  :hook (css-mode . css/init-company)
  )

(use-package rainbow-mode
  :hook
  (css-mode . rainbow-turn-on)
  )

(use-package json-mode)

(use-package json-snatcher)

(use-package json-reformat)

(use-package http
  :preface
  (defun my/pretty-json-buffer ()
    (json-reformat-region (point-min) (point-max))
    )
  :config
  ;; fontify response
  (add-to-list 'http-content-type-mode-alist
               '("application/json" . json-mode))
  ;; prettify response
  (add-to-list 'http-pretty-callback-alist
               '("application/json" . my/pretty-json-buffer))
  )

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  )

(use-package vmd-mode
  :after markdown-mode
  ;; :hook (markdown-mode . vmd-mode)
  )

(use-package npm-mode
  :load-path "site-lisp/npm-mode"
  )


(use-package restclient)

(use-package company-restclient
  :after company
  :init
  (add-to-list 'company-backends 'company-restclient)
  )

(provide 'init-web)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init-web.el ends here
