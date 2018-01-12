;;; init-python --- Python Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  )

;; @github: proofit404/anaconda-mode
(use-package anaconda-mode
  :after python
  :commands (anaconda-mode
             anaconda-eldoc-mode)
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

  ;; @github: proofit404/company-anaconda
  (use-package company-anaconda
    :after company
    :init
    (push 'company-anaconda company-backends)
    )
  )

;; @github: naiquevin/sphinx-doc.el
(use-package sphinx-doc
  :after python
  :commands (sphinx-doc-mode)
  :init
  (add-hook 'python-mode-hook 'sphinx-doc-mode)
  )

;; @github: donkirkby/live-py-plugin
(use-package live-py-mode
  :after python
  :commands (live-py-mode)
  )

(use-package ansible)

(provide 'init-python)
;;; init-python.el ends here
