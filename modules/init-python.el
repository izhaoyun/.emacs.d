(defconst my/python-packages
  '(anaconda-mode
    company-anaconda
    py-yapf
    pip-requirements
    sphinx-doc))

(install-packages my/python-packages)


(defvar python-indent-guess-indent-offset nil)

(defun my/init-anaconda-mode ()
  (require 'anaconda-mode)
  (anaconda-mode)
  )
(add-hook 'python-mode-hook 'my/init-anaconda-mode)

;; (use-package py-yapf
;;   :defer t
;;   :commands (py-yapf-enable-on-save)
;;   :config
;;   (add-hook 'python-mode-hook 'py-yapf-enable-on-save))

(provide 'init-python)
