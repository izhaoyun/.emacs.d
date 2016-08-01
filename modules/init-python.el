(defconst my/python-packages
  '(elpy))

(install-packages my/python-packages)

(defvar python-indent-guess-indent-offset nil)

(use-package elpy
  :init
  (elpy-enable))

(provide 'init-python)
