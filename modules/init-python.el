(defconst my/python-packages
  '(anaconda-mode
    company-anaconda
    py-yapf))

(install-packages my/python-packages)

(defvar python-indent-guess-indent-offset nil)


(provide 'init-python)
