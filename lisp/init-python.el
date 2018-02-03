;;; -*- lexical-binding: t -*-

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  )

(use-package elpy
  :defer t
  :hook (python-mode . elpy-enable)
  )

(use-package sphinx-doc
  :defer t
  :hook (python-mode . sphinx-doc-mode)
  )

(provide 'init-python)
;;; init-python.el ends here
