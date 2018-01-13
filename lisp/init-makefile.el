;;; init-makefile --- Makefile Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :preface
  (defun init-c-c++/setup-company-cmake ()
    (use-package company-cmake
      :commands (company-cmake)
      :init
      (push 'company-cmake company-backends)
      )
    )
  :init
  (add-hook 'cmake-mode-hook 'init-c-c++/setup-company-cmake)
  )

;; @github: Lindydancer/cmake-font-lock
(use-package cmake-font-lock
  :defer t
  :hook (cmake-mode . cmake-font-lock-activate)
  )


(provide 'init-makefile)
;;; init-makefile.el ends here
