(defun local-sgml-mode-hook
  (setq fill-column 80
        indent-tabs-mode nil
        next-line-add-newlines nil
        sgml-indent-data t)
  (auto-fill-mode t)
  (setq indent-line-function 'insert-tab)
  (setq nxml-child-indent 4
        nxml-attribute-indent 4
        nxml-slash-auto-complete-flag t))
(add-hook 'psgml-mode-hook '(lambda () (local-psgml-mode-hook)))
(provide 'nxml-custom)
