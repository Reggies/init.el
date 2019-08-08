(defun my-nxml-mode-hook ()
    (setq nxml-child-indent 4
          nxml-attribute-indent 4
          nxml-slash-auto-complete-flag t))
(add-hook 'nxml-mode-hook '(lambda () (my-nxml-mode-hook)))
(provide 'nxml-custom)
