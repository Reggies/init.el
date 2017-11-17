
(add-to-list 'auto-mode-alist '("TODO\\'" . org-mode))

(setq-default org-todo-keyword-faces
              '(("WONTFIX" . (:foreground "gray" :weight bold :box t))
                ("FAIL" . (:foreground "red" :weight bold :box t))))

(setq-default org-todo-keywords
              '((sequence "TODO" "WONTFIX" "FAIL" "DONE")))
