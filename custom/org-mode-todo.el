
(add-to-list 'auto-mode-alist '("TODO\\'" . org-mode))

(setq-default org-todo-keyword-faces
              '(("WONTFIX" . (:foreground "gray" :weight bold))
                ("FAIL" . (:foreground "red" :weight bold))))

(setq-default org-todo-keywords
              '((sequence "TODO" "DONE" "FAIL" "WONTFIX")))
