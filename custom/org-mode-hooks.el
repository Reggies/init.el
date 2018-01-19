
(defun enable-visual-line-mode ()
  (ignore-errors (visual-line-mode)))

(when (fboundp 'visual-line-mode)
  (add-hook 'org-mode-hook 'enable-visual-line-mode))
