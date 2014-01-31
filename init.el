(setq-default indent-tabs-mode nil
	      inhibit-startup-screen t
              completion-ignore-case t
              read-file-name-completion-ignore-case nil
              column-number-mode t
              comment-column 90
              fill-column 100
              scroll-error-top-bottom nil
              blink-matching-delay 0.25
              backward-delete-char-untabify-method 'hungry
              mark-even-if-inactive nil
              truncate-lines t
              compilation-scroll-output t

              ;; It should be done together
              initial-frame-alist '((cursor-type . box))
              blink-cursor-alist '((box . hollow))              
              
              ;; awk for all
              c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (other . "awk"))

              ;; All we love xmonad
              window-combination-resize t
              focus-follows-mouse t

              ;; `insert-parentheses' is quite handy in any language!
              parens-require-spaces nil)

(require 'fringe)
(set-fringe-mode '(5 . 0))                     ;; left only fringes

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(pending-delete-mode t)
(show-paren-mode t)

(defun move-indent-or-beginning-of-line ()
  "Function sets point at the first non-whitespace character in the current line and further sets point at the beginning of line."
  (interactive)
  (handle-shift-selection)
  (let ((old-point (point)))
    (back-to-indentation)
    (when (equal old-point (point))
        (move-beginning-of-line 1))))

(setq auto-mode-alist 
      (append '(("\\.h\\'" . c++-mode)
                ("\\.c\\'" . c++-mode))
              auto-mode-alist))

(add-hook 'find-file-hook
          (lambda ()
            (auto-save-mode -1)))

(add-hook 'c++-mode-hook
          (lambda ()
            (set (make-local-variable 'make-backup-files) nil)
            (define-key c++-mode-map (kbd "RET") 'newline-and-indent)))

(defun run-compilation ()
  (interactive)
  (compile "make -k"))

(setq special-display-buffer-names
      '("*compilation*"))

(setq special-display-function
      (lambda (buffer &optional args)
        (split-window)
        (switch-to-buffer buffer)
        (get-buffer-window buffer 0)))

(setq handy-keys-mode-map 
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-<f4>") 'ff-find-other-file)
        (define-key map (kbd "C-a") 'move-indent-or-beginning-of-line)
        (define-key map (kbd "<home>") 'move-indent-or-beginning-of-line)
        (define-key map (kbd "M-p") 'backward-kill-word)
        (define-key map (kbd "<f5>") 'run-compilation)
        map))

(define-minor-mode handy-keys-mode
  "Handy keys for your emacs."
  :lighter " Handy"
  :keymap handy-keys-mode-map
  :init-value t)
