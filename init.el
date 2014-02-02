(setq-default indent-tabs-mode nil
	      inhibit-startup-screen t
              completion-ignore-case t
              read-file-name-completion-ignore-case nil
              column-number-mode t
              comment-column 60
              fill-column 60
              scroll-error-top-bottom nil
              blink-matching-delay 0.25
              backward-delete-char-untabify-method 'hungry
              mark-even-if-inactive nil
              truncate-lines nil
              compilation-scroll-output t
              case-fold-search nil

              ;; It should be done together
              initial-frame-alist '((cursor-type . box))
              blink-cursor-alist '((box . hollow))              
              
              ;; awk for all
              c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (other . "awk"))

              ;; All we love xmonad
              window-combination-resize t
              focus-follows-mouse t)

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
            (set (make-local-variable 'parens-require-spaces) nil)
            (define-key c++-mode-map (kbd "RET") 'newline-and-indent)))

(defun run-compilation ()
  (interactive)
  (compile "make -k"))

(setq special-display-buffer-names
      '("*compilation*"))

(setq special-display-function
      (lambda (buffer &optional args)
        (split-window-right)
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

;; C++11 Enum classes hack from gist.github.com/2626303
(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)
