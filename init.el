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

              default-frame-alist '((left-fringe . 5)
                                    (right-fringe . 0)
                                    (background-mode . 'dark))

              auto-insert-query nil

              ;; It should be done together
              initial-frame-alist '((cursor-type . box))
              blink-cursor-alist '((box . hollow))

              tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)

              ;; cc-mode
              c-basic-offset 4

              c-offsets-alist '(cons (statement-case-open . 4)
                                     c-offsets-alist)

              c-default-style '((java-mode . "java")
                                (other . "awk"))

              cc-other-file-alist '(("\\.cc$" ff-cc-hh-converter)
                                    ("\\.hh$" ff-cc-hh-converter)
                                    ("\\.c$" (".h"))
                                    ("\\.hpp$" (".cpp" ".c" ".cc" ".C"))
                                    ("\\.h$" (".cpp" ".c" ".cc" ".C" ".CC" ".cxx"))
                                    ("\\.cpp$" (".h" ".hpp")))
              ;; All we love xmonad
              window-combination-resize t
              focus-follows-mouse t)

(autoload 'glsl-mode "glsl-mode" nil t)
(setq load-path (cons "~/.emacs.d" load-path))

;; (require 'package)
;; (add-to-list 'package-archives 
;;              '("melpa" . "http://melpa.milkybox.net/packages/") t)

(add-to-list 'load-path "~/.emacs.d/yasnippet.el")

(require 'fringe)
(set-fringe-mode '(5 . 0))                     ;; left only fringes

(require 'yasnippet)
(yas-global-mode 1)
(auto-insert-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(pending-delete-mode t)
(show-paren-mode t)
(global-linum-mode t)

(load-theme 'tango)

(defun move-indent-or-beginning-of-line ()
  "Jumps on the line indent and than on the beginning of line."
  (interactive)
  (handle-shift-selection)
  (let ((old-point (point)))
    (back-to-indentation)
    (when (equal old-point (point))
      (move-beginning-of-line 1))))

(defun compile-current-dir ()
  (interactive)
  (compile "make -k"))

(setq auto-mode-alist 
      (append '(("\\.h\\'" . c++-mode)
                ("\\.c\\'" . c++-mode)
                ("\\.vert\\'" . glsl-mode)
                ("\\.vertex\\'" . glsl-mode)
                ("\\.frag\\'" . glsl-mode)
                ("\\.pixel\\'" . glsl-mode)
                ("\\.geom\\'" . glsl-mode)
                ("\\.gs\\'" . glsl-mode)
                ("\\.vs\\'" . glsl-mode)
                ("\\.fs\\'" . glsl-mode))
              auto-mode-alist))

(add-hook 'find-file-hook
          (lambda ()
            (auto-save-mode -1)))

(setq special-display-buffer-names
      '("*compilation*"))

(setq special-display-function
      (lambda (buffer &optional args)
        (delete-other-windows)
        (split-window-right)
        (get-buffer-window buffer 0)))

(defun switch-to-header ()
  "Find other file ignoring includes"
  (interactive)
  (ff-find-other-file nil t))

(defun reload-init-file ()
  "Just load configuration again"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defcustom shrink-delta 10 
  "Used as DELTA in `shrink-window-horizontally'")

(defun upcase-char (n)
  "Upcase forward character"
  (interactive "p")
  (upcase-region (point)
                 (forward-point n))
  (forward-char n))

(defun downcase-char (n)
  "Downcase forward character"
  (interactive "p")
  (downcase-region (point)
                   (forward-point n))
  (forward-char n))

(setq handy-keys-mode-map 
      (let ((map (make-sparse-keymap)))
        ;; ff-find-other-file [in other buffer = nil] [ignore includes = t]
        (define-key map (kbd "C-<f4>") 'switch-to-header)

        (define-key map (kbd "C-<f5>") 'reload-init-file)

        ;; ff-find-other-file [in other buffer = nil] [ignore includes = nil]
        (define-key map (kbd "C-x C-o") 'ff-find-other-file)

        (define-key map (kbd "C-x C-r") 'replace-string)

        (define-key map (kbd "M-\"") 'insert-pair)
        (define-key map (kbd "M-<") 'insert-pair)

        (define-key map (kbd "C-;") 'upcase-char)
        (define-key map (kbd "C-l") 'downcase-char)

        ;; this pair is for backward compatibility, if you found you don't need you anymore - purge it
        (define-key map (kbd "C-S-u") 'upcase-char)
        (define-key map (kbd "C-S-l") 'downcase-char)

        (define-key map (kbd "C-a") 'move-indent-or-beginning-of-line)
        (define-key map (kbd "<home>") 'move-indent-or-beginning-of-line)
        (define-key map (kbd "M-p") 'backward-kill-word)

        (define-key map (kbd "<f5>") 'compile-current-dir)

        (define-key map (kbd "<f12>") (lambda ()
                                        (interactive)
                                        (async-shell-command "./a.out")))

        (define-key map (kbd "C-}") (lambda ()
                                      (interactive)
                                      (shrink-window-horizontally (- shrink-delta))))

        (define-key map (kbd "C-{") (lambda ()
                                      (interactive)
                                      (shrink-window-horizontally shrink-delta)))
        map))

(define-minor-mode handy-keys-mode
  "Handy keys for your emacs."
  :lighter " Handy"
  :keymap handy-keys-mode-map
  :init-value t)

;;
;; Disable backup files for C++
(add-hook 'c++-mode-hook
          (lambda ()
            (set (make-local-variable 'make-backup-files) nil)))

;;
;; \C-( shall not produce any space chars
(add-hook 'c++-mode-hook
          (lambda ()
            (set (make-local-variable 'parens-require-spaces) nil)))

;;
;; Indent new line after RET
(add-hook 'c++-mode-hook
          (lambda ()
            (define-key c++-mode-map (kbd "RET") 'newline-and-indent)))

;;
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

;; Fixin some c++11 keywords as in http://stackoverflow.com/a/17087959
(add-hook
 'c++-mode-hook
 '(lambda()
    (font-lock-add-keywords
     nil '(("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
           ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-keyword-face)

           ;; PREPROCESSOR_CONSTANT, PREPROCESSORCONSTANT
           ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
           ("\\<[A-Z]\\{5,\\}\\>"  . font-lock-constant-face)

           ;; hexadecimal numbers
           ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)

           ;; integer/float/scientific numbers
           ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\f?\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
           ))
    ) t)

;; auto insert include guard into newly created CPP header
(define-auto-insert
  (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "Include guard")
  '(nil
    (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
           (nopath (file-name-nondirectory noext))
           (ident (concat (upcase nopath) "_H_")))
      (concat "#ifndef " ident "\n"
              "#define " ident "\n\n\n"
              "#endif // " ident "\n"))))

(defun get-some (xs)
  (or (car xs)
      (get-some (cdr xs))))

;; auto insert #include into newly created source CPP file
(define-auto-insert
  (cons "\\.\\([Cc]\\|cc\\|cpp\\)\\'" "Include header")
  '(nil
    (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
           (nopath (file-name-nondirectory noext))
           (ident (get-some (mapcar (lambda (ext)
                                      (let ((filename (concat nopath ext)))
                                        (if (file-exists-p filename)
                                            filename
                                          nil)))
                                    (list ".h" ".hpp")))))
      (if (file-exists-p ident)
          (concat "#include \"" ident "\"\n")))))
