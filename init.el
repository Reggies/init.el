(load-theme 'tango-dark)

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
              case-fold-search nil
              tab-width 4
              auto-insert-query nil

              default-frame-alist '((vertical-scroll-bars . nil)
                                    (tool-bar-lines . 0)
                                    (menu-bar-lines . 0)
                                    (left-fringe . 5)
                                    (right-fringe . 0)
                                    (background-mode . dark)
                                    (fullscreen . nil)
                                    (cursor-type . box)
                                    (font . "Monospace-10")
                                    )

              blink-cursor-alist '((box . hollow))

              tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)

              ;; prevent <menu-bar> <file> <exit-emacs> from popup dialogs
              use-dialog-box nil

              confirm-kill-emacs nil

              ;; All we love xmonad
              window-combination-resize t
              focus-follows-mouse t

              windmove-wrap-around t)

(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'load-path (concat user-emacs-directory "local.el/"))
(add-to-list 'load-path (concat user-emacs-directory "dockerfile-mode/"))

(scroll-bar-mode -1)

(load "google-c-style")
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

;; Docker
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;
;; Python!
(load "py-mode-tricks")

;;
;; CC mode
(load "cc-mode-tricks")

;;
;; Custom keys
(autoload 'handy-keys-mode "handy-keys" nil t)
(require 'handy-keys)
(handy-keys-mode t)

;;
;; yasnippet
(autoload 'yasnippet-mode "yasnippet" nil t)
(require 'yasnippet)
(yas-global-mode 1)

;;
;; markdown mode
(autoload 'markdown-mode "markdown-mode" nil t)

;;
;; Configure auto-mode for markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;
;; Qt
(setq auto-mode-alist
      (append '(("\\.pro\\'" . text-mode)
                ("\\.qrc\\'" . xml-mode))
              auto-mode-alist))

;;
;; Octave
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(require 'hippie)
(global-set-key (kbd "M-/") 'hippie-expand)
(add-to-list 'hippie-expand-try-functions-list 'try-expand-flexible-abbrev)

;;
;; Configure auto-mode for GLSL
(autoload 'glsl-mode "glsl-mode" nil t)
(setq auto-mode-alist
      (append '(("\\.vert\\'" . glsl-mode)
                ("\\.vertex\\'" . glsl-mode)
                ("\\.frag\\'" . glsl-mode)
                ("\\.pixel\\'" . glsl-mode)
                ("\\.geom\\'" . glsl-mode)
                ("\\.gs\\'" . glsl-mode)
                ("\\.vs\\'" . glsl-mode)
                ("\\.fs\\'" . glsl-mode))
              auto-mode-alist))

(auto-insert-mode t)
(pending-delete-mode t)
(show-paren-mode t)
(electric-pair-mode t)

(require 'linum)
(global-linum-mode t)

;;
;; Placing all temporary files into /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;
;; Delete other windows before display special buffer
(setq special-display-buffer-names
      '("*compilation*"
        "*Helm Find Files*"))
(setq special-display-function
      (lambda (buffer &optional args)
        (delete-other-windows)
        (split-window-right)
        (get-buffer-window buffer 0)))

;; An attempt to speedup kmacro-call-macro by disabling font-lock-mode
(when (fboundp 'advice-add)
  (advice-add 'kmacro-call-macro :around 
              (lambda (orig &rest args)
                (progn
                  (font-lock-mode -1)
                  (ignore-errors
                    (apply orig args))
                  (font-lock-mode 1)))))

;; TODO find out how can we locate files outside current directory
;; (setq ff-search-directories
;;       '("." "../include" "../include/*" "../source" "../Include" "../Include/*" "../Source" "../src"))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
