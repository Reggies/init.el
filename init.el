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
              compilation-scroll-output t
              case-fold-search nil

              auto-insert-query nil

              default-frame-alist '((vertical-scroll-bars . nil)
                                    (tool-bar-lines . 0)
                                    (menu-bar-lines . 0)
                                    (left-fringe . 5)
                                    (right-fringe . 0)
                                    (background-mode . dark)
                                    (fullscreen . nil)
                                    (cursor-type . box)
                                    (font . "Dejavu Sans Mono-10"))

              blink-cursor-alist '((box . hollow))

              tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)

              ;; prevent <menu-bar> <file> <exit-emacs> from popup dialogs
              use-dialog-box nil

              ;; All we love xmonad
              window-combination-resize t
              focus-follows-mouse t

              windmove-wrap-around t)

(windmove-default-keybindings 'meta)

(add-to-list 'load-path user-emacs-directory)
(load "cc-mode-tricks")

(add-to-list 'load-path (concat user-emacs-directory "helm/"))
(require 'helm-config)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;;
;; Custom keys
(autoload 'handy-keys-mode "handy-keys" nil t)
(require 'handy-keys)
(handy-keys-mode t)

(autoload 'yasnippet-mode "yasnippet" nil t)
(require 'yasnippet)
(yas-global-mode 1)

(scroll-bar-mode -1)

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

;; TODO find out how can we locate files outside current directory
;; (setq ff-search-directories
;;       '("." "../include" "../include/*" "../source" "../Include" "../Include/*" "../Source" "../src"))
