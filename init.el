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
                                    (cursor-type . box))

              blink-cursor-alist '((box . hollow))

              tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)

              ;; All we love xmonad
              window-combination-resize t
              focus-follows-mouse t)

;; (require 'package)
;; (add-to-list 'package-archives 
;;              '("melpa" . "http://melpa.milkybox.net/packages/") t)

(add-to-list 'load-path user-emacs-directory)
(load "cc-mode-tricks")

(autoload 'glsl-mode "glsl-mode" nil t)
(autoload 'yasnippet-mode "yasnippet" nil t)
(autoload 'handy-keys-mode "handy-keys" nil t)

(require 'yasnippet)
(yas-global-mode 1)

(auto-insert-mode t)
(pending-delete-mode t)
(show-paren-mode t)
(global-linum-mode t)
(electric-pair-mode t)

(require 'handy-keys)
(handy-keys-mode t)

(load-theme 'tango-dark)

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

;; TODO find out how can we locate files outside current directory
;; (setq ff-search-directories 
;;       '("." "../include" "../include/*" "../source" "../Include" "../Include/*" "../Source" "../src"))

(add-hook 'find-file-hook
          (lambda ()
            (auto-save-mode -1)))

;; 
;; Delete other windows before display special buffer
(setq special-display-buffer-names
      '("*compilation*"))
(setq special-display-function
      (lambda (buffer &optional args)
        (delete-other-windows)
        (split-window-right)
        (get-buffer-window buffer 0)))
