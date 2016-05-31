(add-to-list 'custom-theme-load-path (concat user-emacs-directory "seoul256.el"))

(load-theme 'seoul256 t)

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
                                    (cursor-type . box))

              blink-cursor-alist '((box . hollow))

              tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)

              ;; prevent <menu-bar> <file> <exit-emacs> from popup dialogs
              use-dialog-box nil

              confirm-kill-emacs nil

              ;; All we love xmonad
              window-combination-resize t
              focus-follows-mouse t

              windmove-wrap-around t)

;; Windows workaround
(if (eq system-type 'gnu/linux)
    (setq-default default-frame-alist
                  (cons '(font . "Monospace-10") default-frame-alist)))

(fset 'yes-or-no-p 'y-or-n-p)
(scroll-bar-mode -1)

(add-to-list 'load-path (concat user-emacs-directory "custom"))

;; Google C/C++ source code style
(load "google-c-style")
(add-hook 'c-mode-common-hook 'google-set-c-style)

;; Dockerfile
(add-to-list 'load-path (concat user-emacs-directory "dockerfile-mode/"))
(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Protobuf
(require 'cl)
(add-to-list 'load-path (concat user-emacs-directory "protobuf-mode/"))
(autoload 'protobuf-mode "protobuf-mode" nil nil)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; CMake
(autoload 'cmake-mode "cmake-mode" nil t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("CMakeCache\\.txt\\'" . cmake-mode))

(autoload 'applescript-mode "applescript-mode"
  "Major mode for editing AppleScript source." t)
(add-to-list 'auto-mode-alist '("\\.applescript$" . applescript-mode))

;; Lua!
(add-to-list 'load-path (concat user-emacs-directory "lua-mode/"))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;;
;; Python!
(load "py-mode-tricks")

;;
;; CC mode
(load "cc-mode-tricks")
(load "c++11-hacks")

;;
;; Custom keys
(load "handy-keys-mode")
(handy-keys-mode t)

;;
;; Markdown mode
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;
;; Qt
(autoload 'qml-mode "qml-mode" "Major mode for Qt QML." t)
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))
(setq auto-mode-alist
      (append '(("\\.pro\\'" . text-mode)
                ("\\.qrc\\'" . xml-mode))
              auto-mode-alist))

;;
;; Treat doxyfile as config file
(add-to-list 'auto-mode-alist '("Doxyfile\\'" . conf-mode))

;;
;; Octave
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;;
;; org-mode for TOOD files
(add-to-list 'auto-mode-alist '("TODO\\'" . org-mode))

(require 'hippie)
(global-set-key (kbd "M-/") 'hippie-expand)
(add-to-list 'hippie-expand-try-functions-list 'try-expand-flexible-abbrev)

(add-to-list 'load-path (concat user-emacs-directory "erlware-mode/"))
(require 'erlang-start)

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
                ("\\.fs\\'" . glsl-mode)
                ("\\.tes\\'" . glsl-mode)
                ("\\.tcs\\'" . glsl-mode))
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
(setq special-display-buffer-names '("*compilation*"))
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
