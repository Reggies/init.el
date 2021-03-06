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

              windmove-wrap-around t
              vc-handled-backends ()
              paragraph-ignore-fill-prefix t
              visible-bell t)

(when (eq system-type 'windows-nt)
  (setq-default ring-bell-function nil
                inhibit-compacting-font-caches t)
  (w32-register-hot-key [C-S-M-f]))

(set-frame-font
 (cond
  ((and (find-font (font-spec :name "Hack"))
        (eq system-type 'windows-nt)) "Hack-10.5")
  ((find-font (font-spec :name "Hack")) "Hack-11")
  ((find-font (font-spec :name "Noto Mono")) "Noto Mono-11")
  ((find-font (font-spec :name "Droid Sans Mono")) "Droid Sans Mono-11")
  ((find-font (font-spec :name "Consolas")) "Consolas-11.5"))) ;; windows fallback

(fset 'yes-or-no-p 'y-or-n-p)

(add-to-list 'load-path (concat user-emacs-directory "custom"))

;; Dockerfile
(add-to-list 'load-path (concat user-emacs-directory "dockerfile-mode/"))
(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; C#
(add-to-list 'load-path (concat user-emacs-directory "csharp-mode/"))
(autoload 'csharp-mode "csharp-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;; Protobuf
(require 'cl)
(add-to-list 'load-path (concat user-emacs-directory "protobuf-mode/"))
(autoload 'protobuf-mode "protobuf-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;; Rust
(add-to-list 'load-path (concat user-emacs-directory "rust-mode/"))
(autoload 'rust-mode "rust-mode" "Major mode for Rust" t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;
;; TOML
(add-to-list 'load-path (concat user-emacs-directory "toml-mode.el/"))
(require 'toml-mode)

;; Caffe
(add-to-list 'load-path (concat user-emacs-directory "caffe-mode/"))
(autoload 'caffe-mode "caffe-mode" "Major mode for Caffe" t)
(add-to-list 'auto-mode-alist '("\\.prototxt\\'" . caffe-mode))
(add-to-list 'auto-mode-alist '("\\.pt\\'" . caffe-mode))

;; CMake
(autoload 'cmake-mode "cmake-mode" nil t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("CMakeCache\\.txt\\'" . cmake-mode))

;; Meson
(add-to-list 'load-path (concat user-emacs-directory "meson-mode/"))
(autoload 'meson-mode "meson-mode" nil t)
(add-to-list 'auto-mode-alist '("meson\\.build\\'" . meson-mode))

;; Scala?
(add-to-list 'load-path (concat user-emacs-directory "emacs-scala-mode/"))
(require 'scala-mode)

;; Haskell
(add-to-list 'load-path (concat user-emacs-directory "haskell-mode/"))
(require 'haskell)

;;
;; Markdown mode
(add-to-list 'load-path (concat user-emacs-directory "markdown-mode/"))
(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; QML
(add-to-list 'load-path (concat user-emacs-directory "qml-mode/"))
(autoload 'qml-mode "qml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.qml\\'" . qml-mode))

;; yaml
(add-to-list 'load-path (concat user-emacs-directory "yaml-mode/"))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Qt
(setq auto-mode-alist
      (append '(("\\.pro\\'" . text-mode)
                ("\\.qrc\\'" . xml-mode))
              auto-mode-alist))

;; 
;; Tianocore
(add-to-list 'auto-mode-alist '("\\.dsc\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.dec\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.inf\\'" . conf-mode))

;;
;; Treat doxyfile as config file
(add-to-list 'auto-mode-alist '("Doxyfile\\'" . conf-mode))

;;
;; Octave
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;:gas
(add-to-list 'load-path (concat user-emacs-directory "gas-mode/"))
(require 'gas-mode)
(add-to-list 'auto-mode-alist '("\\.S\\'" . gas-mode))

;; hippie
(require 'hippie)
(global-set-key (kbd "M-/") 'hippie-expand)
(add-to-list 'hippie-expand-try-functions-list 'try-expand-flexible-abbrev)

;;
(add-to-list 'load-path (concat user-emacs-directory "groovy-mode/"))
(require 'groovy-mode)
(add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode))

;;
;; GLSL
(add-to-list 'load-path (concat user-emacs-directory "glsl-mode/"))
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

(load "py-mode-tricks")
(load "cc-mode-tricks")
(load "c++11-hacks")
(load "org-mode-todo")
(load "org-mode-hooks")
(load "handy-keys-mode")
(load "blank-mode")
(load "nxml-custom")

(handy-keys-mode t)
(auto-insert-mode t)
(pending-delete-mode t)
(show-paren-mode t)
(electric-pair-mode t)

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

(global-display-line-numbers-mode t)

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

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'suspend-frame 'disabled t)

(package-initialize)
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(add-to-list 'package-selected-packages 'magit)
