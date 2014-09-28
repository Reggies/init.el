;;; cc-mode-tricks.el --- C++11, hacks, keys, customizations.

;; Copyright (C) 2014

;; Author:  <reggies@debian>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(setq-default c-basic-offset 4
              c-offsets-alist '(cons (statement-case-open . 4)
                                     (inline-open . 0)
                                     c-offsets-alist)
              c-default-style '((java-mode . "java")
                                (other . "awk"))
              cc-other-file-alist '(("\\.cc$" ff-cc-hh-converter)
                                    ("\\.hh$" ff-cc-hh-converter)
                                    ("\\.c$" (".h"))
                                    ("\\.hpp$" (".cpp" ".c" ".cc" ".C"))
                                    ("\\.h$" (".cpp" ".c" ".cc" ".C" ".CC" ".cxx"))
                                    ("\\.cpp$" (".h" ".hpp"))))

(setq auto-mode-alist
      (append '(("\\.inl\\'" . c++-mode)
                ("\\.h\\'" . c++-mode)
                ("\\.c\\'" . c++-mode))
                auto-mode-alist))

;;
;; Disable backup files for C++
(add-hook 'c++-mode-hook
          (lambda ()
            (set (make-local-variable 'make-backup-files) nil)))

;;
;; insert-parentheses should not produce any space chars
(add-hook 'c-mode-common-hook
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
      (looking-back "enum[ \t]+class[ \t]*[^}]+"))))

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

;;
;; taken from http://stackoverflow.com/a/23553882
(defadvice c-lineup-arglist (around my activate)
  "Improve indentation of continued C++11 lambda function opened as argument."
  (setq ad-return-value
        (if (and (equal major-mode 'c++-mode)
                 (ignore-errors
                   (save-excursion
                     (goto-char (c-langelem-pos langelem))
                     ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                     ;;   and with unclosed brace.
                     (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
            0                           ; no additional indent
          ad-do-it)))                   ; default behavior

;; auto insert include guard into newly created C header
(defun add-include-guard ()
  (add-to-list 'auto-insert-alist
               '(("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "Include guard")
                 nil
                 (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
                        (nopath (file-name-nondirectory noext))
                        (ident (concat (upcase nopath) "_H_")))
                   (concat "#ifndef " ident "\n"
                           "#define " ident "\n\n\n"
                           "#endif // " ident "\n")))))

(add-hook 'c++-mode-hook 'add-include-guard)

(defun first-non-nil (xs)
  (or (car xs)
      (first-non-nil (cdr xs))))

;; auto insert #include into newly created C++ source file
(defun add-header-include ()
  (add-to-list 'auto-insert-alist
               '(("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "Include header")
                 nil
                 (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
                        (nopath (file-name-nondirectory noext))
                        (ident (first-non-nil (mapcar (lambda (ext)
                                                        (let ((filename (concat nopath ext)))
                                                          (if (file-exists-p filename)
                                                              filename
                                                            nil)))
                                                      (list ".h" ".hpp")))))
                   (if (file-exists-p ident)
                       (concat "#include \"" ident "\"\n"))))))

(add-hook 'c++-mode-hook 'add-header-include)

(provide 'cc-mode-tricks)
;;; cc-mode-tricks.el ends here
