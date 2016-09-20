;;; cc-mode-tricks.el --- Hacks, keys, customizations.

;; Copyright (C) 2014  Alexey Natalin

;; Author:  Alexey Natalin <mrreggies@gmail.com>
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

;;; Commentary:

;; 

;;; Code:

(require 'reggies-c-style)

;; Google C/C++ source code style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(c-add-style "Google" google-c-style nil)

(setq-default c-default-style '((java-mode . "java")
                                (c++-mode . "google")
                                (other . "reggies"))
              cc-other-file-alist '(("\\.cc$" ff-cc-hh-converter)
                                    ("\\.hh$" ff-cc-hh-converter)
                                    ("\\.c$" (".h"))
                                    ("\\.hpp$" (".cpp" ".c" ".cc" ".C" ".h"))
                                    ("\\.h$" (".cpp" ".c" ".cc" ".C" ".CC" ".cxx" ".hpp"))
                                    ("\\.cpp$" (".h" ".hpp"))
                                    ("\\.cxx$" (".hpp" ".h"))))

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

;; auto insert include guard into newly created C header
(defun add-include-guard ()
  (add-to-list 'auto-insert-alist
               '(("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "Include guard")
                 (upcase
                  (concat
                   (file-name-nondirectory
                    (file-name-sans-extension buffer-file-name))
                   "_"
                   (file-name-extension buffer-file-name)
                   "_"))
                 "#ifndef " str n "#define " str "\n\n" _ "\n\n#endif\n")))

(add-hook 'c++-mode-hook 'add-include-guard)

;; auto insert #include into newly created C++ source file
(defun add-header-include ()
  (add-to-list 'auto-insert-alist
               '(("\\.\\(cpp\\|[Cc]\\|cc\\)\\'" . "Include header")
                 nil
                 (let* ((stem (file-name-sans-extension buffer-file-name))
                        (ident
                         (cond
                          ((file-exists-p (concat stem ".hpp")) (concat (file-name-nondirectory stem) ".hpp"))
                          ((file-exists-p (concat stem ".h")) (concat (file-name-nondirectory stem) ".h")))))
                   (if (and ident (file-exists-p ident))
                       (concat "#include \"" ident "\"\n"))))))

(add-hook 'c++-mode-hook 'add-header-include)

(provide 'cc-mode-tricks)
;;; cc-mode-tricks.el ends here
