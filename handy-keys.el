;;; handy-keys.el --- It is Handy Keys mode which contains all my custom shortcuts

;; Copyright (C) reggies 2014

;; Author:  reggies
;; Keywords: shortcuts

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

(defun toggle-truncate-lines ()
  "Toggle truncate-lines value in current buffer"
  (interactive)
  (set (make-local-variable 'truncate-lines)
       (not truncate-lines)))

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

(defun shrink-window-to-left ()
  (interactive)
  (shrink-window-horizontally (- shrink-delta)))

(defun shrink-window-to-right ()
  (interactive)
  (shrink-window-horizontally shrink-delta))

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

(defun move-indent-or-beginning-of-line ()
  "Jumps on the line indent and than on the beginning of line."
  (interactive)
  (handle-shift-selection)
  (let ((old-point (point)))
    (back-to-indentation)
    (when (equal old-point (point))
      (move-beginning-of-line 1))))

(defun compile-current-dir ()
  "This function just invokes make on current dir"
  (interactive)
  (compile "make -k"))

(defun async-shell-exec-default ()
  "Asynchronously execute gcc default output executable"
  (interactive)
  (async-shell-command "./a.out"))

(setq handy-keys-mode-map
      (let ((map (make-sparse-keymap)))
        ;;
        ;; ff-find-other-file [in other buffer = nil] [ignore includes = t]
        (define-key map (kbd "C-<f4>") 'switch-to-header)
        (define-key map (kbd "C-<f5>") 'reload-init-file)
        ;;
        ;; ff-find-other-file [in other buffer = nil] [ignore includes = nil]
        (define-key map (kbd "C-x C-o") 'ff-find-other-file)
        (define-key map (kbd "C-x C-r") 'replace-string)
        (define-key map (kbd "M-\"") 'insert-pair)
        (define-key map (kbd "M-<") 'insert-pair)
        (define-key map (kbd "C-;") 'upcase-char)
        (define-key map (kbd "C-l") 'downcase-char)
        (define-key map (kbd "C-a") 'move-indent-or-beginning-of-line)
        (define-key map (kbd "<home>") 'move-indent-or-beginning-of-line)
        (define-key map (kbd "M-p") 'backward-kill-word)
        (define-key map (kbd "<f5>") 'compile-current-dir)
        (define-key map (kbd "C-c C-t") 'toggle-truncate-lines)
        (define-key map (kbd "<f12>") 'async-shell-exec-default)
        (define-key map (kbd "C-}") 'shrink-window-to-right)
        (define-key map (kbd "C-{") 'shrink-window-to-left)
        (define-key map (kbd "C-x C-<backspace>") 'delete-trailing-whitespace)
        map))

(define-minor-mode handy-keys-mode
  "Handy keys for your emacs."
  :lighter " Handy"
  :keymap handy-keys-mode-map
  :init-value t)

(provide 'handy-keys)
;;; handy-keys.el ends here
