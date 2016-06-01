;;; caffe-mode.el --- Major mode for editing Caffe models and solvers  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Alexey Natalin

;; Author: Alexey Natalin <mrreggies@gmail.com>
;; Keywords: languages, tools

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

;; Based on qml-mode

;;; Code:

(require 'js)

(defvar caffe-tab-width 2)

(defconst caffe-re-function "^[ \t]*\\(\\w+\\)[ \t]*{")
(defconst caffe-re-block (concat "\\(" caffe-re-function "\\)"))

(defun caffe-get-beg-of-block ()
  (save-excursion
    (when (re-search-backward caffe-re-block nil t)
      (match-beginning 2))))

(defun caffe-get-end-of-block ()
  (save-excursion
    (when (re-search-backward caffe-re-block nil t)
      (goto-char (match-end 0))
      (backward-char)
      (condition-case nil
          (save-restriction
            (forward-list)
            (point))
        (error nil)))))

(defun caffe-indent ()
  (interactive)
  (if (bobp)
      (indent-line-to 0)
    (let ((cur (point))
          (start (caffe-get-beg-of-block))
          (end (caffe-get-end-of-block))
          (cur-indent nil))
      (save-excursion
        (if (not (and start end (> cur start) (< cur end)))
            (progn
              (if start
                  (goto-char start))
              (setq start (caffe-get-beg-of-block))
              (setq end (caffe-get-end-of-block))
              (while (and (not (eq start nil)) (not (eq end nil)) (not (and (> cur start) (< cur end))))
                (goto-char start)
                (setq start (caffe-get-beg-of-block))
                (setq end (caffe-get-end-of-block)))
              (if (or (eq start nil) (= (point) (point-min)))
                  (progn
                    (goto-char (point-min))
                    (when (re-search-forward caffe-re-block nil t)
                      (goto-char (match-beginning 2))
                      (setq start (point))
                      (goto-char (match-end 0))
                      (backward-char)
                      (condition-case nil
                          (save-restriction
                            (forward-list)
                            (setq end (point))
                            (setq cur-indent 0))
                        (error nil)))))))
        (if (not cur-indent)
            (progn
              (goto-char start)
              (setq cur-indent (current-indentation))
              (goto-char cur)
              (unless (string= (string (char-after (- (point) 1))) "{")
                (setq cur-indent (+ cur-indent caffe-tab-width))))))
      (indent-line-to cur-indent)
      (if (string= (string (char-after (point))) "}")
          (indent-line-to (- cur-indent caffe-tab-width))))))

(defvar caffe-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    table))

(defvar caffe-font-lock-keywords
  '(("#.*" . font-lock-comment-face)
    ("^[ \t]*\\(\\w+\\)[ \t]*{" . font-lock-function-name-face)
    ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([eE][-+]?[0-9]+\\)?\\f?\\>" . font-lock-constant-face)
    ("\\<\\(false\\|true\\)\\>" . font-lock-constant-face)
    ("\\<\\([A-Z]\\w*\\)\\>" . font-lock-constant-face)
    ))

(defun caffe-mode ()
  "Major mode used in `caffe-mode' buffers."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'caffe-mode)
  (setq mode-name "Caffe")

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(caffe-font-lock-keywords))

  (set-syntax-table caffe-mode-syntax-table)
  (modify-syntax-entry ?_   "w" caffe-mode-syntax-table)
  (modify-syntax-entry ?#   "<" caffe-mode-syntax-table)
  (modify-syntax-entry ?\n  ">" caffe-mode-syntax-table)

  (make-local-variable 'tab-width)
  (setq tab-width caffe-tab-width)

  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)

  (make-local-variable indent-line-function)
  (setq indent-line-function 'caffe-indent)

  (make-local-variable 'comment-start)
  (setq comment-start "#"))

(provide 'caffe-mode)
;;; caffe-mode.el ends here
