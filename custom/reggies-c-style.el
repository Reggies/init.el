;;; reggies-c-style.el --- Style details of my default cc style  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Alexey Natalin

;; Author: Alexey Natalin <reggies@natalin-laptop>
;; Keywords: c, languages

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

(c-add-style "reggies"
             '("awk"
               (c-basic-offset . 4)
               (c-offsets-alist . ((statement-case-open . +)
                                   (inline-open . 0)
                                   (inextern-lang . 0)))))


(provide 'reggies-c-style)
;;; reggies-c-style.el ends here
