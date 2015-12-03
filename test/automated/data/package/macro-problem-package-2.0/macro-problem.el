;;; macro-problem.el --- laksd                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Artur Malabarba

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; Keywords: tools
;; Version: 2.0

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

(require 'macro-aux)

(defmacro macro-problem-1 ( &rest forms)
  "Description"
  `(progn ,(cadr (car forms))))


(defun macro-problem-func ()
  ""
  (list (macro-problem-1 '1 'b)
        (macro-aux-1 'a 'b)))

(defmacro macro-problem-3 (&rest _)
  "Description"
  10)

(defun macro-problem-10-and-90 ()
  ""
  (list (macro-problem-3 haha) (macro-aux-3 hehe)))

(provide 'macro-problem)
;;; macro-problem.el ends here
