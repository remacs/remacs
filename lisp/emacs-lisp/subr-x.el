;;; subr-x.el --- extra Lisp functions  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: convenience
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Less commonly used functions that complement basic APIs, often implemented in
;; C code (like hash-tables and strings), and are not eligible for inclusion
;; in subr.el.

;; Do not document these functions in the lispref.
;; http://lists.gnu.org/archive/html/emacs-devel/2014-01/msg01006.html

;; NB If you want to use this library, it's almost always correct to use:
;; (eval-when-compile (require 'subr-x))

;;; Code:

(eval-when-compile (require 'cl-lib))


(defmacro internal--thread-argument (first? &rest forms)
  "Internal implementation for `thread-first' and `thread-last'.
When Argument FIRST? is non-nil argument is threaded first, else
last.  FORMS are the expressions to be threaded."
  (pcase forms
    (`(,x (,f . ,args) . ,rest)
     `(internal--thread-argument
       ,first? ,(if first? `(,f ,x ,@args) `(,f ,@args ,x)) ,@rest))
    (`(,x ,f . ,rest) `(internal--thread-argument ,first? (,f ,x) ,@rest))
    (_ (car forms))))

(defmacro thread-first (&rest forms)
  "Thread FORMS elements as the first argument of their successor.
Example:
    (thread-first
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ (- (/ (+ 5 20) 25)) 40)
Note how the single `-' got converted into a list before
threading."
  (declare (indent 1)
           (debug (form &rest [&or symbolp (sexp &rest form)])))
  `(internal--thread-argument t ,@forms))

(defmacro thread-last (&rest forms)
  "Thread FORMS elements as the last argument of their successor.
Example:
    (thread-last
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ 40 (- (/ 25 (+ 20 5))))
Note how the single `-' got converted into a list before
threading."
  (declare (indent 1) (debug thread-first))
  `(internal--thread-argument nil ,@forms))

(defsubst internal--listify (elt)
  "Wrap ELT in a list if it is not one.
If ELT is of the form ((EXPR)), listify (EXPR) with a dummy symbol."
  (cond
   ((symbolp elt) (list elt elt))
   ((null (cdr elt))
    (list (make-symbol "s") (car elt)))
   (t elt)))

(defsubst internal--check-binding (binding)
  "Check BINDING is properly formed."
  (when (> (length binding) 2)
    (signal
     'error
     (cons "`let' bindings can have only one value-form" binding)))
  binding)

(defsubst internal--build-binding-value-form (binding prev-var)
  "Build the conditional value form for BINDING using PREV-VAR."
  (let ((var (car binding)))
    `(,var (and ,prev-var ,(cadr binding)))))

(defun internal--build-binding (binding prev-var)
  "Check and build a single BINDING with PREV-VAR."
  (thread-first
      binding
    internal--listify
    internal--check-binding
    (internal--build-binding-value-form prev-var)))

(defun internal--build-bindings (bindings)
  "Check and build conditional value forms for BINDINGS."
  (let ((prev-var t))
    (mapcar (lambda (binding)
              (let ((binding (internal--build-binding binding prev-var)))
                (setq prev-var (car binding))
                binding))
            bindings)))

(defmacro if-let* (varlist then &rest else)
  "Bind variables according to VARLIST and eval THEN or ELSE.
Each binding is evaluated in turn, and evaluation stops if a
binding value is nil.  If all are non-nil, the value of THEN is
returned, or the last form in ELSE is returned.

Each element of VARLIST is a list (SYMBOL VALUEFORM) which binds
SYMBOL to the value of VALUEFORM).  An element can additionally
be of the form (VALUEFORM), which is evaluated and checked for
nil; i.e. SYMBOL can be omitted if only the test result is of
interest."
  (declare (indent 2)
           (debug ((&rest [&or symbolp (symbolp form) (sexp)])
                   form body)))
  (if varlist
      `(let* ,(setq varlist (internal--build-bindings varlist))
         (if ,(caar (last varlist))
             ,then
           ,@else))
    `(let* () ,then)))

(defmacro when-let* (varlist &rest body)
  "Bind variables according to VARLIST and conditionally eval BODY.
Each binding is evaluated in turn, and evaluation stops if a
binding value is nil.  If all are non-nil, the value of the last
form in BODY is returned.

VARLIST is the same as in `if-let*'."
  (declare (indent 1) (debug if-let*))
  (list 'if-let* varlist (macroexp-progn body)))

(defmacro and-let* (varlist &rest body)
  "Bind variables according to VARLIST and conditionally eval BODY.
Like `when-let*', except if BODY is empty and all the bindings
are non-nil, then the result is non-nil."
  (declare (indent 1) (debug when-let*))
  (let (res)
    (if varlist
        `(let* ,(setq varlist (internal--build-bindings varlist))
           (if ,(setq res (caar (last varlist)))
               ,@(or body `(,res))))
      `(let* () ,@(or body '(t))))))

(defmacro if-let (spec then &rest else)
  "Bind variables according to SPEC and eval THEN or ELSE.
Like `if-let*' except SPEC can have the form (SYMBOL VALUEFORM)."
  (declare (indent 2)
           (debug ([&or (&rest [&or symbolp (symbolp form) (sexp)])
                        (symbolp form)]
                   form body))
           (obsolete "use `if-let*' instead." "26.1"))
  (when (and (<= (length spec) 2)
             (not (listp (car spec))))
    ;; Adjust the single binding case
    (setq spec (list spec)))
  (list 'if-let* spec then (macroexp-progn else)))

(defmacro when-let (spec &rest body)
  "Bind variables according to SPEC and conditionally eval BODY.
Like `when-let*' except SPEC can have the form (SYMBOL VALUEFORM)."
  (declare (indent 1) (debug if-let)
           (obsolete "use `when-let*' instead." "26.1"))
  (list 'if-let spec (macroexp-progn body)))

(defsubst hash-table-empty-p (hash-table)
  "Check whether HASH-TABLE is empty (has 0 elements)."
  (zerop (hash-table-count hash-table)))

(defsubst hash-table-keys (hash-table)
  "Return a list of keys in HASH-TABLE."
  (cl-loop for k being the hash-keys of hash-table collect k))

(defsubst hash-table-values (hash-table)
  "Return a list of values in HASH-TABLE."
  (cl-loop for v being the hash-values of hash-table collect v))

(defsubst string-empty-p (string)
  "Check whether STRING is empty."
  (string= string ""))

(defsubst string-join (strings &optional separator)
  "Join all STRINGS using SEPARATOR."
  (mapconcat 'identity strings separator))

(define-obsolete-function-alias 'string-reverse 'reverse "25.1")

(defsubst string-trim-left (string &optional regexp)
  "Trim STRING of leading string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
  (if (string-match (concat "\\`\\(?:" (or  regexp "[ \t\n\r]+")"\\)") string)
      (replace-match "" t t string)
    string))

(defsubst string-trim-right (string &optional regexp)
  "Trim STRING of trailing string matching REGEXP.

REGEXP defaults to  \"[ \\t\\n\\r]+\"."
  (if (string-match (concat "\\(?:" (or regexp "[ \t\n\r]+") "\\)\\'") string)
      (replace-match "" t t string)
    string))

(defsubst string-trim (string &optional trim-left trim-right)
  "Trim STRING of leading and trailing strings matching TRIM-LEFT and TRIM-RIGHT.

TRIM-LEFT and TRIM-RIGHT default to \"[ \\t\\n\\r]+\"."
  (string-trim-left (string-trim-right string trim-right) trim-left))

(defsubst string-blank-p (string)
  "Check whether STRING is either empty or only whitespace."
  (string-match-p "\\`[ \t\n\r]*\\'" string))

(defsubst string-remove-prefix (prefix string)
  "Remove PREFIX from STRING if present."
  (if (string-prefix-p prefix string)
      (substring string (length prefix))
    string))

(defsubst string-remove-suffix (suffix string)
  "Remove SUFFIX from STRING if present."
  (if (string-suffix-p suffix string)
      (substring string 0 (- (length string) (length suffix)))
    string))

(defun read-multiple-choice (prompt choices)
  "Ask user a multiple choice question.
PROMPT should be a string that will be displayed as the prompt.

CHOICES is an alist where the first element in each entry is a
character to be entered, the second element is a short name for
the entry to be displayed while prompting (if there's room, it
might be shortened), and the third, optional entry is a longer
explanation that will be displayed in a help buffer if the user
requests more help.

This function translates user input into responses by consulting
the bindings in `query-replace-map'; see the documentation of
that variable for more information.  In this case, the useful
bindings are `recenter', `scroll-up', and `scroll-down'.  If the
user enters `recenter', `scroll-up', or `scroll-down' responses,
perform the requested window recentering or scrolling and ask
again.

When `use-dialog-box' is t (the default), this function can pop
up a dialog window to collect the user input. That functionality
requires `display-popup-menus-p' to return t. Otherwise, a text
dialog will be used.

The return value is the matching entry from the CHOICES list.

Usage example:

\(read-multiple-choice \"Continue connecting?\"
                      \\='((?a \"always\")
                        (?s \"session only\")
                        (?n \"no\")))"
  (let* ((altered-names nil)
         (full-prompt
          (format
           "%s (%s): "
           prompt
           (mapconcat
            (lambda (elem)
              (let* ((name (cadr elem))
                     (pos (seq-position name (car elem)))
                     (altered-name
                      (cond
                       ;; Not in the name string.
                       ((not pos)
                        (format "[%c] %s" (car elem) name))
                       ;; The prompt character is in the name, so highlight
                       ;; it on graphical terminals...
                       ((display-supports-face-attributes-p
                         '(:underline t) (window-frame))
                        (setq name (copy-sequence name))
                        (put-text-property pos (1+ pos)
                                           'face 'read-multiple-choice-face
                                           name)
                        name)
                       ;; And put it in [bracket] on non-graphical terminals.
                       (t
                        (concat
                         (substring name 0 pos)
                         "["
                         (upcase (substring name pos (1+ pos)))
                         "]"
                         (substring name (1+ pos)))))))
                (push (cons (car elem) altered-name)
                      altered-names)
                altered-name))
            (append choices '((?? "?")))
            ", ")))
         tchar buf wrong-char answer)
    (save-window-excursion
      (save-excursion
	(while (not tchar)
	  (message "%s%s"
                   (if wrong-char
                       "Invalid choice.  "
                     "")
                   full-prompt)
          (setq tchar
                (if (and (display-popup-menus-p)
                         last-input-event ; not during startup
                         (listp last-nonmenu-event)
                         use-dialog-box)
                    (x-popup-dialog
                     t
                     (cons prompt
                           (mapcar
                            (lambda (elem)
                              (cons (capitalize (cadr elem))
                                    (car elem)))
                            choices)))
                  (condition-case nil
                      (let ((cursor-in-echo-area t))
                        (read-char))
                    (error nil))))
          (setq answer (lookup-key query-replace-map (vector tchar) t))
          (setq tchar
                (cond
                 ((eq answer 'recenter)
                  (recenter) t)
                 ((eq answer 'scroll-up)
                  (ignore-errors (scroll-up-command)) t)
                 ((eq answer 'scroll-down)
                  (ignore-errors (scroll-down-command)) t)
                 ((eq answer 'scroll-other-window)
                  (ignore-errors (scroll-other-window)) t)
                 ((eq answer 'scroll-other-window-down)
                  (ignore-errors (scroll-other-window-down)) t)
                 (t tchar)))
          (when (eq tchar t)
            (setq wrong-char nil
                  tchar nil))
          ;; The user has entered an invalid choice, so display the
          ;; help messages.
          (when (and (not (eq tchar nil))
                     (not (assq tchar choices)))
	    (setq wrong-char (not (memq tchar '(?? ?\C-h)))
                  tchar nil)
            (when wrong-char
              (ding))
            (with-help-window (setq buf (get-buffer-create
                                         "*Multiple Choice Help*"))
              (with-current-buffer buf
                (erase-buffer)
                (pop-to-buffer buf)
                (insert prompt "\n\n")
                (let* ((columns (/ (window-width) 25))
                       (fill-column 21)
                       (times 0)
                       (start (point)))
                  (dolist (elem choices)
                    (goto-char start)
                    (unless (zerop times)
                      (if (zerop (mod times columns))
                          ;; Go to the next "line".
                          (goto-char (setq start (point-max)))
                        ;; Add padding.
                        (while (not (eobp))
                          (end-of-line)
                          (insert (make-string (max (- (* (mod times columns)
                                                          (+ fill-column 4))
                                                       (current-column))
                                                    0)
                                               ?\s))
                          (forward-line 1))))
                    (setq times (1+ times))
                    (let ((text
                           (with-temp-buffer
                             (insert (format
                                      "%c: %s\n"
                                      (car elem)
                                      (cdr (assq (car elem) altered-names))))
                             (fill-region (point-min) (point-max))
                             (when (nth 2 elem)
                               (let ((start (point)))
                                 (insert (nth 2 elem))
                                 (unless (bolp)
                                   (insert "\n"))
                                 (fill-region start (point-max))))
                             (buffer-string))))
                      (goto-char start)
                      (dolist (line (split-string text "\n"))
                        (end-of-line)
                        (if (bolp)
                            (insert line "\n")
                          (insert line))
                        (forward-line 1)))))))))))
    (when (buffer-live-p buf)
      (kill-buffer buf))
    (assq tchar choices)))

(provide 'subr-x)

;;; subr-x.el ends here
