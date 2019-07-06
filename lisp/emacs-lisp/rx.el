;;; rx.el --- sexp notation for regular expressions  -*- lexical-binding: t -*-

;; Copyright (C) 2001-2019 Free Software Foundation, Inc.

;; Author: Gerd Moellmann <gerd@gnu.org>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: strings, regexps, extensions

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

;; This is another implementation of sexp-form regular expressions.
;; It was unfortunately written without being aware of the Sregex
;; package coming with Emacs, but as things stand, Rx completely
;; covers all regexp features, which Sregex doesn't, doesn't suffer
;; from the bugs mentioned in the commentary section of Sregex, and
;; uses a nicer syntax (IMHO, of course :-).

;; This significantly extended version of the original, is almost
;; compatible with Sregex.  The only incompatibility I (fx) know of is
;; that the `repeat' form can't have multiple regexp args.

;; Now alternative forms are provided for a degree of compatibility
;; with Olin Shivers' attempted definitive SRE notation.  SRE forms
;; not catered for include: dsm, uncase, w/case, w/nocase, ,@<exp>,
;; ,<exp>, (word ...), word+, posix-string, and character class forms.
;; Some forms are inconsistent with SRE, either for historical reasons
;; or because of the implementation -- simple translation into Emacs
;; regexp strings.  These include: any, word.  Also, case-sensitivity
;; and greediness are controlled by variables external to the regexp,
;; and you need to feed the forms to the `posix-' functions to get
;; SRE's POSIX semantics.  There are probably more difficulties.

;; Rx translates a sexp notation for regular expressions into the
;; usual string notation.  The translation can be done at compile-time
;; by using the `rx' macro.  The `regexp' and `literal' forms accept
;; non-constant expressions, in which case `rx' will translate to a
;; `concat' expression.  Translation can be done fully at run time by
;; calling function `rx-to-string'.  See the documentation of `rx' for
;; a complete description of the sexp notation.
;;
;; Some examples of string regexps and their sexp counterparts:
;;
;; "^[a-z]*"
;; (rx line-start (0+ (in "a-z")))
;;
;; "\n[^ \t]"
;; (rx ?\n (not (in " \t")))
;;
;; "\\*\\*\\* EOOH \\*\\*\\*\n"
;; (rx "*** EOOH ***\n")
;;
;; "\\<\\(catch\\|finally\\)\\>[^_]"
;; (rx word-start (submatch (or "catch" "finally")) word-end
;;     (not (in ?_)))
;;
;; "[ \t\n]*:\\($\\|[^:]+\\)"
;; (rx (* (in " \t\n")) ":"
;;     (submatch (or line-end (+ (not (in ?:))))))
;;
;; "^content-transfer-encoding:\\(?:\n?[\t ]\\)*quoted-printable\\(?:\n?[\t ]\\)*"
;; (rx line-start
;;     "content-transfer-encoding:"
;;     (* (? ?\n) (in " \t"))
;;     "quoted-printable"
;;     (* (? ?\n) (in " \t")))
;;
;; (concat "^\\(?:" something-else "\\)")
;; (rx line-start (regexp something-else))
;;
;; (regexp-opt '(STRING1 STRING2 ...))
;; (rx (or STRING1 STRING2 ...)), or in other words, `or' automatically
;; calls `regexp-opt' as needed.
;;
;; "^;;\\s-*\n\\|^\n"
;; (rx (or (seq line-start ";;" (0+ space) ?\n)
;;         (seq line-start ?\n)))
;;
;; "\\$[I]d: [^ ]+ \\([^ ]+\\) "
;; (rx "$Id: "
;;     (1+ (not (in " ")))
;;     " "
;;     (submatch (1+ (not (in " "))))
;;     " ")
;;
;; "\\\\\\\\\\[\\w+"
;; (rx "\\\\[" (1+ word))
;;
;; etc.

;;; History:
;;

;;; Code:

(require 'cl-lib)
(require 'cl-extra)

;; FIXME: support macros.

(defvar rx-constituents              ;Not `const' because some modes extend it.
  '((and		. (rx-and 0 nil))
    (seq		. and)		; SRE
    (:			. and)		; SRE
    (sequence		. and)		; sregex
    (or			. (rx-or 0 nil))
    (|			. or)		; SRE
    (not-newline	. ".")
    (nonl		. not-newline)	; SRE
    (anything		. (rx-anything 0 nil))
    (any		. (rx-any 1 nil rx-check-any)) ; inconsistent with SRE
    (any		. ".")          ; sregex
    (in			. any)
    (char		. any)		; sregex
    (not-char		. (rx-not-char 1 nil rx-check-any)) ; sregex
    (not		. (rx-not 1 1 rx-check-not))
    (repeat		. (rx-repeat 2 nil))
    (=			. (rx-= 2 nil))	   ; SRE
    (>=			. (rx->= 2 nil))   ; SRE
    (**			. (rx-** 2 nil))   ; SRE
    (submatch		. (rx-submatch 1 nil)) ; SRE
    (group		. submatch)     ; sregex
    (submatch-n		. (rx-submatch-n 2 nil))
    (group-n		. submatch-n)
    (zero-or-more	. (rx-kleene 1 nil))
    (one-or-more	. (rx-kleene 1 nil))
    (zero-or-one	. (rx-kleene 1 nil))
    (\?			. zero-or-one)	; SRE
    (\??		. zero-or-one)
    (*			. zero-or-more)	; SRE
    (*?			. zero-or-more)
    (0+			. zero-or-more)
    (+			. one-or-more)	; SRE
    (+?			. one-or-more)
    (1+			. one-or-more)
    (optional		. zero-or-one)
    (opt		. zero-or-one)	; sregex
    (minimal-match	. (rx-greedy 1 1))
    (maximal-match	. (rx-greedy 1 1))
    (backref		. (rx-backref 1 1 rx-check-backref))
    (line-start		. "^")
    (bol		. line-start)	; SRE
    (line-end		. "$")
    (eol		. line-end)	; SRE
    (string-start	. "\\`")
    (bos		. string-start)	; SRE
    (bot		. string-start)	; sregex
    (string-end		. "\\'")
    (eos		. string-end)	; SRE
    (eot		. string-end)	; sregex
    (buffer-start	. "\\`")
    (buffer-end		. "\\'")
    (point		. "\\=")
    (word-start		. "\\<")
    (bow		. word-start)	; SRE
    (word-end		. "\\>")
    (eow		. word-end)	; SRE
    (word-boundary	. "\\b")
    (not-word-boundary	. "\\B")	; sregex
    (symbol-start       . "\\_<")
    (symbol-end         . "\\_>")
    (syntax		. (rx-syntax 1 1))
    (not-syntax		. (rx-not-syntax 1 1)) ; sregex
    (category		. (rx-category 1 1 rx-check-category))
    (eval		. (rx-eval 1 1))
    (literal		. (rx-literal 1 1 stringp))
    (regexp		. (rx-regexp 1 1 stringp))
    (regex		. regexp)       ; sregex
    (digit		. "[[:digit:]]")
    (numeric		. digit)	; SRE
    (num		. digit)	; SRE
    (control		. "[[:cntrl:]]") ; SRE
    (cntrl		. control)	 ; SRE
    (hex-digit		. "[[:xdigit:]]") ; SRE
    (hex		. hex-digit)	  ; SRE
    (xdigit		. hex-digit)	  ; SRE
    (blank		. "[[:blank:]]")  ; SRE
    (graphic		. "[[:graph:]]")  ; SRE
    (graph		. graphic)	  ; SRE
    (printing		. "[[:print:]]")  ; SRE
    (print		. printing)	  ; SRE
    (alphanumeric	. "[[:alnum:]]")  ; SRE
    (alnum		. alphanumeric)	  ; SRE
    (letter		. "[[:alpha:]]")
    (alphabetic		. letter)	; SRE
    (alpha		. letter)	; SRE
    (ascii		. "[[:ascii:]]") ; SRE
    (nonascii		. "[[:nonascii:]]")
    (lower		. "[[:lower:]]") ; SRE
    (lower-case		. lower)	 ; SRE
    (punctuation	. "[[:punct:]]") ; SRE
    (punct		. punctuation)	 ; SRE
    (space		. "[[:space:]]") ; SRE
    (whitespace		. space)	 ; SRE
    (white		. space)	 ; SRE
    (upper		. "[[:upper:]]") ; SRE
    (upper-case		. upper)	 ; SRE
    (word		. "[[:word:]]")	 ; inconsistent with SRE
    (wordchar		. word)		 ; sregex
    (not-wordchar	. "\\W"))
  "Alist of sexp form regexp constituents.
Each element of the alist has the form (SYMBOL . DEFN).
SYMBOL is a valid constituent of sexp regular expressions.
If DEFN is a string, SYMBOL is translated into DEFN.
If DEFN is a symbol, use the definition of DEFN, recursively.
Otherwise, DEFN must be a list (FUNCTION MIN-ARGS MAX-ARGS PREDICATE).
FUNCTION is used to produce code for SYMBOL.  MIN-ARGS and MAX-ARGS
are the minimum and maximum number of arguments the function-form
sexp constituent SYMBOL may have in sexp regular expressions.
MAX-ARGS nil means no limit.  PREDICATE, if specified, means that
all arguments must satisfy PREDICATE.")


(defconst rx-syntax
  '((whitespace		. ?-)
    (punctuation	. ?.)
    (word		. ?w)
    (symbol		. ?_)
    (open-parenthesis	. ?\()
    (close-parenthesis	. ?\))
    (expression-prefix	. ?\')
    (string-quote	. ?\")
    (paired-delimiter	. ?$)
    (escape		. ?\\)
    (character-quote	. ?/)
    (comment-start	. ?<)
    (comment-end	. ?>)
    (string-delimiter	. ?|)
    (comment-delimiter	. ?!))
  "Alist mapping Rx syntax symbols to syntax characters.
Each entry has the form (SYMBOL . CHAR), where SYMBOL is a valid
symbol in `(syntax SYMBOL)', and CHAR is the syntax character
corresponding to SYMBOL, as it would be used with \\s or \\S in
regular expressions.")


(defconst rx-categories
  '((space-for-indent           . ?\s)
    (base                       . ?.)
    (consonant			. ?0)
    (base-vowel			. ?1)
    (upper-diacritical-mark	. ?2)
    (lower-diacritical-mark	. ?3)
    (tone-mark			. ?4)
    (symbol			. ?5)
    (digit			. ?6)
    (vowel-modifying-diacritical-mark . ?7)
    (vowel-sign			. ?8)
    (semivowel-lower		. ?9)
    (not-at-end-of-line		. ?<)
    (not-at-beginning-of-line	. ?>)
    (alpha-numeric-two-byte	. ?A)
    (chinese-two-byte		. ?C)
    (chinse-two-byte		. ?C) ;; A typo in Emacs 21.1-24.3.
    (greek-two-byte		. ?G)
    (japanese-hiragana-two-byte . ?H)
    (indian-two-byte		. ?I)
    (japanese-katakana-two-byte . ?K)
    (strong-left-to-right       . ?L)
    (korean-hangul-two-byte	. ?N)
    (strong-right-to-left       . ?R)
    (cyrillic-two-byte		. ?Y)
    (combining-diacritic	. ?^)
    (ascii			. ?a)
    (arabic			. ?b)
    (chinese			. ?c)
    (ethiopic			. ?e)
    (greek			. ?g)
    (korean			. ?h)
    (indian			. ?i)
    (japanese			. ?j)
    (japanese-katakana		. ?k)
    (latin			. ?l)
    (lao			. ?o)
    (tibetan			. ?q)
    (japanese-roman		. ?r)
    (thai			. ?t)
    (vietnamese			. ?v)
    (hebrew			. ?w)
    (cyrillic			. ?y)
    (can-break			. ?|))
  "Alist mapping symbols to category characters.
Each entry has the form (SYMBOL . CHAR), where SYMBOL is a valid
symbol in `(category SYMBOL)', and CHAR is the category character
corresponding to SYMBOL, as it would be used with `\\c' or `\\C' in
regular expression strings.")


(defvar rx-greedy-flag t
  "Non-nil means produce greedy regular expressions for `zero-or-one',
`zero-or-more', and `one-or-more'.  Dynamically bound.")

(defvar rx--compile-to-lisp nil
  "Nil means return a regexp as a string.
Non-nil means we may return a lisp form which produces a
string (used for `rx' macro).")

(defun rx-info (op head)
  "Return parsing/code generation info for OP.
If OP is the space character ASCII 32, return info for the symbol `?'.
If OP is the character `?', return info for the symbol `??'.
See also `rx-constituents'.
If HEAD is non-nil, then OP is the head of a sexp, otherwise it's
a standalone symbol."
  (cond ((eq op ? ) (setq op '\?))
	((eq op ??) (setq op '\??)))
  (let (old-op)
    (while (and (not (null op)) (symbolp op))
      (setq old-op op)
      (setq op (cdr (assq op rx-constituents)))
      (when (if head (stringp op) (consp op))
        ;; We found something but of the wrong kind.  Let's look for an
        ;; alternate definition for the other case.
        (let ((new-op
               (cdr (assq old-op (cdr (memq (assq old-op rx-constituents)
                                            rx-constituents))))))
          (if (and new-op (not (if head (stringp new-op) (consp new-op))))
              (setq op new-op))))))
  op)


(defun rx-check (form)
  "Check FORM according to its car's parsing info."
  (unless (listp form)
    (error "rx `%s' needs argument(s)" form))
  (let* ((rx (rx-info (car form) 'head))
	 (nargs (1- (length form)))
	 (min-args (nth 1 rx))
	 (max-args (nth 2 rx))
	 (type-pred (nth 3 rx)))
    (when (and (not (null min-args))
	       (< nargs min-args))
      (error "rx form `%s' requires at least %d args"
	     (car form) min-args))
    (when (and (not (null max-args))
	       (> nargs max-args))
      (error "rx form `%s' accepts at most %d args"
	     (car form) max-args))
    (when type-pred
      (dolist (sub-form (cdr form))
	(unless (funcall type-pred sub-form)
	  (error "rx form `%s' requires args satisfying `%s'"
		 (car form) type-pred))))))


(defun rx-group-if (regexp group)
  "Put shy groups around REGEXP if seemingly necessary when GROUP
is non-nil."
  (cond
   ;; for some repetition
   ((eq group '*) (if (rx-atomic-p regexp) (setq group nil)))
   ;; for concatenation
   ((eq group ':)
    (if (rx-atomic-p
         (if (and (stringp regexp)
                  (string-match
                   "\\(?:[?*+]\\??\\|\\\\{[0-9]*,?[0-9]*\\\\}\\)\\'" regexp))
	     (substring regexp 0 (match-beginning 0))
	   regexp))
	(setq group nil)))
   ;; for OR
   ((eq group '|) (setq group nil))
   ;; do anyway
   ((eq group t))
   ((rx-atomic-p regexp t) (setq group nil)))
  (cond ((and group (stringp regexp))
         (concat "\\(?:" regexp "\\)"))
        (group `("\\(?:" ,@regexp "\\)"))
        (t regexp)))


(defvar rx-parent)
;; dynamically bound in some functions.


(defun rx-and (form)
  "Parse and produce code from FORM.
FORM is of the form `(and FORM1 ...)'."
  (rx-check form)
  (rx-group-if
   (rx--subforms (cdr form) ':)
   (and (memq rx-parent '(* t)) rx-parent)))


(defun rx-or (form)
  "Parse and produce code from FORM, which is `(or FORM1 ...)'."
  (rx-check form)
  (rx-group-if
   (cond
    ((null (cdr form)) regexp-unmatchable)
    ((cl-every #'stringp (cdr form))
     (regexp-opt (cdr form) nil t))
    (t (rx--subforms (cdr form) '| "\\|")))
   (and (memq rx-parent '(: * t)) rx-parent)))


(defun rx-anything (form)
  "Match any character."
  (if (consp form)
      (error "rx `anything' syntax error: %s" form))
  (rx-or (list 'or 'not-newline ?\n)))


(defun rx-any-delete-from-range (char ranges)
  "Delete by side effect character CHAR from RANGES.
Only both edges of each range is checked."
  (let (m)
    (cond
     ((memq char ranges) (setq ranges (delq char ranges)))
     ((setq m (assq char ranges))
      (if (eq (1+ char) (cdr m))
	  (setcar (memq m ranges) (1+ char))
	(setcar m (1+ char))))
     ((setq m (rassq char ranges))
      (if (eq (1- char) (car m))
	  (setcar (memq m ranges) (1- char))
	(setcdr m (1- char)))))
    ranges))


(defun rx-any-condense-range (args)
  "Condense by side effect ARGS as range for Rx `any'."
  (let (str
	l)
    ;; set STR list of all strings
    ;; set L list of all ranges
    (mapc (lambda (e) (cond ((stringp e) (push e str))
			    ((numberp e) (push (cons e e) l))
                            ;; Ranges between ASCII and raw bytes are split,
                            ;; to prevent accidental inclusion of Unicode
                            ;; characters later on.
                            ((and (<= (car e) #x7f)
                                  (>= (cdr e) #x3fff80))
                             (push (cons (car e) #x7f) l)
                             (push (cons #x3fff80 (cdr e)) l))
			    (t (push e l))))
	  args)
    ;; condense overlapped ranges in L
    (let ((tail (setq l (sort l #'car-less-than-car)))
	  d)
      (while (setq d (cdr tail))
	(if (>= (cdar tail) (1- (caar d)))
	    (progn
	      (setcdr (car tail) (max (cdar tail) (cdar d)))
	      (setcdr tail (cdr d)))
	  (setq tail d))))
    ;; Separate small ranges to single number, and delete dups.
    (nconc
     (apply #'nconc
	    (mapcar (lambda (e)
		      (cond
		       ((= (car e) (cdr e)) (list (car e)))
		       ((= (1+ (car e)) (cdr e)) (list (car e) (cdr e)))
		       ((list e))))
		    l))
     (delete-dups str))))


(defun rx-check-any-string (str)
  "Turn the `any' argument string STR into a list of characters.
The original order is not preserved.  Ranges, \"A-Z\", become pairs, (?A . ?Z)."
  (let ((decode-char
         ;; Make sure raw bytes are decoded as such, to avoid confusion with
         ;; U+0080..U+00FF.
         (if (multibyte-string-p str)
             #'identity
           (lambda (c) (if (<= #x80 c #xff)
                           (+ c #x3fff00)
                         c))))
        (len (length str))
        (i 0)
        (ret nil))
    (if (= 0 len)
        (error "String arg for Rx `any' must not be empty"))
    (while (< i len)
      (cond ((and (< i (- len 2))
                  (= (aref str (+ i 1)) ?-))
             ;; Range.
             (let ((start (funcall decode-char (aref str i)))
                   (end   (funcall decode-char (aref str (+ i 2)))))
               (cond ((< start end) (push (cons start end) ret))
                     ((= start end) (push start ret))
                     (t
                      (error "Rx character range `%c-%c' is reversed"
                             start end)))
               (setq i (+ i 3))))
            (t
             ;; Single character.
             (push (funcall decode-char (aref str i)) ret)
             (setq i (+ i 1)))))
    ret))


(defun rx-check-any (arg)
   "Check arg ARG for Rx `any'."
   (cond
    ((integerp arg) (list arg))
    ((symbolp arg)
     (let ((translation (condition-case nil
			    (rx-form arg)
			  (error nil))))
       (if (or (null translation)
	       (null (string-match "\\`\\[\\[:[-a-z]+:\\]\\]\\'" translation)))
	   (error "Invalid char class `%s' in Rx `any'" arg))
       (list (substring translation 1 -1)))) ; strip outer brackets
    ((and (characterp (car-safe arg)) (characterp (cdr-safe arg)))
     (unless (<= (car arg) (cdr arg))
       (error "Rx character range `%c-%c' is reversed"
              (car arg) (cdr arg)))
     (list arg))
    ((stringp arg) (rx-check-any-string arg))
    ((error
      "rx `any' requires string, character, char pair or char class args"))))


(defun rx-any (form)
  "Parse and produce code from FORM, which is `(any ARG ...)'.
ARG is optional."
  (rx-check form)
  (let* ((args (rx-any-condense-range
		(apply
		 #'nconc
		 (mapcar #'rx-check-any (cdr form)))))
	 m
	 s)
    (cond
     ;; single close bracket
     ;;	 => "[]...-]" or "[]...--.]"
     ((memq ?\] args)
      ;; set ] at the beginning
      (setq args (cons ?\] (delq ?\] args)))
      ;; set - at the end
      (if (or (memq ?- args) (assq ?- args))
	  (setq args (nconc (rx-any-delete-from-range ?- args)
			    (list ?-)))))
     ;; close bracket starts a range
     ;;  => "[]-....-]" or "[]-.--....]"
     ((setq m (assq ?\] args))
      ;; bring it to the beginning
      (setq args (cons m (delq m args)))
      (cond ((memq ?- args)
	     ;; to the end
	     (setq args (nconc (delq ?- args) (list ?-))))
	    ((setq m (assq ?- args))
	     ;; next to the bracket's range, make the second range
	     (setcdr args (cons m (delq m (cdr args)))))))
     ;; bracket in the end range
     ;;	 => "[]...-]"
     ((setq m (rassq ?\] args))
      ;; set ] at the beginning
      (setq args (cons ?\] (rx-any-delete-from-range ?\] args)))
      ;; set - at the end
      (if (or (memq ?- args) (assq ?- args))
	  (setq args (nconc (rx-any-delete-from-range ?- args)
			    (list ?-)))))
     ;; {no close bracket appears}
     ;;
     ;; bring single bar to the beginning
     ((memq ?- args)
      (setq args (cons ?- (delq ?- args))))
     ;; bar start a range, bring it to the beginning
     ((setq m (assq ?- args))
      (setq args (cons m (delq m args))))
     ;;
     ;; hat at the beginning?
     ((or (eq (car args) ?^) (eq (car-safe (car args)) ?^))
      (setq args (if (cdr args)
		     `(,(cadr args) ,(car args) ,@(cddr args))
		   (nconc (rx-any-delete-from-range ?^ args)
			  (list ?^))))))
    ;; some 1-char?
    (if (and (null (cdr args)) (numberp (car args))
	     (or (= 1 (length
		       (setq s (regexp-quote (string (car args))))))
		 (and (equal (car args) ?^) ;; unnecessary predicate?
		      (null (eq rx-parent '!)))))
	s
      (concat "["
	      (mapconcat
	       (lambda (e) (cond
			    ((numberp e) (string e))
			    ((consp e)
			     (if (and (= (1+ (car e)) (cdr e))
                                      ;; rx-any-condense-range should
                                      ;; prevent this case from happening.
				      (null (memq (car e) '(?\] ?-)))
                                      (null (memq (cdr e) '(?\] ?-))))
				 (string (car e) (cdr e))
			       (string (car e) ?- (cdr e))))
			    (e)))
	       args
	       nil)
	      "]"))))


(defun rx-check-not (arg)
  "Check arg ARG for Rx `not'."
  (unless (or (and (symbolp arg)
		   (string-match "\\`\\[\\[:[-a-z]+:\\]\\]\\'"
				 (condition-case nil
				     (rx-form arg)
				   (error ""))))
	      (eq arg 'word-boundary)
	      (and (consp arg)
		   (memq (car arg) '(not any in syntax category))))
    (error "rx `not' syntax error: %s" arg))
  t)


(defun rx-not (form)
  "Parse and produce code from FORM.  FORM is `(not ...)'."
  (rx-check form)
  (let ((result (rx-form (cadr form) '!))
	case-fold-search)
    (cond ((string-match "\\`\\[\\^" result)
	   (cond
	    ((equal result "[^]") "[^^]")
	    ((and (= (length result) 4) (null (eq rx-parent '!)))
	     (regexp-quote (substring result 2 3)))
	    ((concat "[" (substring result 2)))))
	  ((eq ?\[ (aref result 0))
	   (concat "[^" (substring result 1)))
	  ((string-match "\\`\\\\[scbw]" result)
	   (concat (upcase (substring result 0 2))
		   (substring result 2)))
	  ((string-match "\\`\\\\[SCBW]" result)
	   (concat (downcase (substring result 0 2))
		   (substring result 2)))
	  (t
	   (concat "[^" result "]")))))


(defun rx-not-char (form)
  "Parse and produce code from FORM.  FORM is `(not-char ...)'."
  (rx-check form)
  (rx-not `(not (in ,@(cdr form)))))


(defun rx-not-syntax (form)
  "Parse and produce code from FORM.  FORM is `(not-syntax SYNTAX)'."
  (rx-check form)
  (rx-not `(not (syntax ,@(cdr form)))))


(defun rx-trans-forms (form &optional skip)
  "If FORM's length is greater than two, transform it to length two.
A form (HEAD REST ...) becomes (HEAD (and REST ...)).
If SKIP is non-nil, allow that number of items after the head, i.e.
`(= N REST ...)' becomes `(= N (and REST ...))' if SKIP is 1."
  (unless skip (setq skip 0))
  (let ((tail (nthcdr (1+ skip) form)))
    (if (= (length tail) 1)
	form
      (let ((form (copy-sequence form)))
	(setcdr (nthcdr skip form) (list (cons 'and tail)))
	form))))


(defun rx-= (form)
  "Parse and produce code from FORM `(= N ...)'."
  (rx-check form)
  (setq form (rx-trans-forms form 1))
  (unless (and (integerp (nth 1 form))
	       (> (nth 1 form) 0))
    (error "rx `=' requires positive integer first arg"))
  (let ((subform (rx-form (nth 2 form) '*)))
    (if (stringp subform)
        (format "%s\\{%d\\}" subform (nth 1 form))
      `(,@subform ,(format "\\{%d\\}" (nth 1 form))))))


(defun rx->= (form)
  "Parse and produce code from FORM `(>= N ...)'."
  (rx-check form)
  (setq form (rx-trans-forms form 1))
  (unless (and (integerp (nth 1 form))
	       (> (nth 1 form) 0))
    (error "rx `>=' requires positive integer first arg"))
  (let ((subform (rx-form (nth 2 form) '*)))
    (if (stringp subform)
        (format "%s\\{%d,\\}" subform (nth 1 form))
      `(,@subform ,(format "\\{%d,\\}" (nth 1 form))))))


(defun rx-** (form)
  "Parse and produce code from FORM `(** N M ...)'."
  (rx-check form)
  (rx-form (cons 'repeat (cdr (rx-trans-forms form 2))) '*))


(defun rx-repeat (form)
  "Parse and produce code from FORM.
FORM is either `(repeat N FORM1)' or `(repeat N M FORMS...)'."
  (rx-check form)
  (if (> (length form) 4)
      (setq form (rx-trans-forms form 2)))
  (if (null (nth 2 form))
      (setq form (cons (nth 0 form) (cons (nth 1 form) (nthcdr 3 form)))))
  (cond ((= (length form) 3)
	 (unless (and (integerp (nth 1 form))
		      (> (nth 1 form) 0))
	   (error "rx `repeat' requires positive integer first arg"))
         (let ((subform (rx-form (nth 2 form) '*)))
           (if (stringp subform)
               (format "%s\\{%d\\}" subform (nth 1 form))
             `(,@subform ,(format "\\{%d\\}" (nth 1 form))))))
	((or (not (integerp (nth 2 form)))
	     (< (nth 2 form) 0)
	     (not (integerp (nth 1 form)))
	     (< (nth 1 form) 0)
	     (< (nth 2 form) (nth 1 form)))
	 (error "rx `repeat' range error"))
	(t
         (let ((subform (rx-form (nth 3 form) '*)))
           (if (stringp subform)
               (format "%s\\{%d,%d\\}" subform (nth 1 form) (nth 2 form))
             `(,@subform ,(format "\\{%d,%d\\}" (nth 1 form) (nth 2 form))))))))


(defun rx-submatch (form)
  "Parse and produce code from FORM, which is `(submatch ...)'."
  (let ((subforms (rx--subforms (cdr form) ':)))
    (if (stringp subforms)
        (concat "\\(" subforms "\\)")
      `("\\(" ,@subforms "\\)"))))

(defun rx-submatch-n (form)
  "Parse and produce code from FORM, which is `(submatch-n N ...)'."
  (let ((n (nth 1 form))
        (subforms (rx--subforms (cddr form) ':)))
    (unless (and (integerp n) (> n 0))
      (error "rx `submatch-n' argument must be positive"))
    (if (stringp subforms)
        (concat "\\(?" (number-to-string n) ":" subforms "\\)")
      `("\\(?" ,(number-to-string n) ":" ,@subforms "\\)"))))

(defun rx-backref (form)
  "Parse and produce code from FORM, which is `(backref N)'."
  (rx-check form)
  (format "\\%d" (nth 1 form)))

(defun rx-check-backref (arg)
  "Check arg ARG for Rx `backref'."
  (or (and (integerp arg) (>= arg 1) (<= arg 9))
      (error "rx `backref' requires numeric 1<=arg<=9: %s" arg)))

(defun rx-kleene (form)
  "Parse and produce code from FORM.
FORM is `(OP FORM1)', where OP is one of the `zero-or-one',
`zero-or-more' etc.  operators.
If OP is one of `*', `+', `?', produce a greedy regexp.
If OP is one of `*?', `+?', `??', produce a non-greedy regexp.
If OP is anything else, produce a greedy regexp if `rx-greedy-flag'
is non-nil."
  (rx-check form)
  (setq form (rx-trans-forms form))
  (let ((suffix (cond ((memq (car form) '(* + \? ?\s)) "")
		      ((memq (car form) '(*? +? \?? ??)) "?")
		      (rx-greedy-flag "")
		      (t "?")))
	(op (cond ((memq (car form) '(* *? 0+ zero-or-more)) "*")
		  ((memq (car form) '(+ +? 1+ one-or-more))  "+")
                  (t "?")))
        (subform (rx-form (cadr form) '*)))
    (rx-group-if
     (if (stringp subform)
         (concat subform op suffix)
       `(,@subform ,(concat op suffix)))
     (and (memq rx-parent '(t *)) rx-parent))))


(defun rx-atomic-p (r &optional lax)
  "Return non-nil if regexp string R is atomic.
An atomic regexp R is one such that a suffix operator
appended to R will apply to all of R.  For example, \"a\"
\"[abc]\" and \"\\(ab\\|ab*c\\)\" are atomic and \"ab\",
\"[ab]c\", and \"ab\\|ab*c\" are not atomic.

This function may return false negatives, but it will not
return false positives.  It is nevertheless useful in
situations where an efficiency shortcut can be taken only if a
regexp is atomic.  The function can be improved to detect
more cases of atomic regexps.  Presently, this function
detects the following categories of atomic regexp;

  a group or shy group:  \\(...\\)
  a character class:     [...]
  a single character:    a

On the other hand, false negatives will be returned for
regexps that are atomic but end in operators, such as
\"a+\".  I think these are rare.  Probably such cases could
be detected without much effort.  A guarantee of no false
negatives would require a theoretic specification of the set
of all atomic regexps."
  (if (and rx--compile-to-lisp
           (not (stringp r)))
      nil ;; Runtime value, we must assume non-atomic.
    (let ((l (length r)))
      (cond
       ((<= l 1))
       ((= l 2) (= (aref r 0) ?\\))
       ((= l 3) (string-match "\\`\\(?:\\\\[cCsS_]\\|\\[[^^]\\]\\)" r))
       ((null lax)
        (cond
         ((string-match "\\`\\[\\^?]?\\(?:\\[:[a-z]+:]\\|[^]]\\)*]\\'" r))
         ((string-match "\\`\\\\(\\(?:[^\\]\\|\\\\[^)]\\)*\\\\)\\'" r))))))))


(defun rx-syntax (form)
  "Parse and produce code from FORM, which is `(syntax SYMBOL)'."
  (rx-check form)
  (let* ((sym (cadr form))
	 (syntax (cdr (assq sym rx-syntax))))
    (unless syntax
      ;; Try sregex compatibility.
      (cond
       ((characterp sym) (setq syntax sym))
       ((symbolp sym)
        (let ((name (symbol-name sym)))
          (if (= 1 (length name))
              (setq syntax (aref name 0))))))
      (unless syntax
	(error "Unknown rx syntax `%s'" sym)))
    (format "\\s%c" syntax)))


(defun rx-check-category (form)
  "Check the argument FORM of a `(category FORM)'."
  (unless (or (integerp form)
	      (cdr (assq form rx-categories)))
    (error "Unknown category `%s'" form))
  t)


(defun rx-category (form)
  "Parse and produce code from FORM, which is `(category SYMBOL)'."
  (rx-check form)
  (let ((char (if (integerp (cadr form))
		  (cadr form)
		(cdr (assq (cadr form) rx-categories)))))
    (format "\\c%c" char)))


(defun rx-eval (form)
  "Parse and produce code from FORM, which is `(eval FORM)'."
  (rx-check form)
  (rx-form (eval (cadr form)) rx-parent))


(defun rx-greedy (form)
  "Parse and produce code from FORM.
If FORM is `(minimal-match FORM1)', non-greedy versions of `*',
`+', and `?' operators will be used in FORM1.  If FORM is
`(maximal-match FORM1)', greedy operators will be used."
  (rx-check form)
  (let ((rx-greedy-flag (eq (car form) 'maximal-match)))
    (rx-form (cadr form) rx-parent)))


(defun rx-regexp (form)
  "Parse and produce code from FORM, which is `(regexp STRING)'."
  (cond ((stringp (cadr form))
         (rx-group-if (cadr form) rx-parent))
        (rx--compile-to-lisp
         ;; Always group non-string forms, since we can't be sure they
         ;; are atomic.
         (rx-group-if (cdr form) t))
        (t (rx-check form))))

(defun rx-literal (form)
  "Parse and produce code from FORM, which is `(literal STRING-EXP)'."
  (cond ((stringp (cadr form))
         ;; This is allowed, but makes little sense, you could just
         ;; use STRING directly.
         (rx-group-if (regexp-quote (cadr form)) rx-parent))
        (rx--compile-to-lisp
         (rx-group-if `((regexp-quote ,(cadr form))) rx-parent))
        (t (rx-check form))))

(defun rx-form (form &optional parent)
  "Parse and produce code for regular expression FORM.
FORM is a regular expression in sexp form.
PARENT shows which type of expression calls and controls putting of
shy groups around the result and some more in other functions."
  (let ((rx-parent parent))
    (cond
     ((stringp form)
      (rx-group-if (regexp-quote form)
                   (if (and (eq parent '*) (< 1 (length form)))
                       parent)))
     ((integerp form)
      (regexp-quote (char-to-string form)))
     ((symbolp form)
      (let ((info (rx-info form nil)))
        (cond ((stringp info)
               info)
              ((null info)
               (error "Unknown rx form `%s'" form))
              (t
               (funcall (nth 0 info) form)))))
     ((consp form)
      (let ((info (rx-info (car form) 'head)))
        (unless (consp info)
          (error "Unknown rx form `%s'" (car form)))
        (funcall (nth 0 info) form)))
     (t
      (error "rx syntax error at `%s'" form)))))

(defun rx--subforms (subforms &optional parent separator)
  "Produce code for regular expressions SUBFORMS.
SUBFORMS is a list of regular expression sexps.
PARENT controls grouping, as in `rx-form'.
Insert SEPARATOR between the code from each of SUBFORMS."
  (if (null (cdr subforms))
      ;; Zero or one forms, no need for grouping.
      (and subforms (rx-form (car subforms)))
    (let ((listify (lambda (x)
                     (if (listp x) (copy-sequence x)
                       (list x)))))
      (setq subforms (mapcar (lambda (x) (rx-form x parent)) subforms))
      (cond ((or (not rx--compile-to-lisp)
                 (cl-every #'stringp subforms))
             (mapconcat #'identity subforms separator))
            (separator
             (nconc (funcall listify (car subforms))
                    (mapcan (lambda (x)
                              (cons separator (funcall listify x)))
                            (cdr subforms))))
            (t (mapcan listify subforms))))))


;;;###autoload
(defun rx-to-string (form &optional no-group)
  "Parse and produce code for regular expression FORM.
FORM is a regular expression in sexp form.
NO-GROUP non-nil means don't put shy groups around the result.

In contrast to the `rx' macro, subforms `literal' and `regexp'
will not accept non-string arguments, i.e., (literal STRING)
becomes just a more verbose version of STRING."
  (rx-group-if (rx-form form) (null no-group)))


;;;###autoload
(defmacro rx (&rest regexps)
  "Translate regular expressions REGEXPS in sexp form to a regexp string.
Each argument is one of the forms below; RX is a subform, and RX... stands
for one or more RXs.  For details, see Info node `(elisp) Rx Notation'.
See `rx-to-string' for the corresponding function.

STRING         Match a literal string.
CHAR           Match a literal character.

(seq RX...)    Match the RXs in sequence.  Alias: :, sequence, and.
(or RX...)     Match one of the RXs.  Alias: |.

(zero-or-more RX...) Match RXs zero or more times.  Alias: 0+.
(one-or-more RX...)  Match RXs one or more times.  Alias: 1+.
(zero-or-one RX...)  Match RXs or the empty string.  Alias: opt, optional.
(* RX...)       Match RXs zero or more times; greedy.
(+ RX...)       Match RXs one or more times; greedy.
(? RX...)       Match RXs or the empty string; greedy.
(*? RX...)      Match RXs zero or more times; non-greedy.
(+? RX...)      Match RXs one or more times; non-greedy.
(?? RX...)      Match RXs or the empty string; non-greedy.
(= N RX...)     Match RXs exactly N times.
(>= N RX...)    Match RXs N or more times.
(** N M RX...)  Match RXs N to M times.  Alias: repeat.
(minimal-match RX)  Match RX, with zero-or-more, one-or-more, zero-or-one
                and aliases using non-greedy matching.
(maximal-match RX)  Match RX, with zero-or-more, one-or-more, zero-or-one
                and aliases using greedy matching, which is the default.

(any SET...)    Match a character from one of the SETs.  Each SET is a
                character, a string, a range as string \"A-Z\" or cons
                (?A . ?Z), or a character class (see below).  Alias: in, char.
(not CHARSPEC)  Match one character not matched by CHARSPEC.  CHARSPEC
                can be (any ...), (syntax ...), (category ...),
                or a character class.
not-newline     Match any character except a newline.  Alias: nonl.
anything        Match any character.

CHARCLASS       Match a character from a character class.  One of:
 alpha, alphabetic, letter   Alphabetic characters (defined by Unicode).
 alnum, alphanumeric         Alphabetic or decimal digit chars (Unicode).
 digit numeric, num          0-9.
 xdigit, hex-digit, hex      0-9, A-F, a-f.
 cntrl, control              ASCII codes 0-31.
 blank                       Horizontal whitespace (Unicode).
 space, whitespace, white    Chars with whitespace syntax.
 lower, lower-case           Lower-case chars, from current case table.
 upper, upper-case           Upper-case chars, from current case table.
 graph, graphic              Graphic characters (Unicode).
 print, printing             Whitespace or graphic (Unicode).
 punct, punctuation          Not control, space, letter or digit (ASCII);
                              not word syntax (non-ASCII).
 word, wordchar              Characters with word syntax.
 ascii                       ASCII characters (codes 0-127).
 nonascii                    Non-ASCII characters (but not raw bytes).

(syntax SYNTAX)  Match a character with syntax SYNTAX, being one of:
  whitespace, punctuation, word, symbol, open-parenthesis,
  close-parenthesis, expression-prefix, string-quote,
  paired-delimiter, escape, character-quote, comment-start,
  comment-end, string-delimiter, comment-delimiter

(category CAT)   Match a character in category CAT, being one of:
  space-for-indent, base, consonant, base-vowel,
  upper-diacritical-mark, lower-diacritical-mark, tone-mark, symbol,
  digit, vowel-modifying-diacritical-mark, vowel-sign,
  semivowel-lower, not-at-end-of-line, not-at-beginning-of-line,
  alpha-numeric-two-byte, chinese-two-byte, greek-two-byte,
  japanese-hiragana-two-byte, indian-two-byte,
  japanese-katakana-two-byte, strong-left-to-right,
  korean-hangul-two-byte, strong-right-to-left, cyrillic-two-byte,
  combining-diacritic, ascii, arabic, chinese, ethiopic, greek,
  korean, indian, japanese, japanese-katakana, latin, lao,
  tibetan, japanese-roman, thai, vietnamese, hebrew, cyrillic,
  can-break

Zero-width assertions: these all match the empty string in specific places.
 line-start         At the beginning of a line.  Alias: bol.
 line-end           At the end of a line.  Alias: eol.
 string-start       At the start of the string or buffer.
                     Alias: buffer-start, bos, bot.
 string-end         At the end of the string or buffer.
                     Alias: buffer-end, eos, eot.
 point              At point.
 word-start         At the beginning of a word.
 word-end           At the end of a word.
 word-boundary      At the beginning or end of a word.
 not-word-boundary  Not at the beginning or end of a word.
 symbol-start       At the beginning of a symbol.
 symbol-end         At the end of a symbol.

(group RX...)  Match RXs and define a capture group.  Alias: submatch.
(group-n N RX...) Match RXs and define capture group N.  Alias: submatch-n.
(backref N)    Match the text that capture group N matched.

(literal EXPR) Match the literal string from evaluating EXPR at run time.
(regexp EXPR)  Match the string regexp from evaluating EXPR at run time.
(eval EXPR)    Match the rx sexp from evaluating EXPR at compile time."
  (let* ((rx--compile-to-lisp t)
         (re (cond ((null regexps)
                    (error "No regexp"))
                   ((cdr regexps)
                    (rx-to-string `(and ,@regexps) t))
                   (t
                    (rx-to-string (car regexps) t)))))
    (if (stringp re)
        re
      `(concat ,@re))))


(pcase-defmacro rx (&rest regexps)
  "Build a `pcase' pattern matching `rx' REGEXPS in sexp form.
The REGEXPS are interpreted as in `rx'.  The pattern matches any
string that is a match for the regular expression so constructed,
as if by `string-match'.

In addition to the usual `rx' constructs, REGEXPS can contain the
following constructs:

  (let REF SEXP...)  creates a new explicitly named reference to
                     a submatch that matches regular expressions
                     SEXP, and binds the match to REF.
  (backref REF)      creates a backreference to the submatch
                     introduced by a previous (let REF ...)
                     construct.  REF can be the same symbol
                     in the first argument of the corresponding
                     (let REF ...) construct, or it can be a
                     submatch number.  It matches the referenced
                     submatch.

The REFs are associated with explicitly named submatches starting
from 1.  Multiple occurrences of the same REF refer to the same
submatch.

If a case matches, the match data is modified as usual so you can
use it in the case body, but you still have to pass the correct
string as argument to `match-string'."
  (let* ((vars ())
         (rx-constituents
          `((let
             ,(lambda (form)
                (rx-check form)
                (let ((var (cadr form)))
                  (cl-check-type var symbol)
                  (let ((i (or (cl-position var vars :test #'eq)
                               (prog1 (length vars)
                                 (setq vars `(,@vars ,var))))))
                    (rx-form `(submatch-n ,(1+ i) ,@(cddr form))))))
             1 nil)
            (backref
             ,(lambda (form)
                (rx-check form)
                (rx-backref
                 `(backref ,(let ((var (cadr form)))
                              (if (integerp var) var
                                (1+ (cl-position var vars :test #'eq)))))))
             1 1
             ,(lambda (var)
                (cond ((integerp var) (rx-check-backref var))
                      ((memq var vars) t)
                      (t (error "rx `backref' variable must be one of %s: %s"
                                vars var)))))
            ,@rx-constituents))
         (regexp (rx-to-string `(seq ,@regexps) :no-group)))
    `(and (pred (string-match ,regexp))
          ,@(cl-loop for i from 1
                     for var in vars
                     collect `(app (match-string ,i) ,var)))))

(provide 'rx)

;;; rx.el ends here
