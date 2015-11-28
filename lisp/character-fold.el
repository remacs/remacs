;;; character-fold.el --- match unicode to similar ASCII -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: matching

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(eval-and-compile (put 'character-fold-table 'char-table-extra-slots 1))

(defconst character-fold-table
  (eval-when-compile
    (let ((equiv (make-char-table 'character-fold-table))
          (equiv-multi (make-char-table 'character-fold-table))
          (table (unicode-property-table-internal 'decomposition)))
      (set-char-table-extra-slot equiv 0 equiv-multi)

      ;; Ensure the table is populated.
      (let ((func (char-table-extra-slot table 1)))
        (map-char-table (lambda (char v)
                          (when (consp char)
                            (funcall func (car char) v table)))
                        table))

      ;; Compile a list of all complex characters that each simple
      ;; character should match.
      ;; In summary this loop does 3 things:
      ;; - A complex character might be allowed to match its decomp.
      ;; - The decomp is allowed to match the complex character.
      ;; - A single char of the decomp might be allowed to match the
      ;;   character.
      ;; Some examples in the comments below.
      (map-char-table
       (lambda (char decomp)
         (when (consp decomp)
           ;; Skip trivial cases like ?a decomposing to (?a).
           (unless (and (not (cdr decomp))
                        (eq char (car decomp)))
             (if (symbolp (car decomp))
                 ;; Discard a possible formatting tag.
                 (setq decomp (cdr decomp))
               ;; If there's no formatting tag, ensure that char matches
               ;; its decomp exactly.  This is because we want 'ä' to
               ;; match 'ä', but we don't want '¹' to match '1'.
               (aset equiv char
                     (cons (apply #'string decomp)
                           (aref equiv char))))

             ;; Allow the entire decomp to match char.  If decomp has
             ;; multiple characters, this is done by adding an entry
             ;; to the alist of the first character in decomp.  This
             ;; allows 'ff' to match 'ﬀ', 'ä' to match 'ä', and '1' to
             ;; match '¹'.
             (let ((make-decomp-match-char
                    (lambda (decomp char)
                      (if (cdr decomp)
                          (aset equiv-multi (car decomp)
                                (cons (cons (apply #'string (cdr decomp))
                                            (regexp-quote (string char)))
                                      (aref equiv-multi (car decomp))))
                        (aset equiv (car decomp)
                              (cons (char-to-string char)
                                    (aref equiv (car decomp))))))))
               (funcall make-decomp-match-char decomp char)
               ;; Do it again, without the non-spacing characters.
               ;; This allows 'a' to match 'ä'.
               (let ((simpler-decomp nil)
                     (found-one nil))
                 (dolist (c decomp)
                   (if (> (get-char-code-property c 'canonical-combining-class) 0)
                       (setq found-one t)
                     (push c simpler-decomp)))
                 (when (and simpler-decomp found-one)
                   (funcall make-decomp-match-char simpler-decomp char)
                   ;; Finally, if the decomp only had one spacing
                   ;; character, we allow this character to match the
                   ;; decomp.  This is to let 'a' match 'ä'.
                   (unless (cdr simpler-decomp)
                     (aset equiv (car simpler-decomp)
                           (cons (apply #'string decomp)
                                 (aref equiv (car simpler-decomp)))))))))))
       table)

      ;; Add some manual entries.
      (dolist (it '((?\" "＂" "“" "”" "”" "„" "⹂" "〞" "‟" "‟" "❞" "❝" "❠" "“" "„" "〝" "〟" "🙷" "🙶" "🙸" "«" "»")
                    (?' "❟" "❛" "❜" "‘" "’" "‚" "‛" "‚" "󠀢" "❮" "❯" "‹" "›")
                    (?` "❛" "‘" "‛" "󠀢" "❮" "‹")))
        (let ((idx (car it))
              (chars (cdr it)))
          (aset equiv idx (append chars (aref equiv idx)))))

      ;; Convert the lists of characters we compiled into regexps.
      (map-char-table
       (lambda (char dec-list)
         (let ((re (regexp-opt (cons (char-to-string char) dec-list))))
           (if (consp char)
               (set-char-table-range equiv char re)
             (aset equiv char re))))
       equiv)
      equiv))
  "Used for folding characters of the same group during search.
This is a char-table with the `character-fold-table' subtype.

Let us refer to the character in question by char-x.
Each entry is either nil (meaning char-x only matches literally)
or a regexp.  This regexp should match anything that char-x can
match by itself \(including char-x).  For instance, the default
regexp for the ?+ character is \"[+⁺₊﬩﹢＋]\".

This table also has one extra slot which is also a char-table.
Each entry in the extra slot is an alist used for multi-character
matching (which may be nil).  The elements of the alist should
have the form (SUFFIX . OTHER-REGEXP).  If the characters after
char-x are equal to SUFFIX, then this combination of char-x +
SUFFIX is allowed to match OTHER-REGEXP.  This is in addition to
char-x being allowed to match REGEXP.
For instance, the default alist for ?f includes:
    \((\"fl\" . \"ﬄ\") (\"fi\" . \"ﬃ\")
     (\"i\" . \"ﬁ\") (\"f\" . \"ﬀ\"))

Exceptionally for the space character (32), ALIST is ignored.")

(defun character-fold--make-space-string (n)
  "Return a string that matches N spaces."
  (format "\\(?:%s\\|%s\\)"
          (make-string n ?\s)
          (apply #'concat
                 (make-list n (or (aref character-fold-table ?\s) " ")))))

;;;###autoload
(defun character-fold-to-regexp (string &optional _lax from)
  "Return a regexp matching anything that character-folds into STRING.
Any character in STRING that has an entry in
`character-fold-table' is replaced with that entry (which is a
regexp) and other characters are `regexp-quote'd.

FROM is for internal use.  It specifies an index in the STRING
from which to start."
  (let* ((spaces 0)
         (multi-char-table (char-table-extra-slot character-fold-table 0))
         (lower-case-table (current-case-table))
         (upper-case-table (char-table-extra-slot lower-case-table 0))
         (i (or from 0))
         (end (length string))
         (out nil))
    ;; When the user types a space, we want to match the table entry
    ;; for ?\s, which is generally a regexp like "[ ...]".  However,
    ;; the `search-spaces-regexp' variable doesn't "see" spaces inside
    ;; these regexp constructs, so we need to use "\\( \\|[ ...]\\)"
    ;; instead (to manually expose a space).  Furthermore, the lax
    ;; search engine acts on a bunch of spaces, not on individual
    ;; spaces, so if the string contains sequential spaces like "  ", we
    ;; need to keep them grouped together like this: "\\(  \\|[ ...][ ...]\\)".
    (while (< i end)
      (pcase (aref string i)
        (`?\s (setq spaces (1+ spaces)))
        (c (when (> spaces 0)
             (push (character-fold--make-space-string spaces) out)
             (setq spaces 0))
           (let ((regexp (or (aref character-fold-table c)
                             (regexp-quote (string c))))
                 (alist nil))
             ;; Long string.  The regexp would probably be too long.
             (unless (> end 50)
               (setq alist (aref multi-char-table c))
               (when case-fold-search
                 (let ((other-c (aref lower-case-table c)))
                   (when (or (not other-c)
                             (eq other-c c))
                     (setq other-c (aref upper-case-table c)))
                   (when other-c
                     (setq alist (append alist (aref multi-char-table other-c)))
                     (setq regexp (concat "\\(?:" regexp "\\|"
                                          (or (aref character-fold-table other-c)
                                              (regexp-quote (string other-c)))
                                          "\\)"))))))
             (push (let ((alist-out '("\\)")))
                     (pcase-dolist (`(,suffix . ,out-regexp) alist)
                       (let ((len-suf (length suffix)))
                         (when (eq (compare-strings suffix 0 nil
                                                    string (1+ i) (+ i 1 len-suf)
                                                    nil)
                                   t)
                           ;; FIXME: If N suffixes match, we "branch"
                           ;; out into N+1 executions for the rest of
                           ;; the string.  This involves redundant
                           ;; work and makes a huge regexp.
                           (push (concat "\\|" out-regexp
                                         (character-fold-to-regexp
                                          string nil (+ i 1 len-suf)))
                                 alist-out))))
                     ;; If no suffixes matched, just go on.
                     (if (not (cdr alist-out))
                         regexp
                       ;; Otherwise, add a branch for the
                       ;; no-suffix case, and stop the loop here.
                       (prog1 (apply #'concat "\\(?:" regexp
                                     (character-fold-to-regexp string nil (1+ i))
                                     alist-out)
                         (setq i end))))
                   out))))
      (setq i (1+ i)))
    (when (> spaces 0)
      (push (character-fold--make-space-string spaces) out))
    (apply #'concat (nreverse out))))


;;; Commands provided for completeness.
(defun character-fold-search-forward (string &optional bound noerror count)
  "Search forward for a character-folded version of STRING.
STRING is converted to a regexp with `character-fold-to-regexp',
which is searched for with `re-search-forward'.
BOUND NOERROR COUNT are passed to `re-search-forward'."
  (interactive "sSearch: ")
  (re-search-forward (character-fold-to-regexp string) bound noerror count))

(defun character-fold-search-backward (string &optional bound noerror count)
  "Search backward for a character-folded version of STRING.
STRING is converted to a regexp with `character-fold-to-regexp',
which is searched for with `re-search-backward'.
BOUND NOERROR COUNT are passed to `re-search-backward'."
  (interactive "sSearch: ")
  (re-search-backward (character-fold-to-regexp string) bound noerror count))

(provide 'character-fold)

;;; character-fold.el ends here
