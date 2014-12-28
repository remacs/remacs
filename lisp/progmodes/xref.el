;; xref.el --- Cross-referencing commands              -*-lexical-binding:t-*-

;; Copyright (C) 2014 Free Software Foundation, Inc.

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

;;; Commentary:

;; This file provides a somewhat generic infrastructure for cross
;; referencing commands, in particular "find-definition".
;;
;; Some part of the functionality must be implemented in a language
;; dependent way and that's done by defining `xref-find-function',
;; `xref-identifier-at-point-function' and
;; `xref-identifier-completion-table-function', which see.
;;
;; A major mode should make these variables buffer-local first.
;;
;; `xref-find-function' can be called in several ways, see its
;; description.  It has to operate with "xref" and "location" values.
;;
;; One would usually call `make-xref' and `xref-make-file-location',
;; `xref-make-buffer-location' or `xref-make-bogus-location' to create
;; them.
;;
;; Each identifier must be represented as a string.  Implementers can
;; use string properties to store additional information about the
;; identifier, but they should keep in mind that values returned from
;; `xref-identifier-completion-table-function' should still be
;; distinct, because the user can't see the properties when making the
;; choice.
;;
;; See the functions `etags-xref-find' and `elisp-xref-find' for full
;; examples.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'ring)

(defgroup xref nil "Cross-referencing commands"
  :group 'tools)


;;; Locations

(defclass xref-location () ()
  :documentation "A location represents a position in a file or buffer.")

;; If a backend decides to subclass xref-location it can provide
;; methods for some of the following functions:
(defgeneric xref-location-marker (location)
  "Return the marker for LOCATION.")

(defgeneric xref-location-group (location)
  "Return a string used to group a set of locations.
This is typically the filename.")

;;;; Commonly needed location classes are defined here:

;; FIXME: might be useful to have an optional "hint" i.e. a string to
;; search for in case the line number is sightly out of date.
(defclass xref-file-location (xref-location)
  ((file :type string :initarg :file)
   (line :type fixnum :initarg :line)
   (column :type fixnum :initarg :column))
  :documentation "A file location is a file/line/column triple.
Line numbers start from 1 and columns from 0.")

(defun xref-make-file-location (file line column)
  "Create and return a new xref-file-location."
  (make-instance 'xref-file-location :file file :line line :column column))

(defmethod xref-location-marker ((l xref-file-location))
  (with-slots (file line column) l
    (with-current-buffer
        (or (get-file-buffer file)
            (let ((find-file-suppress-same-file-warnings t))
              (find-file-noselect file)))
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (beginning-of-line line)
          (move-to-column column)
          (point-marker))))))

(defmethod xref-location-group ((l xref-file-location))
  (oref l :file))

(defclass xref-buffer-location (xref-location)
  ((buffer :type buffer :initarg :buffer)
   (position :type fixnum :initarg :position)))

(defun xref-make-buffer-location (buffer position)
  "Create and return a new xref-buffer-location."
  (make-instance 'xref-buffer-location :buffer buffer :position position))

(defmethod xref-location-marker ((l xref-buffer-location))
  (with-slots (buffer position) l
    (let ((m (make-marker)))
      (move-marker m position buffer))))

(defmethod xref-location-group ((l xref-buffer-location))
  (with-slots (buffer) l
    (or (buffer-file-name buffer)
        (format "(buffer %s)" (buffer-name buffer)))))

(defclass xref-bogus-location (xref-location)
  ((message :type string :initarg :message
            :reader xref-bogus-location-message))
  :documentation "Bogus locations are sometimes useful to
indicate errors, e.g. when we know that a function exists but the
actual location is not known.")

(defun xref-make-bogus-location (message)
  "Create and return a new xref-bogus-location."
  (make-instance 'xref-bogus-location :message message))

(defmethod xref-location-marker ((l xref-bogus-location))
  (user-error "%s" (oref l :message)))

(defmethod xref-location-group ((_ xref-bogus-location)) "(No location)")


;;; Cross-reference

(defclass xref--xref ()
  ((description :type string :initarg :description
                :reader xref--xref-description)
   (location :type xref-location :initarg :location
             :reader xref--xref-location))
  :comment "An xref is used to display and locate constructs like
variables or functions.")

(defun xref-make (description location)
  "Create and return a new xref.
DESCRIPTION is a short string to describe the xref.
LOCATION is an `xref-location'."
  (make-instance 'xref--xref :description description :location location))


;;; API

(declare-function etags-xref-find "etags" (action id))
(declare-function tags-lazy-completion-table "etags" ())

;; For now, make the etags backend the default.
(defvar xref-find-function #'etags-xref-find
  "Function to look for cross-references.
It can be called in several ways:

 (definitions IDENTIFIER): Find definitions of IDENTIFIER.  The
result must be a list of xref objects.  If no definitions can be
found, return nil.

 (references IDENTIFIER): Find references of IDENTIFIER.  The
result must be a list of xref objects.  If no references can be
found, return nil.

 (apropos PATTERN): Find all symbols that match PATTERN.  PATTERN
is a regexp.

IDENTIFIER can be any string returned by
`xref-identifier-at-point-function', or from the table returned
by `xref-identifier-completion-table-function'.

To create an xref object, call `xref-make'.")

(defvar xref-identifier-at-point-function #'xref-default-identifier-at-point
  "Function to get the relevant identifier at point.

The return value must be a string or nil.  nil means no
identifier at point found.

If it's hard to determine the identifier precisely (e.g., because
it's a method call on unknown type), the implementation can
return a simple string (such as symbol at point) marked with a
special text property which `xref-find-function' would recognize
and then delegate the work to an external process.")

(defvar xref-identifier-completion-table-function #'tags-lazy-completion-table
  "Function that returns the completion table for identifiers.")

(defun xref-default-identifier-at-point ()
  (let ((thing (thing-at-point 'symbol)))
    (and thing (substring-no-properties thing))))


;;; misc utilities
(defun xref--alistify (list key test)
  "Partition the elements of LIST into an alist.
KEY extracts the key from an element and TEST is used to compare
keys."
  (let ((alist '()))
    (dolist (e list)
      (let* ((k (funcall key e))
             (probe (cl-assoc k alist :test test)))
        (if probe
            (setcdr probe (cons e (cdr probe)))
          (push (cons k (list e)) alist))))
    ;; Put them back in order.
    (cl-loop for (key . value) in (reverse alist)
             collect (cons key (reverse value)))))

(defun xref--insert-propertized (props &rest strings)
  "Insert STRINGS with text properties PROPS."
  (let ((start (point)))
    (apply #'insert strings)
    (add-text-properties start (point) props)))

(defun xref--search-property (property &optional backward)
    "Search the next text range where text property PROPERTY is non-nil.
Return the value of PROPERTY.  If BACKWARD is non-nil, search
backward."
  (let ((next (if backward
                  #'previous-single-char-property-change
                #'next-single-char-property-change))
        (start (point))
        (value nil))
    (while (progn
             (goto-char (funcall next (point) property))
             (not (or (setq value (get-text-property (point) property))
                      (eobp)
                      (bobp)))))
    (cond (value)
          (t (goto-char start) nil))))


;;; Marker stack  (M-. pushes, M-, pops)

(defcustom xref-marker-ring-length 16
  "Length of the xref marker ring."
  :type 'integer
  :version "25.1")

(defvar xref--marker-ring (make-ring xref-marker-ring-length)
  "Ring of markers to implement the marker stack.")

(defun xref-push-marker-stack ()
  "Add point to the marker stack."
  (ring-insert xref--marker-ring (point-marker)))

;;;###autoload
(defun xref-pop-marker-stack ()
  "Pop back to where \\[xref-find-definitions] was last invoked."
  (interactive)
  (let ((ring xref--marker-ring))
    (when (ring-empty-p ring)
      (error "Marker stack is empty"))
    (let ((marker (ring-remove ring 0)))
      (switch-to-buffer (or (marker-buffer marker)
                            (error "The marked buffer has been deleted")))
      (goto-char (marker-position marker))
      (set-marker marker nil nil))))

;; etags.el needs this
(defun xref-clear-marker-stack ()
  "Discard all markers from the marker stack."
  (let ((ring xref--marker-ring))
    (while (not (ring-empty-p ring))
      (let ((marker (ring-remove ring)))
        (set-marker marker nil nil)))))


(defun xref--goto-location (location)
  "Set buffer and point according to xref-location LOCATION."
  (let ((marker (xref-location-marker location)))
    (set-buffer (marker-buffer marker))
    (cond ((and (<= (point-min) marker) (<= marker (point-max))))
          (widen-automatically (widen))
          (t (error "Location is outside accessible part of buffer")))
    (goto-char marker)))

(defun xref--pop-to-location (location &optional window)
  "Goto xref-location LOCATION and display the buffer.
WINDOW controls how the buffer is displayed:
  nil      -- switch-to-buffer
  'window  -- pop-to-buffer (other window)
  'frame   -- pop-to-buffer (other frame)"
  (xref--goto-location location)
  (cl-ecase window
    ((nil)  (switch-to-buffer (current-buffer)))
    (window (pop-to-buffer (current-buffer) t))
    (frame  (let ((pop-up-frames t)) (pop-to-buffer (current-buffer) t)))))


;;; XREF buffer (part of the UI)

;; The xref buffer is used to display a set of xrefs.

(defun xref--display-position (pos other-window recenter-arg)
  ;; show the location, but don't hijack focus.
  (with-selected-window (display-buffer (current-buffer) other-window)
    (goto-char pos)
    (recenter recenter-arg)))

(defun xref--show-location (location)
  (condition-case err
      (progn
        (xref--goto-location location)
        (xref--display-position (point) t 1))
    (user-error (message (error-message-string err)))))

(defun xref--next-line (backward)
  (let ((loc (xref--search-property 'xref-location backward)))
    (when loc
      (save-window-excursion
        (xref--show-location loc)
        (sit-for most-positive-fixnum)))))

(defun xref-next-line ()
  "Move to the next xref and display its source in the other window."
  (interactive)
  (xref--next-line nil))

(defun xref-prev-line ()
  "Move to the previous xref and display its source in the other window."
  (interactive)
  (xref--next-line t))

(defun xref--location-at-point ()
  (or (get-text-property (point) 'xref-location)
      (error "No reference at point")))

(defvar-local xref--window nil)

(defun xref-goto-xref ()
  "Jump to the xref at point and bury the xref buffer."
  (interactive)
  (let ((loc (xref--location-at-point))
        (window xref--window))
    (quit-window)
    (xref--pop-to-location loc window)))

(define-derived-mode xref--xref-buffer-mode fundamental-mode "XREF"
  "Mode for displaying cross-references."
  (setq buffer-read-only t))

(let ((map xref--xref-buffer-mode-map))
  (define-key map (kbd "q") #'quit-window)
  (define-key map [remap next-line] #'xref-next-line)
  (define-key map [remap previous-line] #'xref-prev-line)
  (define-key map (kbd "RET") #'xref-goto-xref)

  ;; suggested by Johan Claesson "to further reduce finger movement":
  (define-key map (kbd ".") #'xref-next-line)
  (define-key map (kbd ",") #'xref-prev-line))

(defconst xref-buffer-name "*xref*"
  "The name of the buffer to show xrefs.")

(defun xref--insert-xrefs (xref-alist)
  "Insert XREF-ALIST in the current-buffer.
XREF-ALIST is of the form ((GROUP . (XREF ...)) ...).  Where
GROUP is a string for decoration purposes and XREF is an
`xref--xref' object."
  (cl-loop for ((group . xrefs) . more1) on xref-alist do
           (xref--insert-propertized '(face bold) group "\n")
           (cl-loop for (xref . more2) on xrefs do
                    (insert "  ")
                    (with-slots (description location) xref
                      (xref--insert-propertized
                       (list 'xref-location location
                             'face 'font-lock-keyword-face)
                       description))
                    (when (or more1 more2)
                      (insert "\n")))))

(defun xref--analyze (xrefs)
  "Find common filenames in XREFS.
Return an alist of the form ((FILENAME . (XREF ...)) ...)."
  (xref--alistify xrefs
                  (lambda (x)
                    (xref-location-group (xref--xref-location x)))
                  #'equal))

(defun xref--show-xref-buffer (xrefs window)
  (let ((xref-alist (xref--analyze xrefs)))
    (with-current-buffer (get-buffer-create xref-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (xref--insert-xrefs xref-alist)
        (xref--xref-buffer-mode)
        (pop-to-buffer (current-buffer))
        (goto-char (point-min))
        (setq xref--window window)
        (current-buffer)))))


;; This part of the UI seems fairly uncontroversial: it reads the
;; identifier and deals with the single definition case.
;;
;; The controversial multiple definitions case is handed off to
;; xref-show-xrefs-function.

(defvar xref-show-xrefs-function 'xref--show-xref-buffer
  "Function to display a list of xrefs.")

(defun xref--show-xrefs (id kind xrefs window)
  (cond
   ((null xrefs)
    (user-error "No known %s for: %s" kind id))
   ((not (cdr xrefs))
    (xref-push-marker-stack)
    (xref--pop-to-location (xref--xref-location (car xrefs)) window))
   (t
    (xref-push-marker-stack)
    (funcall xref-show-xrefs-function xrefs window))))

(defun xref--read-identifier (prompt)
  "Return the identifier at point or read it from the minibuffer."
  (let ((id (funcall xref-identifier-at-point-function)))
    (cond ((or current-prefix-arg (not id))
           (completing-read prompt
                            (funcall xref-identifier-completion-table-function)
                            nil t id))
          (t id))))


;;; Commands

(defun xref--find-definitions (id window)
  (xref--show-xrefs id "definitions"
                    (funcall xref-find-function 'definitions id)
                    window))

;;;###autoload
(defun xref-find-definitions (identifier)
  "Find the definition of the identifier at point.
With prefix argument, prompt for the identifier."
  (interactive (list (xref--read-identifier "Find definitions of: ")))
  (xref--find-definitions identifier nil))

;;;###autoload
(defun xref-find-definitions-other-window (identifier)
  "Like `xref-find-definitions' but switch to the other window."
  (interactive (list (xref--read-identifier "Find definitions of: ")))
  (xref--find-definitions identifier 'window))

;;;###autoload
(defun xref-find-definitions-other-frame (identifier)
  "Like `xref-find-definitions' but switch to the other frame."
  (interactive (list (xref--read-identifier "Find definitions of: ")))
  (xref--find-definitions identifier 'frame))

;;;###autoload
(defun xref-find-references (identifier)
  "Find references to the identifier at point.
With prefix argument, prompt for the identifier."
  (interactive (list (xref--read-identifier "Find references of: ")))
  (xref--show-xrefs identifier "references"
                    (funcall xref-find-function 'references identifier)
                    nil))

;;;###autoload
(defun xref-find-apropos (pattern)
  "Find all meaningful symbols that match PATTERN.
The argument has the same meaning as in `apropos'."
  (interactive (list (read-from-minibuffer
                      "Search for pattern (word list or regexp): ")))
  (require 'apropos)
  (xref--show-xrefs pattern "apropos"
                    (funcall xref-find-function 'apropos
                             (apropos-parse-pattern
                              (if (string-equal (regexp-quote pattern) pattern)
                                  ;; Split into words
                                  (or (split-string pattern "[ \t]+" t)
                                      (user-error "No word list given"))
                                pattern)))
                    nil))


;;; Key bindings

;;;###autoload (define-key esc-map "." #'xref-find-definitions)
;;;###autoload (define-key esc-map "," #'xref-pop-marker-stack)
;;;###autoload (define-key esc-map [?\C-.] #'xref-find-apropos)
;;;###autoload (define-key ctl-x-4-map "." #'xref-find-definitions-other-window)
;;;###autoload (define-key ctl-x-5-map "." #'xref-find-definitions-other-frame)


(provide 'xref)

;;; xref.el ends here
