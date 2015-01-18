;;; eieio-generic.el --- CLOS-style generics for EIEIO  -*- lexical-binding:t -*-

;; Copyright (C) 1995-1996, 1998-2015 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: OO, lisp

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
;;
;; The "core" part of EIEIO is the implementation for the object
;; system (such as eieio-defclass, or eieio-defmethod) but not the
;; base classes for the object system, which are defined in EIEIO.
;;
;; See the commentary for eieio.el for more about EIEIO itself.

;;; Code:

(require 'eieio-core)
(declare-function child-of-class-p "eieio")

(put 'eieio--defalias 'byte-hunk-handler
     #'byte-compile-file-form-defalias) ;;(get 'defalias 'byte-hunk-handler)
(defun eieio--defalias (name body)
  "Like `defalias', but with less side-effects.
More specifically, it has no side-effects at all when the new function
definition is the same (`eq') as the old one."
  (while (and (fboundp name) (symbolp (symbol-function name)))
    ;; Follow aliases, so methods applied to obsolete aliases still work.
    (setq name (symbol-function name)))
  (unless (and (fboundp name)
               (eq (symbol-function name) body))
    (defalias name body)))

(defconst eieio--method-static 0 "Index into :static tag on a method.")
(defconst eieio--method-before 1 "Index into :before tag on a method.")
(defconst eieio--method-primary 2 "Index into :primary tag on a method.")
(defconst eieio--method-after 3 "Index into :after tag on a method.")
(defconst eieio--method-num-lists 4 "Number of indexes into methods vector in which groups of functions are kept.")
(defconst eieio--method-generic-before 4 "Index into generic :before tag on a method.")
(defconst eieio--method-generic-primary 5 "Index into generic :primary tag on a method.")
(defconst eieio--method-generic-after 6 "Index into generic :after tag on a method.")
(defconst eieio--method-num-slots 7 "Number of indexes into a method's vector.")

(defsubst eieio--specialized-key-to-generic-key (key)
  "Convert a specialized KEY into a generic method key."
  (cond ((eq key eieio--method-static) 0) ;; don't convert
	((< key eieio--method-num-lists) (+ key 3)) ;; The conversion
	(t key) ;; already generic.. maybe.
	))


(defsubst generic-p (method)
  "Return non-nil if symbol METHOD is a generic function.
Only methods have the symbol `eieio-method-hashtable' as a property
\(which contains a list of all bindings to that method type.)"
  (and (fboundp method) (get method 'eieio-method-hashtable)))

(defun eieio--generic-primary-only-p (method)
  "Return t if symbol METHOD is a generic function with only primary methods.
Only methods have the symbol `eieio-method-hashtable' as a property (which
contains a list of all bindings to that method type.)
Methods with only primary implementations are executed in an optimized way."
  (and (generic-p method)
       (let ((M (get method 'eieio-method-tree)))
	 (not (or (>= 0 (length (aref M eieio--method-primary)))
                  (aref M eieio--method-static)
                  (aref M eieio--method-before)
                  (aref M eieio--method-after)
                  (aref M eieio--method-generic-before)
                  (aref M eieio--method-generic-primary)
                  (aref M eieio--method-generic-after)))
         )))

(defun eieio--generic-primary-only-one-p (method)
  "Return t if symbol METHOD is a generic function with only primary methods.
Only methods have the symbol `eieio-method-hashtable' as a property (which
contains a list of all bindings to that method type.)
Methods with only primary implementations are executed in an optimized way."
  (and (generic-p method)
       (let ((M (get method 'eieio-method-tree)))
	 (not (or (/= 1 (length (aref M eieio--method-primary)))
                  (aref M eieio--method-static)
                  (aref M eieio--method-before)
                  (aref M eieio--method-after)
                  (aref M eieio--method-generic-before)
                  (aref M eieio--method-generic-primary)
                  (aref M eieio--method-generic-after)))
         )))

(defun eieio--defgeneric-init-form (method doc-string)
  "Form to use for the initial definition of a generic."
  (while (and (fboundp method) (symbolp (symbol-function method)))
    ;; Follow aliases, so methods applied to obsolete aliases still work.
    (setq method (symbol-function method)))

  (cond
   ((or (not (fboundp method))
        (eq 'autoload (car-safe (symbol-function method))))
    ;; Make sure the method tables are installed.
    (eieio--mt-install method)
    ;; Construct the actual body of this function.
    (if doc-string (put method 'function-documentation doc-string))
    (eieio--defgeneric-form method))
   ((generic-p method) (symbol-function method))           ;Leave it as-is.
   (t (error "You cannot create a generic/method over an existing symbol: %s"
             method))))

(defun eieio--defgeneric-form (method)
  "The lambda form that would be used as the function defined on METHOD.
All methods should call the same EIEIO function for dispatch.
DOC-STRING is the documentation attached to METHOD."
  (lambda (&rest local-args)
    (eieio--generic-call method local-args)))

(defun eieio--defgeneric-form-primary-only (method)
  "The lambda form that would be used as the function defined on METHOD.
All methods should call the same EIEIO function for dispatch.
DOC-STRING is the documentation attached to METHOD."
  (lambda (&rest local-args)
    (eieio--generic-call-primary-only method local-args)))

(defvar eieio--generic-call-arglst nil
  "When using `call-next-method', provides a context for parameters.")
(defvar eieio--generic-call-key nil
  "When using `call-next-method', provides a context for the current key.
Keys are a number representing :before, :primary, and :after methods.")
(defvar eieio--generic-call-next-method-list nil
  "When executing a PRIMARY or STATIC method, track the 'next-method'.
During executions, the list is first generated, then as each next method
is called, the next method is popped off the stack.")

(defun eieio--defgeneric-form-primary-only-one (method class impl)
  "The lambda form that would be used as the function defined on METHOD.
All methods should call the same EIEIO function for dispatch.
CLASS is the class symbol needed for private method access.
IMPL is the symbol holding the method implementation."
  (lambda (&rest local-args)
    ;; This is a cool cheat.  Usually we need to look up in the
    ;; method table to find out if there is a method or not.  We can
    ;; instead make that determination at load time when there is
    ;; only one method.  If the first arg is not a child of the class
    ;; of that one implementation, then clearly, there is no method def.
    (if (not (eieio-object-p (car local-args)))
        ;; Not an object.  Just signal.
        (signal 'no-method-definition
                (list method local-args))

      ;; We do have an object.  Make sure it is the right type.
      (if (not (child-of-class-p (eieio--object-class-object (car local-args))
                                 class))

          ;; If not the right kind of object, call no applicable
          (apply #'no-applicable-method (car local-args)
                 method local-args)

        ;; It is ok, do the call.
        ;; Fill in inter-call variables then evaluate the method.
        (let ((eieio--generic-call-next-method-list nil)
              (eieio--generic-call-key eieio--method-primary)
              (eieio--generic-call-arglst local-args)
              )
          (apply impl local-args))))))

(defun eieio-unbind-method-implementations (method)
  "Make the generic method METHOD have no implementations.
It will leave the original generic function in place,
but remove reference to all implementations of METHOD."
  (put method 'eieio-method-tree nil)
  (put method 'eieio-method-hashtable nil))

(defun eieio--method-optimize-primary (method)
  (when eieio-optimize-primary-methods-flag
    ;; Optimizing step:
    ;;
    ;; If this method, after this setup, only has primary methods, then
    ;; we can setup the generic that way.
    ;; Use `defalias' so as to interact properly with nadvice.el.
    (defalias method
      (if (eieio--generic-primary-only-p method)
          ;; If there is only one primary method, then we can go one more
          ;; optimization step.
          (if (eieio--generic-primary-only-one-p method)
              (let* ((M (get method 'eieio-method-tree))
                     (entry (car (aref M eieio--method-primary))))
                (eieio--defgeneric-form-primary-only-one
                 method (car entry) (cdr entry)))
            (eieio--defgeneric-form-primary-only method))
        (eieio--defgeneric-form method)))))

(defun eieio--defmethod (method kind argclass code)
  "Work part of the `defmethod' macro defining METHOD with ARGS."
  (let ((key
         ;; Find optional keys.
         (cond ((memq kind '(:BEFORE :before)) eieio--method-before)
               ((memq kind '(:AFTER :after)) eieio--method-after)
               ((memq kind '(:STATIC :static)) eieio--method-static)
               ((memq kind '(:PRIMARY :primary nil)) eieio--method-primary)
               ;; Primary key.
               ;; (t eieio--method-primary)
               (t (error "Unknown method kind %S" kind)))))

    (while (and (fboundp method) (symbolp (symbol-function method)))
      ;; Follow aliases, so methods applied to obsolete aliases still work.
      (setq method (symbol-function method)))

    ;; Make sure there is a generic (when called from defclass).
    (eieio--defalias
     method (eieio--defgeneric-init-form
             method (or (documentation code)
                        (format "Generically created method `%s'." method))))
    ;; Create symbol for property to bind to.  If the first arg is of
    ;; the form (varname vartype) and `vartype' is a class, then
    ;; that class will be the type symbol.  If not, then it will fall
    ;; under the type `primary' which is a non-specific calling of the
    ;; function.
    (if argclass
        (if (not (class-p argclass))    ;FIXME: Accept cl-defstructs!
            (error "Unknown class type %s in method parameters"
                   argclass))
      ;; Generics are higher.
      (setq key (eieio--specialized-key-to-generic-key key)))
    ;; Put this lambda into the symbol so we can find it.
    (eieio--mt-add method code key argclass)
    )

  (eieio--method-optimize-primary method)

  method)

(define-obsolete-variable-alias 'eieio-pre-method-execution-hooks
  'eieio-pre-method-execution-functions "24.3")
(defvar eieio-pre-method-execution-functions nil
  "Abnormal hook run just before an EIEIO method is executed.
The hook function must accept one argument, the list of forms
about to be executed.")

(defun eieio--generic-call (method args)
  "Call METHOD with ARGS.
ARGS provides the context on which implementation to use.
This should only be called from a generic function."
  ;; We must expand our arguments first as they are always
  ;; passed in as quoted symbols
  (let ((newargs nil) (mclass nil)  (lambdas nil) (tlambdas nil) (keys nil)
	(eieio--generic-call-arglst args)
	(firstarg nil)
	(primarymethodlist nil))
    ;; get a copy
    (setq newargs args
	  firstarg (car newargs))
    ;; Is the class passed in autoloaded?
    ;; Since class names are also constructors, they can be autoloaded
    ;; via the autoload command.  Check for this, and load them in.
    ;; It is ok if it doesn't turn out to be a class.  Probably want that
    ;; function loaded anyway.
    (if (and (symbolp firstarg)
	     (fboundp firstarg)
	     (autoloadp (symbol-function firstarg)))
	(autoload-do-load (symbol-function firstarg)))
    ;; Determine the class to use.
    (cond ((eieio-object-p firstarg)
	   (setq mclass (eieio--object-class-name firstarg)))
	  ((class-p firstarg)
	   (setq mclass firstarg))
	  )
    ;; Make sure the class is a valid class
    ;; mclass can be nil (meaning a generic for should be used.
    ;; mclass cannot have a value that is not a class, however.
    (unless (or (null mclass) (class-p mclass))
      (error "Cannot dispatch method %S on class %S"
	     method mclass)
      )
    ;; Now create a list in reverse order of all the calls we have
    ;; make in order to successfully do this right.  Rules:
    ;; 1) Only call static if this is a static method.
    ;; 2) Only call specifics if the definition allows for them.
    ;; 3) Call in order based on :before, :primary, and :after
    (when (eieio-object-p firstarg)
      ;; Non-static calls do all this stuff.

      ;; :after methods
      (setq tlambdas
	    (if mclass
		(eieio--mt-method-list method eieio--method-after mclass)
	      (list (eieio--generic-form method eieio--method-after nil)))
	    ;;(or (and mclass (eieio--generic-form method eieio--method-after mclass))
	    ;;	(eieio--generic-form method eieio--method-after nil))
	    )
      (setq lambdas (append tlambdas lambdas)
	    keys (append (make-list (length tlambdas) eieio--method-after) keys))

      ;; :primary methods
      (setq tlambdas
	    (or (and mclass (eieio--generic-form method eieio--method-primary mclass))
		(eieio--generic-form method eieio--method-primary nil)))
      (when tlambdas
	(setq lambdas (cons tlambdas lambdas)
	      keys (cons eieio--method-primary keys)
	      primarymethodlist
	      (eieio--mt-method-list method eieio--method-primary mclass)))

      ;; :before methods
      (setq tlambdas
	    (if mclass
		(eieio--mt-method-list method eieio--method-before mclass)
	      (list (eieio--generic-form method eieio--method-before nil)))
	    ;;(or (and mclass (eieio--generic-form method eieio--method-before mclass))
	    ;;	(eieio--generic-form method eieio--method-before nil))
	    )
      (setq lambdas (append tlambdas lambdas)
	    keys (append (make-list (length tlambdas) eieio--method-before) keys))
      )

    (if mclass
	;; For the case of a class,
	;; if there were no methods found, then there could be :static methods.
	(when (not lambdas)
	  (setq tlambdas
		(eieio--generic-form method eieio--method-static mclass))
	  (setq lambdas (cons tlambdas lambdas)
		keys (cons eieio--method-static keys)
		primarymethodlist  ;; Re-use even with bad name here
		(eieio--mt-method-list method eieio--method-static mclass)))
      ;; For the case of no class (ie - mclass == nil) then there may
      ;; be a primary method.
      (setq tlambdas
	    (eieio--generic-form method eieio--method-primary nil))
      (when tlambdas
	(setq lambdas (cons tlambdas lambdas)
	      keys (cons eieio--method-primary keys)
	      primarymethodlist
	      (eieio--mt-method-list method eieio--method-primary nil)))
      )

    (run-hook-with-args 'eieio-pre-method-execution-functions
			primarymethodlist)

    ;; Now loop through all occurrences forms which we must execute
    ;; (which are happily sorted now) and execute them all!
    (let ((rval nil) (lastval nil) (found nil))
      (while lambdas
	(if (car lambdas)
            (let* ((eieio--generic-call-key (car keys))
                   (has-return-val
                    (or (= eieio--generic-call-key eieio--method-primary)
                        (= eieio--generic-call-key eieio--method-static)))
                   (eieio--generic-call-next-method-list
                    ;; Use the cdr, as the first element is the fcn
                    ;; we are calling right now.
                    (when has-return-val (cdr primarymethodlist)))
                   )
              (setq found t)
              ;;(setq rval (apply (car (car lambdas)) newargs))
              (setq lastval (apply (car (car lambdas)) newargs))
              (when has-return-val
                (setq rval lastval))
              ))
	(setq lambdas (cdr lambdas)
	      keys (cdr keys)))
      (if (not found)
	  (if (eieio-object-p (car args))
	      (setq rval (apply #'no-applicable-method (car args) method args))
	    (signal
	     'no-method-definition
	     (list method args))))
      rval)))

(defun eieio--generic-call-primary-only (method args)
  "Call METHOD with ARGS for methods with only :PRIMARY implementations.
ARGS provides the context on which implementation to use.
This should only be called from a generic function.

This method is like `eieio--generic-call', but only
implementations in the :PRIMARY slot are queried.  After many
years of use, it appears that over 90% of methods in use
have :PRIMARY implementations only.  We can therefore optimize
for this common case to improve performance."
  ;; We must expand our arguments first as they are always
  ;; passed in as quoted symbols
  (let ((newargs nil) (mclass nil)  (lambdas nil)
	(eieio--generic-call-arglst args)
	(firstarg nil)
	(primarymethodlist nil)
	)
    ;; get a copy
    (setq newargs args
	  firstarg (car newargs))

    ;; Determine the class to use.
    (cond ((eieio-object-p firstarg)
	   (setq mclass (eieio--object-class-name firstarg)))
	  ((not firstarg)
	   (error "Method %s called on nil" method))
	  (t
	   (error "Primary-only method %s called on something not an object" method)))
    ;; Make sure the class is a valid class
    ;; mclass can be nil (meaning a generic for should be used.
    ;; mclass cannot have a value that is not a class, however.
    (when (null mclass)
      (error "Cannot dispatch method %S on class %S" method mclass)
      )

    ;; :primary methods
    (setq lambdas (eieio--generic-form method eieio--method-primary mclass))
    (setq primarymethodlist  ;; Re-use even with bad name here
	  (eieio--mt-method-list method eieio--method-primary mclass))

    ;; Now loop through all occurrences forms which we must execute
    ;; (which are happily sorted now) and execute them all!
    (let* ((rval nil) (lastval nil)
           (eieio--generic-call-key eieio--method-primary)
           ;; Use the cdr, as the first element is the fcn
           ;; we are calling right now.
           (eieio--generic-call-next-method-list (cdr primarymethodlist))
           )

      (if (or (not lambdas) (not (car lambdas)))

          ;; No methods found for this impl...
          (if (eieio-object-p (car args))
              (setq rval (apply #'no-applicable-method
                                (car args) method args))
            (signal
             'no-method-definition
             (list method args)))

        ;; Do the regular implementation here.

        (run-hook-with-args 'eieio-pre-method-execution-functions
                            lambdas)

        (setq lastval (apply (car lambdas) newargs))
        (setq rval lastval))

      rval)))

(defun eieio--mt-method-list (method key class)
  "Return an alist list of methods lambdas.
METHOD is the method name.
KEY represents either :before, or :after methods.
CLASS is the starting class to search from in the method tree.
If CLASS is nil, then an empty list of methods should be returned."
  ;; Note: eieiomt - the MT means MethodTree.  See more comments below
  ;; for the rest of the eieiomt methods.

  ;; Collect lambda expressions stored for the class and its parent
  ;; classes.
  (let (lambdas)
    (dolist (ancestor (eieio--class-precedence-list (eieio--class-v class)))
      ;; Lookup the form to use for the PRIMARY object for the next level
      (let ((tmpl (eieio--generic-form method key ancestor)))
	(when (and tmpl
		   (or (not lambdas)
		       ;; This prevents duplicates coming out of the
		       ;; class method optimizer.  Perhaps we should
		       ;; just not optimize before/afters?
		       (not (member tmpl lambdas))))
	  (push tmpl lambdas))))

    ;; Return collected lambda. For :after methods, return in current
    ;; order (most general class last); Otherwise, reverse order.
    (if (eq key eieio--method-after)
	lambdas
      (nreverse lambdas))))


;;;
;; eieio-method-tree : eieio--mt-
;;
;; Stored as eieio-method-tree in property list of a generic method
;;
;; (eieio-method-tree . [BEFORE PRIMARY AFTER
;;                       genericBEFORE genericPRIMARY genericAFTER])
;; and
;; (eieio-method-hashtable . [BEFORE PRIMARY AFTER
;;                          genericBEFORE genericPRIMARY genericAFTER])
;;    where the association is a vector.
;;    (aref 0  -- all static methods.
;;    (aref 1  -- all methods classified as :before
;;    (aref 2  -- all methods classified as :primary
;;    (aref 3  -- all methods classified as :after
;;    (aref 4  -- a generic classified as :before
;;    (aref 5  -- a generic classified as :primary
;;    (aref 6  -- a generic classified as :after
;;
(defvar eieio--mt--optimizing-hashtable nil
  "While mapping atoms, this contain the hashtable being optimized.")

(defun eieio--mt-install (method-name)
  "Install the method tree, and hashtable onto METHOD-NAME.
Do not do the work if they already exist."
  (unless (and (get method-name 'eieio-method-tree)
               (get method-name 'eieio-method-hashtable))
    (put method-name 'eieio-method-tree
         (make-vector eieio--method-num-slots nil))
    (let ((emto (put method-name 'eieio-method-hashtable
                     (make-vector eieio--method-num-slots nil))))
      (aset emto 0 (make-hash-table :test 'eq))
      (aset emto 1 (make-hash-table :test 'eq))
      (aset emto 2 (make-hash-table :test 'eq))
      (aset emto 3 (make-hash-table :test 'eq)))))

(defun eieio--mt-add (method-name method key class)
  "Add to METHOD-NAME the forms METHOD in a call position KEY for CLASS.
METHOD-NAME is the name created by a call to `defgeneric'.
METHOD are the forms for a given implementation.
KEY is an integer (see comment in eieio.el near this function) which
is associated with the :static :before :primary and :after tags.
It also indicates if CLASS is defined or not.
CLASS is the class this method is associated with."
  (if (or (> key eieio--method-num-slots) (< key 0))
      (error "eieio--mt-add: method key error!"))
  (let ((emtv (get method-name 'eieio-method-tree))
	(emto (get method-name 'eieio-method-hashtable)))
    ;; Make sure the method tables are available.
    (unless (and emtv emto)
      (error "Programmer error: eieio--mt-add"))
    ;; only add new cells on if it doesn't already exist!
    (if (assq class (aref emtv key))
	(setcdr (assq class (aref emtv key)) method)
      (aset emtv key (cons (cons class method) (aref emtv key))))
    ;; Add function definition into newly created symbol, and store
    ;; said symbol in the correct hashtable, otherwise use the
    ;; other array to keep this stuff.
    (if (< key eieio--method-num-lists)
        (puthash (eieio--class-v class) (list method) (aref emto key)))
    ;; Save the defmethod file location in a symbol property.
    (let ((fname (if load-in-progress
		     load-file-name
		   buffer-file-name)))
      (when fname
	(when (string-match "\\.elc\\'" fname)
	  (setq fname (substring fname 0 (1- (length fname)))))
	(cl-pushnew (list class fname) (get method-name 'method-locations)
                    :test 'equal)))
    ;; Now optimize the entire hashtable.
    (if (< key eieio--method-num-lists)
	(let ((eieio--mt--optimizing-hashtable (aref emto key)))
	  ;; @todo - Is this overkill?  Should we just clear the symbol?
	  (maphash #'eieio--mt--sym-optimize eieio--mt--optimizing-hashtable)))
    ))

(defun eieio--mt-next (class)
  "Return the next parent class for CLASS.
If CLASS is a superclass, return variable `eieio-default-superclass'.
If CLASS is variable `eieio-default-superclass' then return nil.
This is different from function `class-parent' as class parent returns
nil for superclasses.  This function performs no type checking!"
  ;; No type-checking because all calls are made from functions which
  ;; are safe and do checking for us.
  (or (eieio--class-parent (eieio--class-v class))
      (if (eq class 'eieio-default-superclass)
	  nil
	'(eieio-default-superclass))))

(defun eieio--mt--sym-optimize (class s)
  "Find the next class above S which has a function body for the optimizer."
  ;; Set the value to nil in case there is no nearest cell.
  (setcdr s nil)
  ;; Find the nearest cell that has a function body. If we find one,
  ;; we replace the nil from above.
  (catch 'done
    (dolist (ancestor
             (cl-rest (eieio--class-precedence-list class)))
      (let ((ov (gethash ancestor eieio--mt--optimizing-hashtable)))
        (when (car ov)
          (setcdr s ancestor) ;; store ov as our next symbol
          (throw 'done ancestor))))))

(defun eieio--generic-form (method key class)
 "Return the lambda form belonging to METHOD using KEY based upon CLASS.
If CLASS is not a class then use `generic' instead.  If class has
no form, but has a parent class, then trace to that parent class.
The first time a form is requested from a symbol, an optimized path
is memorized for faster future use."
 (if (symbolp class) (setq class (eieio--class-v class)))
 (let ((emto (aref (get method 'eieio-method-hashtable)
		   (if class key (eieio--specialized-key-to-generic-key key)))))
   (if (eieio--class-p class)
       ;; 1) find our symbol
       (let ((cs (gethash class emto)))
	 (unless cs
           ;; 2) If there isn't one, then make one.
           ;;    This can be slow since it only occurs once
           (puthash class (setq cs (list nil)) emto)
           ;; 2.1) Cache its nearest neighbor with a quick optimize
           ;;      which should only occur once for this call ever
           (let ((eieio--mt--optimizing-hashtable emto))
             (eieio--mt--sym-optimize class cs)))
	 ;; 3) If it's bound return this one.
	 (if (car cs)
	     (cons (car cs) class)
	   ;; 4) If it's not bound then this variable knows something
	   (if (cdr cs)
	       (progn
		 ;; 4.1) This symbol holds the next class in its value
		 (setq class (cdr cs)
		       cs (gethash class emto))
		 ;; 4.2) The optimizer should always have chosen a
		 ;;      function-symbol
		 ;;(if (car cs)
		 (cons (car cs) class)
                 ;;(error "EIEIO optimizer: erratic data loss!"))
		 )
             ;; There never will be a funcall...
             nil)))
     ;; for a generic call, what is a list, is the function body we want.
     (let ((emtl (aref (get method 'eieio-method-tree)
 		       (if class key (eieio--specialized-key-to-generic-key key)))))
       (if emtl
	   ;; The car of EMTL is supposed to be a class, which in this
	   ;; case is nil, so skip it.
	   (cons (cdr (car emtl)) nil)
	 nil)))))


(define-error 'no-method-definition "No method definition")
(define-error 'no-next-method "No next method")

;;; CLOS methods and generics
;;
(defmacro defgeneric (method args &optional doc-string)
  "Create a generic function METHOD.
DOC-STRING is the base documentation for this class.  A generic
function has no body, as its purpose is to decide which method body
is appropriate to use.  Uses `defmethod' to create methods, and calls
`defgeneric' for you.  With this implementation the ARGS are
currently ignored.  You can use `defgeneric' to apply specialized
top level documentation to a method."
  (declare (doc-string 3))
  `(eieio--defalias ',method
                    (eieio--defgeneric-init-form
                     ',method
                     ,(if doc-string (help-add-fundoc-usage doc-string args)))))

(defmacro defmethod (method &rest args)
  "Create a new METHOD through `defgeneric' with ARGS.

The optional second argument KEY is a specifier that
modifies how the method is called, including:
   :before  - Method will be called before the :primary
   :primary - The default if not specified
   :after   - Method will be called after the :primary
   :static  - First arg could be an object or class
The next argument is the ARGLIST.  The ARGLIST specifies the arguments
to the method as with `defun'.  The first argument can have a type
specifier, such as:
  ((VARNAME CLASS) ARG2 ...)
where VARNAME is the name of the local variable for the method being
created.  The CLASS is a class symbol for a class made with `defclass'.
A DOCSTRING comes after the ARGLIST, and is optional.
All the rest of the args are the BODY of the method.  A method will
return the value of the last form in the BODY.

Summary:

 (defmethod mymethod [:before | :primary | :after | :static]
                     ((typearg class-name) arg2 &optional opt &rest rest)
    \"doc-string\"
     body)"
  (declare (doc-string 3)
           (debug
            (&define                    ; this means we are defining something
             [&or name ("setf" :name setf name)]
             ;; ^^ This is the methods symbol
             [ &optional symbolp ]                ; this is key :before etc
             list                                 ; arguments
             [ &optional stringp ]                ; documentation string
             def-body                             ; part to be debugged
             )))
  (let* ((key (if (keywordp (car args)) (pop args)))
	 (params (car args))
	 (arg1 (car params))
         (fargs (if (consp arg1)
                   (cons (car arg1) (cdr params))
                 params))
	 (class (if (consp arg1) (nth 1 arg1)))
         (code `(lambda ,fargs ,@(cdr args))))
    `(progn
       ;; Make sure there is a generic and the byte-compiler sees it.
       (defgeneric ,method ,args)
       (eieio--defmethod ',method ',key ',class #',code))))



;;;
;; Method Calling Functions

(defun next-method-p ()
  "Return non-nil if there is a next method.
Returns a list of lambda expressions which is the `next-method'
order."
  eieio--generic-call-next-method-list)

(defun call-next-method (&rest replacement-args)
  "Call the superclass method from a subclass method.
The superclass method is specified in the current method list,
and is called the next method.

If REPLACEMENT-ARGS is non-nil, then use them instead of
`eieio--generic-call-arglst'.  The generic arg list are the
arguments passed in at the top level.

Use `next-method-p' to find out if there is a next method to call."
  (if (and (/= eieio--generic-call-key eieio--method-primary)
	   (/= eieio--generic-call-key eieio--method-static))
      (error "Cannot `call-next-method' except in :primary or :static methods")
    )
  (let ((newargs (or replacement-args eieio--generic-call-arglst))
	(next (car eieio--generic-call-next-method-list))
	)
    (if (not (and next (car next)))
	(apply #'no-next-method newargs)
      (let* ((eieio--generic-call-next-method-list
	      (cdr eieio--generic-call-next-method-list))
	     (eieio--generic-call-arglst newargs)
	     (fcn (car next))
	     )
        (apply fcn newargs)) )))

(defgeneric no-applicable-method (object method &rest args)
  "Called if there are no implementations for OBJECT in METHOD.")

(defmethod no-applicable-method (object method &rest _args)
  "Called if there are no implementations for OBJECT in METHOD.
OBJECT is the object which has no method implementation.
ARGS are the arguments that were passed to METHOD.

Implement this for a class to block this signal.  The return
value becomes the return value of the original method call."
  (signal 'no-method-definition (list method object)))

(defgeneric no-next-method (object &rest args)
"Called from `call-next-method' when no additional methods are available.")

(defmethod no-next-method (object &rest args)
  "Called from `call-next-method' when no additional methods are available.
OBJECT is othe object being called on `call-next-method'.
ARGS are the arguments it is called by.
This method signals `no-next-method' by default.  Override this
method to not throw an error, and its return value becomes the
return value of `call-next-method'."
  (signal 'no-next-method (list object args)))

(add-hook 'help-fns-describe-function-functions 'eieio--help-generic)
(defun eieio--help-generic (generic)
  "Describe GENERIC if it is a generic function."
  (when (and (symbolp generic) (generic-p generic))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward " in `.+'.$" nil t)
	(replace-match ".")))
    (save-excursion
      (insert "\n\nThis is a generic function"
	      (cond
	       ((and (eieio--generic-primary-only-p generic)
		     (eieio--generic-primary-only-one-p generic))
		" with only one primary method")
	       ((eieio--generic-primary-only-p generic)
		" with only primary methods")
	       (t ""))
	      ".\n\n")
      (insert (propertize "Implementations:\n\n" 'face 'bold))
      (let ((i 4)
	    (prefix [ ":STATIC" ":BEFORE" ":PRIMARY" ":AFTER" ] ))
	;; Loop over fanciful generics
	(while (< i 7)
	  (let ((gm (aref (get generic 'eieio-method-tree) i)))
	    (when gm
	      (insert "Generic "
		      (aref prefix (- i 3))
		      "\n"
		      (or (nth 2 gm) "Undocumented")
		      "\n\n")))
	  (setq i (1+ i)))
	(setq i 0)
	;; Loop over defined class-specific methods
	(while (< i 4)
	  (let* ((gm (reverse (aref (get generic 'eieio-method-tree) i)))
		 cname location)
	    (while gm
	      (setq cname (caar gm))
	      (insert "`")
	      (help-insert-xref-button (symbol-name cname)
				       'help-variable cname)
	      (insert "' " (aref prefix i) " ")
	      ;; argument list
	      (let* ((func (cdr (car gm)))
		     (arglst (help-function-arglist func)))
		(prin1 arglst (current-buffer)))
	      (insert "\n"
		      (or (documentation (cdr (car gm)))
			  "Undocumented"))
	      ;; Print file location if available
	      (when (and (setq location (get generic 'method-locations))
			 (setq location (assoc cname location)))
		(setq location (cadr location))
		(insert "\n\nDefined in `")
		(help-insert-xref-button
		 (file-name-nondirectory location)
		 'eieio-method-def cname generic location)
		(insert "'\n"))
	      (setq gm (cdr gm))
	      (insert "\n")))
	  (setq i (1+ i)))))))

;;; Obsolete backward compatibility functions.
;; Needed to run byte-code compiled with the EIEIO of Emacs-23.

(defun eieio-defmethod (method args)
  "Obsolete work part of an old version of the `defmethod' macro."
  (let ((key nil) (body nil) (firstarg nil) (argfix nil) (argclass nil) loopa)
    ;; find optional keys
    (setq key
	  (cond ((memq (car args) '(:BEFORE :before))
		 (setq args (cdr args))
		 eieio--method-before)
		((memq (car args) '(:AFTER :after))
		 (setq args (cdr args))
		 eieio--method-after)
		((memq (car args) '(:STATIC :static))
		 (setq args (cdr args))
		 eieio--method-static)
		((memq (car args) '(:PRIMARY :primary))
		 (setq args (cdr args))
		 eieio--method-primary)
		;; Primary key.
		(t eieio--method-primary)))
    ;; Get body, and fix contents of args to be the arguments of the fn.
    (setq body (cdr args)
	  args (car args))
    (setq loopa args)
    ;; Create a fixed version of the arguments.
    (while loopa
      (setq argfix (cons (if (listp (car loopa)) (car (car loopa)) (car loopa))
			 argfix))
      (setq loopa (cdr loopa)))
    ;; Make sure there is a generic.
    (eieio-defgeneric
     method
     (if (stringp (car body))
	 (car body) (format "Generically created method `%s'." method)))
    ;; create symbol for property to bind to.  If the first arg is of
    ;; the form (varname vartype) and `vartype' is a class, then
    ;; that class will be the type symbol.  If not, then it will fall
    ;; under the type `primary' which is a non-specific calling of the
    ;; function.
    (setq firstarg (car args))
    (if (listp firstarg)
	(progn
	  (setq argclass  (nth 1 firstarg))
	  (if (not (class-p argclass))
	      (error "Unknown class type %s in method parameters"
		     (nth 1 firstarg))))
      ;; Generics are higher.
      (setq key (eieio--specialized-key-to-generic-key key)))
    ;; Put this lambda into the symbol so we can find it.
    (if (byte-code-function-p (car-safe body))
	(eieio--mt-add method (car-safe body) key argclass)
      (eieio--mt-add method (append (list 'lambda (reverse argfix)) body)
		   key argclass))
    )

  (eieio--method-optimize-primary method)

  method)
(make-obsolete 'eieio-defmethod 'eieio--defmethod "24.1")

(defun eieio-defgeneric (method doc-string)
  "Obsolete work part of an old version of the `defgeneric' macro."
  (if (and (fboundp method) (not (generic-p method))
	   (or (byte-code-function-p (symbol-function method))
	       (not (eq 'autoload (car (symbol-function method)))))
	   )
      (error "You cannot create a generic/method over an existing symbol: %s"
	     method))
  ;; Don't do this over and over.
  (unless (fboundp 'method)
    ;; This defun tells emacs where the first definition of this
    ;; method is defined.
    `(defun ,method nil)
    ;; Make sure the method tables are installed.
    (eieio--mt-install method)
    ;; Apply the actual body of this function.
    (put method 'function-documentation doc-string)
    (fset method (eieio--defgeneric-form method))
    ;; Return the method
    'method))
(make-obsolete 'eieio-defgeneric nil "24.1")

(provide 'eieio-generic)

;;; eieio-generic.el ends here
