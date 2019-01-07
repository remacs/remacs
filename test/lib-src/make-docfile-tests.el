(require 'ert)

(defun validate-fundoc (func expected)
  (should (equal
    (car (help-split-fundoc (documentation func) func))
    expected)))

(ert-deftest make-docfile-no-args-defun ()
  (validate-fundoc 'current-buffer "(current-buffer)"))

(ert-deftest make-docfile-optional-args-defun ()
  (validate-fundoc 'buffer-list "(buffer-list &optional FRAME)")
  (validate-fundoc 'set-frame-selected-window
                   "(set-frame-selected-window FRAME WINDOW &optional NORECORD)"))

(ert-deftest make-docfile-many-defun ()
  (validate-fundoc 'funcall-interactively "(funcall-interactively FUNCTION &rest ARGUMENTS)")
  (validate-fundoc 'message "(message FORMAT-STRING &rest ARGS)"))

(ert-deftest make-docfile-simple-defun ()
  (validate-fundoc 'set-buffer "(set-buffer BUFFER-OR-NAME)")
  (validate-fundoc 'buffer-local-value "(buffer-local-value VARIABLE BUFFER)"))

(provide 'make-docfile-tests)
