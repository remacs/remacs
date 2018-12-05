;;; dispnew-tests.el ---                             -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(defun ding-with-args ()
  "These are just some of my favorite dings."
  (ding)
  (ding t)
  (ding nil)
  (ding 6)
  (ding "arg")
  (ding #'ding))

(ert-deftest test-ding ()
  (let ((visible-bell t))
    (ding-with-args))
  (let ((visible-bell nil))
    (ding-with-args)))

(ert-deftest test-redisplay ()
  (redisplay)
  (redisplay t)
  (redisplay 'force))

(provide 'dispnew-tests)
;;; dispnew-tests.el ends here
