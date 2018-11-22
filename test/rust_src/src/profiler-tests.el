;;; profiler-tests.el --- -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'profiler)

(ert-deftest test-profiler ()
  (should (not (profiler-memory-running-p)))
  (should (not (profiler-memory-log)))
  (should (not (profiler-memory-stop)))

  (should (profiler-memory-start))
  (should-error (profiler-memory-start))
  (should (profiler-memory-running-p))

  (should (profiler-memory-stop))
  (should (not (profiler-memory-running-p)))
  (should (profiler-memory-log)))

(provide 'profiler-tests)
;;; profiler-tests.el ends here
