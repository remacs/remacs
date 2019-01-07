(require 'ert)

(ert-deftest window-configuration-predicate-is-t ()
  (should (window-configuration-p (current-window-configuration))))

(ert-deftest window-configuration-predicate-is-nil ()
  (should-not (window-configuration-p nil))
  (should-not (window-configuration-p (selected-window)))
  (should-not (window-configuration-p (selected-frame))))

(ert-deftest window-configuration-frame-checks ()
  (should (eq (window-configuration-frame (current-window-configuration))
              (selected-frame))))

(ert-deftest window-configuration-comparison ()
  (should (compare-window-configurations (current-window-configuration)
                                         (current-window-configuration))))
