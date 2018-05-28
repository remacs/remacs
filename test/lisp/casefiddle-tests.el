;;; Test `casefiddle-tests'.
(ert-deftest upcase ()
  (dolist (it '(("ONE" . "one")
                ("12 A 3C!" . "12 a 3C!")
                ("~!2 " . "~!2 ")
                ("" . "")))
    (should (equal (upcase (cdr it)) (car it)))))

(ert-deftest downcase ()
  (dolist (it '(("one" . "ONE")
                ("12 a 3c!" . "12 a 3C!")
                ("~!2 " . "~!2 ")
                ("" . "")))
    (should (equal (downcase (cdr it)) (car it)))))
