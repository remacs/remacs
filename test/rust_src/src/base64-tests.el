(ert-deftest base64-tests-decode-german ()
  (let ((clear (encode-coding-string "Fußgängerübergänge" 'utf-8))
        (encoded "RnXDn2fDpG5nZXLDvGJlcmfDpG5nZQ=="))
    (should (string= clear (base64-decode-string encoded)))
    (should (string= encoded (base64-encode-string clear)))))

(ert-deftest base64-tests-decode-czech ()
  ;; In this test, the input is not explicitly coded in utf8
  (let ((clear "Dobrý den")
        (raw   "Dobr\375 den")
        (encoded "RG9icv0gZGVu"))
    (should (string= raw (base64-decode-string encoded)))
    (should (string= encoded (base64-encode-string clear)))))
