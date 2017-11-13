;;; Package -- helpers for Remacs porting

(defun remacs-helpers/make-rust-args-from-C-worker (input)
  "Transform function C arguments INPUT into Rust style arguments."
  (mapconcat (lambda (arg) (let* ((is-struct (lambda (s) (string= s "struct")))
                                  (pieces (cl-remove-if is-struct (split-string (string-trim arg) " ")))
                                  (name (s-append ":" (car (last pieces))))
                                  (rest (butlast pieces)))
                             (if (s-starts-with? "\*" name)
                                 (s-join " " (cons (s-chop-prefix "*" name)
                                                   (cons "*" rest)))
                               (s-join " " (cons name rest)))))
             (split-string input ",")
             ", "))

(defun remacs-helpers/make-rust-args-from-C (string &optional from to)
  "Transform provided STRING or region indicated by FROM and TO into Rust style arguments."
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'paragraph)) )
       (list nil (car bds) (cdr bds)) ) ) )

  (let* ((stringInputP (if string t nil))
         (input (if stringInputP string (buffer-substring-no-properties from to)))
         (output (remacs-helpers/make-rust-args-from-C-worker input)))
    (if stringInputP
        output
      (save-excursion
        (delete-region from to)
        (goto-char from)
        (insert output) )) ) )

(provide 'remacs-helpers)
