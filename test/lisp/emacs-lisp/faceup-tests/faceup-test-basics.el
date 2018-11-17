;;; faceup-test-basics.el --- Tests for the `faceup' package.

;; Copyright (C) 2014-2017 Free Software Foundation, Inc.

;; Author: Anders Lindgren
;; Keywords: languages, faces

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Basic tests for the `faceup' package.

;;; Code:

(require 'faceup)

(ert-deftest faceup-functions ()
  "Test primitive functions."
  (should (equal (faceup-normalize-face-property '()) '()))
  (should (equal (faceup-normalize-face-property 'a) '(a)))
  (should (equal (faceup-normalize-face-property '(a)) '(a)))
  (should (equal (faceup-normalize-face-property '(:x t)) '((:x t))))
  (should (equal (faceup-normalize-face-property '(:x t a)) '((:x t))))
  (should (equal (faceup-normalize-face-property '(:x t a b)) '((:x t))))
  (should (equal (faceup-normalize-face-property '(a :x t)) '(a (:x t))))
  (should (equal (faceup-normalize-face-property '(a b :x t))
                 '(a b (:x t))))

  (should (equal (faceup-normalize-face-property '(:x t :y nil))
                 '((:y nil) (:x t))))
  (should (equal (faceup-normalize-face-property '(:x t :y nil a))
                 '((:y nil) (:x t))))
  (should (equal (faceup-normalize-face-property '(:x t  :y nil a b))
                 '((:y nil) (:x t))))
  (should (equal (faceup-normalize-face-property '(a :x t :y nil))
                 '(a (:y nil) (:x t))))
  (should (equal (faceup-normalize-face-property '(a b :x t :y nil))
                 '(a b (:y nil) (:x t)))))


(ert-deftest faceup-markup ()
  "Test basic `faceup' features."
  ;; ----------
  ;; Basics
  (should (equal (faceup-markup-string "")     ""))
  (should (equal (faceup-markup-string "test") "test"))
  ;; ----------
  ;; Escaping
  (should (equal (faceup-markup-string "«") "««"))
  (should (equal (faceup-markup-string "«A«B«C«") "««A««B««C««"))
  (should (equal (faceup-markup-string "»") "«»"))
  (should (equal (faceup-markup-string "»A»B»C»") "«»A«»B«»C«»"))
  ;; ----------
  ;; Plain property.
  ;;
  ;;   UU
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 2 4 '(face underline) s)
    (should (equal (faceup-markup-string s) "AB«U:CD»EF")))
  ;; ----------
  ;; Plain property, full text
  ;;
  ;; UUUUUU
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 0 6 '(face underline) s)
    (should (equal (faceup-markup-string s) "«U:ABCDEF»")))
  ;; ----------
  ;; Anonymous face.
  ;;
  ;;   AA
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 2 4 '(face (:underline t)) s)
    (should (equal (faceup-markup-string s) "AB«:(:underline t):CD»EF")))
  ;; ----------
  ;; Anonymous face -- plist with two keys.
  ;;
  ;;   AA
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 2 4 '(face (:foo t :bar nil)) s)
    (should (equal (faceup-markup-string s)
                   "AB«:(:foo t):«:(:bar nil):CD»»EF")))
  ;; Ditto, with plist in list.
  (let ((s "ABCDEF"))
    (set-text-properties 2 4 '(face ((:foo t :bar nil))) s)
    (should (equal (faceup-markup-string s)
                   "AB«:(:foo t):«:(:bar nil):CD»»EF")))
  ;; ----------
  ;; Anonymous face -- Two plists.
  ;;
  ;;   AA
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 2 4 '(face ((:foo t) (:bar nil))) s)
    (should (equal (faceup-markup-string s)
                   "AB«:(:bar nil):«:(:foo t):CD»»EF")))
  ;; ----------
  ;; Anonymous face -- Nested.
  ;;
  ;;   AA
  ;;  IIII
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 1 2 '(face ((:foo t))) s)
    (set-text-properties 2 4 '(face ((:bar t) (:foo t))) s)
    (set-text-properties 4 5 '(face ((:foo t))) s)
    (should (equal (faceup-markup-string s)
                   "A«:(:foo t):B«:(:bar t):CD»E»F")))
  ;; ----------
  ;; Nested properties.
  ;;
  ;;   UU
  ;;  IIII
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 1 2 '(face italic) s)
    (set-text-properties 2 4 '(face (underline italic)) s)
    (set-text-properties 4 5 '(face italic) s)
    (should (equal (faceup-markup-string s) "A«I:B«U:CD»E»F")))
  ;; ----------
  ;; Overlapping, but not nesting, properties.
  ;;
  ;;   UUU
  ;;  III
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 1 2 '(face italic) s)
    (set-text-properties 2 4 '(face (underline italic)) s)
    (set-text-properties 4 5 '(face underline) s)
    (should (equal (faceup-markup-string s) "A«I:B«U:CD»»«U:E»F")))
  ;; ----------
  ;; Overlapping, but not nesting, properties.
  ;;
  ;;  III
  ;;   UUU
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 1 2 '(face italic) s)
    (set-text-properties 2 4 '(face (italic underline)) s)
    (set-text-properties 4 5 '(face underline) s)
    (should (equal (faceup-markup-string s) "A«I:B»«U:«I:CD»E»F")))
  ;; ----------
  ;; More than one face at the same location.
  ;;
  ;; The property to the front takes precedence, it is rendered as the
  ;; innermost parenthesis pair.
  (let ((s "ABCDEF"))
    (set-text-properties 2 4 '(face (underline italic)) s)
    (should (equal (faceup-markup-string s) "AB«I:«U:CD»»EF")))
  (let ((s "ABCDEF"))
    (set-text-properties 2 4 '(face (italic underline)) s)
    (should (equal (faceup-markup-string s) "AB«U:«I:CD»»EF")))
  ;; ----------
  ;; Equal ranges, full text.
  (let ((s "ABCDEF"))
    (set-text-properties 0 6 '(face (underline italic)) s)
    (should (equal (faceup-markup-string s) "«I:«U:ABCDEF»»")))
  ;; Ditto, with stray markup characters.
  (let ((s "AB«CD»EF"))
    (set-text-properties 0 8 '(face (underline italic)) s)
    (should (equal (faceup-markup-string s) "«I:«U:AB««CD«»EF»»")))

  ;; ----------
  ;; Multiple properties
  (let ((faceup-properties '(alpha beta gamma)))
    ;; One property.
    (let ((s "ABCDEF"))
      (set-text-properties 2 4 '(alpha (a l p h a)) s)
      (should (equal (faceup-markup-string s) "AB«(alpha):(a l p h a):CD»EF")))

    ;; Two properties, inner enclosed.
    (let ((s "ABCDEFGHIJ"))
      (set-text-properties 2 8 '(alpha (a l p h a)) s)
      (font-lock-append-text-property 4 6 'beta '(b e t a) s)
      (should (equal (faceup-markup-string s)
                     "AB«(alpha):(a l p h a):CD«(beta):(b e t a):EF»GH»IJ")))

    ;; Two properties, same end
    (let ((s "ABCDEFGH"))
      (set-text-properties 2 6 '(alpha (a)) s)
      (add-text-properties 4 6 '(beta (b)) s)
      (should
       (equal
        (faceup-markup-string s)
        "AB«(alpha):(a):CD«(beta):(b):EF»»GH")))

    ;; Two properties, overlap.
    (let ((s "ABCDEFGHIJ"))
      (set-text-properties 2 6 '(alpha (a)) s)
      (add-text-properties 4 8 '(beta (b)) s)
      (should
       (equal
        (faceup-markup-string s)
        "AB«(alpha):(a):CD«(beta):(b):EF»»«(beta):(b):GH»IJ")))))


(ert-deftest faceup-clean ()
  "Test the clean features of `faceup'."
  (should (equal (faceup-clean-string "")     ""))
  (should (equal (faceup-clean-string "test") "test"))
  (should (equal (faceup-clean-string "AB«U:CD»EF")         "ABCDEF"))
  (should (equal (faceup-clean-string "«U:ABCDEF»")         "ABCDEF"))
  (should (equal (faceup-clean-string "A«I:B«U:CD»E»F")     "ABCDEF"))
  (should (equal (faceup-clean-string "A«I:B«U:CD»»«U:E»F") "ABCDEF"))
  (should (equal (faceup-clean-string "AB«I:«U:CD»»EF")     "ABCDEF"))
  (should (equal (faceup-clean-string "«I:«U:ABCDEF»»")     "ABCDEF"))
  (should (equal (faceup-clean-string "«(foo)I:ABC»DEF")    "ABCDEF"))
  (should (equal (faceup-clean-string "«:(:foo t):ABC»DEF") "ABCDEF"))
  ;; Escaped markup characters.
  (should (equal (faceup-clean-string "««") "«"))
  (should (equal (faceup-clean-string "«»") "»"))
  (should (equal (faceup-clean-string "A«I:B«U:CD»«»»«U:E»F") "ABCD»EF")))


(ert-deftest faceup-render ()
  "Test the render features of `faceup'."
  (should (equal (faceup-render-string "")     ""))
  (should (equal (faceup-render-string "««") "«"))
  (should (equal (faceup-render-string "«»") "»"))
  (should (equal (faceup-render-string "A«I:B«U:CD»«»»«U:E»F") "ABCD»EF")))


(defvar faceup-test-resources-directory
  (concat (file-name-directory
           (substring (faceup-this-file-directory) 0 -1))
          "faceup-resources/")
  "The `faceup-resources' directory.")


(defvar faceup-test-this-file-directory nil
  "The result of `faceup-this-file-directory' in various contexts.

This is set by the file test support file
`faceup-test-this-file-directory.el'.")


(ert-deftest faceup-directory ()
  "Test `faceup-this-file-directory'."
  (let ((file (concat faceup-test-resources-directory
                      "faceup-test-this-file-directory.el"))
        (load-file-name nil))
    ;; Test normal load.
    (makunbound 'faceup-test-this-file-directory)
    (load file nil :nomessage)
    (should (equal faceup-test-this-file-directory
                   faceup-test-resources-directory))
    ;; Test `eval-buffer'.
    (makunbound 'faceup-test-this-file-directory)
    (save-excursion
      (find-file file)
      (eval-buffer))
    (should (equal faceup-test-this-file-directory
                   faceup-test-resources-directory))
    ;; Test `eval-defun'.
    (makunbound 'faceup-test-this-file-directory)
    (save-excursion
      (find-file file)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          ;; Note: In batch mode, this prints the result of the
          ;; evaluation.  Unfortunately, this is hard to fix.
          (eval-defun nil)
          (forward-sexp))))
    (should (equal faceup-test-this-file-directory
                   faceup-test-resources-directory))))

(provide 'faceup-test-basics)

;;; faceup-test-basics.el ends here
