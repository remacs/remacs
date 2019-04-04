;;; unit tests for rust_src/src/window.rs      -*- lexical-binding: t; -*-

;; Copyright 2017 Free Software Foundation, Inc.

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

(require 'ert)

(ert-deftest windown-tests--window-top-line ()
  "Tests correct functionality of (window-top-line function) with split window."
  (let ((current (window-top-line)))
    (progn
      (split-window-vertically)
      (other-window 1)
      (should (< current (window-top-line))))))

(ert-deftest window-hscroll ()
  "Effectively tests both `window-hscroll' (the getter) and
  `set-window-hscroll' (the setter)."

  ;; Can we change hscroll on this window?
  (set-window-hscroll nil 42)
  (should (= (window-hscroll) 42))
  (should (= (window-hscroll nil) 42))
  (set-window-hscroll nil 7)
  (should (= (window-hscroll) 7))
  (should (= (window-hscroll nil) 7))

  ;; Can we correctly operate on a different window?
  (let ((initial-window (selected-window))
        (other-window (split-window)))
    (set-window-hscroll initial-window 1337)
    (set-window-hscroll other-window 4711)
    (should (= (window-hscroll) 1337))
    (should (= (window-hscroll initial-window) 1337))
    (should (= (window-hscroll other-window) 4711))
    (select-window other-window)
    (should (= (window-hscroll) 4711))
    (should (= (window-hscroll initial-window) 1337))
    (should (= (window-hscroll other-window) 4711))
    (delete-window other-window)
    ))
