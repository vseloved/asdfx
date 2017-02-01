(defpackage :baz
  (:use :cl)
  (:export :baz))

(in-package :baz)

(defun baz ()
  (format *debug-io* "baz 1~%"))
