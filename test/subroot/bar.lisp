(defpackage :bar
  (:use :cl :foo :baz)
  (:export :bar))

(in-package :bar)

(defun bar ()
  (foo)
  (baz)
  (format *debug-io* "bar~%"))
