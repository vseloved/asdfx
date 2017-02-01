(defpackage :bar
  (:use :cl :prem :baz)
  (:export :bar))

(in-package :bar)

(defun bar ()
  (prem)
  (baz)
  (format *debug-io* "bar~%"))
