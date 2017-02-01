(defpackage :bar
  (:use :cl :foo :prem)
  (:export :bar))

(in-package :bar)

(defun bar ()
  (foo)
  (prem)
  (format *debug-io* "bar~%"))
