(defpackage :foo
  (:use :cl :baz :prem)
  (:export :foo))

(in-package :foo)

(defun foo ()
  (baz)
  (prem)
  (format *debug-io* "foo 2~%"))
