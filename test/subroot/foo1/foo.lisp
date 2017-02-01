(defpackage :foo
  (:use :cl :bhav :prem)
  (:export :foo))

(in-package :foo)

(defun foo ()
  (bhav)
  (prem)
  (format *debug-io* "foo 1~%"))
