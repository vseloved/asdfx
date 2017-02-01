(defpackage :quux
  (:use :cl :prem :baz)
  (:export :quux))

(in-package :quux)

(defun quux ()
  (prem)
  (baz)
  (format *debug-io* "quux 1~%"))
