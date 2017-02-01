(defpackage :bar
  (:use :cl :prem :bhav)
  (:export :bar))

(in-package :bar)

(defun bar ()
  (prem)
  (bhav)
  (format *debug-io* "bar~%"))
