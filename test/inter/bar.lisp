(defpackage :bar
  (:use :cl :quux)
  (:export :bar))

(in-package :bar)

(defun bar ()
  (quux)
  (format *debug-io* "bar~%"))
