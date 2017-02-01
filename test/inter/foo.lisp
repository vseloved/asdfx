(defpackage :foo
  (:use :cl :quux)
  (:export :foo))

(in-package :foo)

(defun foo ()
  (quux)
  (format *debug-io* "foo~%"))
