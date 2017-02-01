(defpackage :foo
  (:use :cl :baz :quux)
  (:export :foo))

(in-package :foo)

(defun foo ()
  (baz)
  (quux)
  (format *debug-io* "foo~%"))
