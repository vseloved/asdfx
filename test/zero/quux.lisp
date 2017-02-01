(defpackage :quux
  (:use :cl)
  (:export :quux))

(in-package :quux)

(defun quux ()
  (format *debug-io* "quux~%"))
