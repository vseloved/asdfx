(defpackage :bhav
  (:use :cl :prem)
  (:export :bhav))

(in-package :bhav)

(defun bhav ()
  (prem)
  (format *debug-io* "bhav~%"))
