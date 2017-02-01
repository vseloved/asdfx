(defpackage :root
  (:use :cl :foo :bar))

(in-package :root)

(eval-when (:load-toplevel)
  (foo)
  (bar)
  (format *debug-io* "root~%"))
