(defpackage :root
  (:use :cl :foo :bar :bhav))

(in-package :root)

(eval-when (:load-toplevel)
  (foo)
  (bar)
  (bhav)
  (format *debug-io* "root~%"))
