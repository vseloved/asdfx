(defsystem :quux
  :version "2.0.0"
  :depends-on ((:version :prem "2.0.0")
               (:version :baz "2.0.0"))
  :components ((:file "quux")))

