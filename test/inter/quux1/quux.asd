(defsystem :quux
  :version "1.0.0"
  :depends-on ((:version :prem "1.0.0")
               (:version :baz "1.0.0"))
  :components ((:file "quux")))

