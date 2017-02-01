(defsystem :bar
  :depends-on ((:version :prem "2.0.0")
               (:version :baz "1.0.0"))
  :components ((:file "bar")))

