(defsystem :bar
  :depends-on ((:version :foo "2.0.0")
               (:version :prem "3.0.0"))
  :components ((:file "bar")))

