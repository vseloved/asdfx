(defsystem :bar
  :depends-on ((:version :foo "2.0.0")
               :baz)
  :components ((:file "bar")))

