(defsystem :foo
  :depends-on ((:version :baz "2.0.0")
               (:version :prem "1.0.0"))
  :components ((:file "foo")))
