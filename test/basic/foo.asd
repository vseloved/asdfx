(defsystem :foo
  :depends-on (:baz
               (:version :prem "1.0.0"))
  :components ((:file "foo")))
