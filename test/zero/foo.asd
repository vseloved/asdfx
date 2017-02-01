(defsystem :foo
  :depends-on (:baz
               :quux)
  :components ((:file "foo")))
