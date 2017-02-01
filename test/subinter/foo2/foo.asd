(defsystem :foo
  :version "2.0.0"
  :depends-on ((:version :baz "2.0.0")
               (:version :prem "2.0.0"))
  :components ((:file "foo")))
