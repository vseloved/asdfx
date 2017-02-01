(defsystem :foo
  :version "1.0.0"
  :depends-on ((:version :baz "1.0.0")
               (:version :prem "1.0.0"))
  :components ((:file "foo")))
