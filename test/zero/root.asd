(defsystem :root
  :depends-on (:foo
               :bar)
  :components ((:file "root")))
