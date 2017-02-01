(defsystem :root
  :depends-on ((:version :foo "1.0.0")
               :bar)
  :components ((:file "root")))
