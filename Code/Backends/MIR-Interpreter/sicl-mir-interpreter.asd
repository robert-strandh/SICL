(asdf:defsystem :sicl-mir-interpreter
  :depends-on (:sicl-compiler)
  :components
  ((:file "packages" :depends-on ())
   (:file "mir-interpreter" :depends-on ("packages"))))
