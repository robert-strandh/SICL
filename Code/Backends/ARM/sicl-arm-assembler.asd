(asdf:defsystem :sicl-arm-assembler
  :depends-on (:split-sequence :sicl-compiler)
  :components
  ((:file "packages")
   (:file "instructions" :depends-on ("packages"))
   (:file "lir" :depends-on ("packages" "instructions"))))

