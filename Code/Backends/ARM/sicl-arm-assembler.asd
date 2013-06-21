(asdf:defsystem :sicl-arm-assembler
  :depends-on (:split-sequence)
  :components
  ((:file "packages")
   (:file "instructions" :depends-on ("packages"))
   (:file "assembler" :depends-on ("packages" "instructions"))))
