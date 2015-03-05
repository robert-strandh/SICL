(cl:in-package #:asdf-user)

(defsystem :sicl-arm-assembler
  :depends-on (:split-sequence :sicl-compiler)
  :components
  ((:file "packages")
   (:file "emulator" :depends-on ("packages"))
   (:file "instructions" :depends-on ("packages" "emulator"))
   (:file "lir" :depends-on ("packages" "instructions"))))

