(cl:in-package #:asdf-user)

(defsystem :sicl-arm-assembler
  :depends-on (:split-sequence :sicl-compiler)
  :serial t
  :components
  ((:file "packages")
   (:file "emulator")
   (:file "instructions")
   (:file "lir")))
