(cl:in-package #:asdf-user)

(defsystem :x86-assembler
  :depends-on (:split-sequence)
  :serial t
  :components
  ((:file "packages")
   (:file "assembler")
   (:file "instruction-descriptors")
   (:file "instruction-database")
   (:file "print")
   (:file "test")))

    
