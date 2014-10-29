(cl:in-package #:common-lisp-user)

(asdf:defsystem :x86-assembler
  :depends-on (:split-sequence)
  :components
  ((:file "packages")
   (:file "assembler"
    :depends-on ("packages"))
   (:file "instruction-descriptors"
    :depends-on ("packages"))
   (:file "instruction-database"
    :depends-on ("packages" "instruction-descriptors"))
   (:file "print"
    :depends-on ("packages" "assembler"))
   (:file "test"
    :depends-on ("packages" "assembler" "instruction-descriptors"))))

    
