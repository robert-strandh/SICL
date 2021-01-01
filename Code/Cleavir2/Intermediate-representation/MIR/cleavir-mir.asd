(cl:in-package #:asdf-user)

(defsystem :cleavir-mir
  :depends-on (:cleavir-ir)
  :serial t
  :components
  ((:file "utilities")
   (:file "data")
   (:file "memory-access-instructions")
   (:file "integer-arithmetic-instructions")
   (:file "shift-instructions")
   (:file "bitwise-instructions")
   (:file "sign-extend-instruction")
   (:file "conditions")))
