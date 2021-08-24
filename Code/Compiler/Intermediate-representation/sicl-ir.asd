(cl:in-package #:asdf-user)

(defsystem #:sicl-ir
  :depends-on (:cleavir-ir)
  :serial t
  :components
  ((:file "packages")
   (:file "load-constant-instruction")
   (:file "breakpoint-instruction")
   (:file "named-call-instruction")
   (:file "dynamic-environment")
   (:file "stack")
   (:file "x86-addressing")
   (:file "standard-object-instructions")))
