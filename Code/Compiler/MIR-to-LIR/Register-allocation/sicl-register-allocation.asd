(cl:in-package #:asdf-user)

(defsystem #:sicl-register-allocation
  :depends-on (#:cleavir-lir
               #:cleavir-mir
               #:cleavir-hir
               #:sicl-register-arrangement)
  :serial t
  :components
  ((:file "packages")
   (:file "preprocess-instructions")
   (:file "location-information")
   (:file "find-back-arcs")
   (:file "pool")
   (:file "work-list")
   (:file "estimated-distance-to-use")
   (:file "registers")
   (:file "stack-map")
   (:file "arrangement")
   (:file "spill-unspill")
   (:file "allocate-register")
   (:file "adapt-arrangement")
   (:file "allocate-registers-for-instructions")
   (:file "introduce-registers")
   (:file "register-allocation")))
