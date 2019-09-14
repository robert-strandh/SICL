(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-to-mir
  :depends-on (#:cleavir2-hir
               #:cleavir2-mir)
  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "variables")
   (:file "cons")
   (:file "utilities")
   (:file "general-instance")
   (:file "array")
   (:file "boxing")
   (:file "eliminate-enclose-instructions")
   (:file "gather-enter-instructions")
   (:file "hir-to-mir")))
