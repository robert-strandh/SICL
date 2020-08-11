(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-to-mir
  :depends-on (#:cleavir2-hir
               #:cleavir2-mir
               #:sicl-compiler-base)
  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "expand-funcall-instructions")
   (:file "cons")
   (:file "utilities")
   (:file "standard-object")
   (:file "array")
   (:file "boxing")
   (:file "fixnum")
   (:file "character")
   (:file "augment-catch-instruction")
   (:file "augment-bind-instruction")
   (:file "augment-initialize-values-instruction")
   (:file "eliminate-enclose-instructions")
   (:file "gather-enter-instructions")
   (:file "hir-to-mir")))
