(cl:in-package #:asdf-user)

(defsystem :cleavir-hir
  :depends-on (:cleavir-ir)
  :serial t
  :components
  ((:file "data")
   (:file "box-unbox-mixins")
   (:file "side-effect-mixins")
   (:file "general-purpose-instructions")
   (:file "fixnum-related-instructions")
   (:file "integer-related-instructions")
   (:file "float-related-instructions")
   (:file "cons-related-instructions")
   (:file "standard-object-related-instructions")
   (:file "array-related-instructions")
   (:file "multiple-value-related-instructions")
   (:file "environment-related-instructions")
   (:file "graphviz-drawing")))
