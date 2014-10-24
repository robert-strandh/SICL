(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-ast-to-hir
  :depends-on (:cleavir-ast :cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "general")
   (:file "fixnum")
   (:file "float")
   (:file "cons")
   (:file "standard-object")
   (:file "array")))
