(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-ast-to-mir
  :depends-on (:cleavir-ast :cleavir-mir)
  :serial t
  :components
  ((:file "packages")
   (:file "general")
   (:file "fixnum")
   (:file "accessors")))
