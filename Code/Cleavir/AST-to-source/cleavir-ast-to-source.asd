(cl:in-package #:asdf-user)

(defsystem :cleavir-ast-to-source
  :depends-on (:cleavir-ast :cleavir-primop)
  :serial t
  :components
  ((:file "packages")
   (:file "ast-to-source")
   (:file "fixnum")
   (:file "accessors")))
