(cl:in-package #:asdf-user)

(defsystem cleavir-literals
  :serial t
  :components
  ((:file "packages")
   (:file "variables")
   (:file "macros")
   (:file "generic-functions")
   (:file "make-load-form-using-client")
   (:file "load-time-literal")))
