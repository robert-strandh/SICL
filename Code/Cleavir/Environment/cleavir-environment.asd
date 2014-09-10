(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-environment
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "query")
   (:file "augmentation-functions")
   (:file "default-augmentation-classes")
   (:file "default-info-methods")))
