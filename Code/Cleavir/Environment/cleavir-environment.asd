(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-environment
  :depends-on (:cleavir-internationalization)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "query")
   (:file "augmentation-functions")
   (:file "default-augmentation-classes")
   (:file "default-info-methods")
   (:file "eval")))
