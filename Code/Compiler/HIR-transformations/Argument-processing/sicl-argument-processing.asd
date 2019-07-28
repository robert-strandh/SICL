(cl:in-package #:asdf-user)

(defsystem #:sicl-argument-processing
  :depends-on (:cleavir2-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "call-error")
   (:file "initialize-required-parameters")))
