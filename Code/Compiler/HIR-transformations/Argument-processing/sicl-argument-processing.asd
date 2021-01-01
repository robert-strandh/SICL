(cl:in-package #:asdf-user)

(defsystem #:sicl-argument-processing
  :depends-on (:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "call-error")
   (:file "check-minimum-argument-count")
   (:file "check-maximum-argument-count")
   (:file "initialize-required-parameters")
   (:file "initialize-optional-parameters")
   (:file "initialize-keyword-parameters-to-nil")
   (:file "create-rest-parameter")
   (:file "initialize-keyword-parameters")
   (:file "process-parameters")))
