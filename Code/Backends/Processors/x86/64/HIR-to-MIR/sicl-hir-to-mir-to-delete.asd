(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-to-mir
  :depends-on (:cleavir-hir-to-mir
	       :cleavir-processor-x86-64
               :sicl-client
	       :sicl-target-sicl)
  :serial t
  :components
  ((:file "packages")
   (:file "general")
   (:file "cons")
   (:file "integer")
   (:file "general-instance")
   (:file "environment-related-instructions")
   (:file "hir-to-mir")))
