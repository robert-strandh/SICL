(cl:in-package #:asdf-user)

(defsystem #:cleavir-def-use-chains
  :depends-on (#:cleavir-reaching-definitions)
  :serial t
 :components
  ((:file "packages")
   (:file "def-use-chains")))
