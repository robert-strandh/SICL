(cl:in-package #:asdf-user)

(defsystem #:cleavir-code-utilities
  :depends-on (#:cleavir-code-utilities-base
               #:cleavir-code-utilities-list-structure
               #:cleavir-code-utilities-separate-body)
  :serial t
  :components
  ((:file "conditions")
   (:file "condition-reporters-english")
   (:file "declarations")
   (:file "lambda-list")
   (:file "lambda-list-congruency")
   (:file "lambda-list-variables")
   (:file "destructuring")))
