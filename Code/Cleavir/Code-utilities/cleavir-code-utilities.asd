(cl:in-package #:asdf-user)

(defsystem #:cleavir-code-utilities
  :depends-on (#:cleavir-code-utilities-base)
  :serial t
  :components
  ((:file "conditions")
   (:file "condition-reporters-english")
   (:file "argcount")
   (:file "form")
   (:file "list-structure")
   (:file "declarations")
   (:file "lambda-lists")
   (:file "destructuring")))
