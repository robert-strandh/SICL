(cl:in-package #:asdf-user)

(defsystem #:cleavir-code-utilities-separate-body
  :depends-on (#:cleavir-code-utilities-base)
  :serial t
  :components
  ((:file "separate-body")
   (:file "conditions")
   (:file "condition-reporters-english")))
