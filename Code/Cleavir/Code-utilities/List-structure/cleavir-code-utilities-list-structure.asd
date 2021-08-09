(cl:in-package #:asdf-user)

(defsystem #:cleavir-code-utilities-list-structure
  :depends-on (#:cleavir-code-utilities-base)
  :serial t
  :components
  ((:file "list-structure")))
