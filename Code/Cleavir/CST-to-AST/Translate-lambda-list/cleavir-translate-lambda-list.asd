(cl:in-package #:asdf-user)

(defsystem #:cleavir-translate-lambda-list
  :depends-on ()
  :serial t
  :components
  ((:file "packages")
   (:file "translate-lambda-list")))
