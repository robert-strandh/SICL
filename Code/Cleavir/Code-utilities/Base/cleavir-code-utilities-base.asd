(cl:in-package #:asdf-user)

(defsystem #:cleavir-code-utilities-base
  :depends-on (#:acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")))
