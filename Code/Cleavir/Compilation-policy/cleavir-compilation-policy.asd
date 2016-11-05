(cl:in-package #:asdf-user)

(defsystem :cleavir-compilation-policy
  :depends-on (:cleavir-environment :acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-english")
   (:file "policy")
   (:file "define-policy")
   (:file "optimize")
   (:file "compute")))
