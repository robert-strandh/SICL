(cl:in-package #:asdf-user)

(defsystem :cleavir-kildall-escape
  :depends-on (:cleavir-kildall :cleavir-compilation-policy
               :cleavir-liveness :cleavir-hir :acclimation)
  :components
  ((:file "packages")
   (:file "conditions" :depends-on ("packages"))
   (:file "condition-reporters-english" :depends-on ("packages"
                                                     "conditions"))
   (:file "specialization" :depends-on ("packages"))
   (:file "indicator" :depends-on ("specialization" "packages"))
   (:file "transfer" :depends-on ("specialization" "indicator"
                                  "packages"))
   (:file "interface" :depends-on ("indicator" "packages"))))
