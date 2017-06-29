(cl:in-package #:asdf-user)

(defsystem :cleavir-escape
  :depends-on (:cleavir-compilation-policy :cleavir-hir
               :cleavir-liveness)
  :components
  ((:file "packages")
   (:file "specialization" :depends-on ("packages"))
   (:file "indicator" :depends-on ("packages"))
   (:file "transfer" :depends-on ("indicator" "specialization"
                                              "packages"))
   (:file "interface" :depends-on ("specialization" "packages"))))
