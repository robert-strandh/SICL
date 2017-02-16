(cl:in-package #:asdf-user)

(defsystem :cleavir-kildall-escape
  :depends-on (:cleavir-kildall :cleavir-hir)
  :components
  ((:file "packages")
   (:file "specialization" :depends-on ("packages"))
   (:file "indicator" :depends-on ("packages"))
   (:file "pool" :depends-on ("specialization" "indicator"
                                               "packages"))
   (:file "transfer" :depends-on ("specialization" "indicator"
                                  "pool" "packages"))
   (:file "interface" :depends-on ("pool" "indicator" "packages"))))
