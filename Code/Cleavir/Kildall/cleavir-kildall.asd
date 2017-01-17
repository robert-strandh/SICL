(cl:in-package #:asdf-user)

(defsystem :cleavir-kildall
  :depends-on (:cleavir-hir)
  :components
  ((:file "packages")
   (:file "dictionary" :depends-on ("packages"))
   (:file "pool" :depends-on ("packages"))
   (:file "specialization" :depends-on ("packages"))
   (:file "kildall"
    :depends-on ("dictionary" "pool" "specialization""packages"))))
