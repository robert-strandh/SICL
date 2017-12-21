(asdf:defsystem :cleavir-value-numbering
  :depends-on (:cleavir-kildall :cleavir-liveness :cleavir-hir)
  :components
  ((:file "packages")
   (:file "specialization" :depends-on ("packages"))
   (:file "transfer" :depends-on ("specialization" "packages"))
   (:file "interface" :depends-on ("specialization" "packages"))))
