(cl:in-package #:asdf-user)

(defsystem :cleavir-kildall-type-inference
  :depends-on (:cleavir-kildall :cleavir-hir)
  :components
  ((:file "packages")
   (:file "type-descriptor" :depends-on ("packages"))
   (:file "values-descriptor"
    :depends-on ("type-descriptor" "packages"))
   (:file "function-descriptor"
    :depends-on ("values-descriptor" "packages"))
   (:file "specialization"
    :depends-on ("function-descriptor" "values-descriptor"
                 "type-descriptor" "packages"))
   (:file "transfer"
    :depends-on ("values-descriptor" "type-descriptor" "packages"))
   (:file "interface" :depends-on ("specialization" "packages"))))
