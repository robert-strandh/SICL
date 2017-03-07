(cl:in-package #:asdf-user)

(defsystem :cleavir-kildall-type-inference
  :depends-on (:cleavir-kildall :cleavir-hir)
  :components
  ((:file "packages")
   (:module "Descriptors"
    :depends-on ("packages")
    :components
    ((:file "lattice-descriptor")
     (:file "values-descriptor"
      :depends-on ("lattice-descriptor"))
     (:file "function-descriptor"
      :depends-on ("lattice-descriptor"))
     (:file "unboxed-descriptor")
     (:file "eql-descriptor")
     (:file "descriptor"
      :depends-on ("lattice-descriptor" "values-descriptor"
                   "function-descriptor" "unboxed-descriptor"
                   "eql-descriptor"))))
   (:file "specialization"
    :depends-on ("Descriptors" "packages"))
   (:file "transfer"
    :depends-on ("Descriptors" "specialization" "packages"))
   (:file "interface" :depends-on ("specialization" "packages"))))
