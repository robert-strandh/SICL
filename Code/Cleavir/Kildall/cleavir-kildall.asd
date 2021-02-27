(cl:in-package #:asdf-user)

(defsystem :cleavir-kildall
  :depends-on (:cleavir-hir)
  :components
  ((:file "packages")
   (:file "pool" :depends-on ("packages"))
   (:file "dictionary" :depends-on ("kildall" "pool" "packages"))
   (:file "work-list" :depends-on ("packages"))
   (:file "kildall" :depends-on ("packages"))
   (:file "map-pool" :depends-on ("dictionary" "work-list"
                                               "packages"))
   (:file "iterate" :depends-on ("kildall" "work-list" "packages"))
   (:file "initial-work" :depends-on ("work-list" "kildall"
                                                  "packages"))
   (:file "alist-pool" :depends-on ("map-pool" "packages"))
   (:file "bitset" :depends-on ("kildall" "map-pool" "packages"))
   (:file "interfunction"
    :depends-on ("kildall" "packages"))))
