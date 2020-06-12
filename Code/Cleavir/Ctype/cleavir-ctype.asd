(cl:in-package #:asdf-user)

(defsystem :cleavir-ctype
  :depends-on ()
  :components
  ((:file "packages")
   (:file "generic-functions" :depends-on ("packages"))
   (:file "other-functions" :depends-on ("packages"))
   (:file "default" :depends-on ("generic-functions"
                                 "packages"))))
