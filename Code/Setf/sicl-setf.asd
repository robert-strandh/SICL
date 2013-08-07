(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-setf
  :depends-on (:sicl-code-utilities)
  :components
  ((:file "packages")
   (:file "database" :depends-on ("packages"))
   (:file "setf" :depends-on ("packages" "database"))))
