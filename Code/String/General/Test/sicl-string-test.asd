(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-string-test
  :components
  ((:file "packages")
   (:file "string"
    :depends-on ("packages"))
   (:file "string-test"
    :depends-on ("string"))))

   
