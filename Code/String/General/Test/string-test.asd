(cl:in-package #:common-lisp-user)

(asdf:defsystem :string-test
  :components
  ((:file "packages")
   (:file "string"
    :depends-on ("packages"))
   (:file "string-test"
    :depends-on ("string"))))

   
