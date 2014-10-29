(in-package #:common-lisp-user)

(asdf:defsystem sicl-code-utilities
  :depends-on (#:sicl-additional-types
	       #:sicl-additional-conditions)
  :components
  ((:file "packages")
   (:file "conditions" :depends-on ("packages"))
   (:file "destructuring" :depends-on ("packages"))
   (:file "declarations" :depends-on ("packages"))
   (:file "environment" :depends-on ("packages"))))
