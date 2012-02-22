(in-package #:common-lisp-user)

(asdf:defsystem sicl-code-utilities
  :depends-on (#:sicl-additional-types
	       #:sicl-additional-conditions)
  :components
  ((:file "packages")
   (:file "general" :depends-on ("packages"))
   (:file "lambda-lists" :depends-on ("packages"))
   (:file "declarations" :depends-on ("packages"))))
