(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler-test
  :depends-on (:sicl-compiler
	       :sicl-x86-64)
  :components
  ((:file "conflicts")
   (:file "test-input")))
