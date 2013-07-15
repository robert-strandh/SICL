(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler-reaching-definitions-test
  :depends-on (:sicl-compiler-utilities
	       :sicl-compiler-test-utilities
	       :sicl-compiler-reaching-definitions)
  :components
  ((:file "test-packages")
   (:file "test-reaching-definitions" :depends-on ("test-packages"))))

