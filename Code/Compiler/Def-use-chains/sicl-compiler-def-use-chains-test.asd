(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler-def-use-chains-test
  :depends-on (:sicl-compiler-def-use-chains
	       :sicl-compiler-test-utilities)
  :components
  ((:file "test-packages")
   (:file "test-def-use-chains" :depends-on ("test-packages"))))
