(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-compiler-dominance-test
  :depends-on (:sicl-compiler-utilities
	       :sicl-compiler-dominance
	       :sicl-compiler-test-utilities)
  :components
  ((:file "test-packages" :depends-on ())
   (:file "test-dominance" :depends-on ("test-packages"))))
