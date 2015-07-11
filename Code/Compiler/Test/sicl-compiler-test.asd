(cl:in-package #:asdf-user)

(defsystem :sicl-compiler-test
  :depends-on (:sicl-compiler
	       :sicl-x86-64)
  :components
  ((:file "conflicts")
   (:file "test-input")))
