(in-package #:cl-user)

(asdf:defsystem :sicl-sequences-small
  :depends-on ("sicl-additional-types"
	       "sicl-additional-conditions"
	       "sicl-code-utilities")
  :components
  ((:file "packages" :depends-on ())
   (:file "sequences" :depends-on ("packages"))))

(asdf:defsystem :sicl-sequences-small-test
  :depends-on (:lisp-unit :sicl-sequences-small)
  :components
  ((:file "test" :depends-on ())))
