(in-package #:cl-user)

(asdf:defsystem :sicl-sequences-small
  :depends-on ("sicl-additional-types"
	       "sicl-additional-conditions"
	       "cleavir-code-utilities")
  :components
  ((:file "packages" :depends-on ())
   (:file "sequences" :depends-on ("packages"))))
