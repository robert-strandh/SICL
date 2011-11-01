(in-package #:cl-user)

(asdf:defsystem :sicl-sequences
  :depends-on (:lisp-unit)
  :components
  ((:file "packages" :depends-on ())
   (:file "common" :depends-on ("packages"))
   (:file "sequences" :depends-on ("packages" "common"))
   (:file "condition-reporters-en" :depends-on ("packages" "common"))
   (:file "docstrings-en" :depends-on ("packages"))))

(asdf:defsystem :sicl-sequences-test
  :depends-on (:lisp-unit :sicl-sequences)
  :components
  ((:file "test" :depends-on ())))

(asdf:defsystem :sicl-sequences-tiny
  :depends-on (:lisp-unit)
  :components
  ((:file "packages" :depends-on ())
   (:file "common" :depends-on ("packages"))
   (:file "Tiny/sequences-tiny" :depends-on ("packages" "common"))
   (:file "condition-reporters-en" :depends-on ("packages" "common"))
   (:file "docstrings-en" :depends-on ("packages"))))

