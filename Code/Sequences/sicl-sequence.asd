(cl:in-package #:asdf-user)

(defsystem :sicl-sequences
  :depends-on (:lisp-unit)
  :serial t
  :components
  ((:file "packages")
   (:file "common")
   (:file "sequences")
   (:file "condition-reporters-en")
   (:file "docstrings-en")))

(defsystem :sicl-sequences-tiny
  :depends-on (:lisp-unit)
  :components
  ((:file "packages" :depends-on ())
   (:file "common" :depends-on ("packages"))
   (:file "Tiny/sequences-tiny" :depends-on ("packages" "common"))
   (:file "condition-reporters-en" :depends-on ("packages" "common"))
   (:file "docstrings-en" :depends-on ("packages"))))

