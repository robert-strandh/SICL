(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-sequence-test
  :depends-on (:lisp-unit)
  :serial t
  :components
  ((:file "test-packages")
   (:file "utilities")
   (:file "common")
   (:file "find")
   (:file "condition-reporters-en")))
