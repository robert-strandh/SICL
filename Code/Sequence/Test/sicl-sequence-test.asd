(cl:in-package #:asdf-user)

(defsystem :sicl-sequence-test
  :depends-on (:sicl-sequence-support)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "position-defun")
   (:file "sequence-test")
   (:file "position-test")))
