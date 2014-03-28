(in-package #:cl-user)

(asdf:defsystem :sicl-loop
  :depends-on ("sicl-additional-conditions")
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-en")
   (:file "utilities")
   (:file "loop")))
