(in-package #:cl-user)

(asdf:defsystem :sicl-loop-test
  :depends-on (:sicl-loop-support)
  :serial t
  :components
  ((:file "test-packages")
   (:file "loop-defmacro")
   (:file "loop-test")
   (:file "simple-loop")
   (:file "loop1")
   (:file "loop2")
   (:file "loop3")
   (:file "loop4")))
