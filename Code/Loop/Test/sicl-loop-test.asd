(in-package #:cl-user)

(asdf:defsystem :sicl-loop-test
  :depends-on (:sicl-loop)
  :components
  ((:file "loop-test" :depends-on ())))
