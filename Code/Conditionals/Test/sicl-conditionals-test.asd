(cl:in-package #:asdf-user)

(defsystem :sicl-conditionals-test
  :depends-on (:sicl-conditionals)
  :components
  ((:file "test" :depends-on ())))
