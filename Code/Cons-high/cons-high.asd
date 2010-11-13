(in-package #:cl-user)

(asdf:defsystem :sicl-cons-high
    :components
  ((:file "packages" :depends-on ())
   (:file "cons-high" :depends-on ("packages"))
   (:file "condition-reporters-en" :depends-on ("packages" "cons-high"))))

(asdf:defsystem :sicl-cons-high-test
  :depends-on (:sicl-cons-high)
  :components
  ((:file "test" :depends-on ())))
