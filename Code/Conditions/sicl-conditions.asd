(cl:in-package #:asdf-user)

(defsystem :sicl-conditions
  :components
  ((:file "packages")
   (:file "support"
    :depends-on ("packages"))))
