(cl:in-package #:asdf-user)

(defsystem :sicl-type
  :depends-on (:sicl-global-environment)
  :components
  ((:file "packages")
   (:file "typep-compound")
   (:file "typep-atomic")))
