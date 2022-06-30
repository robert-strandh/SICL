(cl:in-package #:asdf-user)

(defsystem :sicl-cons-load-time
  :depends-on (:sicl-cons-package)
  :serial t
  :components
  ((:file "list-defclass")
   (:file "cons-defclass")
   (:file "null-defclass")))
