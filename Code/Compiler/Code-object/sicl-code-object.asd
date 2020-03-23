(cl:in-package #:asdf-user)

(defsystem #:sicl-code-object
  :serial t
  :components
  ((:file "generic-functions")
   (:file "code-object-defclass")))
