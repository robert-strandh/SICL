(cl:in-package #:asdf-user)

(defsystem #:sicl-environment
  :depends-on (#:sicl-lexical-environment
               #:clostrum
               #:clostrum/virtual)
  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "environment")
   (:file "trucler-methods")
   (:file "function-description")
   (:file "variable-description")
   (:file "class-description")
   (:file "get-setf-expansion-defun")
   (:file "map-defined-functions")
   (:file "map-defined-classes")
   (:file "map-defined-method-combination-templates")))
