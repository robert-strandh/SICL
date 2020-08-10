(cl:in-package #:asdf-user)

(defsystem #:sicl-structure-extrinsic
  :depends-on (#:closer-mop
               #:sicl-global-environment)
  :serial t
  :components
  ((:file "packages-extrinsic")
   (:file "structure-class-defclass-extrinsic")
   (:file "structure-class-defclass-validate-superclass")
   (:file "structure-slot-definition")
   (:file "structure-object-defclass")
   (:file "defstruct-description-defclass")
   (:file "defstruct-parse")))
