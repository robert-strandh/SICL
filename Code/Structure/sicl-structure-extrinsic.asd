(cl:in-package #:asdf-user)

(defsystem #:sicl-structure-extrinsic
  :depends-on (#:closer-mop
               #:sicl-global-environment)
  :serial t
  :components
  ((:file "packages-extrinsic")
   (:file "structure-class-defclass-extrinsic")
   (:file "structure-class-defclass-validate-superclass-extrinsic")
   (:file "structure-slot-definition")
   (:file "structure-object-defclass")
   (:file "find-structure-description-extrinsic")
   (:file "copy-structure")
   (:file "read-structure")
   (:file "print-structure")
   (:file "structure-object-print-object")
   (:file "defstruct-description-defclass")
   (:file "defstruct-parse")
   (:file "defstruct-support")
   (:file "defstruct-expand-object")
   (:file "defstruct-expand-typed")
   (:file "defstruct-defmacro")
   (:file "conditions")))
