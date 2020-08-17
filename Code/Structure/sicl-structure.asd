(cl:in-package #:asdf-user)

(defsystem #:sicl-structure
  :serial t
  :components
  ((:file "packages")
   (:file "structure-class-defclass")
   (:file "structure-slot-definition")
   (:file "structure-object-defclass")
   (:file "find-structure-description")
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
