(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-boot-phase3
  :depends-on (:sicl-boot-phase2)
  :serial t
  :components
  ((:file "clos-package")
   (:file "packages")
   (:file "list-utilities")
   (:file "ensure")
   (:file "defclass-support")
   (:file "defclass-defmacro")
   (:file "define-built-in-class-defmacro")
   (:file "defgeneric-defmacro")
   (:file "make-method-lambda-support")
   (:file "make-method-lambda-defuns")
   (:file "defmethod-support")
   (:file "defmethod-defmacro")))
