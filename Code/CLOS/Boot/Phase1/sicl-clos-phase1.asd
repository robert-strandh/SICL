(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-clos-phase1
  :depends-on (:sicl-code-utilities
	       :sicl-additional-conditions)
  ;; We use :SERIAL T so as to reduce the clutter with respect to the
  ;; dependencies, and to make the order completely predictable.
  :serial t
  :components
  ((:file "packages")
   (:file "defgeneric")
   (:file "accessor-defgenerics")
   (:file "define-built-in-class")
   (:file "defclass")
   (:file "defmethod")
   (:file "mop-class-hierarchy")
   (:file "print-object")))

