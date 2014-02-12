(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-clos-phase3
  :depends-on (:sicl-clos-phase1 :sicl-clos-phase2)
  ;; We use :SERIAL T so as to reduce the clutter with respect to the
  ;; dependencies, and to make the order completely predictable.
  :serial t
  :components
  ((:file "finalize-classes")
   (:file "heap-instance")
   (:file "class-of")
   (:file "allocate-instance-support")
   (:file "allocate-instance-defuns")
   (:file "allocate-built-in-instance")
   (:file "standard-instance-access")
   (:file "slot-value-etc-support")
   (:file "slot-value-etc-defuns")
   (:file "fmakunbound-slot-value")
   (:file "slot-value-etc-specified-defuns")
   (:file "initialize-support")
   (:file "initialize-defgenerics")
   (:file "initialize-defmethods")
   (:file "built-in-initialize-support")
   (:file "built-in-initialize-defgenerics")
   (:file "built-in-initialize-defmethods")
   (:file "install-generic-functions")
   (:file "make-instance-support")
   (:file "fmakunbound-initialize-instance")
   (:file "fmakunbound-make-instance")
   (:file "generic-function-database")
   (:file "defgeneric-defmacro")
   (:file "accessor-defgenerics")
   (:file "class-database")
   ))
