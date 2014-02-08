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
   (:file "fmakunbound-make-instance")
   (:file "standard-instance-access")
   (:file "slot-value-etc-support")
   (:file "slot-value-etc-defuns")
   (:file "fmakunbound-slot-value")
   (:file "slot-value-etc-specified-defuns")
   (:file "initialize-support")
   (:file "initialize-defgenerics")
   (:file "fmakunbound-initialize-instance")
   (:file "install-generic-functions")
   (:file "generic-function-database")
   (:file "defgeneric-defmacro")
   (:file "accessor-defgenerics")
   (:file "class-database")
   ))
