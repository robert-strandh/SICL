(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-clos-phase4
  :depends-on (:sicl-clos-phase3)
  ;; We use :SERIAL T so as to reduce the clutter with respect to the
  ;; dependencies, and to make the order completely predictable.
  :serial t
  :components
  ((:file "install-generic-functions")
   (:file "finalize-inheritance")
   (:file "patch-target-instances")
   (:file "install-more-bridge-generic-functions")
   (:file "print-object")
   (:file "make-instance")
   (:file "make-built-in-instance")
   (:file "create-global-environment")
   ))
