(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-clos-phase3
  :depends-on (:sicl-clos-phase1 :sicl-clos-phase2)
  ;; We use :SERIAL T so as to reduce the clutter with respect to the
  ;; dependencies, and to make the order completely predictable.
  :serial t
  :components
  ((:file "heap-instance")
   (:file "allocate-instance-support")
   ))
