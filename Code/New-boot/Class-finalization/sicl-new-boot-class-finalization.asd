(cl:in-package #:asdf-user)

;;; The purpose of this system is to define an :AFTER method on
;;; INITIALIZE-INSTANCE specialized to STANDARD-CLASS, that finalizes
;;; the class immediately after its initialization.  The reason we
;;; want to do that during bootstrapping is so that class unique
;;; numbers are the same no matter which environment they are in, so
;;; that the class unique numbers depend only on the order in which
;;; class definitions are loaded.

(defsystem "sicl-new-boot-class-finalization"
  :serial t
  :components
  ((:file "class-finalization")))
