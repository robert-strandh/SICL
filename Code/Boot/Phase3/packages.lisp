(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase3
  (:use #:common-lisp #:sicl-clos)
  (:shadowing-import-from
   #:sicl-clos
   #:defclass
   #:defgeneric
   #:defmethod)
  (:shadow
   #:ensure-generic-function)
  (:export
   ))

