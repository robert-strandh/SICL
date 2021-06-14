(cl:in-package #:common-lisp-user)

(defpackage #:sicl-type
  (:use #:common-lisp)
  (:export
   #:typep
   ;; The symbol TYPEQ names a special operator.  It is similar to
   ;; TYPEP, except that it does not evaluate its TYPE argument. 
   #:typeq
   #:type-expander
   #:typexpand))
