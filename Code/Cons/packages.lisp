(cl:in-package #:common-lisp-user)

(defpackage #:sicl-cons
  (:use #:common-lisp)
  (:export
   ;; The symbols LOAD-CAR, LOAD-CDR, STORE-CAR, and STORE-CDR name
   ;; special operators.  The operators LOAD-CAR and LOAD-CDR are
   ;; similar to the functions CAR and CDR, except that they require
   ;; that the argument be a CONS, or else an error is signaled.  The
   ;; operators STORE-CAR and STORE-CDR are like the functions RPLACA
   ;; and RPLACD, except that they must be evaluated in a context
   ;; where no value is required.  
   #:load-car #:load-cdr #:store-car #:store-cdr
   ))

