(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DEFGENERIC.

;;; FIXME: Add options and methods
;;;
;;; FIXME: At compile time, the expansion should set some information
;;; in the compilation environment, such as the lambda list, etc.  At
;;; load time it should call ENSURE-GENERIC-FUNCTION, but NOT with the
;;; ENV environment, because the ENV environment is the compilation
;;; environment.  Instead, it should use something like NIL or perhaps
;;; SICL-GLOBAL-ENVIRONMENT:*GLOBAL-ENVIRONMENT*.
(defmacro defgeneric (&environment env name lambda-list)
  `(ensure-generic-function
    ',name
    :lambda-list ',lambda-list
    :environment ,env))
