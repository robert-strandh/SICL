(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DEFGENERIC.

;;; FIXME: add options and methods
(defmacro defgeneric (&environment env name lambda-list)
  `(ensure-generic-function
    ',name
    :lambda-list ',lambda-list
    :environment ,env))
