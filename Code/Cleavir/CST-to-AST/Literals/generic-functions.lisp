(cl:in-package #:cleavir-literals)

(defgeneric make-load-form-using-client (client object environment))

(defgeneric load-time-literal (client object environment))

;;; This generic function is called in order for client code to
;;; allocate a lexical location in which the value of the creation
;;; form will be stored.  Client code must define a method on this
;;; generic function that returns a fresh lexical location that
;;; corresponds to how client code translates code.
(defgeneric allocate-lexical-location (client environment))
