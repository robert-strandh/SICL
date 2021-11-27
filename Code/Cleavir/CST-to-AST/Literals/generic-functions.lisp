(cl:in-package #:cleavir-literals)

(defgeneric make-load-form-using-client (client object environment))

(defgeneric load-time-literal (client object environment))

;;; This generic function is called in order for client code to
;;; allocate a lexical location in which the value of the creation
;;; form will be stored.  Client code must define a method on this
;;; generic function, that returns a fresh lexical location that
;;; corresponds to how client code translates code.
(defgeneric allocate-lexical-location (client environment))

;;; This generic function is called in order for client code to
;;; convert a form to the notiation that the client uses for this
;;; purpose.  Client code must define a method on this generic
;;; function, that returns an object that represents the result of the
;;; conversion.
(defgeneric convert-form (client form environment))

;;; Client code calls this generic function when the entire
;;; compilation unit has been processed.
(defgeneric finalize-literals (client environment))
