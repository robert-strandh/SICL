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
;;; convert an initialization form to the notiation that the client
;;; uses for this purpose.  Client code must define a method on this
;;; generic function, that returns an object that represents the
;;; result of the conversion.
(defgeneric convert-initialization-form (client form environment))

;;; This generic function is called in order for client code to
;;; convert a creation form to the notiation that the client uses for
;;; this purpose.  Client code must define a method on this generic
;;; function, that returns an object that represents the result of the
;;; conversion, wrapped in an object that binds the lexical location.
(defgeneric convert-creation-form (client form lexical-location environment))

;;; Client code calls this generic function when the entire
;;; compilation unit has been processed.  The function returns a list
;;; of converted (as implemented by CONVERT-INITIALIZATION-FORM and
;;; CONVERT-CREATION-FORM) creation and initialization forms in the
;;; order that they must be executed at load time.
(defgeneric finalize-literals (client environment))
