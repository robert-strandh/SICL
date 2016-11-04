(cl:in-package #:cleavir-environment)

;;; This generic function takes an environment object and returns a
;;; list of allowed optimize qualities.  An optimize quality is
;;; represented as a list (NAME TYPE DEFAULT-VALUE) where NAME is the
;;; name of the optimize quality, TYPE is the type of the values of
;;; this optimize quality, and DEFAULT-VALUE is the default value of
;;; this optimize quality, i.e. the value that is assumed when a
;;; declaration of the form (OPTIMIZE NAME) is encountered.
(defgeneric optimize-qualities (environment))

;;; This method is called on the global environment when no
;;; implementation-specific method has been defined.  It returns the
;;; optimize qualities defined in the HyperSpec.
(defmethod optimize-qualities (environment)
  '((cl:speed (integer 0 3) 3)
    (cl:debug (integer 0 3) 3)
    (cl:space (integer 0 3) 3)
    (cl:compilation-speed (integer 0 3) 3)
    (cl:safety (integer 0 3) 3)))

;;; This method is called on an environment object other than the
;;; global environment.  It simply calls OPTIMIZE-QUALITIES again with
;;; the parent object.
(defmethod optimize-qualities ((environment entry))
  (optimize-qualities (next environment)))
