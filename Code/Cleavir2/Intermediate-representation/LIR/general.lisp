(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class REGISTER-LOCATION.
;;;
;;; This datum corresponds to a processor register.  It is
;;; introduced by the register allocation phase.

(defclass register-location (datum)
  ((%name :initarg :name :reader name)))

(defmethod print-object ((object register-location) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (name object))))
