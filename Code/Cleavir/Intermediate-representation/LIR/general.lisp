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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class STACK-LOCATION.
;;;
;;; This datum corresponds to a stack slot.  It is introduced by the
;;; register allocation phase.

(defclass stack-location (datum)
  ((%offset :initarg :offset :reader offset)))

(defmethod print-object ((object stack-location) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (offset object))))
