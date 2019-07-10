(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RAW-DATUM.
;;;
;;; This class is the base class for all raw data.  It contains a size
;;; attribute that determines the number of bits that this datum
;;; consists of.

(defclass raw-datum (datum)
  ((%size :initarg :size :reader size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum RAW-INTEGER.

(defclass raw-integer (raw-datum)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum RAW-FLOAT.

(defclass raw-float (raw-datum)
  ())
