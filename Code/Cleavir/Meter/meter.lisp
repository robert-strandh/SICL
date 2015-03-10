(cl:in-package #:cleavir-meter)

(defgeneric reset (meter)
  (:method-combination progn))

(defgeneric stream-report (meter stream)
  (:method-combination progn :most-specific-last))

(defun report (meter &optional (stream *standard-output*))
  (stream-report meter stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class METER.
;;;
;;; This is the base class of all meters.

(defclass meter () ())

(defmethod reset progn ((meter meter))
  nil)

(defmethod stream-report progn ((meter meter) stream)
  (declare (ignore stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BASIC-METER.
;;;
;;; A meter class that counts the number of invocations and measures
;;; the total CPU time for the invocations.

(defclass basic-meter (meter)
  ((%sum-cpu-time :initform 0 :accessor sum-cpu-time)
   (%sum-squared-cpu-time :initform 0 :accessor sum-squared-cpu-time)
   (%invocation-count :initform 0 :accessor invocation-count)))

(defmethod reset progn ((meter basic-meter))
  (setf (sum-cpu-time meter) 0)
  (setf (sum-squared-cpu-time meter) 0)
  (setf (invocation-count meter) 0))
