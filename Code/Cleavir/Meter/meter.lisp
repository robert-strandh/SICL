(cl:in-package #:cleavir-meter)

(defgeneric reset (meter)
  (:method-combination progn))

(defgeneric stream-report (meter stream)
  (:method-combination progn :most-specific-last))

(defun report (meter &optional (stream *standard-output*))
  (stream-report meter stream))

(defgeneric invoke-with-meter (meter function))

(defmacro with-meter ((meter-variable meter-form) &body body)
  `(let ((,meter-variable ,meter-form))
     (invoke-with-meter ,meter-variable (lambda () ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class METER.
;;;
;;; This is the base class of all meters.

(defclass meter ()
  ((%name :initform "Unnamed" :initarg :name :reader name)))

(defmethod reset progn ((meter meter))
  nil)

(defmethod stream-report progn ((meter meter) stream)
  (format stream "Report for meter named ~a~%" (name meter)))

(defmethod invoke-with-meter ((meter meter) function)
  (funcall function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BASIC-METER.
;;;
;;; A meter class that counts the number of invocations and measures
;;; the total CPU time for the invocations.

(defclass basic-meter (meter)
  ((%sum-cpu-time :initform 0 :accessor sum-cpu-time)
   (%sum-squared-cpu-time :initform 0 :accessor sum-squared-cpu-time)
   (%min-cpu-time :initform 0 :accessor min-cpu-time)
   (%max-cpu-time :initform 0 :accessor max-cpu-time)
   (%invocation-count :initform 0 :accessor invocation-count)))

(defun register-one-invocation (basic-meter cpu-time)
  (incf (invocation-count basic-meter))
  (incf (sum-cpu-time basic-meter) cpu-time)
  (incf (sum-squared-cpu-time basic-meter) (* cpu-time cpu-time))
  (if (= (invocation-count basic-meter) 1)
      (progn (setf (min-cpu-time basic-meter) cpu-time)
             (setf (max-cpu-time basic-meter) cpu-time))
      (progn (setf (min-cpu-time basic-meter)
                   (min (min-cpu-time basic-meter) cpu-time))
             (setf (max-cpu-time basic-meter)
                   (max (max-cpu-time basic-meter) cpu-time)))))

(defmethod reset progn ((meter basic-meter))
  (setf (sum-cpu-time meter) 0)
  (setf (sum-squared-cpu-time meter) 0)
  (setf (invocation-count meter) 0))

(defmethod stream-report progn ((meter basic-meter) stream)
  (with-accessors ((invocation-count invocation-count)
                   (sum-cpu-time sum-cpu-time)
                   (sum-squared-cpu-time sum-squared-cpu-time)
                   (min-cpu-time min-cpu-time)
                   (max-cpu-time max-cpu-time))
      meter
    (format stream "Invocation count: ~a~%" invocation-count)
    (unless (zerop invocation-count)
      (format stream
              "Total CPU time: ~a seconds~%"
              (float (/ sum-cpu-time
                        internal-time-units-per-second)))
      (format stream
              "Minimum CPU time for an invocation: ~a seconds~%"
              (float (/ min-cpu-time
                        internal-time-units-per-second)))
      (format stream
              "Maximum CPU time for an invocation: ~a seconds~%"
              (float (/ max-cpu-time
                        internal-time-units-per-second)))
      (format stream
              "Average CPU time per invocation: ~a seconds~%"
              (float (/ sum-cpu-time
                        invocation-count
                        internal-time-units-per-second)))
      (format stream
              "Standard deviation of CPU time per invocation: ~a seconds~%"
              (/ (sqrt (/ (- sum-squared-cpu-time
                             (/ (* sum-cpu-time sum-cpu-time)
                                invocation-count))
                          invocation-count))
                 internal-time-units-per-second)))))

(defmethod invoke-with-meter :around ((meter basic-meter) function)
  (declare (ignorable function))
  (let ((time (get-internal-run-time)))
    (multiple-value-prog1
        (call-next-method)
      (register-one-invocation meter (- (get-internal-run-time) time)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SIZE-METER.
;;;
;;; A meter class that, in addition to counting the number of
;;; invocations and measuring the total CPU time for the invocations
;;; also allows client code to supply a SIZE for each invocation.  We
;;; do this by defining a generic function INCREMENT-SIZE.

(defclass size-meter (basic-meter)
  ((%sum-size :initform 0 :accessor sum-size)
   (%sum-squared-size :initform 0 :accessor sum-squared-size)
   (%min-size :initform 0 :accessor min-size)
   (%max-size :initform 0 :accessor max-size)
   (%temp-size :initform 0 :accessor temp-size)))

(defmethod reset progn ((meter size-meter))
  (setf (sum-size meter) 0)
  (setf (sum-squared-size meter) 0))

(defgeneric increment-size (meter &optional increment))

(defmethod increment-size ((meter size-meter) &optional (increment 1))
  (incf (temp-size meter) increment))

(defmethod stream-report progn ((meter size-meter) stream)
  (with-accessors ((invocation-count invocation-count)
                   (sum-size sum-size)
                   (sum-squared-size sum-squared-size)
                   (min-size min-size)
                   (max-size max-size))
      meter
    (unless (zerop invocation-count)
      (format stream
              "Total size: ~a~%"
              (float sum-size))
      (format stream
              "Minimum size for an invocation: ~a~%"
              (float min-size))
      (format stream
              "Maximum size for an invocation: ~a~%"
              (float max-size))
      (format stream
              "Average size per invocation: ~a~%"
              (float (/ sum-size
                        invocation-count)))
      (format stream
              "Standard deviation of size per invocation: ~a~%"
              (sqrt (/ (- sum-squared-size
                          (/ (* sum-size sum-size)
                             invocation-count))
                       invocation-count))))))

(defmethod invoke-with-meter :around ((meter size-meter) function)
  (declare (ignorable function))
  (setf (temp-size meter) 0)
  (multiple-value-prog1
      (call-next-method)
    (incf (sum-size meter) (temp-size meter))
    (incf (sum-squared-size meter) (* (temp-size meter) (temp-size meter)))
    (if (= (invocation-count meter) 1)
        (progn (setf (min-size meter) (temp-size meter))
               (setf (max-size meter) (temp-size meter)))
        (progn (setf (min-size meter)
                     (min (min-size meter) (temp-size meter)))
               (setf (max-size meter)
                     (max (max-size meter) (temp-size meter)))))
    (setf (temp-size meter) 0)))
