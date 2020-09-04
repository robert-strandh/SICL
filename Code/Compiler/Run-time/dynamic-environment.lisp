(cl:in-package #:sicl-run-time)

(defparameter *dynamic-environment* '())

(defclass entry () ())

(defgeneric invalidate-entry (entry))

(defmethod invalidate-entry (entry)
  (declare (ignore entry))
  nil)

(defclass exit-point (entry)
  ((%valid-p :initform t :accessor valid-p)))

(defmethod invalidate-entry ((entry exit-point))
  (setf (valid-p entry) nil))

(defclass block/tagbody-entry (exit-point)
  ((%stack-pointer :initarg :stack-pointer :reader stack-pointer)
   (%frame-pointer :initarg :frame-pointer :reader frame-pointer)
   (%identifier :initarg :identifier :reader identifier)))

(defmethod print-object ((object block/tagbody-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s" (frame-pointer object))))

(defclass catch-entry (exit-point)
  ((%tag :initarg :tag :reader tag)))

(defmethod print-object ((object catch-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s" (tag object))))

(defclass special-variable-entry (entry)
  ((%name :initarg :name :reader name)
   (%value :initarg :value :accessor value)))

(defmethod print-object ((object special-variable-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s" (name object) (value object))))

(defclass unwind-protect-entry (entry)
  ((%thunk :initarg :thunk :reader thunk)))

(defmethod print-object ((object unwind-protect-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s" (thunk object))))
