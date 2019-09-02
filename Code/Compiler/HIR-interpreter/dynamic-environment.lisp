(cl:in-package #:sicl-hir-interpreter)

(defparameter *dynamic-environment* '())

(defclass entry () ())

(defclass exit-point (entry)
  ((%valid-p :initform t :accessor valid-p)))

(defclass block/tagbody-entry (exit-point)
  ((%identifier :initarg :identifier :reader identifier)))

(defmethod print-object ((object block/tagbody-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s" (identifier object))))

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

(defun find-entry (identifier dynamic-environment)
  (loop for entry in dynamic-environment
        when (and (typep entry 'block/tagbody-entry)
                  (eq (identifier entry) identifier))
          return entry))

(defun unwind (identifier continuation dynamic-environment)
  (let ((entry (find-entry identifier dynamic-environment)))
    (cond ((null entry)
           (error "Attempt to unwind to a non-existing exit point."))
          ((not (valid-p entry))
           (error "Attempt to unwind to an expired exit point."))
          (t
           (loop for temp in dynamic-environment
                 until (eq temp entry)
                 when (typep temp 'exit-point)
                   do (setf (valid-p temp) nil))
           (loop for element in dynamic-environment
                 until (eq element entry)
                 when (typep element 'unwind-protect-entry)
                   do (funcall (thunk element)))
           (throw (identifier entry) continuation)))))

(defun compute-source-info (old new)
  (if (null new) old new))

(declaim (notinline compute-source-info))
