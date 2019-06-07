(cl:in-package #:sicl-hir-to-cl)

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

(defun find-entry (identifier)
  (loop for entry in *dynamic-environment*
        when (and (typep entry 'block/tagbody-entry)
                  (eq (identifier entry) identifier))
          return entry))

(defun push-entry (entry)
  (push entry *dynamic-environment*))

(defun pop-entry ()
  (pop *dynamic-environment*))

(defun unwind (identifier continuation origin)
  (let ((entry (find-entry identifier)))
    (cond ((null entry)
           (error "Attempt to unwind to a non-existing exit point."))
          ((not (valid-p entry))
           (error "Attempt to unwind to an expired exit point."))
          (t
           (loop for temp in *dynamic-environment*
                 until (eq temp entry)
                 when (typep temp 'exit-point)
                   do (setf (valid-p temp) nil))
           (loop for top = (first *dynamic-environment*)
                 until (eq top entry)
                 when (typep top 'unwind-protect-entry)
                   do (funcall (thunk entry))
                 do (pop *dynamic-environment*))
           (throw (identifier (first *dynamic-environment*)) continuation)))))

(defun compute-source-info (old new)
  (if (null new) old new))

(declaim (notinline compute-source-info))
