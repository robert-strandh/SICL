(cl:in-package #:sicl-hir-to-cl)

(defparameter *dynamic-environment* '())

(defclass entry () ())

(defclass exit-point (entry)
  ((%valid-p :initform t :accessor valid-p)))

(defclass block/tagbody-entry (exit-point)
  ((%identifier :initarg :identifier :reader identifier)))

(defclass catch-entry (exit-point)
  ((%tag :initarg :tag :reader tag)))

(defclass special-variable-entry (entry)
  ((%name :initarg :name :reader name)
   (%value :initarg :value :accessor value)))

(defclass unwind-protect-entry (entry)
  ((%thunk :initarg :thunk :reader thunk)))

(defun find-entry (identifier)
  (loop for entry in *dynamic-environment*
        when (and (typep entry 'block/tagbody-entry)
                  (eq (identifier entry) identifier))
          return entry))

(defun unwind (identifier continuation)
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
           (throw (identifier (pop *dynamic-environment*)) continuation)))))
