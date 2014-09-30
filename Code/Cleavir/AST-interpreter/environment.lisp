(cl:in-package #:cleavir-ast-interpreter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The static runtime environment.

(defun lookup-lexical (lexical environment)
  (loop for level in environment
	do (multiple-value-bind (value present-p)
	       (gethash lexical level)
	     (when present-p (return value)))
	finally (error "Unknown lexical variable ~s" lexical)))

(defun set-lexical (lexical value environment)
  (loop for level in environment
	do (multiple-value-bind (value present-p)
	       (gethash lexical level)
	     (when present-p
	       (setf (gethash lexical level) value)
	       (return)))
	finally (setf (gethash lexical (first environment) ) value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The dynamic runtime environment.

(defclass special-binding ()
  ((%name :initarg :name :reader name)
   (%value :initarg :value :accessor value)))

(defun lookup-special (name environment)
  (let ((entry (find-if (lambda (entry) (typep entry 'special-binding))
			environment)))
    (if (null entry)
	(symbol-value name)
	(value entry))))


