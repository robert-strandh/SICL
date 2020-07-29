(cl:in-package #:portable-condition-system)

;;; Definition of RESTART as a standard class.

(defclass restart ()
  ((%name
    :initarg :name
    :initform (error "NAME required")
    :reader restart-name)
   (%function
    :initarg :function
    :initform (error "FUNCTION required")
    :reader restart-function)
   (%report-function
    :initarg :report-function
    :initform nil
    :reader restart-report-function)
   (%interactive-function
    :initarg :interactive-function
    :initform nil
    :reader restart-interactive-function)
   (%test-function
    :initarg :test-function
    :initform (constantly t)
    :reader restart-test-function)
   (%associated-conditions
    :initform '()
    :accessor restart-associated-conditions)))

(defun make-restart (&rest arguments)
  (apply #'make-instance 'restart arguments))
