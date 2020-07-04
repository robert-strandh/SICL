;;;; src/conditions.lisp

(in-package #:portable-condition-system)

;;; Standard conditions
;;;
;;; The standard condition types defined within this file are not documented via
;;; documentation strings due to the repetitiveness of the contents of this
;;; file.

(define-condition warning (condition) ())

(define-condition serious-condition (condition) ())

(define-condition error (serious-condition) ())

(define-condition style-warning (warning) ())

(defun report-simple-condition (condition stream)
  (let ((format-control (simple-condition-format-control condition))
        (format-args (simple-condition-format-arguments condition)))
    (if format-control
        (apply #'format stream format-control format-args)
        (format stream "Condition ~S was signaled with format arguments ~S."
                (type-of condition) format-args))))

(define-condition simple-condition ()
  ((format-control :reader simple-condition-format-control
                   :initarg :format-control)
   (format-arguments :reader simple-condition-format-arguments
                     :initarg :format-arguments))
  (:default-initargs :format-control nil :format-arguments '())
  (:report report-simple-condition))

(define-condition simple-warning (simple-condition warning) ())

(define-condition simple-error (simple-condition error) ())

(define-condition storage-condition (serious-condition) ())

(defun report-type-error (condition stream)
  (format stream "~@<The value ~@:_~2@T~S ~@:_is not of type ~@:_~2@T~S.~:@>"
          (type-error-datum condition)
          (type-error-expected-type condition)))

(define-condition type-error (error)
  ((datum :reader type-error-datum :initarg :datum)
   (expected-type :reader type-error-expected-type :initarg :expected-type))
  (:report report-type-error))

(define-condition simple-type-error (simple-condition type-error) ())

(define-condition control-error (error) ())

(define-condition program-error (error) ())

(define-condition cell-error (error)
  ((name :reader cell-error-name :initarg :name)))

(defun report-unbound-variable (condition stream)
  (format stream "The variable ~S is unbound."
          (cell-error-name condition)))

(define-condition unbound-variable (cell-error) ()
  (:report report-unbound-variable))

(defun report-undefined-function (condition stream)
  (format stream "The function ~S is undefined."
          (cell-error-name condition)))

(define-condition undefined-function (cell-error) ()
  (:report report-undefined-function))

(defun report-unbound-slot (condition stream)
  (format stream "The slot ~S is unbound in ~S."
          (cell-error-name condition)
          (unbound-slot-instance condition)))

(define-condition unbound-slot (cell-error)
  ((instance :reader unbound-slot-instance :initarg :instance))
  (:report report-unbound-slot))

(define-condition stream-error (error)
  ((stream :reader stream-error-stream :initarg :stream)))

(define-condition end-of-file (stream-error) ())

(define-condition parse-error (error) (stream))

(define-condition reader-error (parse-error stream-error) ())

(define-condition package-error (error)
  ((package :reader package-error-package :initarg :package)))

(define-condition arithmetic-error (error)
  ((operation :reader operation-error-operation :initarg :operation)
   (operands :reader operands-error-operands :initarg :operands)))

(define-condition division-by-zero (arithmetic-error) ())

(define-condition floating-point-invalid-operation (arithmetic-error) ())

(define-condition floating-point-inexact (arithmetic-error) ())

(define-condition floating-point-overflow (arithmetic-error) ())

(define-condition floating-point-underflow (arithmetic-error) ())

(define-condition file-error (error)
  ((pathname :reader pathname-error-pathname :initarg :pathname)))

(define-condition print-not-readable (error)
  ((object :reader print-not-readable-object :initarg :object)))

;;; Non-standard condition types

(define-condition restart-not-found (control-error)
  ((restart-name :reader restart-not-found-restart-name :initarg :restart-name))
  (:report (lambda (condition stream)
             (format stream "Restart ~S is not active."
                     (restart-not-found-restart-name condition)))))

(define-condition abort-failure (control-error) ()
  (:report "An ABORT restart failed to transfer control."))

(defun report-case-failure (condition stream)
  (format stream "~S fell through ~S expression.~%Wanted one of ~:S."
          (type-error-datum condition)
          (case-failure-name condition)
          (case-failure-possibilities condition)))

(define-condition case-failure (type-error)
  ((name :reader case-failure-name :initarg :name)
   (possibilities :reader case-failure-possibilities :initarg :possibilities))
  (:report report-case-failure))
