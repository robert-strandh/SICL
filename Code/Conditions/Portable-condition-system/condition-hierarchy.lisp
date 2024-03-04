;;;; src/conditions.lisp

(in-package #:portable-condition-system)

(defun report-type-error (condition stream)
  (format stream "~@<The value ~@:_~2@T~S ~@:_is not of type ~@:_~2@T~S.~:@>"
          (type-error-datum condition)
          (type-error-expected-type condition)))

(defun report-unbound-variable (condition stream)
  (format stream "The variable ~S is unbound."
          (cell-error-name condition)))

(defun report-undefined-function (condition stream)
  (format stream "The function ~S is undefined."
          (cell-error-name condition)))

(defun report-unbound-slot (condition stream)
  (format stream "The slot ~S is unbound in ~S."
          (cell-error-name condition)
          (unbound-slot-instance condition)))

(define-condition stream-error (error)
  ((stream :reader stream-error-stream :initarg :stream)))

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
