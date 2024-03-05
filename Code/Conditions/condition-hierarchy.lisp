(cl:in-package #:sicl-conditions)

(defgeneric cell-error-name (cell-error))

(defun report-unbound-variable (condition stream)
  (format stream "The variable ~S is unbound."
          (cell-error-name condition)))

(defun report-undefined-function (condition stream)
  (format stream "The function ~S is undefined."
          (cell-error-name condition)))

(defgeneric unbound-slot-instance (unbound-slot))

(defun report-unbound-slot (condition stream)
  (format stream "The slot ~S is unbound in ~S."
          (cell-error-name condition)
          (unbound-slot-instance condition)))

;;; Non-standard condition types

(defgeneric restart-not-found-restart-name (restart-not-found))

(define-condition restart-not-found (control-error)
  ((restart-name :reader restart-not-found-restart-name :initarg :restart-name))
  (:report (lambda (condition stream)
             (format stream "Restart ~S is not active."
                     (restart-not-found-restart-name condition)))))

(define-condition abort-failure (control-error) ()
  (:report "An ABORT restart failed to transfer control."))

(defgeneric case-failure-name (case-failure))

(defgeneric case-failure-possibilities (case-failure))

(defun report-case-failure (condition stream)
  (format stream "~S fell through ~S expression.~%Wanted one of ~:S."
          (type-error-datum condition)
          (case-failure-name condition)
          (case-failure-possibilities condition)))

(define-condition case-failure (type-error)
  ((name :reader case-failure-name :initarg :name)
   (possibilities :reader case-failure-possibilities :initarg :possibilities))
  (:report report-case-failure))
