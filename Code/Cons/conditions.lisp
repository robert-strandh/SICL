(cl:in-package :sicl-cons)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Externally visible conditions

;;; This condition is used by functions and macros that require
;;; some argument to be a cons cell.
(define-condition must-be-cons (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "A cons cell was required,~@
                      but the following was given:~@
                      ~s"
                     (type-error-datum condition))))
  (:default-initargs :expected-type 'cons))

;;; This condition is used by functions and macros that require
;;; some argument to be a list (a cons or nil).
(define-condition must-be-list (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream
                     "A list (a cons or nil) was required,~@
                      but the following was given:~@
                      ~s"
                     (type-error-datum condition))))
  (:default-initargs :expected-type 'list))
