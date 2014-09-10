(in-package #:cleavir-code-utilities)

(define-condition form-must-be-proper-list (program-error)
  ())

;;; A max-argcount of NIL means no upper bound.
(define-condition invalid-number-of-arguments (program-error)
  ((%form :initarg :form :reader form)
   (%min-argcount :initarg :min-argcount :reader min-argcount)
   (%max-argcount :initarg :max-argcount :reader max-argcount)))

(define-condition too-few-arguments (program-error)
  ())

(define-condition too-many-arguments (program-error)
  ())

(define-condition odd-number-of-keyword-arguments (program-error)
  ())

