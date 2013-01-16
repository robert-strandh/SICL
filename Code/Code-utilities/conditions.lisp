(in-package #:sicl-code-utilities)

(define-condition form-must-be-proper-list (program-error)
  ((%form :initarg :form :reader form)))

(defun check-form-proper-list (form)
  (unless (proper-list-p form)
    (error 'form-must-be-proper-list :form form)))

;;; A max-argcount of NIL means no upper bound.
(define-condition invalid-number-of-arguments (program-error)
  ((%form :initarg :form :reader form)
   (%min-argcount :initarg :min-argcount :reader min-argcount)
   (%max-argcount :initarg :max-argcount :reader max-argcount)))

(defun check-argcount (form min-argcount max-argcount)
  (when (< (length (cdr form)) min-argcount)
    (error 'invalid-number-of-arguments
	   :form form
	   :min-argcount min-argcount
	   :max-argcount max-argcount))
  (when (and (not (null max-argcount))
	     (> (length (cdr form)) max-argcount)
    (error 'invalid-number-of-arguments
	   :form form
	   :min-argcount min-argcount
	   :max-argcount max-argcount))))

