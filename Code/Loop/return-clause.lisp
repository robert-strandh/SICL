(cl:in-package #:sicl-loop)

;;;; Clause RETURN-CLAUSE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RETURN-CLAUSE.
;;;
;;; An RETURN clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    return-clause ::= return {form | it}

(defclass return-clause (unconditional-clause)
  ())

(defmethod accumulation-variables ((clause return-clause))
  '())

(defclass return-it-clause (return-clause)
  ())

(defclass return-form-clause (return-clause)
  ((%form :initarg :form :reader form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser return-it-clause-parser
  (consecutive (lambda (return it)
                 (declare (ignore return it))
                 (make-instance 'return-it-clause))
               (keyword-parser 'return)
               (keyword-parser 'it)))

(define-parser return-form-clause-parser
  (consecutive (lambda (return form)
                 (declare (ignore return))
                 (make-instance 'return-form-clause
                   :form form))
               (keyword-parser 'return)
               'anything-parser))

(define-parser return-clause-parser
  (alternative 'return-it-clause-parser
               'return-form-clause-parser))

(add-clause-parser 'return-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-form.

(defmethod body-form ((clause return-form-clause) end-tag)
  (declare (ignore end-tag))
  `(return-from ,*loop-name* ,(form clause)))

(defmethod body-form ((clause return-it-clause) end-tag)
  (declare (ignore end-tag))
  `(return-from ,*loop-name* ,*it-var*))
