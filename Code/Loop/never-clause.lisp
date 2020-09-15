(cl:in-package #:sicl-loop)

(defclass never-clause (termination-test-clause form-mixin) ())

(defmethod accumulation-variables ((clause never-clause))
  `((nil always/never t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser never-clause-parser
  (consecutive (lambda (never form)
                 (declare (ignore never))
                 (make-instance 'never-clause
                   :form form))
               (keyword-parser 'never)
               'anything-parser))

(add-clause-parser 'never-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-form

(defmethod body-form ((clause never-clause) end-tag)
  (declare (ignore end-tag))
  `(when ,(form clause)
     (return-from ,*loop-name* nil)))
