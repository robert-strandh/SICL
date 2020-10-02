(cl:in-package #:sicl-loop)

(defclass thereis-clause (termination-test-clause form-mixin) ())

(defmethod accumulation-variables ((clause thereis-clause))
  `((nil thereis t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser thereis-clause-parser
  (consecutive (lambda (thereis form)
                 (declare (ignore thereis))
                 (make-instance 'thereis-clause
                   :form form))
               (keyword-parser 'thereis)
               'anything-parser))

(add-clause-parser 'thereis-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-form

(defmethod body-form ((clause thereis-clause) end-tag)
  (declare (ignore end-tag))
  `(let ((temp ,(form clause)))
     (when temp
       (return-from ,*loop-name* temp))))
