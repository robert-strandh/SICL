(cl:in-package #:sicl-loop)

(defclass while-clause (termination-test-clause)
  ((%form :initarg :form :reader form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser while-clause-parser
  (consecutive (lambda (while form)
                 (declare (ignore while))
                 (make-instance 'while-clause
                   :form form))
               (keyword-parser 'while)
               'anything-parser))

(add-clause-parser 'while-clause-parser)

(define-parser until-clause-parser
  (consecutive (lambda (until form)
                 (declare (ignore until))
                 (make-instance 'while-clause
                   :form `(not ,form)))
               (keyword-parser 'until)
               'anything-parser))
  
(add-clause-parser 'until-clause-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the body-form

(defmethod body-form ((clause while-clause) end-tag)
  `(unless ,(form clause)
     (go ,end-tag)))
