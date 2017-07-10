(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form when the head of a compound form is a
;;; CONS.  Then the head must be a lambda expression.  We replace a
;;; call such as ((lambda (params) . body) . args) by (flet ((temp
;;; (params) . body)) (temp . args))
;;;
;;; FIXME: do some more error checking.

(defmethod convert-lambda-call (form env system)
  (destructuring-bind ((lambda lambda-list &rest body) &rest args) form
    (assert (eql lambda 'cl:lambda) nil
            'lambda-call-first-symbol-not-lambda :expr lambda)
    (cleavir-ast:make-call-ast
     (convert-code lambda-list body env system)
     (convert-sequence args env system))))
