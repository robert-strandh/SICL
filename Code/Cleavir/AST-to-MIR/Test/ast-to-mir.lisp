(cl:in-package #:cleavir-ast-to-mir-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Testing framework.

(defparameter *e* (make-instance 'bogus-environment))

(defun test (form value)
  (let* ((ast (cleavir-generate-ast:generate-ast form *e*))
	 (mir (cleavir-ast-to-mir:compile-toplevel ast))
	 (v (cleavir-mir-interpreter:interpret-mir mir)))
    (assert (equalp v value))))

