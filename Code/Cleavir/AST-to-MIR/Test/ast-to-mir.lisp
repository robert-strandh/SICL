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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests

(defun test-constant ()
  (test 234 234))

(defun test-lexical ()
  (test '(let ((x 10)) x)
	10))

(defun test-call ()
  (test '(let ((x 10) (y 20)) (+ x y))
	30)
  (test '(let ((x 10) (y 20)) (+ (1+ x) y))
	31))

		  
