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

(defun test-if ()
  (test '(let ((x 10)) (if (> x 5) 1 2))
	1)
  (test '(let ((x 10)) (if (> x 20) 1 2))
	2))

(defun test-function ()
  (test '(flet ((f (x) (> x 3)))
	  (find-if #'f '(1 2 5 7)))
	5))

(defun run-tests ()
  (test-constant)
  (test-lexical)
  (test-call)
  (test-if)
  (test-function))
