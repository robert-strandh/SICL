(in-package #:cleavir-test-generate-ast)

(defgeneric same-p (ast1 ast2))

(defvar *table*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Testing framework.

(defparameter *e* (make-instance 'bogus-environment))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* cleavir-io:*io-readtable*))

(defun test (form value)
  (let* ((ast (cleavir-generate-ast:generate-ast form *e*))
	 (v (cleavir-ast-interpreter:interpret ast)))
    (assert (equalp v value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests

(defun test-constant-ast ()
  (test 234 234))

(defun test-lexical-ast ()
  (test '(let ((x 10)) x)
	10))

(defun test-symbol-value-ast ()
  (test '*print-base*
	10))

(defun test-block-return-from-ast ()
  (test '(block x (return-from x 10) 20)
	10))

(defun test-if-ast ()
  (test '(if t 10 20)
	10)
  (test '(if nil 10 20)
	20))

(defun test-tagbody-ast ()
  (test '(let ((x 1)) (tagbody (setq x 2) (go a) (setq x 3) a) x)
	2))

(defun test-fdefinition-ast ()
  (test '(function car)
	#'car))

(defun test-call ()
  (test '(1+ *print-base*)
	11)
  (test '(flet ((f () 1))
	  (+ (f) 2))
	3)
  (test '(flet ((f (x) x))
	  (+ (f 1) 2))
	3)
  (test '(flet ((f (x) x)
		(g (x) x))
	  (+ (f 1) (g 2)))
	3))

(defun run-tests ()
  (test-constant-ast)
  (test-lexical-ast)
  (test-symbol-value-ast)
  (test-block-return-from-ast)
  (test-if-ast)
  (test-tagbody-ast)
  (test-fdefinition-ast)
  (test-call))
