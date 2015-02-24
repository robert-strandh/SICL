(cl:in-package #:cleavir-ast-to-hir-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Testing framework.

(defparameter *e* (make-instance 'bogus-environment))

;;; FIXME: pass an implementation instance rather than NIL to
;;; GENERATE-AST.
(defun test (form value)
  (let* ((ast (cleavir-generate-ast:generate-ast form *e* nil))
	 (hir (cleavir-ast-to-hir:compile-toplevel ast))
	 (v (cleavir-hir-interpreter:interpret-hir hir)))
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

(defun test-let* ()
  (test '(let* ((x 10)) x)
	10)
  (test '(let* ((x 10) (y (1+ x))) (+ x y))
	21)
  (test '(let* ((x 10) (y (1+ x)))
	  (declare (type integer x))
	  (+ x y))
	21))

(defun test-function ()
  (test '(flet ((f (x) (> x 3)))
	  (find-if #'f '(1 2 5 7)))
	5))

(defun test-progn ()
  (test '(progn)
	nil))

(defun run-tests ()
  (test-constant)
  (test-lexical)
  (test-call)
  (test-if)
  (test-let*)
  (test-function)
  (test-progn))
