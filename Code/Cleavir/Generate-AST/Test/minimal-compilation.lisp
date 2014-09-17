(cl:in-package #:common-lisp-user)

(defpackage #:cleavir-test-minimal-compilation
  (:use #:common-lisp))

(in-package #:cleavir-test-minimal-compilation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass bogus-environment () ()))

(defparameter *e* (make-instance 'bogus-environment))

;;; Any variable not otherwise mentioned explicitly is considered to
;;; be a special variable.
(defmethod cleavir-env:variable-info
    ((environment bogus-environment) symbol)
  (make-instance 'cleavir-env:special-variable-info
    :name symbol))

;;; Any function not otherwise mentioned explicitly is considered to
;;; be a global function.
(defmethod cleavir-env:function-info
    ((environment bogus-environment) name)
  (make-instance 'cleavir-env:global-function-info
    :name name))

;;; When the name UNDEFINED-VARIABLE is used as a global variable,
;;; then return NIL to indicate that there is no such variable.
(defmethod cleavir-env:variable-info
    ((environment bogus-environment) (name (eql 'undefined-variable)))
  nil)

;;; When the name GSM1 is used as a global variable, then it is
;;; considered a global symbol macro that expands to the following
;;; form: (HELLO1 HELLO2)
(defmethod cleavir-env:variable-info
    ((environment bogus-environment) (name (eql 'gsm1)))
  (make-instance 'cleavir-env:symbol-macro-info 
    :name name
    :expansion '(hello1 hello2)))

;;; When the name GSM2 is used as a global variable, then it is
;;; considered a global symbol macro that expands to the following
;;; form: GSM1
(defmethod cleavir-env:variable-info
    ((environment bogus-environment) (name (eql 'gsm2)))
  (make-instance 'cleavir-env:symbol-macro-info 
    :name name
    :expansion 'gsm1))

;;; When the name UNDEFINED-FUNCTION is used as a global function,
;;; then return NIL to indicate that there is no such function.
(defmethod cleavir-env:function-info
    ((environment bogus-environment) (name (eql 'undefined-function)))
  nil)

;;; When the name LET is used as a global function, then it is
;;; considered a special operator.
(defmethod cleavir-env:function-info
    ((environment bogus-environment) (name (eql 'let)))
  (make-instance 'cleavir-env:special-operator-info
     :name name))

(defun run-tests ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  'hello
		  *e*)
		 'hello))
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(hello)
		  *e*)
		 '(hello)))
  ;; Check that the symbol macro is expanded correctly.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  'gsm1
		  *e*)
		 '(hello1 hello2)))
  ;; Check that the symbol macro is expanded correctly and that the
  ;; expansion is then minimally compiled.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  'gsm2
		  *e*)
		 '(hello1 hello2)))
  ;; Check that the symbol macro is expanded in an argument position,
  ;; but not in a function position.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(gsm1 gsm1)
		  *e*)
		 '(gsm1 (hello1 hello2))))
  ;; Check that a local variable shadows the global symbol macro with
  ;; the same name.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(let ((gsm1 10)) (gsm1 gsm1))
		  *e*)
		 '(let ((gsm1 10)) (gsm1 gsm1))))
  (format t "Tests passed~%"))

(run-tests)
