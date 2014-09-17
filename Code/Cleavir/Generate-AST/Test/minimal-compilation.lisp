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

;;; When the name UNDEFINED-FUNCTION is used as a global function,
;;; then return NIL to indicate that there is no such function.
(defmethod cleavir-env:function-info
    ((environment bogus-environment) (name (eql 'undefined-function)))
  nil)

(defun run-tests ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  'hello
		  *e*)
		 'hello))
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(hello)
		  *e*)
		 '(hello)))
  (format t "Tests passed~%"))

(run-tests)
