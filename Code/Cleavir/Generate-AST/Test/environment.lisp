(cl:in-package #:cleavir-bogus-test-environment)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass bogus-environment () ()))

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

;;; When the name GM1 is used as a global function, then it is
;;; considered a global macro that expands to the following form:
;;; (HELLO <arg>) where <arg> is the argument given to the macro
(defmethod cleavir-env:function-info
    ((environment bogus-environment) (name (eql 'gm1)))
  (make-instance 'cleavir-env:global-macro-info 
    :name name
    :expander (lambda (form env)
		(declare (ignore env))
		`(hello ,(second form)))
    :compiler-macro nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *special-operators*
    '(block catch eval-when flet function go if labels let let* load-time-value
      locally macrolet multiple-value-call multiple-value-prog1 progn progv
      quote return-from setq symbol-macrolet tagbody the throw unwind-protect)))

;;; Add some special operators to the environment.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for operator in *special-operators*
	do (eval `(defmethod cleavir-env:function-info
		      ((environment bogus-environment) (name (eql ',operator)))
		    (make-instance 'cleavir-env:special-operator-info
		      :name name)))))

;;; When the name LET is used as a global function, then it is
;;; considered a special operator.
(defmethod cleavir-env:function-info
    ((environment bogus-environment) (name (eql 'let)))
  (make-instance 'cleavir-env:special-operator-info
     :name name))


