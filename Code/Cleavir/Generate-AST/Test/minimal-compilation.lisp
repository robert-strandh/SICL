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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test BLOCK

(defun test-block ()
  ;; Check that the name of the block is not expanded, but that the
  ;; body forms are.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(block gsm1 gsm1 gsm1)
		  *e*)
		 '(block gsm1 (hello1 hello2) (hello1 hello2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test CATCH

(defun test-catch ()
  ;; Check that the all the arguments are expanded.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(catch gsm1 gsm1)
		  *e*)
		 '(catch (hello1 hello2) (hello1 hello2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test EVAL-WHEN

(defun test-eval-when ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(eval-when (:compile-toplevel)
		    gsm1 gsm1)
		  *e*)
		 '(eval-when (:compile-toplevel)
		   (hello1 hello2) (hello1 hello2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test FLET

(defun test-flet ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(flet ((fun1 (x &optional (y gsm1))
			   (f x y gsm1))
			  (fun2 (x &key (y gsm1 gsm1))
			   (f x y gsm1)))
		    gsm1)
		  *e*)
		 '(flet ((fun1 (x &optional (y (hello1 hello2)))
			  (f x y (hello1 hello2)))
			 (fun2 (x &key ((:y y) (hello1 hello2) gsm1))
			  (f x y gsm1)))
		   (hello1 hello2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test GO

(defun test-go ()
  ;; Check that that the argument of GO is not compiled.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(go gsm1)
		  *e*)
		 '(go gsm1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test IF

(defun test-if ()
  ;; Check that all three arguments of IF are minimally compiled.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(if gsm1 gsm1 gsm1)
		  *e*)
		 `(if (hello1 hello2) (hello1 hello2) (hello1 hello2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test LET

(defun test-let ()
  ;; Check that the symbol macro is not shadowed by the first variable
  ;; binding.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(let ((gsm1 10)
			 (var gsm1))
		    x)
		  *e*)
		 '(let ((gsm1 10) (var (hello1 hello2))) x)))
  ;; Check that a local variable shadows the global symbol macro with
  ;; the same name.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(let ((gsm1 10)) (gsm1 gsm1))
		  *e*)
		 '(let ((gsm1 10)) (gsm1 gsm1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test LET*

(defun test-let* ()
  ;; Check that the symbol-macro is shadowed by the first variable
  ;; binding.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(let* ((gsm1 10)
			 (var gsm1))
		    x)
		  *e*)
		 '(let* ((gsm1 10) (var gsm1)) x)))
  ;; Check that a local variable shadows the global symbol macro with
  ;; the same name.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(let* ((gsm1 10)) (gsm1 gsm1))
		  *e*)
		 '(let* ((gsm1 10)) (gsm1 gsm1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test LOAD-TIME-VALUE

(defun test-load-time-value ()
  ;; Check that the first argument of LOAD-TIME-VALUE is minimally
  ;; compiled, and that the second argument is preserved intact.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(load-time-value gsm1 t)
		  *e*)
		 '(load-time-value (hello1 hello2) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test PROGN

(defun test-progn ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(progn gsm1 gsm1)
		  *e*)
		 '(progn (hello1 hello2) (hello1 hello2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test SETQ

(defun test-setq ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(setq x gsm1 y gsm1)
		  *e*)
		 `(progn (setq x (hello1 hello2))
			 (setq y (hello1 hello2)))))
  ;; Check that the second gsm1 is expanded.  SETQ is handled as SETF
  ;; if the variable is defined as a symbol macro.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(setq x gsm1 gsm1 z)
		  *e*)
		 `(progn (setq x (hello1 hello2))
			 (setf (hello1 hello2) z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global function for running all the tests.

(defun run-tests ()
  (test-block)
  (test-catch)
  (test-eval-when)
  (test-flet)
  (test-go)
  (test-if)
  (test-let)
  (test-let*)
  (test-load-time-value)
  (test-progn)
  (test-setq)
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
  (format t "Tests passed~%"))

(run-tests)
