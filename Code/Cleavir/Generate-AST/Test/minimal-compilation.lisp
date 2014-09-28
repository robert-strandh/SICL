(in-package #:cleavir-test-minimal-compilation)

(defparameter *e* (make-instance 'bogus-environment))

(defun test (form equivalent-form)
  (assert (equal (eval (cleavir-generate-ast:minimally-compile form *e*))
		 (eval equivalent-form))))

;;; When the name GSM3 is used as a global variable, then it is
;;; considered a global symbol macro that expands to the following
;;; form: 234
(defmethod cleavir-env:variable-info
    ((environment bogus-environment) (name (eql 'gsm3)))
  (make-instance 'cleavir-env:symbol-macro-info 
    :name name
    :expansion 234))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test BLOCK

(defun test-block ()
  ;; Check that the name of the block is not expanded, but that the
  ;; body forms are.  If the name of the block would be expanded,
  ;; then an error would be signaled during evaluation, because
  ;; numbers are not valid block names.  And if the body forms would
  ;; not be expanded, then an error would result because there is no
  ;; global variable named GSM3.
  (test '(block gsm3 gsm3 gsm3) 234))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test CATCH

(defun test-catch ()
  ;; Check that the catch tag is minimally compiled. 
  (test '(catch (if (numberp gsm3) 'a 'b) (throw 'a 10))
	10)
  (test '(catch 'a (unless (numberp gsm3) (error "bad")) gsm3)
	234))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test EVAL-WHEN

(defun test-eval-when ()
  ;; Test that all body forms are minimally compiled.
  (test '(eval-when (:execute)
	  (unless (numberp gsm3) (error "bad"))
	  gsm3)
	234))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test FLET

(defun test-flet ()
  ;; Test that a required parameter of the local function shadows the
  ;; global symbol macro in the &OPTIONAL part of the lambda list of
  ;; the local function, but not in the body of the FLET.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(flet ((fun (gsm1 &optional (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(flet ((fun (gsm1 &optional (x gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that a required parameter of the local function shadows the
  ;; global symbol macro in the &KEY part of the lambda list of
  ;; the local function, but not in the body of the FLET.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(flet ((fun (gsm1 &key (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(flet ((fun (gsm1 &key ((:x x) gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that a required parameter of the local function shadows the
  ;; global symbol macro in the &AUX part of the lambda list of
  ;; the local function, but not in the body of the FLET.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(flet ((fun (gsm1 &aux (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(flet ((fun (gsm1 &aux (x gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that a required parameter of the local function shadows the
  ;; global symbol macro in the body of the local function, but not in
  ;; the body of the FLET.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(flet ((fun (gsm1) gsm1))
		     (fun gsm1))
		  *e*)
		 `(flet ((fun (gsm1) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &OPTIONAL parameter of the local function shadows
  ;; the global symbol macro in the &KEY part of the lambda list of
  ;; the local function, but not in the body of the FLET.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(flet ((fun (&optional (gsm1 12) &key (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(flet ((fun (&optional (gsm1 12) &key ((:x x) gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &OPTIONAL parameter of the local function shadows
  ;; the global symbol macro in the remaining &OPTIONAL part of the
  ;; lambda list of the local function, but not in the body of the
  ;; FLET.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(flet ((fun (&optional (gsm1 12) (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(flet ((fun (&optional (gsm1 12) (x gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &OPTIONAL parameter of the local function shadows
  ;; the global symbol macro in the &AUX part of the lambda list of
  ;; the local function, but not in the body of the FLET.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(flet ((fun (&optional (gsm1 12) &aux (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(flet ((fun (&optional (gsm1 12) &aux (x gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &OPTIONAL parameter of the local function shadows
  ;; the global symbol macro in body of the local function, but not in
  ;; the body of the FLET.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(flet ((fun (&optional (gsm1 12)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(flet ((fun (&optional (gsm1 12)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &KEY parameter of the local function shadows the
  ;; global symbol macro in the remaining &KEY part of the lambda list
  ;; of the local function, but not in the body of the FLET.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(flet ((fun (&key (gsm1 12) (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(flet ((fun (&key ((:gsm1 gsm1) 12) ((:x x) gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &KEY parameter of the local function shadows the
  ;; global symbol macro in the &AUX part of the lambda list of the
  ;; local function, but not in the body of the FLET.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(flet ((fun (&key (gsm1 12) &aux (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(flet ((fun (&key ((:gsm1 gsm1) 12) &aux (x gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &KEY parameter of the local function shadows the
  ;; global symbol macro in the body of the local function, but not in
  ;; the body of the FLET.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(flet ((fun (&key (gsm1 12)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(flet ((fun (&key ((:gsm1 gsm1) 12)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &AUX parameter of the local function shadows
  ;; the global symbol macro in the remaining &AUX part of the
  ;; lambda list of the local function, but not in the body of the
  ;; FLET.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(flet ((fun (&aux (gsm1 12) (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(flet ((fun (&aux (gsm1 12) (x gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &AUX parameter of the local function shadows the
  ;; global symbol macro in the body of the local function, but not in
  ;; the body of the FLET.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(flet ((fun (&aux (gsm1 12)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(flet ((fun (&aux (gsm1 12)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that the name of the local function shadows the global macro
  ;; in the body of the flet, but not in the body of the local
  ;; function.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(flet ((gm1 (x) (gm1 x)))
		    (gm1 x))
		  *e*)
		 '(flet ((gm1 (x) (hello x)))
		   (gm1 x))))
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
;;; Test FUNCTION

(defun test-function ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(function fff)
		  *e*)
		 '(function fff)))
  ;;; Test that the name of the function is not expanded as a symbol
  ;;; macro.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(function gsm1)
		  *e*)
		 '(function gsm1)))
  ;;; Test that required parameter of lambda expression shadows global
  ;;; symbol macro inside the body of the lambda expression.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(function (lambda (gsm1) gsm1))
		  *e*)
		 '(function (lambda (gsm1) gsm1)))))

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
  (test '(if (numberp gsm3) gsm3 (1+ gsm3))
	'234)
  (test '(if (symbolp gsm3) gsm3 (1+ gsm3))
	'235))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test LABELS

(defun test-labels ()
  ;; Test that a required parameter of the local function shadows the
  ;; global symbol macro in the &OPTIONAL part of the lambda list of
  ;; the local function, but not in the body of the LABELS.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(labels ((fun (gsm1 &optional (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(labels ((fun (gsm1 &optional (x gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that a required parameter of the local function shadows the
  ;; global symbol macro in the &KEY part of the lambda list of
  ;; the local function, but not in the body of the LABELS.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(labels ((fun (gsm1 &key (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(labels ((fun (gsm1 &key ((:x x) gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that a required parameter of the local function shadows the
  ;; global symbol macro in the &AUX part of the lambda list of
  ;; the local function, but not in the body of the LABELS.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(labels ((fun (gsm1 &aux (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(labels ((fun (gsm1 &aux (x gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that a required parameter of the local function shadows the
  ;; global symbol macro in the body of the local function, but not in
  ;; the body of the LABELS.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(labels ((fun (gsm1) gsm1))
		     (fun gsm1))
		  *e*)
		 `(labels ((fun (gsm1) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &OPTIONAL parameter of the local function shadows
  ;; the global symbol macro in the &KEY part of the lambda list of
  ;; the local function, but not in the body of the LABELS.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(labels ((fun (&optional (gsm1 12) &key (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(labels ((fun (&optional (gsm1 12) &key ((:x x) gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &OPTIONAL parameter of the local function shadows
  ;; the global symbol macro in the remaining &OPTIONAL part of the
  ;; lambda list of the local function, but not in the body of the
  ;; LABELS.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(labels ((fun (&optional (gsm1 12) (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(labels ((fun (&optional (gsm1 12) (x gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &OPTIONAL parameter of the local function shadows
  ;; the global symbol macro in the &AUX part of the lambda list of
  ;; the local function, but not in the body of the LABELS.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(labels ((fun (&optional (gsm1 12) &aux (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(labels ((fun (&optional (gsm1 12) &aux (x gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &OPTIONAL parameter of the local function shadows
  ;; the global symbol macro in body of the local function, but not in
  ;; the body of the LABELS.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(labels ((fun (&optional (gsm1 12)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(labels ((fun (&optional (gsm1 12)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &KEY parameter of the local function shadows the
  ;; global symbol macro in the remaining &KEY part of the lambda list
  ;; of the local function, but not in the body of the LABELS.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(labels ((fun (&key (gsm1 12) (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(labels ((fun (&key ((:gsm1 gsm1) 12) ((:x x) gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &KEY parameter of the local function shadows the
  ;; global symbol macro in the &AUX part of the lambda list of the
  ;; local function, but not in the body of the LABELS.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(labels ((fun (&key (gsm1 12) &aux (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(labels ((fun (&key ((:gsm1 gsm1) 12) &aux (x gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &KEY parameter of the local function shadows the
  ;; global symbol macro in the body of the local function, but not in
  ;; the body of the LABELS.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(labels ((fun (&key (gsm1 12)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(labels ((fun (&key ((:gsm1 gsm1) 12)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &AUX parameter of the local function shadows
  ;; the global symbol macro in the remaining &AUX part of the
  ;; lambda list of the local function, but not in the body of the
  ;; LABELS.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(labels ((fun (&aux (gsm1 12) (x gsm1)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(labels ((fun (&aux (gsm1 12) (x gsm1)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that an &AUX parameter of the local function shadows the
  ;; global symbol macro in the body of the local function, but not in
  ;; the body of the LABELS.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  `(labels ((fun (&aux (gsm1 12)) gsm1))
		     (fun gsm1))
		  *e*)
		 `(labels ((fun (&aux (gsm1 12)) gsm1))
		    (fun (hello1 hello2)))))
  ;; Test that the name of the local function shadows the global macro
  ;; both in the body of the LABELS and in the body of the local
  ;; function.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(labels ((gm1 (x) (gm1 x)))
		    (gm1 x))
		  *e*)
		 '(labels ((gm1 (x) (gm1 x)))
		   (gm1 x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test LET

(defun test-let ()
  ;; Check that the symbol macro is shadowed by the first variable
  ;; binding in the body of the let, but not in the initialization of
  ;; the second binding.
  (test '(let ((gsm3 10)
	       (var gsm3))
	  (+ gsm3 var))
	244))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test LET*

(defun test-let* ()
  ;; Check that the symbol macro is shadowed by the first variable
  ;; binding both in the body of the let and in the initialization of
  ;; the second binding.
  (test '(let* ((gsm3 10)
		(var gsm3))
	  (+ gsm3 var))
	20))

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
;;; Test LOCALLY

(defun test-locally ()
  (test '(locally gsm3)
	234))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test MACROLET

(defun test-macrolet ()
  ;; Test that a call to the local macro is expanded.
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(macrolet ((gm1 (a b) `(cons ,a ,b)))
		    (gm1 x y))
		  *e*)
		 '(locally (cons x y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test MULTIPLE-VALUE-CALL.

(defun test-multiple-value-call ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(multiple-value-call gsm1 gsm1 gsm1)
		  *e*)
		 '(multiple-value-call
		   (hello1 hello2) (hello1 hello2) (hello1 hello2)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test MULTIPLE-VALUE-PROG1.

(defun test-multiple-value-prog1 ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(multiple-value-prog1 gsm1 gsm1 gsm1)
		  *e*)
		 '(multiple-value-prog1
		   (hello1 hello2) (hello1 hello2) (hello1 hello2)))))
  
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
;;; Test PROGV

(defun test-progv ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(progv gsm1 gsm1 gsm1 gsm1)
		  *e*)
		 '(progv (hello1 hello2) (hello1 hello2)
		   (hello1 hello2) (hello1 hello2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test QUOTE

(defun test-quote ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(quote gsm1)
		  *e*)
		 '(quote gsm1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test RETURN-FROM

(defun test-return-from ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(return-from gsm1 gsm1)
		  *e*)
		 '(return-from gsm1 (hello1 hello2)))))

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
;;; Test SYMBOL-MACROLET

(defun test-symbol-macrolet ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(symbol-macrolet ((gsm1 (bla bla)))
		    gsm1 gsm2)
		  *e*)
		 `(locally (bla bla) (bla bla)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test TAGBODY.

(defun test-tagbody ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(tagbody gsm1 (progn gsm1) gsm1 (progn gsm1))
		  *e*)
		 `(tagbody
		   gsm1 (progn (hello1 hello2))
		   gsm1 (progn (hello1 hello2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test THE.

(defun test-the ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(the gsm1 gsm1)
		  *e*)
		 `(the gsm1 (hello1 hello2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test THROW.

(defun test-throw ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(throw gsm1 gsm1)
		  *e*)
		 '(throw (hello1 hello2) (hello1 hello2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test UNWIND-PROTECT.

(defun test-unwind-protect ()
  (assert (equal (cleavir-generate-ast:minimally-compile
		  '(unwind-protect gsm1 gsm1 gsm1)
		  *e*)
		 '(unwind-protect (hello1 hello2)
		   (hello1 hello2)
		   (hello1 hello2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global function for running all the tests.

(defun run-tests ()
  (test-block)
  (test-catch)
  (test-eval-when)
  (test-flet)
  (test-function)
  (test-go)
  (test-if)
  (test-labels)
  (test-let)
  (test-let*)
  (test-load-time-value)
  (test-locally)
  (test-macrolet)
  (test-multiple-value-call)
  (test-multiple-value-prog1)
  (test-progn)
  (test-progv)
  (test-quote)
  (test-return-from)
  (test-setq)
  (test-symbol-macrolet)
  (test-tagbody)
  (test-the)
  (test-throw)
  (test-unwind-protect)
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
