(in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFINE-COMPILER-MACRO.

(defmacro define-compiler-macro (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (compiler-macro-function ',name)
	   ,(cleavir-code-utilities:parse-macro
	     name
	     lambda-list
	     body))))

;;; The next form requires some explanation.  In the native compiler,
;;; the symbols defmacro and cl:defmacro are the same, so then this
;;; next form only redefines the macro define-compiler-macro.  In the
;;; cross compiler, however, the two symbols are different.  The
;;; effect of this form, then, is to define a host macro named
;;; sicl-global-environment:define-compiler-macro and which puts
;;; host functions into the global SICL environment as compiler-macro
;;; functions.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defmacro define-compiler-macro (name lambda-list &body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (compiler-macro-function ',name)
	     ,(cleavir-code-utilities:parse-macro
	       name
	       lambda-list
	       body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler macro >

(define-compiler-macro > (number &rest more-numbers)
  (let ((temps (loop repeat (1+ (length more-numbers))
		     collect (gensym))))
    `(let ,(loop for temp in temps
		 for n in (cons number more-numbers)
		 collect `(,temp ,n))
       ,(if (null more-numbers)
	    `(if (realp ,(car temps)
			t
			(error "~a is not a real" ,(car temps))))
	    `(and ,@(loop for arg1 in temps
			  for arg2 in (cdr temps)
			  collect `(binary-> ,arg1 ,arg2)))))))

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler macro <

(define-compiler-macro < (number &rest more-numbers)
  (let ((temps (loop repeat (1+ (length more-numbers))
		     collect (gensym))))
    `(let ,(loop for temp in temps
		 for n in (cons number more-numbers)
		 collect `(,temp ,n))
       ,(if (null more-numbers)
	    `(if (realp ,(car temps)
			t
			(error "~a is not a real" ,(car temps))))
	    `(and ,@(loop for arg1 in temps
			  for arg2 in (cdr temps)
			  collect `(binary-< ,arg1 ,arg2)))))))

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler macro >=

(define-compiler-macro >= (number &rest more-numbers)
  (let ((temps (loop repeat (1+ (length more-numbers))
		     collect (gensym))))
    `(let ,(loop for temp in temps
		 for n in (cons number more-numbers)
		 collect `(,temp ,n))
       ,(if (null more-numbers)
	    `(if (realp ,(car temps)
			t
			(error "~a is not a real" ,(car temps))))
	    `(and ,@(loop for arg1 in temps
			  for arg2 in (cdr temps)
			  collect `(binary->= ,arg1 ,arg2)))))))

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler macro <=

(define-compiler-macro <= (number &rest more-numbers)
  (let ((temps (loop repeat (1+ (length more-numbers))
		     collect (gensym))))
    `(let ,(loop for temp in temps
		  for n in (cons number more-numbers)
		  collect `(,temp ,n))
       ,(if (null more-numbers)
	    `(if (realp ,(car temps)
			t
			(error "~a is not a real" ,(car temps))))
	    `(and ,@(loop for arg1 in temps
			  for arg2 in (cdr temps)
			  collect `(binary-<= ,arg1 ,arg2)))))))

	
