;;;; This is an implementation of a virtual machine the purpose of which
;;;; is to simplify the testing of large parts of a compiler.  
;;;; This virtual machine makes certain objects explicit such as the 
;;;; call stack and the registers, but does not contain instructions
;;;; for things like arithmetic or logic.  Instead, we import those
;;;; functions directly from Common Lisp.  We also do not implement basic 
;;;; memory primitives like car, cdr, and cons, which we also import 
;;;; from Common Lisp.  Higher-level function, on the other hand, should
;;;; be compiled by our compiler into code for this virtual machine. 

(defpackage #:runtime
    (:use #:common-lisp)
  (:shadow #:function)
  (:export))

(in-package #:runtime)

(defclass environment ()
  ((%bindings :initarg :bindings :reader bindings)
   (%static-link :initarg :static-link :reader static-link)
   (%dynamic-link :initarg :dynamic-link :reader dynamic-link)))

(defclass code ()
  ((%object-vector :initarg :object-vector :reader object-vector)
   (%entry-point :initarg :entry-point :reader entry-point)))

(defclass function () ())

(defclass lisp-function (function)
  ((%function :initarg :function :reader function)))

(defclass closure (function)
  ((%code :initarg :code :reader code)
   (%environment :initarg :environment :reader environment)))

(defclass virtual-machine ()
  ((%scratch-registers :initform (make-array 100) :reader scratch-registers)
   (%argument-registers :initform (make-array 100) :reader argument-registers)
   ;; The number of argument registers containing valid objects.
   ;; This one is set just before a function is called. 
   (%argument-count :initform 0 :accessor argument-count)
   (%value-registers :initform (make-array 100) :reader value-registers)
   ;; The number of value registers containing valid objects.
   ;; This one is set just before a function returns
   (%value-count :initform 0 :accessor value-count)
   ;; This one is set by the caller of a function.
   ;; The callee must save this if necessary and put its value
   ;; in current-basic-block as part of the return protocol.
   (%return-address :initform 0 :accessor return-address)
   (%current-environment :initform nil :accessor current-environment)
   (%current-basic-block :initarg :current-basic-block :accessor current-basic-block)
   (%current-object-vector :initarg nil :accessor current-object-vector)
   ;; when a function is called, this slot is set to its captured environment
   (%static-environment :initform nil :accessor static-environment))

(defparameter *virtual-machine* nil)

(defun run-virtual-machine (virtual-machine initial-code)
  (setf (current-basic-block virtual-machine) (entry-point initial-code))
  (setf (current-object-vector virtual-machine) (object-vector initial-code))
  (catch 'stop
    (let ((*virtual-machine* virtual-machine))
      (loop (funcall (current-basic-block virtual-machine))))))

;;;; Instructions that can be part of basic blocks.
;;;; These instructions never take objects of the virtual machine
;;;; as arguments, and instead use integer index values into
;;;; vectors of the virtual machine.

;;; Look up a value in a local environment.
;;; The environment argument indicates how many steps up the 
;;; static link the environment is located (with 0 meaning the current one),
;;; and the offset is the index into the bindings vector of that environment.   
(defun lookup-local (target-register environment offset)
  (loop with env = (current-environment *virtual-machine*)
	repeat environment
	do (setf env (static-link env))
	finally (setf (aref (scratch-registers *virtual-machine*) target-register)
		      (aref (bindings env) offset))))

(defun save-local (source-register environment offset)
  (loop with env = (current-environment *virtual-machine*)
	repeat environment
	do (setf env (static-link env))
	finally (setf (aref (bindings env) offset)
		      (aref (scratch-registers *virtual-machine*) target-register))))

;;; If the register indicated contains true, then jump to the
;;; true-basic-block, else jump to the false-basic-block.
(defun conditional (test-register true-basic-block false-basic-block)
  (setf (current-basic-block *virtual-machine*)
	(aref (current-object-vector *virtual-machine*)
	      (if (aref (scratch-registers *virtual-machine*) test-register)
		  true-basic-block
		  false-basic-block))))

(defun pass-argument (argument register)
  (setf (aref (argument-registers *virtual-machine*) argument)
	(aref (scratch-registers *virtual-machine*) register)))

(defun set-argument-count (count)
  (setf (argument-count *virtual-machine*) count))

(defun access-value (register value)
  (setf (aref (scratch-registers *virtual-machine*) register)
	(aref (value-registers *virtual-machine*) value)))

(defun call-function (register)
  (let ((fun (aref (scratch-registers *virtual-machine*) register)))
    (if (typep fun 'lisp-function)
	(let* ((args (loop for i from 0 below (argument-count *virtual-machine*)
			   collect (aref (argument-registers *virtual-machine*) i)))
	       (values (multiple-value-list (apply (function fun) args))))
	  (setf (value-count *virtual-machine*) (length values))
	  (loop for i from 0 below (length values)
		for value in values
		do (setf (aref (value-registers *virtual-machine*) i) value)))
	(progn
	  (setf (static-environment *virtual-machine*)
		(environment fun))
	  (setf (current-object-vector *virtual-machine*) (object-vector (code fun)))
	  (setf (current-basic-block *virtual-machine*) (entry-point (code fun)))))))

;;; factorial
(defparameter *example-1*
  (let* ((object-vector
	  (vector
	   0
	   1
	   10
	   ;; initial basic block
	   (lambda ()
	     ;; put 0 in register 0
	     (setf (aref (scratch-registers *virtual-machine*) 0)
		   (aref (current-object-vector *virtual-machine*) 0))
	     ;; put a 1 in register 1
	     (setf (aref (scratch-registers *virtual-machine*) 1)
		   (aref (current-object-vector *virtual-machine*) 1))
	     ;; put 10 in register 2
	     (setf (aref (scratch-registers *virtual-machine*) 2)
		   (aref (current-object-vector *virtual-machine*) 2))
	     ;; go to next basic block
	     (setf (current-basic-block *virtual-machine*)
		   (aref (current-object-vector *virtual-machine*) 4)))
	   ;; test whether register 0 has the same value as register 2
	   (lambda ()
	     ;; this should be done by calling the function = of course.
	     (if (= (aref (scratch-registers *virtual-machine*) 0)
		    (aref (scratch-registers *virtual-machine*) 2))
		 (setf (current-basic-block *virtual-machine*)
		       (aref (current-object-vector *virtual-machine*) 5))
		 (setf (current-basic-block *virtual-machine*)
		       (aref (current-object-vector *virtual-machine*) 6))))
	   ;; they are equal, return the result
	   (lambda ()
	     (throw 'stop (aref (scratch-registers *virtual-machine*) 1)))
	   ;; not equal
	   (lambda ()
	     (setf (aref (scratch-registers *virtual-machine*) 1)
		   (* (aref (scratch-registers *virtual-machine*) 1)
		      (aref (scratch-registers *virtual-machine*) 2)))
	     (decf (aref (scratch-registers *virtual-machine*) 2))
	     (setf (current-basic-block *virtual-machine*)
		   (aref (current-object-vector *virtual-machine*) 4))))))
    (make-instance 'code
		   :object-vector object-vector
		   :entry-point (aref object-vector 3))))

		   
	     
		 
		    
		    
