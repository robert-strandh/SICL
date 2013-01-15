(defpackage #:sicl-compiler-phase-2
  (:use #:common-lisp #:sicl-compiler-phase-1)
  (:export
   ))

(in-package #:sicl-compiler-phase-2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction graph.

(defclass instruction ()
  ())

(defclass no-successor-instruction (instruction)
  ())

(defclass end-instruction (no-successor-instruction)
  ())

(defclass single-successor-instruction (instruction)
  ((%next :initarg :next :accessor next)))

(defclass dual-successor-instruction (instruction)
  ((%next1 :initarg :next1 :accessor next1)
   (%next2 :initarg :next2 :accessor next2)))

(defclass multiple-successor-instruction (instruction)
  ((%successors :initarg :successors :accessor successors)))

(defclass nop-instruction (single-successor-instruction)
  ())

(defclass constant-assignment-instruction (single-successor-instruction)
  ((%target :initarg :target :accessor target)
   (%constant :initarg :constant :accessor constant)))

(defclass variable-assignment-instruction (single-successor-instruction)
  ((%target :initarg :target :accessor target)
   (%variable :initarg :variable :accessor variable)))

(defclass value-assignment-instruction (single-successor-instruction)
  ((%target :initarg :target :accessor target)
   (%value-number :initarg :value-number :accessor value-number)))

(defclass test-instruction (dual-successor-instruction)
  ((%test :initarg :test :accessor test)))

(defclass funcall-instruction (single-successor-instruction)
  ((%fun :initarg :fun :accessor fun)
   (%arguments :initarg :arguments :accessor arguments)))

(defclass out-instruction (single-successor-instruction)
  ((%value-number :initarg :value-number :accessor value-number)
   (%variable :initarg :variable :accessor variable)))

(defclass value-count-instruction (single-successor-instruction)
  ((%value-count :initarg :value-count :accessor value-count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an abstract syntax tree.

(defun new-temporary ()
  (make-instance 'variable-ast
    :binding (make-instance 'lexical-variable-entry
			    :name 'temp)
    :type t))

(defun nil-fill (value-context successor)
  (let ((next successor))
    (loop for value in value-context
	  do (setf next
		   (make-instance 'constant-assignment-instruction
		     :target value
		     :constant nil
		     :next next))
	  finally (return next))))

(defgeneric compile-ast (ast value-context successor))

(defmethod compile-ast ((ast constant-ast) value-context successor)
  (cond ((null value-context)
	 (warn "Constant found in a context where no value required")
	 successor)
	((eq value-context t)
	 (let ((temp (new-temporary)))
	   (make-instance 'constant-assignment-instruction
	     :target temp
	     :constant (value ast)
	     :next (make-instance 'out-instruction
	             :value-number 0
		     :variable temp
		     :next (make-instance 'value-count-instruction
		             :value-count 1
			     :next successor)))))
	(t
	 (make-instance 'constant-assignment-instruction
	   :target (car value-context)
	   :value (value ast)
	   :next (nil-fill (cdr value-context) successor)))))

(defmethod compile-ast ((ast if-ast) value-context successor)
  (let ((test-temp (new-temporary)))
    (let ((then-branch (compile-ast (then ast) value-context successor))
	  (else-branch (compile-ast (else ast) value-context successor)))
      (let ((test (make-instance 'test-instruction
		    :test test-temp
		    :next1 then-branch
		    :next2 else-branch)))
	(compile-ast (test ast) (list test-temp) test)))))

(defmethod compile-ast ((ast progn-ast) value-context successor)
  (let ((next (compile-ast (car (last (forms ast))) value-context successor)))
    (loop for sub-ast in (cdr (reverse (forms ast)))
	  do (setf next (compile-ast sub-ast '() next)))
    next))

(defparameter *block-info* nil)

(defmethod compile-ast ((ast block-ast) value-context successor)
  (setf (gethash (binding ast) *block-info*)
	(list value-context successor))
  (compile-ast (body ast) value-context successor))

(defmethod compile-ast ((ast return-from-ast) value-context successor)
  (declare (ignore value-context successor))
  (destructuring-bind (value-context successor)
      (gethash (binding ast) *block-info*)
    (compile-ast (form ast) value-context successor)))

(defparameter *go-info* nil)

(defmethod compile-ast ((ast tagbody-ast) value-context successor)
  (loop for item in (items ast)
	do (unless (typep item 'ast)
	     (setf (gethash item *go-info*)
		   (make-instance 'nop-instruction))))
  (let ((next (compile-ast (make-instance 'constant-ast :value nil)
			   value-context
			   successor)))
    (loop for item in (reverse (items ast))
	  do (setf next
		   (if (typep item 'ast)
		       (compile-ast item '() next)
		       (let ((instruction (gethash item *go-info*)))
			 (setf (next instruction) next)
			 instruction))))
    next))

(defmethod compile-ast ((ast go-ast) value-context successor)
  (declare (ignore value-context successor))
  (gethash (binding ast) *go-info*))

(defmethod compile-ast ((ast function-call-ast) value-context successor)
  (let ((next successor))
    (unless (eq value-context t)
      (loop for val in value-context
	    for value-number from 0
	    do (setf next
		     (make-instance 'value-assignment-instruction
		       :target val
		       :value-number value-number
		       :next next))))
    (let ((temps (loop for arg in (arguments ast)
		      collect (new-temporary))))
      (setf next
	    (make-instance 'funcall-instruction
	      :fun (binding ast)
	      :arguments temps
	      :next next))
      (loop for temp in (reverse temps)
	    for arg in (reverse (arguments ast))
	    do (setf next
		     (compile-ast arg (list temp) next))))
    next))

(defun compile-toplevel (ast)
  (let ((*block-info* (make-hash-table :test #'eq))
	(*go-info* (make-hash-table :test #'eq))
	(end (make-instance 'end-instruction)))
    (compile-ast ast t end)))

