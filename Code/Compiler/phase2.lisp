(in-package #:sicl-compiler-phase-2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction graph.

(defclass instruction ()
  ((%successors :initform '() :initarg :successors :accessor successors)
   (%inputs :initform '() :initarg :inputs :reader inputs)
   (%outputs :initform '() :initarg :outputs :reader outputs)))

(defclass end-instruction (instruction)
  ())

(defclass nop-instruction (instruction)
  ())

(defclass constant-assignment-instruction (instruction)
  ((%constant :initarg :constant :accessor constant)))

(defclass variable-assignment-instruction (instruction)
  ())

(defclass test-instruction (instruction)
  ())

(defclass funcall-instruction (instruction)
  ((%fun :initarg :fun :accessor fun)))

(defclass get-arguments-instruction (instruction)
  ((%lambda-list :initarg :lambda-list :accessor lambda-list)))

(defclass get-values-instruction (instruction)
  ())

(defclass put-arguments-instruction (instruction)
  ())

(defclass put-values-instruction (instruction)
  ())

(defmethod outputs ((instruction get-arguments-instruction))
  (p1:required (lambda-list instruction)))

(defclass enter-instruction (instruction)
  ())

(defclass leave-instruction (instruction)
  ())

(defclass return-instruction (instruction)
  ())

(defclass close-instruction (instruction)
  ((%target :initarg :target :accessor target)
   (%code :initarg :code :accessor code)))  

(defclass go-instruction (instruction)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile an abstract syntax tree.

(defun new-temporary ()
  (make-instance 'sicl-env:lexical-location))

(defun nil-fill (value-context successor)
  (let ((next successor))
    (loop for value in value-context
	  do (setf next
		   (make-instance 'constant-assignment-instruction
		     :outputs (list value)
		     :constant nil
		     :successors (list next)))
	  finally (return next))))

(defgeneric compile-ast (ast value-context successor))

(defmethod compile-ast ((ast p1:constant-ast) value-context successor)
  (cond ((null value-context)
	 (warn "Constant found in a context where no value required")
	 successor)
	((eq value-context t)
	 (let ((temp (new-temporary)))
	   (make-instance 'constant-assignment-instruction
	     :outputs (list temp)
	     :constant (p1:value ast)
	     :successors (list (make-instance 'put-values-instruction
				 :inputs (list temp)
				 :successors (list successor))))))
	(t
	 (make-instance 'constant-assignment-instruction
	   :outputs (list (car value-context))
	   :constant (p1:value ast)
	   :successors (list (nil-fill (cdr value-context) successor))))))

(defmethod compile-ast ((ast p1:if-ast) value-context successor)
  (let ((test-temp (new-temporary)))
    (let ((then-branch (compile-ast (p1:then ast) value-context successor))
	  (else-branch (compile-ast (p1:else ast) value-context successor)))
      (let ((test (make-instance 'test-instruction
		    :inputs (list test-temp)
		    :successors (list then-branch else-branch))))
	(compile-ast (p1:test ast) (list test-temp) test)))))

(defmethod compile-ast ((ast p1:progn-ast) value-context successor)
  (let ((next (compile-ast (car (last (p1:form-asts ast)))
			   value-context successor)))
    (loop for sub-ast in (cdr (reverse (p1:form-asts ast)))
	  do (setf next (compile-ast sub-ast '() next)))
    next))

(defparameter *block-info* nil)

(defmethod compile-ast ((ast p1:block-ast) value-context successor)
  (setf (gethash ast *block-info*)
	(list value-context successor))
  (compile-ast (p1:body ast) value-context successor))

(defmethod compile-ast ((ast p1:return-from-ast) value-context successor)
  (declare (ignore value-context successor))
  (destructuring-bind (value-context successor)
      (gethash (p1:block-ast ast) *block-info*)
    (compile-ast (p1:form-ast ast) value-context successor)))

(defparameter *go-info* nil)

(defmethod compile-ast ((ast p1:tagbody-ast) value-context successor)
  (loop for item in (p1:items ast)
	do (when (typep item 'p1:tag-ast)
	     (setf (gethash item *go-info*)
		   (make-instance 'nop-instruction))))
  (let ((next (compile-ast (make-instance 'p1:constant-ast :value nil)
			   value-context
			   successor)))
    (loop for item in (reverse (p1:items ast))
	  do (setf next
		   (if (typep item 'p1:tag-ast)
		       (let ((instruction (gethash item *go-info*)))
			 (setf (successors instruction) (list next))
			 instruction)
		       (compile-ast item '() next))))
    next))

(defmethod compile-ast ((ast p1:go-ast) value-context successor)
  (declare (ignore value-context successor))
  (make-instance 'go-instruction
		 :successors (list (gethash (p1:tag-ast ast) *go-info*))))

(defmethod compile-ast ((ast p1:function-call-ast) value-context successor)
  (let ((next successor))
    (unless (eq value-context t)
      (setf next
	    (make-instance 'get-values-instruction
	      :outputs value-context
	      :successors (list successor))))
    (let ((temps (loop for arg in (p1:arguments ast)
		      collect (new-temporary))))
      (setf next
	    (make-instance 'funcall-instruction
	      :fun (p1:function-location ast)
	      :successors (list next)))
      (setf next
	    (make-instance 'put-arguments-instruction
	      :inputs temps
	      :successors (list next)))
      (loop for temp in (reverse temps)
	    for arg in (reverse (p1:arguments ast))
	    do (setf next
		     (compile-ast arg (list temp) next))))
    next))

(defmethod compile-ast ((ast p1:code-ast) value-context successor)
  (declare (ignore value-context successor))
  (let ((next (make-instance 'return-instruction)))
    (setf next
	  (make-instance 'leave-instruction
			 :successors (list next)))
    (setf next
	  (compile-ast (p1:body-ast ast) t next))
    (setf next
	  (make-instance 'get-arguments-instruction
			 :lambda-list (p1:lambda-list ast)
			 :successors (list next)))
    (make-instance 'enter-instruction
		   :successors (list next))))

(defmethod compile-ast ((ast p1:close-ast) value-context successor)
  (let ((code (compile-ast (p1:code-ast ast) nil nil)))
    (cond ((eq value-context t)
	   (let ((temp (new-temporary)))
	     (make-instance 'close-instruction
	       :outputs (list temp)
	       :code code
	       :successors (list (make-instance 'put-values-instruction
		                   :inputs (list temp)
				   :successors (list successor))))))
	  ((null value-context)
	   (warn "closure compiled in a context with no values"))
	  (t
	   (make-instance 'close-instruction
	     :outputs (list (car value-context))
	     :code code
	     :successors (list (nil-fill (cdr value-context) successor)))))))

(defmethod compile-ast ((ast p1:typed-location-ast) value-context successor)
  (cond ((eq value-context t)
	 (make-instance 'put-values-instruction
	   :inputs (list (p1:location ast))
	   :successors (list successor)))
	((null value-context)
	 (warn "variable compiled in a context with no values"))
	(t
	 (make-instance 'variable-assignment-instruction
	   :outputs (list (car value-context))
	   :inputs (list (p1:location ast))
	   :successors (list (nil-fill (cdr value-context) successor))))))

(defun compile-toplevel (ast)
  (let ((*block-info* (make-hash-table :test #'eq))
	(*go-info* (make-hash-table :test #'eq))
	(end (make-instance 'end-instruction)))
    (compile-ast ast t end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing instructions.

(defparameter *instruction-table* nil)

(defgeneric draw-instruction (instruction stream))
  
(defmethod draw-instruction :around (instruction stream)
  (when (null (gethash instruction *instruction-table*))
    (setf (gethash instruction *instruction-table*) (gensym))
    (format stream "  ~a [shape = box];~%"
	    (gethash instruction *instruction-table*))
    (call-next-method)))

(defmethod draw-instruction :before ((instruction instruction) stream)
  (loop for next in (successors instruction)
	do (draw-instruction next stream))
  (loop for next in (successors instruction)
	do (format stream
		   "  ~a -> ~a [style = bold];~%"
		   (gethash instruction *instruction-table*)
		   (gethash next *instruction-table*))))
  
(defmethod draw-instruction (instruction stream)
  (format stream "   ~a [label = \"~a\"];~%"
	  (gethash instruction *instruction-table*)
	  (class-name (class-of instruction))))

(defgeneric draw-location (location stream))

(defmethod draw-location :around (location stream)
  (when (null (gethash location *instruction-table*))
    (setf (gethash location *instruction-table*) (gensym))
    (format stream "  ~a [shape = circle];~%"
	    (gethash location *instruction-table*))
    (call-next-method)))

(defmethod draw-location (location stream)
  (format stream "   ~a [label = \"?\"];~%"
	  (gethash location *instruction-table*)))

(defmethod draw-location ((location sicl-env:global-location) stream)
  (format stream "   ~a [label = \"?\", style = filled, fillcolor = green];~%"
	  (gethash location *instruction-table*)))

(defmethod draw-instruction :after (instruction stream)
  (loop for location in (inputs instruction)
	do (draw-location location stream)
	   (format stream "  ~a -> ~a [color = red, style = dashed];~%"
		   (gethash location *instruction-table*)
		   (gethash instruction *instruction-table*)))
  (loop for location in (outputs instruction)
	do (draw-location location stream)
	   (format stream "  ~a -> ~a [color = blue, style = dashed];~%"
		   (gethash instruction *instruction-table*)
		   (gethash location *instruction-table*))))
  
(defmethod draw-instruction ((instruction close-instruction) stream)
  (format stream "   ~a [label = \"close\"];~%"
	  (gethash instruction *instruction-table*))
  (draw-instruction (code instruction) stream)
  (format stream "  ~a -> ~a [color = pink, style = dashed];~%"
	  (gethash (code instruction) *instruction-table*)
	  (gethash instruction *instruction-table*)))

(defmethod draw-instruction ((instruction get-arguments-instruction) stream)
  (format stream "   ~a [label = \"get-arguments\"];~%"
	  (gethash instruction *instruction-table*)))

(defmethod draw-instruction ((instruction get-values-instruction) stream)
  (format stream "   ~a [label = \"get-values\"];~%"
	  (gethash instruction *instruction-table*)))

(defmethod draw-instruction ((instruction put-arguments-instruction) stream)
  (format stream "   ~a [label = \"put-values\"];~%"
	  (gethash instruction *instruction-table*)))

(defmethod draw-instruction ((instruction put-values-instruction) stream)
  (format stream "   ~a [label = \"put-values\"];~%"
	  (gethash instruction *instruction-table*)))

(defmethod draw-instruction
    ((instruction constant-assignment-instruction) stream)
  (format stream "   ~a [label = \"<=\"];~%"
	  (gethash instruction *instruction-table*))
  (let ((name (gensym)))
    (format stream "   ~a [label = \"~a\", style = filled, fillcolor = pink];~%"
	    name
	    (constant instruction))
    (format stream "   ~a [fillcolor = pink];~%"
	    name)
    (format stream "   ~a -> ~a [color = pink, style = dashed];~%"
	    name
	    (gethash instruction *instruction-table*))))

(defmethod draw-instruction
    ((instruction variable-assignment-instruction) stream)
  (format stream "   ~a [label = \"<-\"];~%"
	  (gethash instruction *instruction-table*)))

(defmethod draw-instruction ((instruction enter-instruction) stream)
  (format stream "   ~a [label = \"enter\"];~%"
	  (gethash instruction *instruction-table*)))

(defmethod draw-instruction ((instruction leave-instruction) stream)
  (format stream "   ~a [label = \"leave\"];~%"
	  (gethash instruction *instruction-table*)))

(defmethod draw-instruction ((instruction return-instruction) stream)
  (format stream "   ~a [label = \"ret\"];~%"
	  (gethash instruction *instruction-table*)))

(defmethod draw-instruction ((instruction end-instruction) stream)
  (format stream "   ~a [label = \"end\"];~%"
	  (gethash instruction *instruction-table*)))

(defmethod draw-instruction ((instruction nop-instruction) stream)
  (format stream "   ~a [label = \"nop\"];~%"
	  (gethash instruction *instruction-table*)))

(defmethod draw-instruction ((instruction funcall-instruction) stream)
  (draw-location (fun instruction) stream)
  (format stream "   ~a [label = \"funcall\"];~%"
	  (gethash instruction *instruction-table*))
  (format stream "   ~a -> ~a [color = red, style = dashed];~%"
	  (gethash (fun instruction) *instruction-table*)
	  (gethash instruction *instruction-table*)))

(defmethod draw-instruction ((instruction go-instruction) stream)
  (format stream "   ~a [label = \"go\"];~%"
	  (gethash instruction *instruction-table*)))

(defmethod draw-instruction ((instruction test-instruction) stream)
  (format stream "   ~a [label = \"test\"];~%"
	  (gethash instruction *instruction-table*)))

(defun draw-flowchart (start filename)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede)
    (let ((*instruction-table* (make-hash-table :test #'eq)))
	(format stream "digraph G {~%")
	(draw-instruction start stream)
	(format stream "}~%"))))
