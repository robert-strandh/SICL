(in-package #:sicl-word)

(defclass arguments-mixin ()
  ((%arguments :initarg :arguments :accessor arguments)))

(defun convert-arguments (arguments env)
  (loop for argument in arguments
	collect (p1:convert argument env)))

;;; This function provides the glue between a DESIRED results
;;; and an instruction that can only generate between MIN-SUPPLIED and
;;; MAX-SUPPLIED values.
;;;
;;; Return two values.  The first value is the new context to be used
;;; in place of the first one, and the second value is a successor
;;; instruction to be used by the requesting instruction.
;;;
;;; If the desired results is T, i.e., all the values are
;;; required, then we generate a PUT-VALUES-INSTRUCTION that takes the
;;; maximum number of arguments that the instruction can generate.
;;; 
;;; If the results is not T, and it contains fewer elements than
;;; the minimum number that the instruction generates or more elements
;;; than the maximum number that the instruction generates, then we
;;; signal an error.
;;;
;;; Otherwise, no adaptation is required and we return the desired
;;; results that we recieved and the successor of the instruction. 
(defun adapt-results (desired min-supplied max-supplied successor)
  (cond ((eq desired t)
	 (let ((temps (loop repeat max-supplied collect (p2:new-temporary))))
	   (values temps
		   (make-instance 'p2:put-values-instruction
		     :inputs temps
		     :successors (list successor)))))
	((<= min-supplied (length desired) max-supplied)
	 (values desired successor))
	(t
	 (error "incorrect number of values expected"))))

(defun make-temps (arguments)
  (loop repeat (length arguments) collect (p2:new-temporary)))

(defun compile-arguments (arguments temps successor)
  (loop with succ = successor
	for arg in (reverse arguments)
	for temp in (reverse temps)
	do (setf succ (p2:compile-ast arg (p2:context (list temp) (list succ))))
	finally (return succ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation MEMALLOC.
;;;
;;; Allocate a sequence of bytes of memory.
;;;
;;; Arguments (1):
;;;
;;;   * The number of bytes to be allocated.
;;;
;;; Values (1):
;;;
;;;   * A pointer to the first byte allocated.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convert a MEMALLOC form into a MEMALLOC-AST.

(defclass memalloc-ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql 'memalloc)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (make-instance 'memalloc-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast memalloc-ast) stream)
  (format stream "   ~a [label = \"memalloc\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a MEMALLOC-AST.
;;;
;;; Allow only a context in which the RESULTS has exactly one
;;; element and in which there is a single successor.
;;;
;;; Allowing a results of T would mean that the result of this
;;; AST could be returned from a function, but since the result of
;;; this AST is not a tagged object, but a raw pointer, we cannot
;;; allow it to escape from the lexical locations of the function. 

(defclass memalloc-instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast memalloc-ast) context)
  (with-accessors ((results p2:results)
		   (successors p2:successors))
      context
    (unless (and (listp results)
		 (= (length results) 1)
		 (= (length successors) 1))
      (error "Invalid results for memalloc."))
    (let* ((temps (make-temps (arguments ast)))
	   (instruction (make-instance 'memalloc-instruction
			  :inputs temps
			  :outputs results
			  :successors successors)))
      (compile-arguments (arguments ast) temps instruction))))

(defmethod p2:draw-instruction ((instruction memalloc-instruction) stream)
  (format stream "   ~a [label = \"memalloc\"];~%"
	  (gethash instruction p2:*instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation MEMREF.
;;;
;;; Load a word from memory.
;;; 
;;; Arguments (1):
;;;
;;;   * An address of a word in memory.
;;;
;;; Values (1):
;;;  
;;;   * The contents of memory at that address. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convert a MEMREF form into a MEMREF-AST.

(defclass memref-ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql 'memref)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (make-instance 'memref-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast memref-ast) stream)
  (format stream "   ~a [label = \"memref\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a MEMREF-AST.
;;;

(defclass memref-instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast memref-ast) context)
  (with-accessors ((results p2:results)
		   (successors p2:successors))
      context
    (let* ((temps (make-temps (arguments ast)))
	   (instruction
	     (ecase (length successors)
	       (1 (let ((next (car successors)))
		    (cond ((null results)
			   (warn "MEMREF operation in a context of no results.")
			   next)
			  ((eq results t)
			   (setf next 
				 (make-instance 'p2:put-values-instruction
				   :inputs temps
				   :successors (list next)))
			   (make-instance 'memref-instruction
			     :inputs temps
			     :successors (list next)))
			  (t
			   (setf next (p2:nil-fill (cdr results) next))
			   (make-instance 'memref-instruction
			     :inputs temps
			     :successors (list next))))))
	       (2 (if (eq results t)
		      (error "Illegal context for memref")
		      (let* ((location (if (null results)
					   (p2:new-temporary)
					   (car results)))
			     (next (make-instance 'p2:test-instruction
				     :inputs (list location)
				     :outputs '()
				     :successors successors)))
			(setf next
			      (make-instance 'memref-instruction
				:inputs temps
				:successors (list next)))
			(p2:nil-fill (cdr results) next)))))))
      (compile-arguments (arguments ast) temps instruction))))

(defmethod p2:draw-instruction ((instruction memref-instruction) stream)
  (format stream "   ~a [label = \"memref\"];~%"
	  (gethash instruction p2:*instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation MEMSET.
;;;
;;; Store a word into memory.
;;;
;;; Arguments (2):
;;;
;;;   * An address of a word in memory.
;;;   * A value to store at that address.
;;;
;;; No values.

(defclass memset-ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql 'memset)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 'memset-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast memset-ast) stream)
  (format stream "   ~a [label = \"memset\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

(defclass memset-instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast memset-ast) context)
  (with-accessors ((results p2:results)
		   (successors p2:successors))
      context
    (unless (and (= (length successors) 1)
		 (zerop (length results)))
      (error "Illegal context for memset."))
    (let* ((temps (make-temps (arguments ast)))
	   (instruction
	     (make-instance 'memset-instruction
	       :inputs temps
	       :outputs '()
	       :successors successors)))
      (compile-arguments (arguments ast) temps instruction))))

(defmethod p2:draw-instruction ((instruction memset-instruction) stream)
  (format stream "   ~a [label = \"memset\"];~%"
	  (gethash instruction p2:*instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiling a simple arithmetic operation.

(defun compile-simple-arithmetic (arguments instruction-class context)
  (with-accessors ((results p2:results)
		   (successors p2:successors))
      context
    (let* ((temps (make-temps arguments))
	   (instruction
	     (ecase (length successors)
	       (1 (let ((next (car successors)))
		    (cond ((null results)
			   (warn "Arithmetic operation in a context of no results.")
			   next)
			  ((eq results t)
			   (let ((temp (p2:new-temporary)))
			     (setf next 
				   (make-instance 'p2:put-values-instruction
				     :inputs (list temp)
				     :successors (list next)))
			     (make-instance instruction-class
			       :inputs temps
			       :outputs (list temp)
			       :successors (list next))))
			  (t
			   (setf next (p2:nil-fill (cdr results) next))
			   (make-instance instruction-class
			     :inputs temps
			     :outputs (list (car results))
			     :successors (list next))))))
	       (2 (if (or (eq results t) (> (length results) 1))
		      (error "Illegal context for simple arithmetic.")
		      (make-instance instruction-class
			:inputs temps
			:successors successors))))))
      (compile-arguments arguments temps instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation U+.
;;;
;;; Unsigned integer addition.
;;;
;;; Arguments (2 or 3):
;;;
;;;   * First operand, an unsigned integer.
;;;   * Second operand, an unsigned integer.
;;;   * A Boolean indicating carry in (optional).
;;;
;;; Values (2):
;;;
;;;   * The sum of the first two operands modulo word size
;;;   * A Boolean indicating carry out.

(defclass u+-ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql 'u+)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 3)
  (make-instance 'u+-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast u+-ast) stream)
  (format stream "   ~a [label = \"u+\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

(defclass u+-instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast u+-ast) context)
  (compile-simple-arithmetic (arguments ast) 'u+-instruction context))

(defmethod p2:draw-instruction ((instruction u+-instruction) stream)
  (format stream "   ~a [label = \"u+\"];~%"
	  (gethash instruction p2:*instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation U-.
;;;
;;; Unsigned integer subtraction.
;;;
;;; Arguments (2 or 3):
;;;
;;;   * First operand, an unsigned integer.
;;;   * Second operand, an unsigned integer.
;;;   * A Boolean indicating carry in (optional).
;;;
;;; Values (2):
;;;
;;;   * The difference of the first two operands modulo word size
;;;   * A Boolean indicating carry out.

(defclass u--ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql 'u-)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 3)
  (make-instance 'u--ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast u--ast) stream)
  (format stream "   ~a [label = \"u-\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

(defclass u--instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast u--ast) context)
  (compile-simple-arithmetic (arguments ast) 'u--instruction context))

(defmethod p2:draw-instruction ((instruction u--instruction) stream)
  (format stream "   ~a [label = \"u-\"];~%"
	  (gethash instruction p2:*instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation S+.
;;;
;;; Signed integer addition.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, a signed integer.
;;;   * Second operand, a signed integer.
;;;
;;; Values (2):
;;;
;;;   * The sum of the first two operands modulo word size
;;;   * A Boolean indicating overflow.

(defclass s+-ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql 's+)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 's+-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast s+-ast) stream)
  (format stream "   ~a [label = \"s+\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

(defclass s+-instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast s+-ast) context)
  (compile-simple-arithmetic (arguments ast) 'u--instruction context))

(defmethod p2:draw-instruction ((instruction s+-instruction) stream)
  (format stream "   ~a [label = \"s+\"];~%"
	  (gethash instruction p2:*instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation S-.
;;;
;;; Signed integer subtraction.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, a signed integer.
;;;   * Second operand, a signed integer.
;;;
;;; Values (2):
;;;
;;;   * The difference of the first two operands modulo word size
;;;   * A Boolean indicating overflow.

(defclass s--ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql 's-)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 's--ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast s--ast) stream)
  (format stream "   ~a [label = \"s-\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

(defclass s--instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast s--ast) context)
  (compile-simple-arithmetic (arguments ast) 'u--instruction context))

(defmethod p2:draw-instruction ((instruction s--instruction) stream)
  (format stream "   ~a [label = \"s-\"];~%"
	  (gethash instruction p2:*instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation NEG.
;;;
;;; Arithmetic 2-complement negation.
;;;
;;; Arguments (1):
;;;
;;;   * A signed integer.
;;;
;;; Values (2):
;;;
;;;   * The negative value of the argument. 
;;;   * A Boolean indicating overflow.

(defclass neg-ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql 'neg)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (make-instance 'neg-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast neg-ast) stream)
  (format stream "   ~a [label = \"neg\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

(defclass neg-instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast neg-ast) context)
  (compile-simple-arithmetic (arguments ast) 'u--instruction context))

(defmethod p2:draw-instruction ((instruction neg-instruction) stream)
  (format stream "   ~a [label = \"neg\"];~%"
	  (gethash instruction p2:*instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a logic operation.
;;;
;;; Logic operations are characterized by the fact that they compute a
;;; single value and that this value can not generate an overflow or a
;;; carry.  Therefore, the corresponding instruction must have a
;;; single successor.
;;;
;;; We can not exclude that the result of a logic operation is a
;;; tagged Lisp object, so we must be prepared for all possible
;;; result contexts. 

(defun compile-logic (arguments instruction-class context)
  (with-accessors ((results p2:results)
		   (successors p2:successors))
      context
    (unless (= (length successors) 1)
      (error "Logic operation must have a single successor."))
    (let* ((next (car successors))
	   (temps (make-temps arguments))
	   (instruction
	     (cond ((null results)
		    (warn "Logic operation in a context of no results.")
		    next)
		   ((eq results t)
		    (setf next 
			  (make-instance 'p2:put-values-instruction
			    :inputs temps
			    :successors (list next)))
		    (make-instance instruction-class
		      :inputs temps
		      :successors (list next)))
		   (t
		    (setf next (p2:nil-fill (cdr results) next))
		    (make-instance instruction-class
		      :inputs temps
		      :outputs (list (car results))
		      :successors (list next))))))
      (compile-arguments arguments temps instruction))))

      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation &.
;;;
;;; Bitwise logical AND.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, a word.
;;;   * Second operand, a word.
;;;
;;; Values (1):
;;;
;;;   * The bitwise logical AND between the two words.

(defclass &-ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql '&)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance '&-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast &-ast) stream)
  (format stream "   ~a [label = \"&\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

(defclass &-instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast &-ast) context)
  (compile-logic (arguments ast) '&-instruction context))

(defmethod p2:draw-instruction ((instruction &-instruction) stream)
  (format stream "   ~a [label = \"&\"];~%"
	  (gethash instruction p2:*instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation IOR.
;;;
;;; Bitwise logical inclusive OR.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, a word.
;;;   * Second operand, a word.
;;;
;;; Values (1):
;;;
;;;   * The bitwise logical inclusive OR between the two words.

(defclass ior-ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql 'ior)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 'ior-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast ior-ast) stream)
  (format stream "   ~a [label = \"ior\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

(defclass ior-instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast ior-ast) context)
  (compile-logic (arguments ast) 'ior-instruction context))

(defmethod p2:draw-instruction ((instruction ior-instruction) stream)
  (format stream "   ~a [label = \"ior\"];~%"
	  (gethash instruction p2:*instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation XOR.
;;;
;;; Bitwise logical exclusive OR.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, a word.
;;;   * Second operand, a word.
;;;
;;; Values (1):
;;;
;;;   * The bitwise logical exclusive OR between the two words.

(defclass xor-ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql 'xor)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 'xor-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast xor-ast) stream)
  (format stream "   ~a [label = \"xor\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

(defclass xor-instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast xor-ast) context)
  (compile-logic (arguments ast) 'xor-instruction context))

(defmethod p2:draw-instruction ((instruction xor-instruction) stream)
  (format stream "   ~a [label = \"xor\"];~%"
	  (gethash instruction p2:*instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation ~.
;;;
;;; Bitwise logical inverse.
;;;
;;; Arguments (1):
;;;
;;;   * A word.
;;;
;;; Values (1):
;;;
;;;   * The bitwise logical inverse of the argument.

(defclass ~-ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql '~)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance '~-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast ~-ast) stream)
  (format stream "   ~a [label = \"~\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

(defclass ~-instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast ~-ast) context)
  (compile-logic (arguments ast) '~-instruction context))

(defmethod p2:draw-instruction ((instruction ~-instruction) stream)
  (format stream "   ~a [label = \"~\"];~%"
	  (gethash instruction p2:*instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a test.

(defun make-boolean (boolean result successor)
  (make-instance 'p2:constant-assignment-instruction
    :outputs result
    :constant boolean
    :successors (list successor)))

(defun compile-test (arguments instruction-class context)
  (with-accessors ((results p2:results)
		   (successors p2:successors)
		   (false-required-p p2:false-required-p ))
      context
    (let* ((temps (make-temps arguments))
	   (instruction
	     (ecase (length successors)
	       (1 (let ((next (car successors)))
		    (cond ((null results)
			   (warn "Compilation of a test that is not used.")
			   next)
			  ((eq results t)
			   (let ((temp (p2:new-temporary)))
			     (setf next 
				   (make-instance 'p2:put-values-instruction
				     :inputs (list temp)
				     :successors (list next)))
			     (let ((false (make-boolean nil temp next))
				   (true (make-boolean t temp next)))
			       (make-instance instruction-class
				 :inputs temps
				 :outputs '()
				 :successors (list false true)))))
			  (t
			   (setf next (p2:nil-fill (cdr results) next))
			   (let ((false (make-boolean nil (car results) next))
				 (true (make-boolean t (car results) next)))
			     (make-instance instruction-class
			       :inputs temps
			       :outputs '()
			       :successors (list false true)))))))
	       (2 (if (null results)
		      (make-instance instruction-class
			:inputs temps
			:outputs '()
			:successors successors)
		      (let ((next (make-instance instruction-class
				    :inputs temps
				    :outputs '()
				    :successors successors)))
			(setf next (p2:nil-fill (cdr results) next))
			(let ((false (if false-required-p
					 (make-boolean nil (car results) next)
					 next))
			      (true (make-boolean t (car results) next)))
			  (make-instance instruction-class
			    :inputs temps
			    :outputs '()
			    :successors (list false true)))))))))
      (compile-arguments arguments temps instruction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation ==.
;;;
;;; Word equality.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, a word.
;;;   * Second operand, a word.
;;;
;;; Values (1):
;;;
;;;   * A Boolean indicating whether the two are the same.

(defclass ==-ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql '==)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance '==-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast ==-ast) stream)
  (format stream "   ~a [label = \"==\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

(defclass ==-instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast ==-ast) context)
  (compile-test (arguments ast) '==-instruction context))

(defmethod p2:draw-instruction ((instruction ==-instruction) stream)
  (format stream "   ~a [label = \"==\"];~%"
	  (gethash instruction p2:*instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation s<.
;;;
;;; Signed integer less than.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, a signed integer.
;;;   * Second operand, a signed integer.
;;;
;;; Values (1):
;;;
;;;   * A Boolean indicating whether the first is less than the second.

(defclass s<-ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql 's<)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 's<-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast s<-ast) stream)
  (format stream "   ~a [label = \"s<\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

(defclass s<-instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast s<-ast) context)
  (compile-test (arguments ast) 's<-instruction context))

(defmethod p2:draw-instruction ((instruction s<-instruction) stream)
  (format stream "   ~a [label = \"s<\"];~%"
	  (gethash instruction p2:*instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation s<=.
;;;
;;; Signed integer less than or equal.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, a signed integer.
;;;   * Second operand, a signed integer.
;;;
;;; Values (1):
;;;
;;;   * A Boolean indicating whether the first is less than or equal
;;;     to the second. 

(defclass s<=-ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql 's<=)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 's<=-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast s<=-ast) stream)
  (format stream "   ~a [label = \"s<=\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

(defclass s<=-instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast s<=-ast) context)
  (compile-test (arguments ast) 's<=-instruction context))

(defmethod p2:draw-instruction ((instruction s<=-instruction) stream)
  (format stream "   ~a [label = \"s<=\"];~%"
	  (gethash instruction p2:*instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation u<.
;;;
;;; Unsigned integer less than.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, an unsigned integer.
;;;   * Second operand, an unsigned integer.
;;;
;;; Values (1):
;;;
;;;   * A Boolean indicating whether the first is less than the second.

(defclass u<-ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql 'u<)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 'u<-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast u<-ast) stream)
  (format stream "   ~a [label = \"u<\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

(defclass u<-instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast u<-ast) context)
  (compile-test (arguments ast) 'u<-instruction context))

(defmethod p2:draw-instruction ((instruction u<-instruction) stream)
  (format stream "   ~a [label = \"u<\"];~%"
	  (gethash instruction p2:*instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word operation u<=.
;;;
;;; Unsigned integer less than or equal.
;;;
;;; Arguments (2):
;;;
;;;   * First operand, an unsigned integer.
;;;   * Second operand, an unsigned integer.
;;;
;;; Values (1):
;;;
;;;   * A Boolean indicating whether the first is less than or equal
;;;     to the second. 

(defclass u<=-ast (p1:ast arguments-mixin)
  ())

(defmethod p1:convert-compound ((symbol (eql 'u<=)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 'u<=-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod p1:draw-ast ((ast u<=-ast) stream)
  (format stream "   ~a [label = \"u<=\"];~%"
	  (p1:id ast))
  (loop for argument-ast in (arguments ast)
	do (p1:draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (p1:id ast) (p1:id argument-ast))))

(defclass u<=-instruction (p2:instruction)
  ())

(defmethod p2:compile-ast ((ast u<=-ast) context)
  (compile-test (arguments ast) 'u<=-instruction context))

(defmethod p2:draw-instruction ((instruction u<=-instruction) stream)
  (format stream "   ~a [label = \"u<=\"];~%"
	  (gethash instruction p2:*instruction-table*)))
