(defpackage #:sicl-word
  (:use #:common-lisp)
  (:import-from #:sicl-compiler-phase-1
		#:convert
		#:convert-compound
		#:ast
		#:draw-ast #:id)
  (:import-from #:sicl-compiler-phase-2
		#:compile-ast
		#:instruction
		#:no-successor-instruction
		#:single-successor-instruction
		#:dual-successor-instruction
		#:multiple-successor-instruction
		#:new-temporary)
  (:export
   #:memalloc #:memref #:memset
   #:u+ #:u- #:s+ #:s- #:neg
   #:u* #:s*
   #:lshift #:ashift
   #:& #:ior #:xor #:~
   #:== #:s< #:s<= #:u< #:u<=
   ))

(in-package #:sicl-word)

(defclass arguments-mixin ()
  ((%arguments :initarg :arguments :accessor arguments)))

(defun convert-arguments (arguments env)
  (loop for argument in arguments
	collect (convert argument env)))

(defclass in-out-mixin ()
  ((%inputs :initarg :inputs :reader inputs)
   (%outputs :initarg :outputs :reader outputs)))

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

(defclass memalloc-ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql 'memalloc)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (make-instance 'memalloc-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast memalloc-ast) stream)
  (format stream "   ~a [label = \"memalloc\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass memalloc-instruction (single-successor-instruction in-out-mixin)
  ())

(defmethod compile-ast ((ast memalloc-ast) value-context successor)
  (unless (and (list value-context)
	       (= (length value-context) 1))
    (error "invalid value context"))
  (let ((temp (new-temporary)))
    (let ((instruction (make-instance 'memalloc-instruction
				      :inputs (list temp)
				      :outputs (list (car value-context))
				      :next successor)))
      (compile-ast (car (arguments ast)) (list temp) instruction))))

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

(defclass memref-ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql 'memref)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (make-instance 'memref-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast memref-ast) stream)
  (format stream "   ~a [label = \"memref\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass memref-instruction (single-successor-instruction in-out-mixin)
  ())

(defmethod compile-ast ((ast memref-ast) value-context successor)
  (unless (and (list value-context)
	       (= (length value-context) 1))
    (error "invalid value context"))
  (let ((temp (new-temporary)))
    (let ((instruction (make-instance 'memref-instruction
				      :inputs (list temp)
				      :outputs (list (car value-context))
				      :next successor)))
      (compile-ast (car (arguments ast)) (list temp) instruction))))

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

(defclass memset-ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql 'memset)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 'memset-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast memset-ast) stream)
  (format stream "   ~a [label = \"memset\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass memset-instruction (single-successor-instruction in-out-mixin)
  ())

(defmethod compile-ast ((ast memset-ast) value-context successor)
  (unless (null value-context)
    (error "invalid value context"))
  (let ((temp1 (new-temporary))
	(temp2 (new-temporary)))
    (let ((instruction (make-instance 'memset-instruction
				      :inputs (list temp1 temp2)
				      :outputs '()
				      :next successor)))
      (compile-ast (car (arguments ast))
		   (list temp1)
		   (compile-ast (cadr (arguments ast))
				(list temp2)
				instruction)))))

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

(defclass u+-ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql 'u+)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 3)
  (make-instance 'u+-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast u+-ast) stream)
  (format stream "   ~a [label = \"u+\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass u+-instruction (single-successor-instruction in-out-mixin)
  ())

(defmethod compile-ast ((ast u+-ast) value-context successor)
  (unless (and (list value-context)
	       (member (length value-context) '(1 2)))
    (error "invalid value context"))
  (let ((temp1 (new-temporary))
	(temp2 (new-temporary)))
    (let ((instruction (make-instance 'u+-instruction
				      :inputs (list temp1 temp2)
				      :outputs (car value-context)
				      :next successor)))
      (compile-ast (car (arguments ast))
		   (list temp1)
		   (compile-ast (cadr (arguments ast))
				(list temp2)
				instruction)))))

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

(defclass u--ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql 'u-)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 3)
  (make-instance 'u--ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast u--ast) stream)
  (format stream "   ~a [label = \"u-\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass u--instruction (single-successor-instruction in-out-mixin)
  ())

(defmethod compile-ast ((ast u--ast) value-context successor)
  (unless (and (list value-context)
	       (member (length value-context) '(1 2)))
    (error "invalid value context"))
  (let ((temp1 (new-temporary))
	(temp2 (new-temporary)))
    (let ((instruction (make-instance 'u--instruction
				      :inputs (list temp1 temp2)
				      :outputs (car value-context)
				      :next successor)))
      (compile-ast (car (arguments ast))
		   (list temp1)
		   (compile-ast (cadr (arguments ast))
				(list temp2)
				instruction)))))

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

(defclass s+-ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql 's+)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 's+-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast s+-ast) stream)
  (format stream "   ~a [label = \"s+\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass s+-instruction (single-successor-instruction in-out-mixin)
  ())

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

(defclass s--ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql 's-)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 's--ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast s--ast) stream)
  (format stream "   ~a [label = \"s-\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass s--instruction (single-successor-instruction in-out-mixin)
  ())

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

(defclass neg-ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql 'neg)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 1 1)
  (make-instance 'neg-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast neg-ast) stream)
  (format stream "   ~a [label = \"neg\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass neg-instruction (single-successor-instruction in-out-mixin)
  ())

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

(defclass &-ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql '&)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance '&-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast &-ast) stream)
  (format stream "   ~a [label = \"&\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass &-instruction (single-successor-instruction in-out-mixin)
  ())

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

(defclass ior-ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql 'ior)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 'ior-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast ior-ast) stream)
  (format stream "   ~a [label = \"ior\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass ior-instruction (single-successor-instruction in-out-mixin)
  ())

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

(defclass xor-ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql 'xor)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 'xor-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast xor-ast) stream)
  (format stream "   ~a [label = \"xor\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass xor-instruction (single-successor-instruction in-out-mixin)
  ())

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

(defclass ~-ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql '~)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance '~-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast ~-ast) stream)
  (format stream "   ~a [label = \"~\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass ~-instruction (single-successor-instruction in-out-mixin)
  ())

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

(defclass ==-ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql '==)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance '==-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast ==-ast) stream)
  (format stream "   ~a [label = \"==\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass ==-instruction (single-successor-instruction in-out-mixin)
  ())

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

(defclass s<-ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql 's<)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 's<-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast s<-ast) stream)
  (format stream "   ~a [label = \"s<\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass s<-instruction (single-successor-instruction in-out-mixin)
  ())

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

(defclass s<=-ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql 's<=)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 's<=-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast s<=-ast) stream)
  (format stream "   ~a [label = \"s<=\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass s<=-instruction (single-successor-instruction in-out-mixin)
  ())

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

(defclass u<-ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql 'u<)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 'u<-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast u<-ast) stream)
  (format stream "   ~a [label = \"u<\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass u<-instruction (single-successor-instruction in-out-mixin)
  ())

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

(defclass u<=-ast (ast arguments-mixin)
  ())

(defmethod convert-compound ((symbol (eql 'u<=)) form env)
  (sicl-code-utilities:check-form-proper-list form)
  (sicl-code-utilities:check-argcount form 2 2)
  (make-instance 'u<=-ast
		 :arguments (convert-arguments (cdr form) env)))

(defmethod draw-ast ((ast u<=-ast) stream)
  (format stream "   ~a [label = \"u<=\"];~%"
	  (id ast))
  (loop for argument-ast in (arguments ast)
	do (draw-ast argument-ast stream)
	   (format stream "   ~a -> ~a~%" (id ast) (id argument-ast))))

(defclass u<=-instruction (single-successor-instruction in-out-mixin)
  ())

