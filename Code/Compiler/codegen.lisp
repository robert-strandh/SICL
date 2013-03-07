(in-package #:sicl-program)

;;;; Code generator for x86. 
;;;;
;;;; The generated code is incredibly slow and wasteful, but it will
;;;; allow us to get started with a native system.
;;;;
;;;; Step one of the executing function is to establish a stack frame,
;;;; by copying ESP to EBP and subtracting enough from EBP to
;;;; establish a stack frame.  The stack frame is of fixed size.  It
;;;; contains the following items:
;;;; 
;;;;   [ebp - 4]  contains the executing function object. 
;;;;   [ebp - 8]  contains the static environment.
;;;;   [ebp - 12] contains the dynamic environment.
;;;;
;;;; Next, the executing functions saves the executing function object
;;;; on the stack from its fixed location in memory.
;;;;
;;;; The executing function is going to need to call other functions
;;;; to build its environment, so it must save all the arguments that
;;;; were given to it.  It saves them on the stack by looping as many
;;;; times as there are arguments in argument/value count and pushing
;;;; each argument on the stack.
;;;;
;;;; The next step for the excuting function is to build its own
;;;; lexical environment.  It does this by accessing the lexical
;;;; environment stored in the function object.  This environment is
;;;; an ordinary list, and the executing function adds a level to the
;;;; front of it.  The level is a Lisp vector.  First, a level is
;;;; allocated by a call to MAKE-VECTOR.  The argument passed to the
;;;; number of elements required represented as a fixnum.  Next, that
;;;; vector and the saved environment are passed as arguments to CONS.
;;;; The resulting environment is stored on the stack in [ebp - 8].
;;;; Clearly, MAKE-VECTOR and CONS can not use this convention
;;;; themselves.
;;;;
;;;; Next, the executing function accesses the arguments that were
;;;; previously pushed on the stack, and pops them to the freshly
;;;; allocated lexical environment.  From then on, all lexical values
;;;; are accessed from the lexical environment.
;;;; 
;;;; If a callee returns zero values, then it will still set the first
;;;; element in the argument/values vector to NIL so that a caller
;;;; that requires exactly one value can always get that value without
;;;; checking the argument/value count. 
;;;;
;;;; Each low-level operation used EAX and EBX.  The arguments are
;;;; loaded into these registers, and the result is saved from EAX to
;;;; its lexical location. 

(defparameter *labels* nil)
(defparameter *depth* nil)

(defun load-level-location (location)
  (let ((count (- *depth* (lexical-depth (owner location)))))
    `(;; Load the lexical environment from the stack.
      "	move ebx, [ebp - 4]"
      ;; The environment is a list of levels.  Take the CDR as many
      ;; times as necessary to get to the right CONS cell of the list.
      ,@(loop repeat count
	      collect "	move ebx, [ebx + 3]")
      ;; Take the CAR to get to the level. 
      "	move ebx, [ebx - 1]"
      ;; The level is a heap object.  Load the contents vector. 
      "	move ebx, [ebx + 1]")))

(defun source/destination-operand (location)
  (format nil "[ebx + ~a]" (* 4 (1+ (index location)))))

(defgeneric load-input (input))

(defmethod load-input ((input sicl-env:lexical-location))
  (let ((count (- *depth* (lexical-depth (owner input)))))
    `(;; Load the lexical environment from the stack.
      "	move eax, [ebp - 4]"
      ;; The environment is a list of levels.  Take the CDR as many
      ;; times as necessary to get to the right CONS cell of the list.
      ,@(loop repeat count
	      collect "	move eax, [eax + 3]")
      ;; Take the CAR to get to the level. 
      "	move eax, [eax - 1]"
      ;; The level is a heap object.  Load the contents vector. 
      "	move eax, [eax + 1]"
      ;; Load the appropriate element from the contents vector.
      ,(format nil "	move eax, [eax + ~a]"
	       (* 4 (1+ (index input)))))))

(defmethod load-input ((input sicl-mir:immediate-input))
  `(,(format nil "	move eax, ~a"
	     (sicl-mir:value input))))

(defmethod load-input ((input sicl-mir:external-input))
  `("	move eax, [ebp - 8]"
    ,(format nil "	move eax, [eax + ~a]"
	     (* 4 (index input)))))

(defun load-from-argument (i)
  `(,(format nil "	move eax, [edb + ~a]"
	     (* 4 i))))

(defun save-to-argument (i)
  `(,(format nil "	move [edb + ~a], eax"
	     (* 4 i))))

(defun save-result (location)
  (let ((count (- *depth* (lexical-depth (owner location)))))
    `(;; Load the lexical environment from the stack.
      "	move ebx, [ebp - 4]"
      ;; The environment is a list of levels.  Take the CDR as many
      ;; times as necessary to get to the right CONS cell of the list.
      ,@(loop repeat count
	      collect "	move ebx, [ebx + 3]")
      ;; Take the CAR to get to the level. 
      "	move ebx, [ebx - 1]"
      ;; The level is a heap object.  Load the contents vector. 
      "	move ebx, [ebx + 1]"
      ;; Load the appropriate element from the contents vector.
      ,(format nil "	move [ebx + ~a], eax"
	       (* 4 (1+ (index location)))))))

(defgeneric codegen-instruction (instruction))

(defmethod codegen-instruction :around (instruction)
  (let ((label (gethash instruction *labels*)))
    (if (null label)
	(if (> (length (predecessors instruction)) 1)
	    (progn (setf label (gensym))
		   (setf (gethash instruction *labels*) label)
		   (cons label (call-next-method)))
	    (progn (setf (gethash instruction *labels*) t)
		   (call-next-method)))
	`(,(format nil "	jump ~a"
		   (gethash instruction *labels*))))))

(defmethod codegen-instruction (instruction)
  nil)

;;; FIXME: wrong
(defmethod codegen-instruction
    ((instruction sicl-mir:enter-instruction))
  (codegen-instruction (car (successors instruction))))

(defmethod codegen-instruction
    ((instruction sicl-mir:return-instruction))
  '("	ret"))

(defun save-function ()
  `("	move [ebp - 4], ecx"))

;;; Push the arguments in reverse order. 
(defun push-arguments ()
  (let ((label1 (gensym))
	(label2 (gensym)))
    `("	shl ebx, 2"
      ,(format nil "~a:" label1)
      "	cmp ebx, 0"
      ,(format nil "	jz, ~a" label2)
      "	sub ebx, 4"
      "	push [eax + ebx]"
      ,(format nil "	jump ~a" label1)
      ,(format nil "~a:" label2))))

(defmethod codegen-instruction
    ((instruction sicl-mir:get-arguments-instruction))
  (append (loop for output in (outputs instruction)
		for i from 0
		append (load-from-argument i)
		append (save-result output))
	  (codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:put-values-instruction))
  (append (loop for input in (inputs instruction)
		for i from 0
		append (load-input input)
		append (save-to-argument i))
	  (codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:enclose-instruction))
  `(;; FIXME: wrong
    "	enclose"
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:nop-instruction))
  (codegen-instruction (car (successors instruction))))

(defmethod codegen-instruction
    ((instruction sicl-mir:assignment-instruction))
  `(,@(load-input (car (inputs instruction)))
    ,@(save-result (car (outputs instruction)))
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:memref-instruction))
  `(,@(load-input (car (inputs instruction)))
    "	move eax, [eax]"
    ,@(save-result (car (outputs instruction)))
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:memset-instruction))
  `(,@(load-input (cadr (inputs instruction)))
    "	move ebx, eax"
    ,@(load-input (car (inputs instruction)))
    "	move [eax], ebx"
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:u+-instruction))
  `(,@(load-input (cadr (inputs instruction)))
    "	move ebx eax"
    ,@(load-input (car (inputs instruction)))
    "	add eax ebx"
    ,@(save-result (car (outputs instruction)))
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:u--instruction))
  `(,@(load-input (cadr (inputs instruction)))
    "	move ebx, eax"
    ,@(load-input (car (inputs instruction)))
    "	sub eax, ebx"
    ,@(save-result (car (outputs instruction)))
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:s+-instruction))
  `(,@(load-input (cadr (inputs instruction)))
    "	move ebx, eax"
    ,@(load-input (car (inputs instruction)))
    "	add eax, ebx"
    ,@(save-result (car (outputs instruction)))
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:s--instruction))
  `(,@(load-input (cadr (inputs instruction)))
    "	move ebx, eax"
    ,@(load-input (car (inputs instruction)))
    "	sub eax, ebx"
    ,@(save-result (car (outputs instruction)))
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:neg-instruction))
  `(,@(load-input (car (inputs instruction)))
    "	neg eax"
    ,@(save-result (car (outputs instruction)))
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:&-instruction))
  `(,@(load-input (cadr (inputs instruction)))
    "	move ebx, eax"
    ,@(load-input (car (inputs instruction)))
    "	and eax, ebx"
    ,@(save-result (car (outputs instruction)))
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:ior-instruction))
  `(,@(load-input (cadr (inputs instruction)))
    "	move ebx, eax"
    ,@(load-input (car (inputs instruction)))
    "	or eax, ebx"
    ,@(save-result (car (outputs instruction)))
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:xor-instruction))
  `(,@(load-input (cadr (inputs instruction)))
    "	move ebx, eax"
    ,@(load-input (car (inputs instruction)))
    "	xor eax, ebx"
    ,@(save-result (car (outputs instruction)))
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:~-instruction))
  `(,@(load-input (car (inputs instruction)))
    "	not eax"
    ,@(save-result (car (outputs instruction)))
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:==-instruction))
  (let ((label (gensym))
	(else-branch (codegen-instruction (cadr (successors instruction))))
	(then-branch (codegen-instruction (car (successors instruction)))))
    `(,@(load-input (cadr (inputs instruction)))
      "	move ebx, eax"
      ,@(load-input (car (inputs instruction)))
      "	cmp eax, ebx"
      ,(format nil "	jne ~a" label)
      ,@then-branch
      ,(format nil "~a:" label)
      ,@else-branch)))
	  
(defmethod codegen-instruction
    ((instruction sicl-mir:s<-instruction))
  (let ((label (gensym))
	(else-branch (codegen-instruction (cadr (successors instruction))))
	(then-branch (codegen-instruction (car (successors instruction)))))
    `(,@(load-input (cadr (inputs instruction)))
      "	move ebx, eax"
      ,@(load-input (car (inputs instruction)))
      "	cmp eax, ebx"
      ,(format nil "	jge ~a" label)
      ,@then-branch
      ,(format nil "~a:" label)
      ,@else-branch)))
	  
(defmethod codegen-instruction
    ((instruction sicl-mir:s<=-instruction))
  (let ((label (gensym))
	(else-branch (codegen-instruction (cadr (successors instruction))))
	(then-branch (codegen-instruction (car (successors instruction)))))
    `(,@(load-input (cadr (inputs instruction)))
      "	move ebx, eax"
      ,@(load-input (car (inputs instruction)))
      "	cmp eax, ebx"
      ,(format nil "	jg ~a" label)
      ,@then-branch
      ,(format nil "~a:" label)
      ,@else-branch)))
	  
(defmethod codegen-instruction
    ((instruction sicl-mir:u<-instruction))
  (let ((label (gensym))
	(else-branch (codegen-instruction (cadr (successors instruction))))
	(then-branch (codegen-instruction (car (successors instruction)))))
    `(,@(load-input (cadr (inputs instruction)))
      "	move ebx, eax"
      ,@(load-input (car (inputs instruction)))
      "	cmp eax, ebx"
      ,(format nil "	jnb ~a" label)
      ,@then-branch
      ,(format nil "~a:" label)
      ,@else-branch)))
	  
(defmethod codegen-instruction
    ((instruction sicl-mir:u<=-instruction))
  (let ((label (gensym))
	(else-branch (codegen-instruction (cadr (successors instruction))))
	(then-branch (codegen-instruction (car (successors instruction)))))
    `(,@(load-input (cadr (inputs instruction)))
      "	move ebx, eax"
      ,@(load-input (car (inputs instruction)))
      "	cmp eax, ebx"
      ,(format nil "	ja ~a" label)
      ,@then-branch
      ,(format nil "~a:" label)
      ,@else-branch)))
	  
(defun codegen-procedure (procedure)
  (let ((*depth* (lexical-depth procedure)))
    (codegen-instruction (initial-instruction procedure))))

(defun codegen (program)
  (let ((*program* program)
	(*labels* (make-hash-table :test #'eq)))
    (loop for procedure in (procedures program)
	  append (codegen-procedure procedure))))