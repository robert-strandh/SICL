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

(defun source/destination-operand (location)
  (format nil "[ebx + ~a]" (* 4 (1+ (index location)))))

(defgeneric load-input (input))

(defmethod load-input ((input sicl-mir:immediate-input))
  `(,(format nil "	mov eax, ~a	; Load immediate."
	     (sicl-mir:value input))))

(defmethod load-input ((input sicl-mir:external-input))
  `("	mov eax, [ebp - 8]"
    ,(format nil "	mov eax, [eax + ~a]	; Load external."
	     (* 4 (index input)))))

(defun load-from-argument (i)
  `(,(format nil "	mov eax, [edb + ~a]	; Load argument."
	     (* 4 i))))

(defun save-to-argument (i)
  `(,(format nil "	mov [edb + ~a], eax	; Save to argument."
	     (* 4 i))))

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

(defmethod codegen-instruction
    ((instruction sicl-mir:return-instruction))
  `(";;; Start of RETURN instruction."
    ,@(loop for input in (inputs instruction)
	    for i from 0
	    append (load-input input)
	    append (save-to-argument i))
    "	ret"    
    ";;; End of RETURN instruction."))

(defun save-function ()
  `("	mov [ebp - 4], ecx"))

;;; Push the arguments in reverse order. 
(defun push-arguments ()
  (let ((label1 (gensym))
	(label2 (gensym)))
    `(";;; Push all arguments to stack."
      "	shl ebx, 2	; Number of args times size of arg."
      ,(format nil "~a:" label1)
      "	cmp ebx, 0	; Any arguments left?"
      ,(format nil "	jz, ~a	; No.  Go!" label2)
      "	sub ebx, 4	; Yes, find the last of the remaining."
      "	push [eax + ebx]	; Push it on the stack."
      ,(format nil "	jump ~a	; Loop." label1)
      ,(format nil "~a:		; No more arguments to push." label2)
      ";;; Finished pushing all arguments to stack.")))

(defmethod codegen-instruction
    ((instruction sicl-mir:get-values-instruction))
  `(";;; Start of GET-VALUES instruction."
    ,@(loop for output in (outputs instruction)
	    for i from 0
	    append (load-from-argument i)
	    append (save-result output))
    ";;; End of GET-VALUES instruction."
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:funcall-instruction))
  `(";;; Start of FUNCALL instruction."
    ,@(load-input (car (inputs instruction)))
    ;; Store function object for the callee to access.
    ;; FIXME: probably wrong.
    ,(format nil "	mov [~a], eax	; Store callee." (+ (ash 1 30) 12))
    "	mov eax, [eax + 1]	; Load rack of function."
    "	mov ebx, [eax + 8]	; Load the entry-point offset."
    "	mov eax, [eax]	; Load the code object."
    "	mov eax, [eax + 1]	; Load rack of the code object."
    "	mov eax, [eax]	; Load the start of the code."
    "	add eax, ebx	; Add the entry-point offset."
    "	call eax	; Call the entry point."
    ";;; Ennd of FUNCALL instruction."
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:enclose-instruction))
  `("	enclose	; FIXME: wrong"
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:nop-instruction))
  (codegen-instruction (car (successors instruction))))

(defmethod codegen-instruction
    ((instruction sicl-mir:assignment-instruction))
  `(";;; Start of assignment instruction."
    ,@(load-input (car (inputs instruction)))
    ,@(save-result (car (outputs instruction)))
    ";;; End of assignment instruction."
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:memref-instruction))
  `(";;; Start of MEMREF instruction."
    ,@(load-input (car (inputs instruction)))
    "	mov eax, [eax]	; Memref."
    ,@(save-result (car (outputs instruction)))
    ";;; End of MEMREF instruction."
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:memset-instruction))
  `(";;; Start of MEMSET instruction."
    ,@(load-input (cadr (inputs instruction)))
    "	mov ebx, eax	; Move object to store."
    ,@(load-input (car (inputs instruction)))
    "	mov [eax], ebx	; Store the object."
    ";;; End of MEMSET instruction."
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:u+-instruction))
  `(";;; Start of u+ instruction."
    ,@(load-input (cadr (inputs instruction)))
    "	mov ebx eax	; Second argument of u+."
    ,@(load-input (car (inputs instruction)))
    "	add eax ebx	; Compute u+ of first and second argument."
    ,@(save-result (car (outputs instruction)))
    ";;; End of u+ instruction."
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:u--instruction))
  `(";;; Start of u- instruction."
    ,@(load-input (cadr (inputs instruction)))
    "	mov ebx, eax	; Second argument of u-."
    ,@(load-input (car (inputs instruction)))
    "	sub eax, ebx	; Compute u- of first and second argument."
    ,@(save-result (car (outputs instruction)))
    ";;; End of u- instruction."
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:s+-instruction))
  `(";;; Start of s+ instruction."
    ,@(load-input (cadr (inputs instruction)))
    "	mov ebx, eax	; Second argument of s+."
    ,@(load-input (car (inputs instruction)))
    "	add eax, ebx	; Copute s+ of first and second argument."
    ,@(save-result (car (outputs instruction)))
    ";;; End of s+ instruction."
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:s--instruction))
  `(";;; Start of s- instruction."
    ,@(load-input (cadr (inputs instruction)))
    "	mov ebx, eax	; Second argument of s-."
    ,@(load-input (car (inputs instruction)))
    "	sub eax, ebx	; Compute s- of first and second argument."
    ,@(save-result (car (outputs instruction)))
    ";;; End of s- instruction."
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:neg-instruction))
  `(";;; Start of neg instruction."
    ,@(load-input (car (inputs instruction)))
    "	neg eax	; Compute negation of argument."
    ,@(save-result (car (outputs instruction)))
    ";;; End of neg instruction."
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:&-instruction))
  `(";;; Start of & instruction."
    ,@(load-input (cadr (inputs instruction)))
    "	mov ebx, eax	; Second argument of &."
    ,@(load-input (car (inputs instruction)))
    "	and eax, ebx	; Compute & of first and second argument."
    ,@(save-result (car (outputs instruction)))
    ";;; End of & instruction."
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:ior-instruction))
  `(";;; Start of ior instruction."
    ,@(load-input (cadr (inputs instruction)))
    "	mov ebx, eax	; Second argument of ior."
    ,@(load-input (car (inputs instruction)))
    "	or eax, ebx	; Compute ior of first and second argument."
    ,@(save-result (car (outputs instruction)))
    ";;; End of ior instruction."
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:xor-instruction))
  `(";;; Start of xor instruction."
    ,@(load-input (cadr (inputs instruction)))
    "	mov ebx, eax	; Second argument of xor."
    ,@(load-input (car (inputs instruction)))
    "	xor eax, ebx	; Compute xor of first and second argument."
    ,@(save-result (car (outputs instruction)))
    ";;; End of xor instruction."
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:~-instruction))
  `(";;; Start of ~ instruction."
    ,@(load-input (car (inputs instruction)))
    "	not eax	; Compute logical not of argument."
    ,@(save-result (car (outputs instruction)))
    ";;; End of ~ instruction."
    ,@(codegen-instruction (car (successors instruction)))))

(defmethod codegen-instruction
    ((instruction sicl-mir:==-instruction))
  (let ((label (gensym))
	(else-branch (codegen-instruction (cadr (successors instruction))))
	(then-branch (codegen-instruction (car (successors instruction)))))
    `(";;; Start of == instruction."
      ,@(load-input (cadr (inputs instruction)))
      "	mov ebx, eax	; Second argument of ==."
      ,@(load-input (car (inputs instruction)))
      "	cmp eax, ebx	; Compare first and second argument of ==."
      ,(format nil "	jne ~a	; Go if not ==." label)
      ";;; Start of THEN branch."
      ,@then-branch
      ";;; End of THEN branch."
      ,(format nil "~a:		; Come here if not ==." label)
      ";;; Start of ELSE branch."
      ,@else-branch
      ";;; End of ELSE branch.")))
	  
(defmethod codegen-instruction
    ((instruction sicl-mir:s<-instruction))
  (let ((label (gensym))
	(else-branch (codegen-instruction (cadr (successors instruction))))
	(then-branch (codegen-instruction (car (successors instruction)))))
    `(";;; Start of s< instruction."
      ,@(load-input (cadr (inputs instruction)))
      "	mov ebx, eax	; Second argument of s<."
      ,@(load-input (car (inputs instruction)))
      "	cmp eax, ebx	; Compare first and second argument of s<."
      ,(format nil "	jge ~a	; Go if not s<." label)
      ";;; Start of THEN branch."
      ,@then-branch
      ";;; End of THEN branch."
      ,(format nil "~a:		; Come here if not s<." label)
      ";;; Start of ELSE branch."
      ,@else-branch
      ";;; End of ELSE branch.")))
	  
(defmethod codegen-instruction
    ((instruction sicl-mir:s<=-instruction))
  (let ((label (gensym))
	(else-branch (codegen-instruction (cadr (successors instruction))))
	(then-branch (codegen-instruction (car (successors instruction)))))
    `(";;; Start of s<= instruction."
      ,@(load-input (cadr (inputs instruction)))
      "	mov ebx, eax	; Second argument of s<=."
      ,@(load-input (car (inputs instruction)))
      "	cmp eax, ebx	; Compare first and second argument of s<=."
      ,(format nil "	jg ~a	; Go if not s<=." label)
      ";;; Start of THEN branch."
      ,@then-branch
      ";;; End of THEN branch."
      ,(format nil "~a:		; Come here if not s<=." label)
      ";;; Start of ELSE branch."
      ,@else-branch
      ";;; End of ELSE branch.")))
	  
(defmethod codegen-instruction
    ((instruction sicl-mir:u<-instruction))
  (let ((label (gensym))
	(else-branch (codegen-instruction (cadr (successors instruction))))
	(then-branch (codegen-instruction (car (successors instruction)))))
    `(";;; Start of u< instruction."
      ,@(load-input (cadr (inputs instruction)))
      "	mov ebx, eax	; Second argument of u<."
      ,@(load-input (car (inputs instruction)))
      "	cmp eax, ebx	; Compare first and second argument of u<."
      ,(format nil "	jnb ~a	; Go if not u<." label)
      ";;; Start of THEN branch."
      ,@then-branch
      ";;; End of THEN branch."
      ,(format nil "~a:		; Come here if not u<." label)
      ";;; Start of ELSE branch."
      ,@else-branch
      ";;; End of ELSE branch.")))
	  
(defmethod codegen-instruction
    ((instruction sicl-mir:u<=-instruction))
  (let ((label (gensym))
	(else-branch (codegen-instruction (cadr (successors instruction))))
	(then-branch (codegen-instruction (car (successors instruction)))))
    `(";;; Start of u<= instruction."
      ,@(load-input (cadr (inputs instruction)))
      "	mov ebx, eax	; Second argument of u<=."
      ,@(load-input (car (inputs instruction)))
      "	cmp eax, ebx	; Compare first and second argument of u<=."
      ,(format nil "	ja ~a	; Go if not u<=." label)
      ";;; Start of THEN branch."
      ,@then-branch
      ";;; End of THEN branch."
      ,(format nil "~a:		; Come here if not u<=." label)
      ";;; Start of ELSE branch."
      ,@else-branch
      ";;; End of ELSE branch.")))
	  
(defun codegen-procedure (procedure)
  (codegen-instruction (initial-instruction procedure)))

(defun codegen (program)
  (let ((*program* program)
	(*labels* (make-hash-table :test #'eq)))
    (loop for procedure in (procedures program)
	  append (codegen-procedure procedure))))
