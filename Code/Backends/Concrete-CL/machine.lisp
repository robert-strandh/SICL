(cl:in-package #:sicl-machine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Virtual machine organization

;;; We use a fixed number of registers to pass arguments to functions.
;;; The same register file is used to pass return values.  The variable
;;; *NUMBER-OF-ARGUMENTS* contains the current number of registers
;;; that have significant values in them.  When a function returns
;;; zero arguments, it nevertheless puts the symbol NIL in register 0,
;;; so that callers that need exactly one return value can count on
;;; register 0 containing what they need.
(defparameter *max-number-of-arguments* 100)
(defparameter *arguments* (make-array *max-number-of-arguments*))
(defparameter *number-of-arguments* 0)

;;; The program counter contains the address, as a machine word,
;;; of the next instruction to be executed. 
(defunbound *program-counter*)

;;; This variable contains the old program counter after a
;;; jump-and-link instruction has been executed.  The return
;;; instruction moves the contents of this variable to the program
;;; counter.
(defunbound *program-counter-save*)

;;; This variable contains the start address of a sequence of
;;; consecutive words that are used by the current instructions to
;;; access constants.  The symbol NIL is always the first constant
;;; (offset zero) in this sequence. 
(defunbound *constants*)

;;; The operand stack is what we use instead of general registers.  It
;;; is used for arithmetic and logic operations, but also as an
;;; intermediate location for moving data around.  The operand stack
;;; is relative to the current dynamic frame, so it is saved when a
;;; function is called and restored when a function returns.  The
;;; operand stack is represented as a list (of CONS cells) containing
;;; machine words.
(defunbound *operand-stack*)

;;; The call stack is a list (of CONS cells) of dynamic frames.
;;; Typically when a function is called, a new frame is pushed, and
;;; when a function returns, the old frame is restored.  Each frame
;;; also contains information about bindings for special variables,
;;; catch tags, etc., so that a full context (except for the program
;;; counter and the operand stack which have to be restored
;;; "manually") is restored when a stack is modified. 
(defunbound *call-stack*)

;;; This register holds the carry bit used in arithmetic operations
(defparameter *carry* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; VM operations

(defun next-op ()
  (prog1 (memref *program-counter*)
    (incf *program-counter* +word-size-in-bytes+)))

(defparameter *trace-operations* nil)

(defun optrace (control &rest args)
  (when *trace-operations*
    (apply #'format *trace-output* control args)))

(defmacro defop (name in-ops out-ops args &body body)
  `(progn
     (asm:define-operation
	 ',name
       ,(length args)
       (lambda ()
	 (optrace "Executing operation: ~s~%" ',name)
	 (let ,(append (loop for arg in args
			     collect `(,arg (next-op)))
		       (loop for in-op in (reverse in-ops)
			     collect `(,in-op (prog1 (scl:car *operand-stack*)
						(setf *operand-stack*
						      (scl:cdr *operand-stack*)))))
		       (loop for out-op in (set-difference
					    out-ops (union in-ops args))
			     collect `(,out-op nil)))
	   ,(if (null in-ops)
		`(optrace "   No input operands~%")
		`(progn (optrace "   Input operands:~%")
			,@(loop for in-op in in-ops
				collect `(optrace
					  "     [~s : ~s (~a)]~%"
					  ',in-op
					  ,in-op
					  (interpret ,in-op)))))
	   ,(if (null args)
		`(optrace "   No arguments~%")
		`(progn (optrace "   Arguments: ")
			,@(loop for arg in args
				collect `(optrace "     [~s : ~s]~%" ',arg ,arg))))
	   ,@body
	   ,(if (null out-ops)
		`(optrace "   No output operands~%")
		`(progn (optrace "   Output operands:~%")
			,@(loop for out-op in out-ops
				collect `(optrace
					  "     [~s : ~s (~a)]~%"
					  ',out-op
					  ,out-op
					  (interpret ,out-op)))))
	   ,@(loop for out-op in out-ops
		   collect `(setf *operand-stack*
				  (scl:cons ,out-op *operand-stack*))))))))

(defparameter *breakpoints* (make-hash-table))

(defun set-breakpoint (address)
  (setf (gethash address *breakpoints*) t))

(defun clear-breakpoint (address)
  (remhash address *breakpoints*))

(defun execute-next-instruction ()
  (when (gethash *program-counter* *breakpoints*)
    (format *trace-output*
	    "breakpoint encountered at ~s"
	    *program-counter*)
    (throw 'halt nil))
  (optrace "------------------------------------------~%")
  (optrace "PC before: ~s  PC-saved before: ~s~%"
	   *program-counter* *program-counter-save*)
  (funcall (asm:opcode-to-operation (next-op)))
  (optrace "PC after: ~s  PC-saved after: ~s~%"
	   *program-counter* *program-counter-save*))

(defun run-machine ()
  (catch 'halt
    (loop do (execute-next-instruction)))
  (format *trace-output* "Halted~%"))

(defop pop (x) () ())

(defop push-immediate () (val) (val))

(defop dup (x) (x x) ())

(defop dup2 (x y) (x y x) ())

(defop swap (x y) (y x) ())

(defop swap2 (x y z) (z y x) ())

;;; The branch operations are PC relative
(defop branch () () (amount)
  (setf *program-counter*
	(word-from-signed-host-number
	 (+ (signed-host-number-from-word *program-counter*)
	    (* (signed-host-number-from-word amount)
	       +word-size-in-bytes+)))))

(defop branch-zero (x) () (amount)
  (when (zerop x)
    (setf *program-counter*
	  (word-from-signed-host-number
	   (+ (signed-host-number-from-word *program-counter*)
	      (* (signed-host-number-from-word amount)
		 +word-size-in-bytes+))))))

(defop branch-positive (x) () (amount)
  (when (plusp x)
    (setf *program-counter*
	  (word-from-signed-host-number
	   (+ (signed-host-number-from-word *program-counter*)
	      (* (signed-host-number-from-word amount)
		 +word-size-in-bytes+))))))

(defop branch-negative (x) () (amount)
  (when (minusp x)
    (setf *program-counter*
	  (word-from-signed-host-number
	   (+ (signed-host-number-from-word *program-counter*)
	      (* (signed-host-number-from-word amount)
		 +word-size-in-bytes+))))))

(defop jump (address) () ()
  (setf *program-counter* address))

(defop jump-and-link (address) () ()
  (setf *program-counter-save* *program-counter*)
  (setf *program-counter* address))

(defop link-return () () ()
  (setf *program-counter* *program-counter-save*))

(defop unsigned-add (x y) (z) ()
  (multiple-value-bind (result carry)
      (wu+ x y)
    (setf z result
	  *carry* carry)))

(defop signed-add (x y) (z) ()
  (multiple-value-bind (result carry)
      (ws+ x y)
    (setf z result
	  *carry* carry)))

(defop unsigned-sub (x y) (z) ()
  (multiple-value-bind (result carry)
      (wu- x y)
    (setf z result
	  *carry* carry)))

(defop signed-sub (x y) (z) ()
  (multiple-value-bind (result carry)
      (ws- x y)
    (setf z result
	  *carry* carry)))

(defop neg (x) (y) ()
  (multiple-value-bind (result carry)
      (wneg x)
    (setf y result
	  *carry* carry)))

;;; FIXME: introduce signed/unsigned add/sub with carry

(defop unsigned-mul (w x) (y z) ()
  (multiple-value-bind (low high)
      (wu* w x)
    ;; Is this the best order?
    (setf y low
	  z high)))

(defop signed-mul (w x) (y z) ()
  (multiple-value-bind (low high)
      (ws* w x)
    ;; Is this the best order?
    (setf y low
	  z high)))

(defop log-shift (x y) (z) ()
  (setf z (wlogshift x y)))

(defop ar-shift (x y) (z) ()
  (setf z (warshift x y)))

(defop log-ior (x y) (z) ()
  (setf z (wior x y)))

(defop log-and (x y) (z) ()
  (setf z (wand x y)))

(defop log-not (x) (y) ()
  (setf y (wnot x)))

(defop put-char (x) () ()
  (write-char (code-char x)))

(defop halt () () ()
  (throw 'halt nil))

(defop malloc (bytes) (pointer) ()
  (setf pointer (malloc bytes)))

(defop memref (address) (value) ()
  (setf value (memref address)))

(defop memset (address value) () ()
  (memset address value))

(defop push-from-arg-count () (x) ()
  (setf x *number-of-arguments*))

(defop pop-to-arg-count (x) () ()
  (setf *number-of-arguments* x))

(defop push-from-arg (arg-no) (x) ()
  (setf x (aref *arguments* arg-no)))

(defop pop-to-arg (value arg-no) () ()
  (setf (aref *arguments* arg-no) value))

(defop push-from-arg-immediate () (x) (arg-no)
  (setf x (aref *arguments* arg-no)))

(defop pop-to-arg-immediate (value) () (arg-no)
  (setf (aref *arguments* arg-no) value))

(defop push-constant () (constant) (constant-no)
  (setf constant
	(memref (+ *constants* (* constant-no +word-size-in-bytes+)))))

(defop push-from-operand-stack () (operand-stack) ()
  (setf operand-stack *operand-stack*))

(defop pop-to-operand-stack (new-operand-stack) () ()
  (setf *operand-stack* new-operand-stack))

(defop push-from-call-stack () (call-stack) ()
  (setf call-stack *call-stack*))

(defop pop-to-call-stack (new-call-stack) () ()
  (setf *call-stack* new-call-stack))

(defop push-from-program-counter () (program-counter) ()
  (setf program-counter *program-counter*))

(defop push-from-program-counter-save () (program-counter-save) ()
  (setf program-counter-save *program-counter-save*))

(defop pop-to-program-counter-save (new-program-counter-save) () ()
  (setf *program-counter-save* new-program-counter-save))

(defop push-from-constants () (constants) ()
  (setf constants *constants*))

(defop pop-to-constants (new-constants) () ()
  (setf *constants* new-constants))

;;; We cheat by defining an instruction that does memory
;;; allocation.  That way, we can put off its detailed 
;;; implementation until later. 
(defop malloc (size) (address) ()
  (setf address (malloc size)))

;;; The bind stack for specials is organized as follows:
;;;
;;;   ((special-var-1 binding-1 binding-2 ... binding-n1)
;;;    (special-var-2 binding-1 binding-2 ... binding-n2)
;;;    ...)
;;;
;;; Each element of the list representing the bind stack is an
;;; individual bind stack for that special variable, so that binding-1
;;; is the most recent binding, binding-2 the next-most recent, etc. 
;;;
;;; There is no particular order between the specials themselves, i.e.
;;; between the elements of the list representing the bind stack.  We
;;; never destructively modify this list, so that executing a RETURN
;;; instruction restores the bind stack of the previous dynamic frame,
;;; except of course to modify the value of the most recent binding of
;;; a special variable, in which case binding-1 is replaced by the new
;;; value.
;;; 
;;; To add a new binding for a special variable (say special-var-i),
;;; we copy a prefix of the list representing the bind stack, so as to
;;; avoid destructive modifications.  It so happens that in our
;;; binding code, the prefix is then reversed, but that is not
;;; important.  The new inidividual bind stack for special i will be
;;; on top of the stack, which also is not important, but it could be
;;; advantageous if the same variable is bound consecutively.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Assembly snippets.

(asm:define-assembly-macro comment (string)
  (declare (ignore string))
  `())

(asm:define-assembly-macro branch-nil (label)
  `((push-nil)
    (unsigned-sub)
    (branch-zero ,label)))

;;; Allocate sequential words and leave the address to the first
;;; element on top of the operand stack.
;;; Before: ---
;;; After : Words ---
(asm:define-assembly-macro allocate-words (size-in-words)
  `((push-immediate ,(* size-in-words +word-size-in-bytes+))
    (malloc)))

;;; Add a fixnum tag to the element on top of the stack.
;;; It is assumed that the element has no tag to start with.
(asm:define-assembly-macro add-fixnum-tag ()
  `())

;;; Add a cons tag to the element on top of the stack
;;; It is assumed that the element has no tag to start with.
(asm:define-assembly-macro add-cons-tag ()
  `((push-immediate 1)
    (log-ior)))

;;; Add an immediate tag to the element on top of the stack
;;; It is assumed that the element has no tag to start with.
(asm:define-assembly-macro add-immediate-tag ()
  `((push-immediate 2)
    (log-ior)))

;;; Add a heap tag to the element on top of the stack
;;; It is assumed that the element has no tag to start with.
(asm:define-assembly-macro add-heap-tag ()
  `((push-immediate 3)
    (log-ior)))

;;; Remove the tag from the top element of the stack.
(asm:define-assembly-macro remove-tag ()
  `((push-immediate 3)
    (log-not)
    (log-and)))

;;; Replace the untagged sequence of words on top of the
;;; stack by the address of the i:th element of that
;;; sequence.
(asm:define-assembly-macro address-element-untagged (i)
  `((push-immediate ,(* i +word-size-in-bytes+))
    (unsigned-add)))

;;; Replace the untagged sequence of words on top of the
;;; stack by the i:th element of that sequence.
(asm:define-assembly-macro access-element-untagged (i)
  `((address-element-untagged ,i)
    (memref)))

;;; Replace the CONS at the top of the stack by the
;;; contents of the CAR slot of that cell.
(asm:define-assembly-macro car ()
  `((remove-tag)
    (memref)))

;;; Replace the CONS at the top of the stack by the
;;; contents of the CDR slot of that cell.
(asm:define-assembly-macro cdr ()
  `((remove-tag)
    (push-immediate ,+word-size-in-bytes+)
    (unsigned-add)
    (memref)))

;;; Given two elements CAR and CDR, with the CAR on top
;;; of the stack, replace two by a new CONS cell containing
;;; those elements. 
(asm:define-assembly-macro cons ()
  `(                                ; CAR CDR ---
    (allocate-words 2)              ; CONS_ CAR CDR ---
    (swap)                          ; CAR CONS_ CDR ---
    (dup2)                          ; CONS_ CAR CONS_ CDR ---
    (swap)                          ; CAR CONS_ CONS_ CDR ---
    (memset)                        ; CONS  CDR ---
    (swap)                          ; CDR CONS  ---
    (dup2)                          ; CONS  CDR CONS  ---
    (address-element-untagged 1)    ; CONS +i CDR CONS  ---
    (swap)                          ; CDR CONS +i CONS_ ---
    (memset)                        ; CONS  ---
    (add-cons-tag)                  ; CONS
    ))

(asm:define-assembly-macro push-nil ()
  `((push-constant 0)))

(asm:define-assembly-macro step-up ()
  `(                                        ; X ---
    (push-immediate ,+word-size-in-bytes+)  ; ws X ---
    (unsigned-add)                          ; X+ws ---
    ))

(asm:define-assembly-macro step-down ()
  `(                                        ; X ---
    (push-immediate ,+word-size-in-bytes+)  ; ws X ---
    (unsigned-sub)                          ; X-ws ---
    ))

(asm:define-assembly-macro memref-preserve ()
  `(                          ; Address ---
    (dup)                     ; Address Address ---
    (memref)                  ; M[Address] Address ---
    (swap)                    ; Address M[Address] ---
    ))

;;; Given an address on top of the operand stack, and an object just
;;; below it, set the contents of the address to that object, but
;;; leave the address on top of the stack.
(asm:define-assembly-macro memset-preserve ()
  `(                          ; Address Obj ---
    (swap)                    ; Obj Address ---
    (dup2)                    ; Address Obj Address ---
    (swap)                    ; Obj Address Address ---
    (memset)                  ; Address ---
    ))

;;; Given an operand stack where N words have been pushed, allocate a
;;; sequences of N words in memory and initilize them from the operand
;;; stack so that the top of the operand stack is in the last word of
;;; the sequence.
(asm:define-assembly-macro build-sequence (n)
  (let ((sh (round (log +word-size-in-bytes+ 2))))
    `(                             ; sn-1 sn-2 ... s1 s0 ---
      (allocate-words ,n)          ; Obj_ sn-1 sn-2 ... s1 s0 ---
      (push-immediate ,n)          ; n Obj_ sn-1 sn-2 ... s1 s0 ---
      (push-immediate ,sh)         ; sh n Obj_ sn-1 sn-2 ... s1 s0 ---
      (ar-shift)                   ; n*ws Obj_ sn-1 sn-2 ... s1 s0 ---
      (unsigned-add)               ; n*ws+Obj_ sn-1 sn-2 ... s1 s0 ---
      ,@(loop repeat n
	      append
	      `(                   ; i*ws+Obj_ si-1 si-2 ... s1 s0 ---
		(step-down)        ; (i-1)*ws+Obj_ si-1 si-2 ... s1 s0 ---
		(memset-preserve)  ; (i-1)*ws+Obj_ si-2 ... s1 s0 ---
		))                 ; Obj_ ---
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro for getting the global value of a symbol.  
;;; Replace it later with a function, etc.

(asm:define-assembly-macro push-symbol-value ()
  `(                                       ; Symbol ---
    (remove-tag)                           ; Symbol_ ---
    ;; A symbol is a standard instance.
    ;; Get the slot vector.
    (access-element-untagged 1)            ; SV ---
    (access-element-untagged 3)            ; Value ---
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros for managing lexical bindings.

(asm:define-assembly-macro bind (n)
  `(;; Create a Lisp vector of size n elements
    (push-constant 'sicl-system:*class-vector*)
                                              ; Class ---
    ;; The Length is represented as a fixnum
    (push-immediate ,(* n (1+ +tag-mask+)))   ; Len Class ---
    ;; Element type FIXME: Wrong
    (push-immediate 1)                        ; ElemT Len Class ---
    ;; Simplep FIXME: Wrong                           
    (push-immediate 1)                        ; Simplep ElemT Len Class ---
    ;; Allocate the elements 
    (allocate-words ,n)                       ; Elems Simplep ElemT Len Class ---
    ;; Build the vector object itself
    (build-sequence 5)                        ; Vector_ ---
    (add-heap-tag)                            ; Vector ---
    ;; Find the current static environment
    (push-from-call-stack)                    ; CS Vector ---
    (car)                                     ; Frame Vector ---
    (remove-tag)                              ; Frame_ Vector ---
    (address-element-untagged 2)              ; aSEnv Vector ---
    (swap)                                    ; Vector aSEnv ---
    (dup2)                                    ; aSEnv Vector aSEnv ---
    (memref)                                  ; SEnv Vector aSEnv ---
    (swap)                                    ; Vector SEnv aSEnv ---
    (cons)                                    ; NewSEnv aSEnv ---
    (memset)                                  ; ---
    ))

(asm:define-assembly-macro push-from-lexical (level index)
  `((push-from-call-stack)                    ; CS ---
    (car)                                     ; Frame ---
    (remove-tag)                              ; Frame_ ---
    ;; Find the static environment
    (access-element-untagged 2)               ; SE ---
    ;; Do as many CDRs are there are levels to go up
    ,@(loop repeat level collect '(cdr))      ; SE ---
    ;; Find the Vector at the right level
    (car)                                     ; Vector ---
    (remove-tag)                              ; Vector_ ---
    ;; Find its elements
    (access-element-untagged 4)               ; Elements ---
    (access-element-untagged ,index)          ; Object ---
    ))

(asm:define-assembly-macro pop-to-lexical (level index)
  `((push-from-call-stack)                    ; CS Object ---
    (car)                                     ; Frame Object ---
    (remove-tag)                              ; Frame_ Object ---
    (access-element-untagged 2)               ; SE Object ---
    ;; Do as many CDRs are there are levels to go up
    ,@(loop repeat level collect '(cdr))      ; SE Object ---
    ;; Find the Vector at the right level
    (car)                                     ; Vector Object ---
    (remove-tag)                              ; Vector_ Object ---
    ;; Find its elements
    (access-element-untagged 4)               ; Elements Object ---
    (address-element-untagged ,index)         ; aElement Object ---
    (swap)                                    ; Object aElement ---
    (memset)                                  ; ---
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros for managing the call/return protocol.

;;; When we call a subroutine or a function, we execute a jump-and-link
;;; instruction which sets the program counter to the new address, and 
;;; saves the old value of the program counter in program-counter-save.
;;; This macro takes it from there and stores it in the current dynamic
;;; frame so that it won't be lost. 
(asm:define-assembly-macro save-saved-pc ()
  `(                                  ; --- 
    (push-from-call-stack)            ; CS ---
    (car)                             ; TCS ---
    (remove-tag)                      ; TCS  ---
    (address-element-untagged 3)      ; aSPC ---
    (push-from-program-counter-save)  ; PCS aSPC ---
    (memset)                          ; ---
    ))

(asm:define-assembly-macro save-operand-stack ()
  `(                                  ; Obj
    (push-from-call-stack)            ; CS Obj ---
    (car)                             ; TCS Obj ---
    (remove-tag)                      ; TCS  Obj ---
    (address-element-untagged 4)      ; aOS Obj ---
    (push-from-operand-stack)         ; (aOS Obj ---) aOS Obj ---
    ;; Now we have to be a bit careful.  The operand stack that we
    ;; pushed does not contain itself.  But it does contain the
    ;; address of the slot we just pushed and of course the object
    ;; that was there to start with.  We want to get rid of those
    ;; before we save the operand stack to the dynamic frame.
    (cdr)                             ; (Obj ---) aOS Obj ---
    (cdr)                             ; (---) aOS Obj ---
    ;; OK, now we save it.
    (memset)                          ; Obj ---
    ))

;;; This macro is used to call a subroutine.  What we mean by a
;;; subroutine is a code fragment inside the same top-level form as
;;; the caller.  This has some consequences that we exploit, namely
;;; that we can use the same code object and the same environment.  We
;;; just have to do a jump-and-link to the right absolute address.  In
;;; fact, the main tricky part is to find the absolute address to jump
;;; to, because the label we are given comes out a a relative branch
;;; index.  This index needs to be multiplied by the word size, and
;;; added to the current program counter.  But the current program
;;; counter is a bit off, so we need to adjust for that.
(asm:define-assembly-macro call-subroutine (label)
  (let ((sh (round (log +word-size-in-bytes+ 2))))
    `((push-from-program-counter)           ; PC ---
      (push-immediate ,label)               ; RelIndex PC ---
      ;; What was pushed last was the relative index between the next
      ;; instruction and the first instruction of the subroutine.  But
      ;; the PC pushed is not that of the next instruction, but that of
      ;; the previous instruction, which has a length of 2. We need to
      ;; adjust for that discrepancy.
      (push-immediate 2)                    ; Adjust RelIndex PC ---
      (signed-add)                          ; FinalRelIndex PC ---
      (push-immediate ,sh)                  ; sh FinalRelIndex PC ---
      (ar-shift)                            ; FinalRelAddr PC ---
      (signed-add)                          ; AbsoluteAddr ---
      (jump-and-link)
      )))

;;; This macro, in contrast to the previous one, is used to call
;;; any object on top of the operand stack.  It finds the linkage
;;; routine of that object and does a jump-and-link to it.  
(asm:define-assembly-macro call ()
  (let ((label (gensym)))
    `(                                    ; Obj ---
      ;; Find the linkage routine of the object.
      ;;  FIXME: handle non-heap objects by signaling an error.
      ;; (dup)                               ; Obj Obj ---
      ;; (remove-tag)                        ; Obj Obj ---
      ;; (access-element-untagged 1)         ; Linkage Obj ---
      ;; (jump-and-link)                     ; Obj ---
      (dup)                               ; Obj Obj ---
      (push-immediate 3)                  ; TagMask Obj Obj ---
      (log-and)                           ; Tag Obj ---
      (push-immediate 3)                  ; TagHeap Tag Obj ---
      (unsigned-sub)                      ; Tag-TagHeap Obj ---
      (branch-zero ,label)                ; Obj ---
      ;; FIXME: Do this better
      (push-immediate #.(char-code #\l))  ; Char Obj ---
      (put-char)                          ; Obj ---
      (push-immediate #.(char-code #\i))  ; Char Obj ---
      (put-char)                          ; Obj ---
      (push-immediate #.(char-code #\n))  ; Char Obj ---
      (put-char)                          ; Obj ---
      (push-immediate #.(char-code #\k))  ; Char Obj ---
      (put-char)                          ; Obj ---
      (push-immediate #.(char-code #\a))  ; Char Obj ---
      (put-char)                          ; Obj ---
      (push-immediate #.(char-code #\g))  ; Char Obj ---
      (put-char)                          ; Obj ---
      (push-immediate #.(char-code #\e))  ; Char Obj ---
      (put-char)                          ; Obj ---
      (halt)
      ,label                              ; Obj ---
      (dup)
      (remove-tag)                        ; Obj  Obj ---
      ;; Find the class of the object
      (memref)                            ; Class Obj ---
      ;; Find the slot-vector of the class metaobject
      (remove-tag)                        ; Class_ Obj ---
      (access-element-untagged 1)         ; ClassSlotVector Obj ---
      ;; Find the linkage routine
      (access-element-untagged 1)         ; Linkage Obj ---
      (jump-and-link)                     ; Obj ---
      )))

;;; When this prologue is used, we have just done a jump-and-link to
;;; the linkage code for the function, and that linkage routine has
;;; done a jump to the beginning of the code.  We need to push a new
;;; dynamic frame that takes the static environment and the code
;;; object from the function object which is on top of the operand
;;; stack.  The program counter of the new frame is initilized to some
;;; illegal value.  The operand stack is saved from the register.  The
;;; special binding, the catch tags, and the unwind protects are taken
;;; from the old frame. 
(asm:define-assembly-macro function-prologue ()
  '(                             ; CL ---
    (save-saved-pc)              ; CL ---
    ;; The operand stack saved does not have
    ;; the topmost object on it.
    (save-operand-stack)         ; CL ---
    ;; Create a new dynamic frame.
    ;; To do that, we push the values of all the slots of
    ;; the new frame in the right order.
    ;; Start with the class slot from the old
    ;; frame.
    ;; Duplicate the function for later use.
    (dup)                        ; CL CL ---
    (push-from-call-stack)       ; CS CL CL ---
    (car)                        ; TopFrame CL CL ---
    (remove-tag)                 ; TopFrame_ CL CL ---
    (memref)                     ; slot0 CL CL ---
    (swap)                       ; CL slot0 CL ---
    ;; Push the code and the static environment
    ;; from the function onto the operand stack.
    (remove-tag)                 ; CL  slot0 CL ---
    ;; A function is a standard instance, so access the slot vector
    (access-element-untagged 1)  ; SV slot0 CL ---
    (dup)                        ; SV SV slot0 CL ---
    (access-element-untagged 1)  ; Code SV slot0 CL ---
    (swap)                       ; SV Code slot0 CL ---
    (access-element-untagged 2)  ; Env Code slot0 CL ---
    (push-immediate 1)           ; 1 Env Code slot0 CL ---
    (push-constant 'NIL)         ; NIL 1 Env Code slot0 CL ---
    (push-from-call-stack)       ; CS NIL 1 Env Code slot0 CL ---
    (car)                        ; Frame NIL 1 Env Code slot0 CL ---
    (remove-tag)                 ; Frame_ NIL 1 Env Code slot0 CL ---
    (address-element-untagged 6) ; Frame_.6 NIL 1 Env Code slot0 CL ---
    (memref-preserve)            ; Frame_.6 SB NIL 1 Env Code slot0 CL ---
    (step-up)                    ; Frame_.7 SB NIL 1 Env Code slot0 CL ---
    (memref-preserve)            ; Frame_.7 SB NIL 1 Env Code slot0 CL ---
    (step-up)                    ; Frame_.8 CT SB NIL 1 Env Code slot0 CL ---
    (memref)                     ; UP CT SB NIL 1 Env Code slot0 CL ---
    (build-sequence 8)           ; F_ CL ---
    (add-heap-tag)               ; F CL ---
    (push-from-call-stack)       ; CS F CL ---
    (swap)                       ; F CS CL ---
    (cons)                       ; NewCS CL ---
    (pop-to-call-stack)          ; CL ---
    ;; Set the constants from the code object of the new
    ;; dynamic frame.
    (push-from-call-stack)       ; CS CL ---
    (car)                        ; TCS CL ---
    (remove-tag)                 ; TCS  CL ---
    ;; Find the code object
    (access-element-untagged 1)  ; Code CL ---
    (remove-tag)                 ; Code  CL ---
    ;; Find the constants vector of the code object.
    (access-element-untagged 2)  ; ConstantsVector CL ---
    ;; The constants are the elements of the vector.
    (remove-tag)                 ; ConstantsVector_ CL ---
    (access-element-untagged 4)  ; Constants CL ---
    (pop-to-constants)           ; CL ---
    ;; Get rid of the function for now, maybe save it later.
    (pop)                        ; CL ---
    ))

;;; When this prologue is used, we have just done a jump-and-link to a
;;; subroutine address in the same code object as the caller.  We need
;;; to push a new dynamic frame that has copies of everything in the
;;; current one except for the program counter and the call stack.
(asm:define-assembly-macro subroutine-prologue ()
  `((save-saved-pc)
    (push-from-call-stack) ; CS ---
    (car)                  ; F ---
    (remove-tag)           ; (F_+0) ---
    ;; Get the first slot (class)
    (memref-preserve)      ; (F_+0) s0 ---
    (step-up)              ; (F_+1) s0 ---
    ;; Get the next slot (code)
    (memref-preserve)      ; (F_+1) s1 s0 ---
    (step-up)              ; (F_+2) s1 s0 ---
    ;; Get the next slot (static-environment)
    (memref-preserve)      ; (F_+2) s2 s1 s0 ---
    (step-up)              ; (F_+3) s2 s1 s0 ---
    ;; Skip two slots
    (step-up)              ; (F_+4) s2 s1 s0 ---
    (step-up)              ; (F_+5) s2 s1 s0 ---
    ;; Instead push new initial values for program
    ;; counter and operand stack.
    ;; Use an illegal value for PC.
    (push-immediate 1)     ; 1 (F_+5) s2 s1 s0 ---
    (swap)                 ; (F_+5) 1 s2 s1 s0 ---
    ;; Use NIL for the operand stack
    (push-constant 'NIL)   ; NIL (F_+5) 1 s2 s1 s0 ---
    (swap)                 ; (F_+5) NIL 1 s2 s1 s0 ---
    ;; Get the next slot
    (memref-preserve)      ; (F_+5) s5 NIL 1 s2 s1 s0 ---
    (step-up)              ; (F_+6) s5 NIL 1 s2 s1 s0 ---
    ;; Get the next slot
    (memref-preserve)      ; (F_+6) s6 s5 NIL 1 s2 s1 s0 ---
    (step-up)              ; (F_+7) s6 s5 NIL 1 s2 s1 s0 ---
    ;; Get the last slot.  No need to save the address anymore.
    (memref)               ; s7 s6 s5 NIL 1 s2 s1 s0 ---
    ;; Create a new dynamic frame.
    (build-sequence 8)     ; F_ ---
    (add-heap-tag)         ; F ---
    (push-from-call-stack) ; CS F ---
    (swap)                 ; F CS ---
    (cons)                 ; newCS ---
    (pop-to-call-stack)    ; ---
    ;; Finally set the operand stack to NIL
    (push-constant 'NIL)   ; NIL ---
    (pop-to-operand-stack) ; ---
    ))

;;; We have a call stack on top of the operand stack.
;;; We could have gotten it either by just taking the CDR of
;;; the current call stack, in which case we return normally,
;;; or we could have gotten it from some kind of non-local
;;; transfer or control, such as TROW, GO, or RETURN-FROM.
;;; Either way, the call stack is restored and we do a 
;;; link-return to continue execution where this top
;;; frame of the call stack to restore was suspended. 
(asm:define-assembly-macro restore-call-stack ()
  `(                                    ; CS ---
    (dup)                               ; CS CS ---
    (pop-to-call-stack)                 ; CS ---
    (car)                               ; Frame ---
    (remove-tag)                        ; Frame_ ---
    ;; Restore the constants.
    (dup)                               ; Frame  Frame  ---
    ;; Get the code object from the frame.
    (access-element-untagged 1)         ; Code Frame_ ---
    (remove-tag)                        ; Code_ Frame_ ---
    ;; Get the vector of constants from the code object
    (access-element-untagged 2)         ; VConstants Frame_ ---
    (remove-tag)                        ; VConstants_ Frame_ ---
    ;; Get the elements of the vector
    (access-element-untagged 4)         ; Constants Frame  ---
    ;; Restore constants
    (pop-to-constants)                  ; Frame  ---
    ;; Restore the saved program counter.
    (dup)                               ; Frame  Frame  ---
    (access-element-untagged 3)         ; PC Frame_ ---
    (pop-to-program-counter-save)       ; Frame_ ---
    ;; Restore the operand stack.
    (access-element-untagged 4)         ; OS ---
    ;; When we pop to the operand stack, we really have no idea what
    ;; we have, because it is whatever was there when it was saved.
    (pop-to-operand-stack)              ; ???
    ;; Restore the program counter from the saved program counter.
    ;; We don't know where we will end up.
    (link-return)))

;;; Return to the previous dynamic frame from a call to a function.
;;; This epilogue is used when the corresponding prologue was used,
;;; i.e., the function that was called pushed a new dynamic frame and
;;; altered the constants vector, so it must be restored from the
;;; frame that is to be restored. 
(asm:define-assembly-macro function-epilogue ()
  `((push-from-call-stack)              ; CS ---
    (cdr)                               ; OldCS ---
    (restore-call-stack)))

;;; Return to the previous dynamic frame from a call to a subroutine.
;;; This epilogue is used when the corresponding prologue was used,
;;; i.e., the subroutine that was called pushed a new dynamic frame,
;;; so the old one needs to be restored.  However, since it is a
;;; subroutine, no modifications were made to the constants vector.
(asm:define-assembly-macro subroutine-epilogue ()
  `((push-from-call-stack)              ; CS ---
    (cdr)                               ; OldCS ---
    (dup)                               ; OldCS OldCS ---
    (pop-to-call-stack)                 ; OldCS ---
    (car)                               ; Frame ---
    (remove-tag)                        ; Frame_  ---
    ;; Restore the saved program counter.
    (dup)                               ; Frame  Frame  ---
    (access-element-untagged 3)         ; PC Frame_ ---
    (pop-to-program-counter-save)       ; Frame_ ---
    ;; Restore the operand stack.
    (access-element-untagged 4)         ; OS ---
    ;; When we pop to the operand stack, we really have no idea what
    ;; we have, because it is whatever was there when it was saved.
    (pop-to-operand-stack)              ; ???
    ;; Restore the program counter from the saved program counter.
    ;; We don't know where we will end up.
    (link-return)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Build a function.

(asm:define-assembly-macro enclose (label)
  (let ((sh (round (log +word-size-in-bytes+ 2))))
    `(;; Push the class for later use.
      (push-constant 'sicl-system:*class-function*)
					; Sym ---
      (push-symbol-value)         ; Class ---
      ;; Push the history; NIL for now.
      (push-nil)                  ; NIL Class ---
      ;; Get the code object and the environment from the call stack. 
      (push-from-call-stack)      ; CS NIL Class ---
      (car)                       ; Frame
      (remove-tag)                ; Frame_ NIL Class ---
      (dup)                       ; Frame_ Frame_ NIL Class ---
      ;; First the code
      (access-element-untagged 1) ; Code Frame_ NIL Class ---
      (swap)                      ; Frame_ Code NIL Class ---
      ;; Then the environment
      (access-element-untagged 2) ; Env Code NIL
      ;; Now for the delicate task of computing the absolute
      ;; address of the beginning of the code for the function. 
      (push-from-program-counter) ; PC Env Code NIL Class ---
      (push-immediate ,label)     ; RelIndex PC Env Code NIL Class ---
      ;; What was pushed last was the relative index between the next
      ;; instruction and the first instruction of the subroutine.  But
      ;; the PC pushed is not that of the next instruction, but that of
      ;; the previous instruction, which has a length of 2. We need to
      ;; adjust for that discrepancy.
      (push-immediate 2)          ; Adjust RelIndex PC Env Code NIL Class ---
      (signed-add)                ; FinalRelIndex PC Env Code NIL Class ---
      (push-immediate ,sh)        ; sh FinalRelIndex PC Env Code NIL Class ---
      (ar-shift)                  ; FinalRelAddr PC Env Code NIL Class ---
      (signed-add)                ; AbsoluteAddr Env Code NIL Class ---
      ;; Build the slot vector. 
      (build-sequence 4)          ; SV Class ---
      (build-sequence 2)          ; Function_ ---
      (add-heap-tag)              ; Function ---
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Linkage code.
;;;
;;; When the linkage code of some object is called, the
;;; *program-counter-save* register contains the program counter value
;;; to be restored when the call has been accomplished.  Before this
;;; value is restored, the call stack, the constants and the operand
;;; stack must be restored, should they be modified by the callee.  

;;; Linkage code for functions.  Just accesss the entry point of the
;;; function.  Count on the function to save and restore registers if
;;; it is required to do so in order to accomplish its task. 
(defparameter *linkage-function-instructions*
  '(                                  ; CL ---
    (dup)                             ; CL CL ---
    (remove-tag)                      ; CL  CL ---
    ;; A function is a standard instance, so access the slot vector
    (access-element-untagged 1)       ; SV CL ---
    (access-element-untagged 3)       ; PC CL ---
    (jump)                            ; CL ---
    ))

;;; Linkage code for symbols.  Find the contents of the
;;; symbol-function slot of the symbol and then the linkage code of
;;; the object contained therein, and finally, jump to that linkage
;;; code, supposedly that of a function. 
(defparameter *linkage-symbol-instructions*
  '(                                  ; Sym ---
    ;; Find the slot vector.
    (remove-tag)                      ; Sym  ---
    (access-element-untagged 1)       ; SV ---
    ;; Find the contents of the function slot
    (access-element-untagged 4)       ; Fun ---
    ;; Find the linkage routine of the object.
    ;; FIXME: handle non-heap objects by signaling an error.
    (dup)                               ; Fun Fun ---
    (remove-tag)                        ; Fun  Fun ---
    ;; Find the class metaobject
    (memref)                            ; Class Fun ---
    (remove-tag)                        ; Class_ Fun ---
    ;; Find the slot vector of the class metaobject
    (access-element-untagged 1)         ; SV Fun ---
    ;; Find the linkage code 
    (access-element-untagged 1)         ; Linkage Fun ---
    (jump)                              ; Fun ---
    ))

;;; Linkage code for objects that can't be called as functions.
;;; FIXME: this is clearly unacceptable.  Call some kind of error
;;; routine.  The difficulty is that we don't have access to the
;;; symbol ERROR so that we can reach the ERROR function. 
(defparameter *linkage-error-instructions*
  `((halt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Host function.
;;;
;;; The great cheat object.

(defparameter *host-functions* (make-hash-table))

(defparameter *next-available-host-function-number* 0)

(defunbound *class-host-function*)

;;; SYMBOL will be a host symbol to be translated to a target
;;; symbol.  
(defun define-host-function (function-or-host-symbol symbol)
  (let ((fun (if (symbolp function-or-host-symbol)
		 (symbol-function function-or-host-symbol)
		 function-or-host-symbol))
	(target-sym (object-from-host-object symbol)))
    (setf (gethash *next-available-host-function-number* *host-functions*)
	  fun)
    (let ((slot-vector (malloc-words 2)))
      (memset (+ slot-vector (* 0 +word-size-in-bytes+))
	      +unbound+)
      (memset (+ slot-vector (* 1 +word-size-in-bytes+))
	      *next-available-host-function-number*)
      (let ((host-function (malloc-words 2)))
	(memset (+ host-function (* 0 +word-size-in-bytes+))
		*class-host-function*)
	(memset (+ host-function (* 1 +word-size-in-bytes+))
		slot-vector)
	(setf (scl:symbol-function target-sym)
	      host-function))))
  (incf *next-available-host-function-number*))

(defop call-host-function (host-function) (host-function) ()
  (let* ((untagged (remove-tag host-function))
	 (slot-vector (memref (+ untagged (* 1 +word-size-in-bytes+))))
	 (function-number (memref (+ slot-vector (* 1 +word-size-in-bytes+))))
	 (fun (gethash function-number *host-functions*))
	 (args (loop for i from 0 below *number-of-arguments*
		     collect (aref *arguments* i)))
	 (results (multiple-value-list (apply fun args))))
    (loop for val in results
	  for i from 0
	  do (setf (aref *arguments* i) val))
    (setf *number-of-arguments* (length results))))

(defunbound *linkage-host-function*)

;;; Linkage code for host functions.  
(defparameter *linkage-host-function-instructions*
  `((call-host-function)
    (link-return)))
    
(defun init-host-functions ()
  (setf *host-functions* (make-hash-table))
  (setf *next-available-host-function-number* 0)
  (setunbound *linkage-host-function* (malloc-words 100))
  (setunbound *class-host-function*
	      (make-skeleton-class *linkage-host-function*))
  (fill-linkage *linkage-host-function-instructions*
		*linkage-host-function*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test of host functions.

(defun target-print (thing)
  (format t "printing-target-value: ~s~%" thing))

;;; The problem this function solves is the following: Before we
;;; started allocating target data objects on the heap, we needed the
;;; address of linkage code for those objects, because that address is
;;; stored in the object itself.  So for each different linkage
;;; routine we needed, we allocated a sequence of words that was
;;; sufficiently long (1000 words) that any linkage routine would fit
;;; there, but the module that did that allocation is totally ignorant
;;; of the details of those linkage routines.  Therefore, no code was
;;; actually put into those words.  
;;;
;;; Now are task is as follows: we must take the address of such a
;;; sequence of words, and the symbolic instructions of a linkage
;;; routine.  We must turn the symbolic instructions into a target
;;; vector V, and then copy the elements of V to the sequence of words
;;; beginning with the address in question, hoping that there are
;;; fewer than 1000 elements.  Then we must make the address in
;;; question the elements of the vector, so that we have a proper Lisp
;;; object that can be traced by the garbage collector.
;;; FIXME: do the GC protection later.
(defun fill-linkage (symbolic-instructions start-address)
  (let ((code (asm:assemble symbolic-instructions)))
    ;; Copy the instructions to the sequence starting at start-address.
    (loop for addr from start-address
	    by +word-size-in-bytes+
	  for host-number in code
	  do (memset addr (word-from-signed-host-number host-number)))))

;;; Something like 
;;; (defun sym ...)
;;; macroexpands into 
;;; (setf (fdefintion 'sym) fun)
;;; which macroexpands into
;;; (funcall (function (setf fdefinition)) fun 'sym)
;;;
;;; and
;;;
;;; Something like 
;;; (defun (setf sym) ...)
;;; macroexpands into 
;;; (setf (fdefintion '(setf sym)) fun)
;;; which macroexpands into
;;; (funcall (function (setf fdefinition)) fun '(setf sym))
;;;
;;; The compiler recognizes (function (setf bla)) as a special
;;; case and generates code to look in the setf-function slot
;;; of bla. 
;;;
;;; So to get things started, we must have a rudimentary function
;;; in the setf-function slot of the symbol fdefinition.  
;;; This function takes two arguments, a function object and either
;;; a symbol or a list of the form (setf symbol).  In the first case,
;;; this rudimentary function must set the function slot of the symbol,
;;; and in the second case, it must set the setf-function slot of 
;;; the symbol.  Clear?
(defun make-rudimentary-setf-fdefinition ()
  (let* ((lab1 (gensym))
	 (lab2 (gensym))
	 (code `(;; Function in arg0, symbol or (setf symbol)
		 ;; in arg1.
		 (push-from-arg-immediate 1)        ; arg1 ---
		 (dup)                              ; arg1 arg1 ---
		 (push-immediate ,+tag-mask+)       ; M arg1 arg1 ---
		 (log-and)                          ; T arg1 ---
		 (push-immediate ,+tag-cons+)       ; TC T arg1 ---
		 (unsigned-sub)                     ; T-TC arg1 ---
		 (branch-zero ,lab1)                ; arg1 ---
					            ; symbol ---
		 (remove-tag)                       ; symbol_ ---
		 (access-element-untagged 1)        ; SV ---
		 (address-element-untagged 4)       ; aFun ---
		 (push-from-arg-immediate 0)        ; Fun aFun ---
		 (memset)                           ; ---
		 (branch ,lab2)
		 ,lab1                              ; (setf symbol) ---
		 (cdr)                              ; (symbol) ---
		 (car)                              ; symbol ---
		 (remove-tag)                       ; symbol_ ---
		 (access-element-untagged 1)        ; SV ---
		 (address-element-untagged 5)       ; aSetfFun ---
		 (push-from-arg-immediate 0)        ; Fun aSetfFun ---
		 (memset)                           ; ---
		 ,lab2
		 (link-return))))
    (setf (symbol-setf-function (object-from-host-object 'scl:fdefinition))
	  (make-function (assemble-and-binarize code)))))

;;; In addition, we need a function definition for FUNCALL, which
;;; pushes its first argument on the operand stack, shifts other arguments
;;; over by two positions, modifies the argument count and calls the 
;;; function which is now on top of the operand stack. 
(defun make-rudimentary-funcall ()
  (let* ((lab1 (gensym))
	 (lab2 (gensym))
	 (code `((push-from-arg-immediate 0)        ; Fun ---
		 (push-immediate 1)                 ; 1 Fun ---
		 ,lab1                              ; i Fun ---
		 (dup)                              ; i i Fun ---
		 (push-from-arg-count)              ; AC i i Fun ---
		 (unsigned-sub)                     ; i-AC i Fun ---
		 (branch-zero ,lab2)                
					; i Fun ---
		 (dup)                              ; i i Fun ---
		 (push-from-arg)                    ; argi i Fun ---
		 (dup2)                             ; i argi i Fun ---
		 (push-immediate 1)                 ; 1 i argi i Fun ---
		 (unsigned-sub)                     ; i-1 argi i Fun ---
		 (pop-to-arg)                       ; i Fun ---
		 (push-immediate 1)                 ; 1 i Fun ---
		 (unsigned-add)                     ; i+1 Fun ---
		 (branch ,lab1)                     ; i+1 Fun ---
		 ,lab2                              ; AC Fun ---
		 (pop)                              ; Fun ---
		 (call))))
    (setf (scl:symbol-function (object-from-host-object 'scl:funcall))
	  (make-function (assemble-and-binarize code)))))

(defun initialize-machine ()
  (sicl-low:initialize-low)

  (loop for i from 0 below (length *arguments*)
	do (setf (aref *arguments* i) 0))

  (setf *number-of-arguments* 0)

  (setf *call-stack* scl:NIL)
  (setf *operand-stack* scl:NIL)
  (fill-linkage *linkage-function-instructions*
		sicl-system:*linkage-function*)
  (fill-linkage *linkage-symbol-instructions*
		sicl-system:*linkage-symbol*)
  (fill-linkage *linkage-error-instructions*
		sicl-system:*linkage-error*)
  (make-rudimentary-setf-fdefinition)
  (make-rudimentary-funcall)
  (init-host-functions)
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Turn the output of the assembler into code objects. 

;;; Take a list of host integers and return a target vector
;;; of machine words corresponding to those integers. 
(defun vector-from-host-integers (host-integers)
  (let* ((length (object-from-host-object (length host-integers)))
	 (vector (make-word-vector length)))
    (loop for number in host-integers
  	  for index from 0
  	  do (setf (vector-element vector (object-from-host-object index))
		   (word-from-signed-host-number number)))
    vector))


(defun vector-from-host-objects (host-integers)
  (let* ((length (object-from-host-object (length host-integers)))
	 (vector (make-word-vector length)))
    (loop for number in host-integers
  	  for index from 0
  	  do (setf (vector-element vector (object-from-host-object index))
		   (object-from-host-object number)))
    vector))

(defun assemble-and-binarize (program)
  (multiple-value-bind (instructions constants)
      (asm:assemble (append program '((halt))))
    (make-code (vector-from-host-integers instructions)
	       (vector-from-host-objects constants))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load the machine so that execution of machine instructions
;;; can start.

(defun prepare-machine-for-execution (code)
  (let* ((saved-program-counter (word-from-signed-host-number 1))
	 (bottom-frame (make-dynamic-frame code saved-program-counter)))
    (setf *call-stack* (scl:cons bottom-frame scl:NIL))
    (setf *program-counter-save* saved-program-counter)
    (setf *program-counter* (vector-elements (code-instructions code)))
    (setf *constants* (vector-elements (code-constants code)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Temporary stuff for testing.

(defun load-and-go (instructions)
  (prepare-machine-for-execution
   (assemble-and-binarize instructions))
  (run-machine))

(defun exform (form)
  (load-and-go 
   (append (sicl-cross-compiler:cross-compile-form form)
	   '((halt)))))

