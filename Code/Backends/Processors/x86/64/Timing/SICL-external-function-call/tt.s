;;; This code simulates a function call where a register contains 
;;; a pointer to the function object, and the function object
;;; contains the address of the function, the static environment,
;;; and the linkage rack.  The call conforms to the full SICL function
;;; in that the caller establishes a stack frame (empty in this case)
;;; for the callee, and destroys it after the call.
	BITS 64
	global _start
_start:
	mov r15, 10000000000
	;; Make R12 contain a tagged pointer to the function object.
	mov r12, function_object
	add r12, 3
	;; Make R13 contain some argument to pass.
	mov r13, 123
again:	
	;; Load the rack of the function into a scratch register (R11).
	mov r11, [r12 + 5]
	;; Load the linkage rack into its register (R9).
	mov r9, [r11 + 24]
	;; Load the static environment into its register (R10).
	mov r10, [r11 + 32]
	;; Load the address to call into a scratch register (R11).
	mov r11, [r11 + 16]
	;; Pass the argument in RDI
	mov rdi, r13
	;; Set the argument count to 1
	mov r8, 4
	;; Save RBP of caller
	push rbp
	;; Establish an empty stack frame
	mov rbp, rsp
	;; Call the address contained in R11
	call r11
	;; Restore our own stack frame.
	pop rbp
	sub r15, 1
	jnz again
	jmp exit
function:
	cmp r8, 4		; Check argument count
	jne exit		; Exit if wrong.
	mov rdi, 1		; Set value count.
	;; The following instruction only needs to be emitted
	;; if the current invocation needs to allocate space
	;; on the stack.
	lea rsp, [rbp - 8]	; Remove stack frame except return address.
	ret
exit:	
	mov rax, 60		; system call 60 is exit
        xor rdi, rdi		; we want return code 0
        syscall                 ; invoke operating system to exit
function_object:
	dq 0			; The class, we don't need it
	dq function_rack	; The rack of the object
function_rack:
	dq 0			; Dummy for stamp
	dq 0 			; Dummy for list of slots
	dq function		; The address to call
	dq 0			; Linkage rack
	dq 0			; Static environment
	
	
