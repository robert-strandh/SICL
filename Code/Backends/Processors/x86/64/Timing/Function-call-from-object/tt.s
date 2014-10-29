;;; This code simulates a function call where a register contains 
;;; a pointer to the function object, and the function object
;;; contains the address of the function, the static environment,
;;; and the linkage rack. 
	BITS 64
	global _start
_start:
	mov r15, 10000000000
	;; Make R12 contain a tagged pointer to the function object.
	mov r12, function_object
	add r12, 3
again:	
	;; Load the rack of the function into a scratch register (R11).
	mov r11, [r12 + 5]
	;; Load the linkage rack into its register (R9)
	mov r9, [r11 + 24]
	;; Load the static environment into its register (R10).
	mov r10, [r11 + 32]
	;; Load the address to call into a scratch register (R11).
	mov r11, [r11 + 16]
	;; Set the argument count
	mov r8, 2
	;; Call the address contained in R11
	call r11
	sub r15, 1
	jnz again
	jmp exit
function:
	cmp r8, 2		; Check argument count
	jne exit		; Exit if wrong.
	mov rdi, 1		; Set value count.
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
	
	
