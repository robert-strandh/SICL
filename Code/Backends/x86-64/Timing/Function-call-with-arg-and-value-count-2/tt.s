;;; This code simulates a function call where the argument 
;;; count is set in R8 prior to the call, and checked inside
;;; the function in a way that the entry where the argument count is
;;; already checked is at a constant offset from the normal entry.
;;; The function sets the value count before returning.
	BITS 64
	global _start
_start:
	mov r15, 10000000000
	mov rbx, function
again:	
	mov r8, 2		; Set argument count.
	call rbx
	sub r15, 1
	jnz again
	jmp exit
function:
	jmp check_arg_count
arg_count_checked_entry:	
	mov rdi, 1		; Set value count.
	ret
check_arg_count:
	cmp r8, 2		; Check argument count
	jne exit		; Exit if wrong.
	jmp arg_count_checked_entry
exit:	
	mov rax, 60		; System call 60 is exit.
        xor rdi, rdi		; We want return code 0.
        syscall                 ; Invoke operating system to exit.
