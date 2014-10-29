	BITS 64
	global _start
_start:
	mov r15, 10000000000
	;; Make R12 contain a tagged pointer to the list.
	mov r12, list
	add r12, 1
again:	
	;; Check whether we are done looping
	cmp r15, 0
	je exit
	sub r15, 1
	;; Check that r12 contains a CONS cell.
	lea r11, [r12 - 1]
	and r11, 3
	jnz exit
	;; Take the CAR of the CONS cell
	mov r13, [r11]
	;; Check that the CAR is a general instance
	lea r11, [r13 - 3]
	and r11, 3
	jnz exit
	;; Load the rack of the instance in r13 to r11
	mov r11, [r13 + 5]
	;; Load the stamp into r11
	mov r11, [r11]		
	;; Check that the stamp is equal to 10
	cmp r11, 10
	jne exit
	;; Take the CDR of the list
	mov r12, [r12 + 7]
	jmp again
exit:	
	mov rax, 60		; system call 60 is exit
        xor rdi, rdi		; we want return code 0
        syscall                 ; invoke operating system to exit
	align 8
rack:
	dq 10			; Stamp
object:
	dq 0			; The class, we don't need it
	dq rack			; The rack of the object
list:
	dq object + 3		; Tagged general instance
	dq list + 1		; Tagged cons to itself
