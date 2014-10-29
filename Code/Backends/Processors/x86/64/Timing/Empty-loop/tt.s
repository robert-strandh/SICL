	BITS 64
	global _start
_start:
	mov r15, 10000000000
again:	
	sub r15, 1
	jnz again
	mov rax, 60		; System call 60 is exit.
        xor rdi, rdi		; We want return code 0.
        syscall                 ; Invoke operating system to exit.
