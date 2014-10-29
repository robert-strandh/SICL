	BITS 64
	global _start
_start:
	mov r15, 10000000000
	mov r13, 10001
again:	
	lea r11, [r13 - 1]
	and r11, 3
	jnz exit
	sub r15, 1
	jnz again
	jmp exit
exit:	
	mov rax, 60		; System call 60 is exit.
        xor rdi, rdi		; We want return code 0.
        syscall                 ; Invoke operating system to exit.
