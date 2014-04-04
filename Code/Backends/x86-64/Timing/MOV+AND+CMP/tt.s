	BITS 64
	global _start
_start:
	mov r15, 10000000000
	mov r13, 10001
again:	
	mov r11, r13
	and r11, 3
	cmp r11, 1
	jne exit
	sub r15, 1
	jnz again
	jmp exit
exit:	
	mov rax, 60		; System call 60 is exit.
        xor rdi, rdi		; We want return code 0.
        syscall                 ; Invoke operating system to exit.
