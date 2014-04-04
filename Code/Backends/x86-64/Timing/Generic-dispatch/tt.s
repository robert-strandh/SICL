	BITS 64
	global _start
_start:
	mov r15, 10000000000
	mov r13, object
	add r13, 3
again:	
	;; Check the tag
	mov r11, r13
	and r11, 3
	cmp r11, 3
	jne exit
	;; Load the rack
	mov r11, [r13 + 5]
	;; Load the stamp
	mov r11, [r11]
	;; Start testing the stamp
	cmp r11, 3
	jle l0_3
	;; [4-7]
	cmp r11, 5
	jle l4_5
	;; [6-7]
	cmp r11, 6
	jle l6_6
	;; = 7
	jmp out
l6_6:	
	jmp out
l4_5:	
	cmp r11, 4
	jle l4_4
	;; = 5
	jmp out
l4_4:
	jmp out
l0_3:	cmp r11, 1
	jle l0_1
	;; [2-3]
	cmp r11, 2
	jle l2_2
	;; = 3
	jmp out
l2_2:
	jmp out
l0_1:
	cmp r11, 0
	jle l0_0
	;; = 1
	jmp out
l0_0:
	jmp out
out:	
	sub r15, 1
	jnz again
	jmp exit
exit:	
	mov rax, 60		; System call 60 is exit.
        xor rdi, rdi		; We want return code 0.
        syscall                 ; Invoke operating system to exit.
	align 8
object:
	dq 0			; Class, we don't need it
	dq rack			; pointer to the rack
rack:	dq 3			; Stamp
