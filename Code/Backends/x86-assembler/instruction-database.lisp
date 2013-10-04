(in-package #:x86-assembler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic AAA

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic AAD

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic AAM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic AAS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic ADC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic ADD

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add an 8-bit immediate value to the contents of GPR A and store
;;; the result in GPR A.
;;; 
;;; Opcodes: 04

(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr-a 8) (imm 8))
  :opcodes (#x04)
  :encoding (- imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add an immediate value (16/32) to GPR A (16/32/64).
;;; 
;;; Opcodes: 05

;;; Add imm16 to GPR AX
(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr-a 16) (imm 16))
  :opcodes (#x05)
  :encoding (- imm)
  :operand-size-override t)

;;; Add imm32 to GPR EAX
(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr-a 32) (imm 32))
  :opcodes (#x05)
  :encoding (- imm))

;;; Add sign-extended imm32 to GPR RAX
(define-instruction "ADD"
  :modes (64)
  :operands ((gpr-a 64) (simm 32))
  :opcodes (#x05)
  :encoding (- imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add an immediate 8-bit value to an 8-bit register or memory
;;; location.
;;; 
;;; Opcodes: 80
;;; Opcode extension: 0

;;; Add an immediate 8-bit value to an 8-bit register.
(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 0
  :encoding (modrm imm))

;;; Add an immediate 8-bit value to an 8-bit memory location.
(define-instruction "ADD"
  :modes (32 64)
  :operands ((memory 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 0
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add imm to reg/mem, 16/32/64
;;; Opcodes: 81
;;; Opcode extension 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add imm16 to reg/mem16

;;; Add imm16 to reg16
(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 0
  :encoding (modrm imm)
  :operand-size-override t)

;;; Add imm16 to mem16
(define-instruction "ADD"
  :modes (32 64)
  :operands ((memory 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 0
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add imm32 to reg/mem32

;;; Add imm32 to reg32
(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 0
  :encoding (modrm imm))

;;; Add imm32 to mem32
(define-instruction "ADD"
  :modes (32 64)
  :operands ((memory 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 0
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add sign-extended imm32 to reg/mem64

;;; Add imm32 to reg64
(define-instruction "ADD"
  :modes (64)
  :operands ((gpr 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 0
  :encoding (modrm imm)
  :rex.w t)

;;; Add imm32 to mem64
(define-instruction "ADD"
  :modes (64)
  :operands ((memory 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 0
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add sign-extended imm8 to reg/mem 16/32/64
;;; Opcodes: 83
;;; Opcode extension: 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add sign-extended imm8 to reg/mem16

;;; Add sign-extended imm8 to reg16
(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 0
  :encoding (modrm imm)
  :operand-size-override t)

;;; Add sign-extended imm8 to mem16
(define-instruction "ADD"
  :modes (32 64)
  :operands ((memory 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 0
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add sign-extended imm8 to reg/mem32

;;; Add sign-extended imm8 to reg32
(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 0
  :encoding (modrm imm))

;;; Add sign-extended imm8 to mem32
(define-instruction "ADD"
  :modes (32 64)
  :operands ((memory 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 0
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add sign-extended imm8 to reg/mem64

;;; Add sign-extended imm8 to reg64
(define-instruction "ADD"
  :modes (64)
  :operands ((gpr 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 0
  :encoding (modrm imm)
  :rex.w t)

;;; Add sign-extended imm8 to mem64
(define-instruction "ADD"
  :modes (64)
  :operands ((memory 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 0
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add reg8 to reg/mem8
;;; Opcodes: 00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add reg8 to reg8

(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x00)
  :encoding (modrm reg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add reg8 to mem8

(define-instruction "ADD"
  :modes (32 64)
  :operands ((memory 8) (gpr 8))
  :opcodes (#x00)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add reg to reg/mem, 16/32/64
;;; Opcodes: 01

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add reg16 to reg/mem16

;;; Add reg16 to reg16
(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x01)
  :encoding (modrm reg)
  :operand-size-override t)

;;; Add reg16 to mem16
(define-instruction "ADD"
  :modes (32 64)
  :operands ((memory 16) (gpr 16))
  :opcodes (#x01)
  :encoding (modrm reg)
  :lock t
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add reg32 to reg/mem32

;;; Add reg32 to reg32
(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x01)
  :encoding (modrm reg))

;;; Add reg32 to mem32
(define-instruction "ADD"
  :modes (32 64)
  :operands ((memory 32) (gpr 32))
  :opcodes (#x01)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add reg64 to reg/mem64

;;; Add reg64 to reg64
(define-instruction "ADD"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x01)
  :encoding (modrm reg)
  :rex.w t)

;;; Add reg64 to mem64
(define-instruction "ADD"
  :modes (64)
  :operands ((memory 64) (gpr 64))
  :opcodes (#x01)
  :encoding (modrm reg)
  :lock t
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add reg/mem8 to reg8
;;; Opcodes: 02

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add reg8 to reg8

(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x02)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add mem8 to reg8

(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr 8) (memory 8))
  :opcodes (#x02)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add reg/mem to reg, 16/32/64
;;; Opcodes: 03

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add reg/mem16 to reg16

;;; Add reg16 to reg16
(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x03)
  :encoding (reg modrm)
  :operand-size-override t)

;;; Add mem16 to reg16
(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr 16) (memory 16))
  :opcodes (#x03)
  :encoding (reg modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add reg/mem32 to reg32

;;; Add reg32 to reg32
(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x03)
  :encoding (reg modrm))

;;; Add mem32 to reg32
(define-instruction "ADD"
  :modes (32 64)
  :operands ((gpr 32) (memory 32))
  :opcodes (#x03)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Add reg/mem64 to reg64

;;; Add reg64 to reg64
(define-instruction "ADD"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x03)
  :encoding (reg modrm)
  :rex.w t)

;;; Add mem64 to reg64
(define-instruction "ADD"
  :modes (64)
  :operands ((gpr 64) (memory 64))
  :opcodes (#x03)
  :encoding (reg modrm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic AND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of AL with an immediate 8-bit value and store the
;;; result in AL.
;;;
;;; Opcodes: 24

(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr-a 8) (imm 8))
  :opcodes (#x24)
  :encoding (- imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of GPR A (16/32/64) with an immediate value
;;; (16/32) and store the result in GPR A.
;;;
;;; Opcodes: 25

;;; AND the contents of GPR AX with an immediate 16-bit value and
;;; store the result in GPR AX.
(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr-a 16) (imm 16))
  :opcodes (#x25)
  :encoding (- imm)
  :operand-size-override t)

;;; AND the contents of GPR EAX with an immediate 32-bit value and
;;; store the result in GPR EAX.
(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr-a 32) (imm 32))
  :opcodes (#x25)
  :encoding (- imm))

;;; AND the contents of GPR RAX with a sign-extended 32-bit immediate
;;; value and store the result in GPR RAX.
(define-instruction "AND"
  :modes (64)
  :operands ((gpr-a 64) (simm 32))
  :opcodes (#x25)
  :encoding (- imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of reg/mem8 with an immediate 8-bit value, and
;;; store the result in reg/mem8.
;;;
;;; Opcodes: 80
;;; Opcode extension: 4

;;; AND the contents of reg8 with an immediate 8-bit value, and store
;;; the result in reg8.
(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 4
  :encoding (modrm imm))

;;; AND the contents of mem8 with an immediate 8-bit value, and store
;;; the result in mem8.
(define-instruction "AND"
  :modes (32 64)
  :operands ((memory 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 4
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of reg/mem (16/32/64) with an immediate value
;;; (16/32) and store the result in reg/mem.
;;;
;;; Opcodes: 81
;;; Opcode extension 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of reg/mem16 with an immediate 16-bit value and
;;; store the result in reg/mem16.

;;; AND the contents of reg16 with an immediate 16-bit value and
;;; store the result in reg16.
(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 4
  :encoding (modrm imm)
  :operand-size-override t)

;;; AND the contents of mem16 with an immediate 16-bit value and
;;; store the result in mem16.
(define-instruction "AND"
  :modes (32 64)
  :operands ((memory 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 4
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of reg/mem32 with an immediate 32-bit value and
;;; store the result in reg/mem32.

;;; AND the contents of reg32 with an immediate 32-bit value and
;;; store the result in reg32.
(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 4
  :encoding (modrm imm))

;;; AND the contents of mem32 with an immediate 32-bit value and
;;; store the result in mem32.
(define-instruction "AND"
  :modes (32 64)
  :operands ((memory 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 4
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of reg/mem64 with a sign-extended immediate
;;; 32-bit value and store the result in reg/mem64.

;;; AND the contents of reg64 with a sign-extended immediate
;;; 32-bit value and store the result in reg64.
(define-instruction "AND"
  :modes (64)
  :operands ((gpr 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 4
  :encoding (modrm imm)
  :rex.w t)

;;; AND the contents of mem64 with a sign-extended immediate
;;; 32-bit value and store the result in mem64.
(define-instruction "AND"
  :modes (64)
  :operands ((memory 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 4
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of reg/mem (16/32/64) with a sign-extended 8-bit
;;; immediate value, and store the result in reg/mem.
;;;
;;; Opcodes: 83
;;; Opcode extension: 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of reg/mem16 with a sign-extended 8-bit immediate
;;; value, and store the result in reg/mem16.

;;; AND the contents of reg16 with a sign-extended 8-bit immediate
;;; value, and store the result in reg16.
(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 4
  :encoding (modrm imm)
  :operand-size-override t)

;;; AND the contents of mem16 with a sign-extended 8-bit immediate
;;; value, and store the result in mem16.
(define-instruction "AND"
  :modes (32 64)
  :operands ((memory 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 4
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of reg/mem32 with a sign-extended 8-bit immediate
;;; value, and store the result in reg/mem32.

;;; AND the contents of reg32 with a sign-extended 8-bit immediate
;;; value, and store the result in reg32.
(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 4
  :encoding (modrm imm))

;;; AND the contents of mem32 with a sign-extended 8-bit immediate
;;; value, and store the result in mem32.
(define-instruction "AND"
  :modes (32 64)
  :operands ((memory 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 4
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of reg/mem64 with a sign-extended 8-bit immediate
;;; value, and store the result in reg/mem64.

;;; AND the contents of reg64 with a sign-extended 8-bit immediate
;;; value, and store the result in reg64.
(define-instruction "AND"
  :modes (64)
  :operands ((gpr 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 4
  :encoding (modrm imm)
  :rex.w t)

;;; AND the contents of mem64 with a sign-extended 8-bit immediate
;;; value, and store the result in mem64.
(define-instruction "AND"
  :modes (64)
  :operands ((memory 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 4
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of a reg/mem8 and a reg8 and store the result in
;;; reg/mem8.
;;;
;;; Opcodes: 20

;;; AND the contents of a reg8 and a reg8 and store the result in
;;; reg8.
(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x20)
  :encoding (modrm reg))

;;; AND the contents of a mem8 and a reg8 and store the result in
;;; mem8.
(define-instruction "AND"
  :modes (32 64)
  :operands ((memory 8) (gpr 8))
  :opcodes (#x20)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of a reg/mem and a reg and store the contents in
;;; reg/mem (16/32/64).
;;; 
;;; Opcodes: 21

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of a reg/mem16 and a reg16 and store the contents
;;; in reg/mem16.

;;; AND the contents of a reg16 and a reg16 and store the contents
;;; in reg16.
(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x21)
  :encoding (modrm reg)
  :operand-size-override t)

;;; AND the contents of a mem16 and a reg16 and store the contents
;;; in mem16.
(define-instruction "AND"
  :modes (32 64)
  :operands ((memory 16) (gpr 16))
  :opcodes (#x21)
  :encoding (modrm reg)
  :lock t
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of a reg/mem32 and a reg32 and store the contents
;;; in reg/mem32.

;;; AND the contents of a reg32 and a reg32 and store the contents
;;; in reg32.
(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x21)
  :encoding (modrm reg))

;;; AND the contents of a mem32 and a reg32 and store the contents
;;; in mem32.
(define-instruction "AND"
  :modes (32 64)
  :operands ((memory 32) (gpr 32))
  :opcodes (#x21)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of a reg/mem64 and a reg64 and store the contents
;;; in reg/mem64.

;;; AND the contents of a reg64 and a reg64 and store the contents
;;; in reg64.
(define-instruction "AND"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x21)
  :encoding (modrm reg)
  :rex.w t)

;;; AND the contents of a mem64 and a reg64 and store the contents
;;; in mem64.
(define-instruction "AND"
  :modes (64)
  :operands ((memory 64) (gpr 64))
  :opcodes (#x21)
  :encoding (modrm reg)
  :lock t
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of an 8-bit register and an 8-bit reg/mem and
;;; store the result in the register.
;;;
;;; Opcodes: 22

;;; AND the contents of an 8-bit register and an 8-bit register and
;;; store the result in the register.
(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x22)
  :encoding (reg modrm))

;;; AND the contents of an 8-bit register and an 8-bit memory location
;;; and store the result in the register.
(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr 8) (memory 8))
  :opcodes (#x22)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of a register and the contents of a memory
;;; location or a register, and store the result in the register
;;; (16/32/64).
;;;
;;; Opcodes: 23

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of a 16-bit register and the contents of a 16-bit
;;; memory location or a 16-bit register, and store the result in the
;;; 16-bit register.

;;; AND the contents of a 16-bit register and the contents of a 16-bit
;;; a 16-bit register, and store the result in the 16-bit register.
(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x23)
  :encoding (reg modrm)
  :operand-size-override t)

;;; AND the contents of a 16-bit register and the contents of a 16-bit
;;; memory location, and store the result in the 16-bit register.
(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr 16) (memory 16))
  :opcodes (#x23)
  :encoding (reg modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of a 32-bit register and the contents of a 32-bit
;;; memory location or a 32-bit register, and store the result in the
;;; 32-bit register.

;;; AND the contents of a 32-bit register and the contents of a 32-bit
;;; a 32-bit register, and store the result in the 32-bit register.
(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x23)
  :encoding (reg modrm))

;;; AND the contents of a 32-bit register and the contents of a 32-bit
;;; memory location, and store the result in the 32-bit register.
(define-instruction "AND"
  :modes (32 64)
  :operands ((gpr 32) (memory 32))
  :opcodes (#x23)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND the contents of a 64-bit register and the contents of a 64-bit
;;; memory location or a 64-bit register, and store the result in the
;;; 64-bit register.

;;; AND the contents of a 64-bit register and the contents of a 64-bit
;;; a 64-bit register, and store the result in the 64-bit register.
(define-instruction "AND"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x23)
  :encoding (reg modrm)
  :rex.w t)

;;; AND the contents of a 64-bit register and the contents of a 64-bit
;;; memory location, and store the result in the 64-bit register.
(define-instruction "AND"
  :modes (64)
  :operands ((gpr 64) (memory 64))
  :opcodes (#x23)
  :encoding (reg modrm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic CALL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Near call with target specified by a relative displacement.
;;;
;;; Opcodes : E8

;;; Near call with target specified by a 16-bit relative displacement.
(define-instruction "CALL"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#xE8)
  :encoding (label)
  :operand-size-override t)

;;; Near call with target specified by a 32-bit relative displacement.
(define-instruction "CALL"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#xE8)
  :encoding (label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Near call with target specified by a register or a value in
;;; memory.
;;;
;;; Opcodes : FF
;;; Opcode extension: 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Near call with target specified by a 16-bit register or a 16-bit
;;; value in memory.

;;; Near call with target specified by a 16-bit register.
(define-instruction "CALL"
  :modes (32 64)
  :operands ((gpr 16))
  :opcodes (#xFF)
  :opcode-extension 2
  :encoding (modrm)
  :operand-size-override t)

;;; Near call with target specified by a 16-bit memory value.
(define-instruction "CALL"
  :modes (32 64)
  :operands ((memory 16))
  :opcodes (#xFF)
  :opcode-extension 2
  :encoding (modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Near call with target specified by a 32-bit register or a 32-bit
;;; value in memory.

;;; Near call with target specified by a 32-bit register.
(define-instruction "CALL"
  :modes (32)
  :operands ((gpr 32))
  :opcodes (#xFF)
  :opcode-extension 2
  :encoding (modrm))

;;; Near call with target specified by a 32-bit memory value.
(define-instruction "CALL"
  :modes (32)
  :operands ((memory 32))
  :opcodes (#xFF)
  :opcode-extension 2
  :encoding (modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Near call with target specified by a 64-bit register or a 64-bit
;;; value in memory.

;;; Near call with target specified by a 64-bit register.
(define-instruction "CALL"
  :modes (64)
  :operands ((gpr 64))
  :opcodes (#xFF)
  :opcode-extension 2
  :encoding (modrm))

;;; Near call with target specified by a 64-bit memory value.
(define-instruction "CALL"
  :modes (64)
  :operands ((memory 64))
  :opcodes (#xFF)
  :opcode-extension 2
  :encoding (modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonics CMP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit immediate value (source) with the contents of the
;;; AL GPR (destination)

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr-a 8) (imm 8))
  :opcodes (#x3C)
  :encoding (- imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare a 16-bit immediate value (source) with the contents of
;;; the AX GPR (destination)

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr-a 16) (imm 16))
  :opcodes (#x3D)
  :encoding (- imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare a 32-bit immediate value (source) with the contents of
;;; the EAX GPR (destination)

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr-a 32) (imm 32))
  :opcodes (#x3D)
  :encoding (- imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare a 32-bit immediate value (source) with the contents of
;;; the RAX GPR (destination)

(define-instruction "CMP"
  :modes (64)
  :operands ((gpr-a 64) (imm 32))
  :opcodes (#x3D)
  :encoding (- imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit immediate value (source) with the contents of an
;;; 8-bit GPR (destination)

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 7
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit immediate value (source) with the contents of an
;;; 8-bit memory location (destination)

(define-instruction "CMP"
  :modes (32 64)
  :operands ((memory 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 7
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare a 16-bit immediate value (source) with the contents of a
;;; 16-bit GPR (destination).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 7
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare a 32-bit immediate value (source) with the contents of a
;;; 32-bit GPR (destination).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 7
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare a 32-bit immediate value (source) with the contents of a
;;; 32-bit memory location (destination).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((memory 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 7
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare a 32-bit signed immediate value (source) with the contents
;;; of a 64-bit GPR (destination).

(define-instruction "CMP"
  :modes (64)
  :operands ((gpr 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 7
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare a 32-bit signed immediate value (source) with the contents
;;; of a 64-bit memory location (destination).

(define-instruction "CMP"
  :modes (64)
  :operands ((memory 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 7
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit signed immediate value (source) with the contents
;;; of a 16-bit GPR (destination).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 7
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit signed immediate value (source) with the contents
;;; of a 16-bit memory location (destination).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((memory 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 7
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit signed immediate value (source) with the contents
;;; of a 32-bit GPR (destination).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 7
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit signed immediate value (source) with the contents
;;; of a 32-bit memory location (destination).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((memory 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 7
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit signed immediate value (source) with the contents
;;; of a 64-bit GPR (destination).

(define-instruction "CMP"
  :modes (64)
  :operands ((gpr 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 7
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare an 8-bit signed immediate value (source) with the contents
;;; of a 64-bit memory location (destination).

(define-instruction "CMP"
  :modes (64)
  :operands ((memory 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 7
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of an 8-bit register or memory location
;;; (destination) with the contents of an 8-bit register (source).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of an 8-bit register (destination) with the
;;; contents of an 8-bit register (source).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x38)
  :encoding (modrm reg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of an 8-bit memory location (destination)
;;; with the contents of an 8-bit register (source).

(define-instruction "CMP"
  :modes (32 64)
  :operands ((memory 8) (gpr 8))
  :opcodes (#x38)
  :encoding (modrm reg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of a register or memory location
;;; (destination) with the contents of a register (source).
;;;
;;; Opcodes: 39

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of a 16-bit register or memory location
;;; (destination) with the contents of a 16-bit register (source).

;;; Compare the contents of a 16-bit register (destination) with the
;;; contents of a 16-bit register (source).
(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x39)
  :encoding (modrm reg)
  :operand-size-override t)

;;; Compare the contents of a 16-bit memory location (destination)
;;; with the contents of a 16-bit register (source).
(define-instruction "CMP"
  :modes (32 64)
  :operands ((memory 16) (gpr 16))
  :opcodes (#x39)
  :encoding (modrm reg)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of a 32-bit register or memory location
;;; (destination) with the contents of a 32-bit register (source).

;;; Compare the contents of a 32-bit register (destination) with the
;;; contents of a 32-bit register (source).
(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x39)
  :encoding (modrm reg))

;;; Compare the contents of a 32-bit memory location (destination)
;;; with the contents of a 32-bit register (source).
(define-instruction "CMP"
  :modes (32 64)
  :operands ((memory 32) (gpr 32))
  :opcodes (#x39)
  :encoding (modrm reg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of a 64-bit register or memory location
;;; (destination) with the contents of a 64-bit register (source).

;;; Compare the contents of a 64-bit register (destination) with the
;;; contents of a 64-bit register (source).
(define-instruction "CMP"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x39)
  :encoding (modrm reg)
  :rex.w t)

;;; Compare the contents of a 64-bit memory location (destination)
;;; with the contents of a 64-bit register (source).
(define-instruction "CMP"
  :modes (64)
  :operands ((memory 64) (gpr 64))
  :opcodes (#x39)
  :encoding (modrm reg)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of a register (destination) with the contents
;;; of a register or memory location (source).
;;;
;;; Opcodes: 3B

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of a 16-bit register (destination) with the
;;; contents of a 16-bit register or memory location (source).

;;; Compare the contents of a 16-bit register (destination) with the
;;; contents of a 16-bit register (source).
(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x3B)
  :encoding (reg modrm)
  :operand-size-override t)

;;; Compare the contents of a 16-bit register (destination) with the
;;; contents of a 16-bit memory location (source).
(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 16) (memory 16))
  :opcodes (#x3B)
  :encoding (reg modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of a 32-bit register (destination) with the
;;; contents of a 32-bit register or memory location (source).

;;; Compare the contents of a 32-bit register (destination) with the
;;; contents of a 32-bit register (source).
(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x3B)
  :encoding (reg modrm))

;;; Compare the contents of a 32-bit register (destination) with the
;;; contents of a 32-bit memory location (source).
(define-instruction "CMP"
  :modes (32 64)
  :operands ((gpr 32) (memory 32))
  :opcodes (#x3B)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compare the contents of a 64-bit register (destination) with the
;;; contents of a 64-bit register or memory location (source).

;;; Compare the contents of a 64-bit register (destination) with the
;;; contents of a 64-bit register (source).
(define-instruction "CMP"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x3B)
  :encoding (reg modrm)
  :rex.w t)

;;; Compare the contents of a 64-bit register (destination) with the
;;; contents of a 64-bit memory location (source).
(define-instruction "CMP"
  :modes (64)
  :operands ((gpr 64) (memory 64))
  :opcodes (#x3B)
  :encoding (reg modrm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonics Jcc

(define-instruction "JO"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x70)
  :encoding (label))

(define-instruction "JO"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x80)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JO"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x80)
  :encoding (label))

(define-instruction "JNO"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x71)
  :encoding (label))

(define-instruction "JNO"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x81)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNO"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x81)
  :encoding (label))

(define-instruction "JB"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x72)
  :encoding (label))

(define-instruction "JB"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x82)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JB"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x82)
  :encoding (label))

(define-instruction "JC"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x72)
  :encoding (label))

(define-instruction "JC"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x82)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JC"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x82)
  :encoding (label))

(define-instruction "JNAE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x72)
  :encoding (label))

(define-instruction "JNAE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x82)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNAE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x82)
  :encoding (label))

(define-instruction "JNB"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x73)
  :encoding (label))

(define-instruction "JNB"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x83)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNB"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x83)
  :encoding (label))

(define-instruction "JNC"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x73)
  :encoding (label))

(define-instruction "JNC"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x83)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNC"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x83)
  :encoding (label))

(define-instruction "JAE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x73)
  :encoding (label))

(define-instruction "JAE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x83)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JAE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x83)
  :encoding (label))

(define-instruction "JZ"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x74)
  :encoding (label))

(define-instruction "JZ"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x84)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JZ"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x84)
  :encoding (label))

(define-instruction "JE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x74)
  :encoding (label))

(define-instruction "JE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x84)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x84)
  :encoding (label))

(define-instruction "JNZ"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x75)
  :encoding (label))

(define-instruction "JNZ"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x85)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNZ"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x85)
  :encoding (label))

(define-instruction "JNE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x75)
  :encoding (label))

(define-instruction "JNE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x85)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x85)
  :encoding (label))

(define-instruction "JBE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x76)
  :encoding (label))

(define-instruction "JBE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x86)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JBE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x86)
  :encoding (label))

(define-instruction "JNA"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x76)
  :encoding (label))

(define-instruction "JNA"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x86)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNA"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x86)
  :encoding (label))

(define-instruction "JNBE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x77)
  :encoding (label))

(define-instruction "JNBE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x87)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNBE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x87)
  :encoding (label))

(define-instruction "JA"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x77)
  :encoding (label))

(define-instruction "JA"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x87)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JA"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x87)
  :encoding (label))

(define-instruction "JS"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x78)
  :encoding (label))

(define-instruction "JS"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x88)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JS"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x88)
  :encoding (label))

(define-instruction "JNS"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x79)
  :encoding (label))

(define-instruction "JNS"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x89)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNS"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x89)
  :encoding (label))

(define-instruction "JP"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7A)
  :encoding (label))

(define-instruction "JP"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8A)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JP"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8A)
  :encoding (label))

(define-instruction "JPE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7A)
  :encoding (label))

(define-instruction "JPE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8A)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JPE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8A)
  :encoding (label))

(define-instruction "JNP"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7B)
  :encoding (label))

(define-instruction "JNP"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8B)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNP"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8B)
  :encoding (label))

(define-instruction "JPO"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7B)
  :encoding (label))

(define-instruction "JPO"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8B)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JPO"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8B)
  :encoding (label))

(define-instruction "JL"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7C)
  :encoding (label))

(define-instruction "JL"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8C)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JL"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8C)
  :encoding (label))

(define-instruction "JNGE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7C)
  :encoding (label))

(define-instruction "JNGE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8C)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNGE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8C)
  :encoding (label))

(define-instruction "JNL"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7D)
  :encoding (label))

(define-instruction "JNL"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8D)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNL"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8D)
  :encoding (label))

(define-instruction "JGE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7D)
  :encoding (label))

(define-instruction "JGE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8D)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JGE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8D)
  :encoding (label))

(define-instruction "JLE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7E)
  :encoding (label))

(define-instruction "JLE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8E)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JLE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8E)
  :encoding (label))

(define-instruction "JNG"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7E)
  :encoding (label))

(define-instruction "JNG"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8E)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNG"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8E)
  :encoding (label))

(define-instruction "JNLE"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7F)
  :encoding (label))

(define-instruction "JNLE"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8F)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JNLE"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8F)
  :encoding (label))

(define-instruction "JG"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#x7F)
  :encoding (label))

(define-instruction "JG"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#x0F #x8F)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JG"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#x0F #x8F)
  :encoding (label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic JMP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Short jump with the target specified by an 8-bit signed
;;; displacement.
;;;
;;; Opcodes: EB

(define-instruction "JMP"
  :modes (32 64)
  :operands ((label 8))
  :opcodes (#xEB)
  :encoding (label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Near jump with the target specified by a 16-bit or 32-bit signed
;;; displacement.
;;;
;;; Opcodes: E9

(define-instruction "JMP"
  :modes (32 64)
  :operands ((label 16))
  :opcodes (#xE9)
  :encoding (label)
  :operand-size-override t)

(define-instruction "JMP"
  :modes (32 64)
  :operands ((label 32))
  :opcodes (#xE9)
  :encoding (label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Near jump with the target specified by a 16-bit, 32-bit, or 64-bit
;;; address in register or memory.
;;;
;;; Opcodes: FF

(define-instruction "JMP"
  :modes (32 64)
  :operands ((gpr 16))
  :opcodes (#xFF)
  :opcode-extension 4
  :encoding (modrm)
  :operand-size-override t)

(define-instruction "JMP"
  :modes (32 64)
  :operands ((memory 16))
  :opcodes (#xFF)
  :opcode-extension 4
  :encoding (modrm)
  :operand-size-override t)

(define-instruction "JMP"
  :modes (32)
  :operands ((gpr 32))
  :opcodes (#xFF)
  :opcode-extension 4
  :encoding (modrm))

(define-instruction "JMP"
  :modes (32)
  :operands ((memory 32))
  :opcodes (#xFF)
  :opcode-extension 4
  :encoding (modrm))

(define-instruction "JMP"
  :modes (64)
  :operands ((gpr 64))
  :opcodes (#xFF)
  :opcode-extension 4
  :encoding (modrm))

(define-instruction "JMP"
  :modes (64)
  :operands ((memory 64))
  :opcodes (#xFF)
  :opcode-extension 4
  :encoding (modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic MOV

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To an 8-bit register or memory location (destination), move the
;;; contents of an 8-bit register (source).
;;;
;;; Opcodes: 88

;;; To an 8-bit register (destination), move the contents of an 8-bit
;;; register (source).
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x88)
  :encoding (modrm reg))

;;; To an 8-bit memory location (destination), move the contents of an
;;; 8-bit register (source).
(define-instruction "MOV"
  :modes (32 64)
  :operands ((memory 8) (reg 8))
  :opcodes (#x88)
  :encoding (modrm reg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a register or memory location (destination), move the
;;; contents of a register (source).
;;;
;;; Opcodes: 89

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 16-bit register or memory location (destination), move the
;;; contents of a 16-bit register (source).

;;; To a 16-bit register (destination), move the contents of a 16-bit
;;; register (source).
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x89)
  :encoding (modrm reg)
  :operand-size-override t)

;;; To a 16-bit memory location (destination), move the contents of a
;;; 16-bit register (source).
(define-instruction "MOV"
  :modes (32 64)
  :operands ((memory 16) (gpr 16))
  :opcodes (#x89)
  :encoding (modrm reg)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 32-bit register or memory location (destination), move the
;;; contents of a 32-bit register (source).

;;; To a 32-bit register (destination), move the contents of a 32-bit
;;; register (source).
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x89)
  :encoding (modrm reg))

;;; To a 32-bit memory location (destination), move the contents of a
;;; 32-bit register (source).
(define-instruction "MOV"
  :modes (32 64)
  :operands ((memory 32) (gpr 32))
  :opcodes (#x89)
  :encoding (modrm reg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 64-bit register or memory location (destination), move the
;;; contents of a 64-bit register (source).

;;; To a 64-bit register (destination), move the contents of a 64-bit
;;; register (source).
(define-instruction "MOV"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x89)
  :encoding (modrm reg)
  :rex.w t)

;;; To a 64-bit memory location (destination), move the contents of a
;;; 64-bit register (source).
(define-instruction "MOV"
  :modes (64)
  :operands ((memory 64) (gpr 64))
  :opcodes (#x89)
  :encoding (modrm reg)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; To an 8-bit register (destination), move the contents of an 8-bit
;;; register or memory location.
;;;
;;; Opcodes: 8A

;;; To an 8-bit register (destination), move the contents of an 8-bit
;;; register.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x8A)
  :encoding (reg modrm))

;;; To an 8-bit register (destination), move the contents of an 8-bit
;;; memory location.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 8) (memory 8))
  :opcodes (#x8A)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a register (destination), move the contents of a register or
;;; memory location.
;;;
;;; Opcodes: 8B

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 16-bit register (destination), move the contents of a 16-bit
;;; register or memory location.

;;; To a 16-bit register (destination), move the contents of a 16-bit
;;; register.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x8B)
  :encoding (reg modrm)
  :operand-size-override t)

;;; To a 16-bit register (destination), move the contents of a 16-bit
;;; memory location.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 16) (memory 16))
  :opcodes (#x8B)
  :encoding (reg modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 32-bit register (destination), move the contents of a 32-bit
;;; register or memory location.

;;; To a 32-bit register (destination), move the contents of a 32-bit
;;; register.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x8B)
  :encoding (reg modrm))

;;; To a 32-bit register (destination), move the contents of a 32-bit
;;; memory location.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 32) (memory 32))
  :opcodes (#x8B)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 64-bit register (destination), move the contents of a 64-bit
;;; register or memory location.

;;; To a 64-bit register (destination), move the contents of a 64-bit
;;; register.
(define-instruction "MOV"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x8B)
  :encoding (reg modrm)
  :rex.w t)

;;; To a 64-bit register (destination), move the contents of a 64-bit
;;; memory location.
(define-instruction "MOV"
  :modes (64)
  :operands ((gpr 64) (memory 64))
  :opcodes (#x8B)
  :encoding (reg modrm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To an 8-bit register (destination) move an 8-bit immediate value.
;;;
;;; Opcodes: B0, B1, ..., B7

(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 8) (imm 8))
  :opcodes (#xB0)
  :encoding (+r imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a register (destination) move an immediate value.
;;;
;;; Opcodes: B8, B9, ..., BF

;;; To a 16-bit register (destination) move a 16-bit immediate value.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 16) (imm 16))
  :opcodes (#xB8)
  :encoding (+r imm)
  :operand-size-override t)

;;; To a 32-bit register (destination) move a 32-bit immediate value.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 32) (imm 32))
  :opcodes (#xB8)
  :encoding (+r imm))

;;; To a 64-bit register (destination) move a 64-bit immediate value.
(define-instruction "MOV"
  :modes (64)
  :operands ((gpr 64) (imm 64))
  :opcodes (#xB8)
  :encoding (+r imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To an 8-bit register or memory location, move an 8-bit immediate
;;; value.
;;;
;;; Opcodes: C6

;;; To an 8-bit register, move an 8-bit immediate value.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 8) (imm 8))
  :opcodes C6
  :opcode-extension 0
  :encoding (modrm imm))

;;; To an 8-bit memory location, move an 80bit immediate value.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((memory 8) (imm 8))
  :opcodes C6
  :opcode-extension 0
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a register or memory location, move an immediate value.
;;;
;;; Opcodes: C7

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 16-bit register or memory location, move a 16-bit immediate
;;; value.

;;; To a 16-bit register, move a 16-bit immediate
;;; value.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 16) (imm 16))
  :opcodes (#xC7)
  :opcode-extension 0
  :encoding (modrm imm)
  :operand-size-override t)

;;; To a 16-bit memory location, move a 16-bit immediate
;;; value.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((memory 16) (imm 16))
  :opcodes (#xC7)
  :opcode-extension 0
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 32-bit register or memory location, move a 32-bit immediate
;;; value.

;;; To a 32-bit register, move a 32-bit immediate
;;; value.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((gpr 32) (imm 32))
  :opcodes (#xC7)
  :opcode-extension 0
  :encoding (modrm imm))

;;; To a 32-bit memory location, move a 32-bit immediate
;;; value.
(define-instruction "MOV"
  :modes (32 64)
  :operands ((memory 32) (imm 32))
  :opcodes (#xC7)
  :opcode-extension 0
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To a 64-bit register or memory location, move a 64-bit immediate
;;; value.

;;; To a 64-bit register, move a 64-bit immediate
;;; value.
(define-instruction "MOV"
  :modes (64)
  :operands ((gpr 64) (imm 64))
  :opcodes (#xC7)
  :opcode-extension 0
  :encoding (modrm imm)
  :rex.w t)

;;; To a 64-bit memory location, move a 64-bit immediate
;;; value.
(define-instruction "MOV"
  :modes (64)
  :operands ((memory 64) (imm 64))
  :opcodes (#xC7)
  :opcode-extension 0
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic OR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of AL with an immediate 8-bit value and store the
;;; result in AL.
;;;
;;; Opcodes: OC

(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr-a 8) (imm 8))
  :opcodes (#x0C)
  :encoding (- imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of GPR A (16/32/64) with an immediate value
;;; (16/32) and store the result in GPR A.
;;;
;;; Opcodes: 0D

;;; OR the contents of GPR AX with an immediate 16-bit value and
;;; store the result in GPR AX.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr-a 16) (imm 16))
  :opcodes (#x0D)
  :encoding (- imm)
  :operand-size-override t)

;;; OR the contents of GPR EAX with an immediate 32-bit value and
;;; store the result in GPR EAX.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr-a 32) (imm 32))
  :opcodes (#x0D)
  :encoding (- imm))

;;; OR the contents of GPR RAX with a sign-extended 32-bit immediate
;;; value and store the result in GPR RAX.
(define-instruction "OR"
  :modes (64)
  :operands ((gpr-a 64) (imm 32))
  :opcodes (#x0D)
  :encoding (- imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of reg/mem8 with an immediate 8-bit value, and
;;; store the result in reg/mem8.
;;;
;;; Opcodes: 80
;;; Opcode extension: 1

;;; OR the contents of reg8 with an immediate 8-bit value, and store
;;; the result in reg8.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 1
  :encoding (modrm imm))

;;; OR the contents of mem8 with an immediate 8-bit value, and store
;;; the result in mem8.
(define-instruction "OR"
  :modes (32 64)
  :operands ((memory 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 1
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of reg/mem (16/32/64) with an immediate value
;;; (16/32) and store the result in reg/mem.
;;;
;;; Opcodes: 81
;;; Opcode extension 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of reg/mem16 with an immediate 16-bit value and
;;; store the result in reg/mem16.

;;; OR the contents of reg16 with an immediate 16-bit value and
;;; store the result in reg16.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 1
  :encoding (modrm imm)
  :operand-size-override t)

;;; OR the contents of mem16 with an immediate 16-bit value and
;;; store the result in mem16.
(define-instruction "OR"
  :modes (32 64)
  :operands ((memory 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 1
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of reg/mem32 with an immediate 32-bit value and
;;; store the result in reg/mem32.

;;; OR the contents of reg32 with an immediate 32-bit value and
;;; store the result in reg32.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 1
  :encoding (modrm imm))

;;; OR the contents of mem32 with an immediate 32-bit value and
;;; store the result in mem32.
(define-instruction "OR"
  :modes (32 64)
  :operands ((memory 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 1
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of reg/mem64 with a sign-extended immediate
;;; 32-bit value and store the result in reg/mem64.

;;; OR the contents of reg64 with a sign-extended immediate
;;; 32-bit value and store the result in reg64.
(define-instruction "OR"
  :modes (64)
  :operands ((gpr 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 1
  :encoding (modrm imm)
  :rex.w t)

;;; OR the contents of mem64 with a sign-extended immediate
;;; 32-bit value and store the result in mem64.
(define-instruction "OR"
  :modes (64)
  :operands ((memory 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 1
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of reg/mem (16/32/64) with a sign-extended 8-bit
;;; immediate value, and store the result in reg/mem.
;;;
;;; Opcodes: 83
;;; Opcode extension: 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of reg/mem16 with a sign-extended 8-bit immediate
;;; value, and store the result in reg/mem16.

;;; OR the contents of reg16 with a sign-extended 8-bit immediate
;;; value, and store the result in reg16.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 1
  :encoding (modrm imm)
  :operand-size-override t)

;;; OR the contents of mem16 with a sign-extended 8-bit immediate
;;; value, and store the result in mem16.
(define-instruction "OR"
  :modes (32 64)
  :operands ((memory 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 1
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of reg/mem32 with a sign-extended 8-bit immediate
;;; value, and store the result in reg/mem32.

;;; OR the contents of reg32 with a sign-extended 8-bit immediate
;;; value, and store the result in reg32.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 1
  :encoding (modrm imm))

;;; OR the contents of mem32 with a sign-extended 8-bit immediate
;;; value, and store the result in mem32.
(define-instruction "OR"
  :modes (32 64)
  :operands ((memory 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 1
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of reg/mem64 with a sign-extended 8-bit immediate
;;; value, and store the result in reg/mem64.

;;; OR the contents of reg64 with a sign-extended 8-bit immediate
;;; value, and store the result in reg64.
(define-instruction "OR"
  :modes (64)
  :operands ((gpr 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 1
  :encoding (modrm imm)
  :rex.w t)

;;; OR the contents of mem64 with a sign-extended 8-bit immediate
;;; value, and store the result in mem64.
(define-instruction "OR"
  :modes (64)
  :operands ((memory 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 1
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a reg/mem8 and a reg8 and store the result in
;;; reg/mem8.
;;;
;;; Opcodes: 08

;;; OR the contents of a reg8 and a reg8 and store the result in
;;; reg8.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x08)
  :encoding (modrm reg))

;;; OR the contents of a mem8 and a reg8 and store the result in
;;; mem8.
(define-instruction "OR"
  :modes (32 64)
  :operands ((memory 8) (gpr 8))
  :opcodes (#x08)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a reg/mem and a reg and store the contents in
;;; reg/mem (16/32/64).
;;; 
;;; Opcodes: 09

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a reg/mem16 and a reg16 and store the contents
;;; in reg/mem16.

;;; OR the contents of a reg16 and a reg16 and store the contents
;;; in reg16.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x09)
  :encoding (modrm reg)
  :operand-size-override t)

;;; OR the contents of a mem16 and a reg16 and store the contents
;;; in mem16.
(define-instruction "OR"
  :modes (32 64)
  :operands ((memory 16) (gpr 16))
  :opcodes (#x09)
  :encoding (modrm reg)
  :lock t
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a reg/mem32 and a reg32 and store the contents
;;; in reg/mem32.

;;; OR the contents of a reg32 and a reg32 and store the contents
;;; in reg32.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x09)
  :encoding (modrm reg))

;;; OR the contents of a mem32 and a reg32 and store the contents
;;; in mem32.
(define-instruction "OR"
  :modes (32 64)
  :operands ((memory 32) (gpr 32))
  :opcodes (#x09)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a reg/mem64 and a reg64 and store the contents
;;; in reg/mem64.

;;; OR the contents of a reg64 and a reg64 and store the contents
;;; in reg64.
(define-instruction "OR"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x09)
  :encoding (modrm reg)
  :rex.w t)

;;; OR the contents of a mem64 and a reg64 and store the contents
;;; in mem64.
(define-instruction "OR"
  :modes (64)
  :operands ((memory 64) (gpr 64))
  :opcodes (#x09)
  :encoding (modrm reg)
  :lock t
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of an 8-bit register and an 8-bit reg/mem and
;;; store the result in the register.
;;;
;;; Opcodes: 0A

;;; OR the contents of an 8-bit register and an 8-bit register and
;;; store the result in the register.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x0A)
  :encoding (reg modrm))

;;; OR the contents of an 8-bit register and an 8-bit memory location
;;; and store the result in the register.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 8) (memory 8))
  :opcodes (#x0A)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a register and the contents of a memory
;;; location or a register, and store the result in the register
;;; (16/32/64).
;;;
;;; Opcodes: 0B

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a 16-bit register and the contents of a 16-bit
;;; memory location or a 16-bit register, and store the result in the
;;; 16-bit register.

;;; OR the contents of a 16-bit register and the contents of a 16-bit
;;; a 16-bit register, and store the result in the 16-bit register.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x0B)
  :encoding (reg modrm)
  :operand-size-override t)

;;; OR the contents of a 16-bit register and the contents of a 16-bit
;;; memory location, and store the result in the 16-bit register.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 16) (memory 16))
  :opcodes (#x0B)
  :encoding (reg modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a 32-bit register and the contents of a 32-bit
;;; memory location or a 32-bit register, and store the result in the
;;; 32-bit register.

;;; OR the contents of a 32-bit register and the contents of a 32-bit
;;; a 32-bit register, and store the result in the 32-bit register.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x0B)
  :encoding (reg modrm))

;;; OR the contents of a 32-bit register and the contents of a 32-bit
;;; memory location, and store the result in the 32-bit register.
(define-instruction "OR"
  :modes (32 64)
  :operands ((gpr 32) (memory 32))
  :opcodes (#x0B)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OR the contents of a 64-bit register and the contents of a 64-bit
;;; memory location or a 64-bit register, and store the result in the
;;; 64-bit register.

;;; OR the contents of a 64-bit register and the contents of a 64-bit
;;; a 64-bit register, and store the result in the 64-bit register.
(define-instruction "OR"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x0B)
  :encoding (reg modrm)
  :rex.w t)

;;; OR the contents of a 64-bit register and the contents of a 64-bit
;;; memory location, and store the result in the 64-bit register.
(define-instruction "OR"
  :modes (64)
  :operands ((gpr 64) (memory 64))
  :opcodes (#x0B)
  :encoding (reg modrm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opcodes: C3

(define-instruction "RET"
  :modes (32 64)
  :operands ()
  :opcodes (#xC3)
  :encoding ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opcodes: C2

(define-instruction "RET"
  :modes (32 64)
  :operands ((imm 16))
  :opcodes (#xC2)
  :encoding (imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic SUB

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From the contents of GPR A (destination), subtract an 8-bit
;;; immediate value (source), and store the result in GPR A.
;;; 
;;; Opcodes: 2C

(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr-a 8) (imm 8))
  :opcodes (#x2C)
  :encoding (- imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From GPR A (16/32/64) (destination), subtract an immediate value
;;; (16/32) (source), and store the result in GPR A.
;;; 
;;; Opcodes: 2D

;;; From GPR AX (destination), subtract a 16-bit immediate value
;;; (source), and store the result in GPR AX.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr-a 16) (imm 16))
  :opcodes (#x2D)
  :encoding (- imm)
  :operand-size-override t)

;;; From GPR EAX (destination), subtract a 16-bit immediate value
;;; (source), and store the result in GPR EAX.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr-a 32) (imm 32))
  :opcodes (#x2D)
  :encoding (- imm))

;;; From GPR RAX (destination), subtract a 32-bit sign-extended
;;; immediate value (source), and store the result in GPR RAX.
(define-instruction "SUB"
  :modes (64)
  :operands ((gpr-a 64) (simm 32))
  :opcodes (#x2D)
  :encoding (- imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From an 8-bit register or memory location (destination), subtract
;;; an 8-bit immediate value (source), and store the result in the
;;; destination.
;;; 
;;; Opcodes: 80
;;; Opcode extension: 5

;;; From an 8-bit GPR (destination), subtract an 8-bit immediate value
;;; (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 5
  :encoding (modrm imm))

;;; From an 8-bit memory location (destination), subtract an 8-bit
;;; immediate value (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((memory 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 5
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a register or memory location (16/32/64) (destination),
;;; subtract an immediate value (16/32) (source) and store the result
;;; in the destination.
;;;
;;; Opcodes: 81
;;; Opcode extension 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 16-bit register or memory location (destination), subtract
;;; an immediate 16-bit value (source) and store the result in the
;;; destination.

;;; From a 16-bit GPR (destination), subtract an immediate 16-bit
;;; value (source) and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 5
  :encoding (modrm imm)
  :operand-size-override t)

;;; From a 16-bit memory location (destination), subtract an immediate
;;; 16-bit value (source) and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((memory 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 5
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 32-bit register or memory location (destination), subtract
;;; an immediate 32-bit value (source) and store the result in the
;;; destination.

;;; From a 32-bit GPR (destination), subtract an immediate 32-bit
;;; value (source) and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 5
  :encoding (modrm imm))

;;; From a 32-bit memory location (destination), subtract an immediate
;;; 32-bit value (source) and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((memory 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 5
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 64-bit register or memory location (destination), subtract
;;; an immediate sign-extended 32-bit value (source) and store the
;;; result in the destination.

;;; From a 64-bit GPR (destination), subtract an immediate
;;; sign-extended 32-bit value (source) and store the result in the
;;; destination.
(define-instruction "SUB"
  :modes (64)
  :operands ((gpr 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 5
  :encoding (modrm imm)
  :rex.w t)

;;; From a 64-bit memory location (destination), subtract an immediate
;;; sign-extended 32-bit value (source) and store the result in the
;;; destination.
(define-instruction "SUB"
  :modes (64)
  :operands ((memory 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 5
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a register or memory location (16/32/64) (destination),
;;; subtract an 8-bit sign-extended immediate value (source), and
;;; store the result in the destination.
;;;
;;; Opcodes: 83
;;; Opcode extension: 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 16-bit register or memory location (destination), subtract
;;; an 8-bit sign-extended immediate value (source), and store the
;;; result in the destination.

;;; From a 16-bit GPR (destination), subtract an 8-bit sign-extended
;;; immediate value (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 5
  :encoding (modrm imm)
  :operand-size-override t)

;;; From a 16-bit memory location (destination), subtract an 8-bit
;;; sign-extended immediate value (source), and store the result in
;;; the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((memory 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 5
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 32-bit register or memory location (destination), subtract
;;; an 8-bit sign-extended immediate value (source), and store the
;;; result in the destination.

;;; From a 32-bit GPR (destination), subtract an 8-bit sign-extended
;;; immediate value (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 5
  :encoding (modrm imm))

;;; From a 32-bit memory location (destination), subtract an 8-bit
;;; sign-extended immediate value (source), and store the result in
;;; the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((memory 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 5
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 64-bit register or memory location (destination), subtract
;;; an 8-bit sign-extended immediate value (source), and store the
;;; result in the destination.

;;; From a 64-bit GPR (destination), subtract an 8-bit sign-extended
;;; immediate value (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (64)
  :operands ((gpr 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 5
  :encoding (modrm imm)
  :rex.w t)

;;; From a 64-bit memory location (destination), subtract an 8-bit
;;; sign-extended immediate value (source), and store the result in
;;; the destination.
(define-instruction "SUB"
  :modes (64)
  :operands ((memory 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 5
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From an 8-bit register or memory location (destination), subtract
;;; the contents of an 8-bit register (source) and store the result in
;;; the destination.
;;;
;;; Opcodes: 28

;;; From an 8-bit GPR (destination), subtract the contents of an 8-bit
;;; register (source) and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x28)
  :encoding (modrm reg))

;;; From an 8-bit memory location (destination), subtract the contents
;;; of an 8-bit register (source) and store the result in the
;;; destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((memory 8) (gpr 8))
  :opcodes (#x28)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a GPR or memory location (16/32/64) (destination), subtract
;;; the contents of a GPR (16/32/64) (source), and store the result in
;;; the destination.
;;;
;;; Opcodes: 29

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 16-bit GPR or memory location (destination), subtract the
;;; contents of a 16-bit GPR (source), and store the result in the
;;; destination.

;;; From a 16-bit GPR (destination), subtract the contents of a 16-bit
;;; GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x29)
  :encoding (modrm reg)
  :operand-size-override t)

;;; From a 16-bit memory location (destination), subtract the contents
;;; of a 16-bit GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((memory 16) (gpr 16))
  :opcodes (#x29)
  :encoding (modrm reg)
  :lock t
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 32-bit GPR or memory location (destination), subtract the
;;; contents of a 32-bit GPR (source), and store the result in the
;;; destination.

;;; From a 32-bit GPR (destination), subtract the contents of a 32-bit
;;; GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x29)
  :encoding (modrm reg))

;;; From a 32-bit memory location (destination), subtract the contents
;;; of a 32-bit GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((memory 32) (gpr 32))
  :opcodes (#x29)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 64-bit GPR or memory location (destination), subtract the
;;; contents of a 64-bit GPR (source), and store the result in the
;;; destination.

;;; From a 64-bit GPR (destination), subtract the contents of a 64-bit
;;; GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x29)
  :encoding (modrm reg)
  :rex.w t)

;;; From a 64-bit memory location (destination), subtract the contents
;;; of a 64-bit GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (64)
  :operands ((memory 64) (gpr 64))
  :opcodes (#x29)
  :encoding (modrm reg)
  :lock t
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From an 8-bit GPR (destination), subtract the contents of an 8-bit
;;; GPR or memory location (source), and store the result in the
;;; destination.
;;;
;;; Opcodes: 2A

;;; From an 8-bit GPR (destination), subtract the contents of an 8-bit
;;; GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x2A)
  :encoding (reg modrm))

;;; From an 8-bit GPR (destination), subtract the contents of an 8-bit
;;; memory location (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 8) (memory 8))
  :opcodes (#x2A)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a GPR (16/32/64) (destination), subtract the contents of a
;;; GPR or memory location (16/32/64) (source), and store the result
;;; in the destination.
;;;
;;; Opcodes: 2B

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 16-bit GPR (destination), subtract the contents of a 16-bit
;;; GPR or memory location (source), and store the result in the
;;; destination.

;;; From a 16-bit GPR (destination), subtract the contents of a 16-bit
;;; GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x2B)
  :encoding (reg modrm)
  :operand-size-override t)

;;; From a 16-bit GPR (destination), subtract the contents of a 16-bit
;;; memory location (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 16) (memory 16))
  :opcodes (#x2B)
  :encoding (reg modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 32-bit GPR (destination), subtract the contents of a 32-bit
;;; GPR or memory location (source), and store the result in the
;;; destination.

;;; From a 32-bit GPR (destination), subtract the contents of a 32-bit
;;; GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x2B)
  :encoding (reg modrm))

;;; From a 32-bit GPR (destination), subtract the contents of a 32-bit
;;; memory location (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (32 64)
  :operands ((gpr 32) (memory 32))
  :opcodes (#x2B)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a 64-bit GPR (destination), subtract the contents of a 64-bit
;;; GPR or memory location (source), and store the result in the
;;; destination.

;;; From a 64-bit GPR (destination), subtract the contents of a 64-bit
;;; GPR (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x2B)
  :encoding (reg modrm)
  :rex.w t)

;;; From a 64-bit GPR (destination), subtract the contents of a 64-bit
;;; memory location (source), and store the result in the destination.
(define-instruction "SUB"
  :modes (64)
  :operands ((gpr 64) (memory 64))
  :opcodes (#x2B)
  :encoding (reg modrm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic XOR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of AL with an immediate 8-bit value and store the
;;; result in AL.
;;;
;;; Opcodes: 34

(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr-a 8) (imm 8))
  :opcodes (#x34)
  :encoding (- imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of GPR A (16/32/64) with an immediate value
;;; (16/32) and store the result in GPR A.
;;;
;;; Opcodes: 35

;;; XOR the contents of GPR AX with an immediate 16-bit value and
;;; store the result in GPR AX.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr-a 16) (imm 16))
  :opcodes (#x35)
  :encoding (- imm)
  :operand-size-override t)

;;; XOR the contents of GPR EAX with an immediate 32-bit value and
;;; store the result in GPR EAX.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr-a 32) (imm 32))
  :opcodes (#x35)
  :encoding (- imm))

;;; XOR the contents of GPR RAX with a sign-extended 32-bit immediate
;;; value and store the result in GPR RAX.
(define-instruction "XOR"
  :modes (64)
  :operands ((gpr-a 64) (imm 32))
  :opcodes (#x35)
  :encoding (- imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of reg/mem8 with an immediate 8-bit value, and
;;; store the result in reg/mem8.
;;;
;;; Opcodes: 80
;;; Opcode extension: 6

;;; XOR the contents of reg8 with an immediate 8-bit value, and store
;;; the result in reg8.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 6
  :encoding (modrm imm))

;;; XOR the contents of mem8 with an immediate 8-bit value, and store
;;; the result in mem8.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((memory 8) (imm 8))
  :opcodes (#x80)
  :opcode-extension 6
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of reg/mem (16/32/64) with an immediate value
;;; (16/32) and store the result in reg/mem.
;;;
;;; Opcodes: 81
;;; Opcode extension 6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of reg/mem16 with an immediate 16-bit value and
;;; store the result in reg/mem16.

;;; XOR the contents of reg16 with an immediate 16-bit value and
;;; store the result in reg16.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 6
  :encoding (modrm imm)
  :operand-size-override t)

;;; XOR the contents of mem16 with an immediate 16-bit value and
;;; store the result in mem16.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((memory 16) (imm 16))
  :opcodes (#x81)
  :opcode-extension 6
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of reg/mem32 with an immediate 32-bit value and
;;; store the result in reg/mem32.

;;; XOR the contents of reg32 with an immediate 32-bit value and
;;; store the result in reg32.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 6
  :encoding (modrm imm))

;;; XOR the contents of mem32 with an immediate 32-bit value and
;;; store the result in mem32.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((memory 32) (imm 32))
  :opcodes (#x81)
  :opcode-extension 6
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of reg/mem64 with a sign-extended immediate
;;; 32-bit value and store the result in reg/mem64.

;;; XOR the contents of reg64 with a sign-extended immediate
;;; 32-bit value and store the result in reg64.
(define-instruction "XOR"
  :modes (64)
  :operands ((gpr 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 6
  :encoding (modrm imm)
  :rex.w t)

;;; XOR the contents of mem64 with a sign-extended immediate
;;; 32-bit value and store the result in mem64.
(define-instruction "XOR"
  :modes (64)
  :operands ((memory 64) (simm 32))
  :opcodes (#x81)
  :opcode-extension 6
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of reg/mem (16/32/64) with a sign-extended 8-bit
;;; immediate value, and store the result in reg/mem.
;;;
;;; Opcodes: 83
;;; Opcode extension: 6

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of reg/mem16 with a sign-extended 8-bit immediate
;;; value, and store the result in reg/mem16.

;;; XOR the contents of reg16 with a sign-extended 8-bit immediate
;;; value, and store the result in reg16.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 6
  :encoding (modrm imm)
  :operand-size-override t)

;;; XOR the contents of mem16 with a sign-extended 8-bit immediate
;;; value, and store the result in mem16.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((memory 16) (simm 8))
  :opcodes (#x83)
  :opcode-extension 6
  :encoding (modrm imm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of reg/mem32 with a sign-extended 8-bit immediate
;;; value, and store the result in reg/mem32.

;;; XOR the contents of reg32 with a sign-extended 8-bit immediate
;;; value, and store the result in reg32.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 6
  :encoding (modrm imm))

;;; XOR the contents of mem32 with a sign-extended 8-bit immediate
;;; value, and store the result in mem32.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((memory 32) (simm 8))
  :opcodes (#x83)
  :opcode-extension 6
  :encoding (modrm imm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of reg/mem64 with a sign-extended 8-bit immediate
;;; value, and store the result in reg/mem64.

;;; XOR the contents of reg64 with a sign-extended 8-bit immediate
;;; value, and store the result in reg64.
(define-instruction "XOR"
  :modes (64)
  :operands ((gpr 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 6
  :encoding (modrm imm)
  :rex.w t)

;;; XOR the contents of mem64 with a sign-extended 8-bit immediate
;;; value, and store the result in mem64.
(define-instruction "XOR"
  :modes (64)
  :operands ((memory 64) (simm 8))
  :opcodes (#x83)
  :opcode-extension 6
  :encoding (modrm imm)
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a reg/mem8 and a reg8 and store the result in
;;; reg/mem8.
;;;
;;; Opcodes: 30

;;; XOR the contents of a reg8 and a reg8 and store the result in
;;; reg8.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x30)
  :encoding (modrm reg))

;;; XOR the contents of a mem8 and a reg8 and store the result in
;;; mem8.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((memory 8) (gpr 8))
  :opcodes (#x30)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a reg/mem and a reg and store the contents in
;;; reg/mem (16/32/64).
;;; 
;;; Opcodes: 31

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a reg/mem16 and a reg16 and store the contents
;;; in reg/mem16.

;;; XOR the contents of a reg16 and a reg16 and store the contents
;;; in reg16.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x31)
  :encoding (modrm reg)
  :operand-size-override t)

;;; XOR the contents of a mem16 and a reg16 and store the contents
;;; in mem16.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((memory 16) (gpr 16))
  :opcodes (#x31)
  :encoding (modrm reg)
  :lock t
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a reg/mem32 and a reg32 and store the contents
;;; in reg/mem32.

;;; XOR the contents of a reg32 and a reg32 and store the contents
;;; in reg32.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x31)
  :encoding (modrm reg))

;;; XOR the contents of a mem32 and a reg32 and store the contents
;;; in mem32.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((memory 32) (gpr 32))
  :opcodes (#x31)
  :encoding (modrm reg)
  :lock t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a reg/mem64 and a reg64 and store the contents
;;; in reg/mem64.

;;; XOR the contents of a reg64 and a reg64 and store the contents
;;; in reg64.
(define-instruction "XOR"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x31)
  :encoding (modrm reg)
  :rex.w t)

;;; XOR the contents of a mem64 and a reg64 and store the contents
;;; in mem64.
(define-instruction "XOR"
  :modes (64)
  :operands ((memory 64) (gpr 64))
  :opcodes (#x31)
  :encoding (modrm reg)
  :lock t
  :rex.w t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of an 8-bit register and an 8-bit reg/mem and
;;; store the result in the register.
;;;
;;; Opcodes: 32

;;; XOR the contents of an 8-bit register and an 8-bit register and
;;; store the result in the register.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 8) (gpr 8))
  :opcodes (#x32)
  :encoding (reg modrm))

;;; XOR the contents of an 8-bit register and an 8-bit memory location
;;; and store the result in the register.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 8) (memory 8))
  :opcodes (#x32)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a register and the contents of a memory
;;; location or a register, and store the result in the register
;;; (16/32/64).
;;;
;;; Opcodes: 33

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a 16-bit register and the contents of a 16-bit
;;; memory location or a 16-bit register, and store the result in the
;;; 16-bit register.

;;; XOR the contents of a 16-bit register and the contents of a 16-bit
;;; a 16-bit register, and store the result in the 16-bit register.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 16) (gpr 16))
  :opcodes (#x33)
  :encoding (reg modrm)
  :operand-size-override t)

;;; XOR the contents of a 16-bit register and the contents of a 16-bit
;;; memory location, and store the result in the 16-bit register.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 16) (memory 16))
  :opcodes (#x33)
  :encoding (reg modrm)
  :operand-size-override t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a 32-bit register and the contents of a 32-bit
;;; memory location or a 32-bit register, and store the result in the
;;; 32-bit register.

;;; XOR the contents of a 32-bit register and the contents of a 32-bit
;;; a 32-bit register, and store the result in the 32-bit register.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 32) (gpr 32))
  :opcodes (#x33)
  :encoding (reg modrm))

;;; XOR the contents of a 32-bit register and the contents of a 32-bit
;;; memory location, and store the result in the 32-bit register.
(define-instruction "XOR"
  :modes (32 64)
  :operands ((gpr 32) (memory 32))
  :opcodes (#x33)
  :encoding (reg modrm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; XOR the contents of a 64-bit register and the contents of a 64-bit
;;; memory location or a 64-bit register, and store the result in the
;;; 64-bit register.

;;; XOR the contents of a 64-bit register and the contents of a 64-bit
;;; a 64-bit register, and store the result in the 64-bit register.
(define-instruction "XOR"
  :modes (64)
  :operands ((gpr 64) (gpr 64))
  :opcodes (#x33)
  :encoding (reg modrm)
  :rex.w t)

;;; XOR the contents of a 64-bit register and the contents of a 64-bit
;;; memory location, and store the result in the 64-bit register.
(define-instruction "XOR"
  :modes (64)
  :operands ((gpr 64) (memory 64))
  :opcodes (#x33)
  :encoding (reg modrm)
  :rex.w t)
