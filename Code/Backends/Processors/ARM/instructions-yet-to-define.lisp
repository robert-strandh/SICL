;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;; Instruction: BLX (register)
;; ;;;
;; ;;; Branch with Link and Exchange (register) calls a subroutine at an
;; ;;; address and instruction set specified by a register.
;; (define-instruction
;;     ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
;;     ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
;;     "| cond  |0 0 0 1 0 0 1 0|1 1 1 1|1 1 1 1|1 1 1 1|0 0 1 1|  Rm   |"
;;     t
;;   (BLX{<c>} <Rm>)
;;   ())

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;; Instruction: BX
;; ;;;
;; ;;; Branch and Exchange causes a branch to an address and instruction
;; ;;; set specified by a register.
;; (define-instruction
;;     ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
;;     ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
;;     "| cond  |0 0 0 1 0 0 1 0|1 1 1 1|1 1 1 1|1 1 1 1|0 0 0 1|  Rm   |"
;;     t
;;   (BX{<c>} <Rm>)
;;   ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: BXJ
;;;
;;; Branch and Exchange Jazelle attempts to change to Jazelle
;;; state. If the attempt fails, it branches to an address and
;;; instruction set specified by a register as though it were a BX
;;; instruction.  In an implementation that includes the
;;; Virtualization Extensions, if HSTR.TJDBX is set to 1, execution of
;;; a BXJ instruction in a Non-secure mode other than Hyp mode
;;; generates a Hyp Trap exception.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0 0 1 0 0 1 0|1 1 1 1|1 1 1 1|1 1 1 1|0 0 1 0|  Rm   |"
    t
  (BXJ{<c>} <Rm>)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: CDP
;;; 
;;; Coprocessor Data Processing tells a coprocessor to perform an
;;; operation that is independent of ARM core registers and memory. If
;;; no coprocessor can execute the instruction, an Undefined
;;; Instruction exception is generated.  This is a generic coprocessor
;;; instruction. Some of the fields have no functionality defined by
;;; the architecture and are free for use by the coprocessor
;;; instruction set designer. These are the opc1, opc2, CRd, CRn, and
;;; CRm fields.  However, coprocessors CP8-CP15 are reserved for use
;;; by ARM, and this manual defines the valid CDP and CDP2
;;; instructions when coproc is in the range p8-p15
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |1 1 1 0| opc1  |  CRn  |  CRd  |coproc |opc2 |0|  CRm  |"
    (not (and (not (equal coproc #*1010)) (not (equal coproc #*1011))))
  (CDP{<c>} <coproc> <opc1> <CRd> <CRn> CRm> {<opc2>})
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: CDP2
;;;
;;; Coprocessor Data Processing tells a coprocessor to perform an
;;; operation that is independent of ARM core registers and memory. If
;;; no coprocessor can execute the instruction, an Undefined
;;; Instruction exception is generated.  This is a generic coprocessor
;;; instruction. Some of the fields have no functionality defined by
;;; the architecture and are free for use by the coprocessor
;;; instruction set designer. These are the opc1, opc2, CRd, CRn, and
;;; CRm fields.  However, coprocessors CP8-CP15 are reserved for use
;;; by ARM, and this manual defines the valid CDP and CDP2
;;; instructions when coproc is in the range p8-p15
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "|1 1 1 1|1 1 1 0| opc1  |  CRn  |  CRd  |coproc |opc2 |0|  CRm  |"
    (not (and (not (equal coproc #*1010)) (not (equal coproc #*1011))))
  (CDP2 <coproc> <opc1> <CRd> <CRn> CRm> {<opc2>})
  ())

;;; CLREX
;;;
;;; Clear-Exclusive clears the local record of the executing processor
;;; that an address has had a request for an exclusive access.
(define-instruction
    "|1 1 1 1|0 1 0 1 0 1 1 1|1|1 1 1|1 1 1 1|0 0 0 0|0 0 0 1|1 1 1 1|"
    t
  (CLREX))

;;; CLZ 
;;;
;;; Count Leading Zeros returns the number of binary zero bits before
;;; the first binary one bit in a value.
(define-instruction
    "| cond  |0 0 0 1 0 1 1 0|1 1 1 1|  Rd   |1 1 1 1|0 0 0 1|  Rm   |"
    t
  (CLZ{<c>} <Rd> <Rm>))

;;; CMN (immediate)
;;;
;;; Compare Negative (immediate) adds a register value and an
;;; immediate value. It updates the condition flags based on the
;;; result, and discards the result.
(define-instruction
    "| cond  |0 0|1|1 0 1 1|1|  Rn   |0 0 0 0|         imm12         |"
    t
  (CMN{<c>} Rn <imm12>))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;; Instruction: LDR (literal)
;; ;;;
;; ;;; Load Register (literal) calculates an address from the PC value
;; ;;; and an immediate offset, loads a word from memory, and writes it
;; ;;; to a register.
;; (define-instruction
;;     ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
;;     ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
;;     "| cond  |0 1 0|1|U|0|0|1|1 1 1 1|  Rt   |         imm12         |"
;;     t
;;     (LDR{<c>} <Rt> (PC +/-<imm>))
;;   ())
